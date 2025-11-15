use std::collections::VecDeque;

use arptypes::{
    GameLog, Item, Note, PlayerCommand, PlayerID, SceneID, SerializedCreature,
    SerializedPlayerGame,
    multitenant::{GameID, GameIndex, PlayerGameAndMetadata, RPIGameRequest, Role},
};
use dioxus::prelude::*;

use foldertree::FolderPath;
use tracing::{error, info};

use crate::{
    PLAYER_SPEC, PlayerSpec,
    chat::PlayerChat,
    components::{
        creature::CreatureCard,
        modal::Modal,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    },
    grid::{CreatureMenuAction, SceneGrid},
    rpi::{Connector, send_request, use_ws},
};

pub static GAME: GlobalSignal<SerializedPlayerGame> = Signal::global(|| Default::default());
pub static GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(|| String::new());

#[component]
pub fn PlayerGamePage(id: GameID, player_id: PlayerID) -> Element {
    use_effect({
        let player_id = player_id.clone();
        move || *PLAYER_SPEC.write() = Some(PlayerSpec::Player(player_id.clone()))
    });
    rsx! {
      Connector {
        role: Role::Player,
        game_id: id,
        game_signal: None,
        player_game_signal: Some(GAME.resolve()),
        game_logs_signal: GAME_LOGS.resolve(),

        GameLoader { player_id }
      }
    }
}

#[component]
fn GameLoader(player_id: PlayerID) -> Element {
    let ws = use_ws();
    let future: Resource<anyhow::Result<SerializedPlayerGame>> = use_resource(move || async move {
        info!("fetching game state for player view");
        let response =
            send_request::<PlayerGameAndMetadata>(RPIGameRequest::PlayerGetGame, ws).await?;
        *GAME.write() = response.game.clone();
        *GAME_LOGS.write() = response.logs;
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(response.game)
    });

    match &*future.read_unchecked() {
        Some(Ok(game)) => {
            let scene_id = game.players.get(&player_id).and_then(|p| p.scene);
            rsx! {
              Shell {
                player_id: player_id.clone(),
                scene_id
              }
            }
        }
        Some(Err(err)) => {
            error!("Player view failed to load game state: {:?}", err);
            rsx! { div { "Unable to load player view." } }
        }
        None => {
            rsx! { div { "Loading player view..." } }
        }
    }
}

#[component]
fn Shell(player_id: PlayerID, scene_id: Option<SceneID>) -> Element {
    let tabs = rsx! {Tabs {
        class: "player-view-tabs".to_string(),
        default_value: "creatures".to_string(),
        TabList {
            TabTrigger { value: "creatures".to_string(), index: 0usize, "Creatures" }
            TabTrigger { value: "notes".to_string(), index: 1usize, "Notes" }
        }
        TabContent { index: 0usize, value: "creatures".to_string(),
            Creatures { player_id: player_id.clone() }
        }
        TabContent { index: 1usize, value: "notes".to_string(),
            Notes { player_id: player_id.clone() }
        }
    }};
    let chat = rsx! {
        PlayerChat { player_id: player_id.clone() }
    };
    let game = GAME();

    let get_creature_actions = move |creature_id| {
        if let Some(player) = game.players.get(&player_id) {
            if player.creatures.contains(&creature_id) {
                return vec![CreatureMenuAction::PlayerWalk];
            }
        }
        vec![]
    };

    rsx! {
      div {
        class: "player-view-shell flex w-full",
        div {
          class: "player-view-shell__main grow",
            SceneGrid { scene: game.active_scene, get_creature_actions: get_creature_actions }
        }
        div {
          class: "player-view-shell__sidebar w-96",
          SplitPane {
            direction: SplitDirection::Vertical,
            first: tabs,
            second: chat
          }
        }
      }
    }
}

#[component]
fn Creatures(player_id: PlayerID) -> Element {
    let game = GAME.read();

    if let Some(player) = game.players.get(&player_id) {
        if player.creatures.is_empty() {
            rsx! {
                div { class: "player-view-tab player-view-tab--creatures",
                    p { "Overview for {player_id}" }
                    p { "You have no creatures in your control yet." }
                }
            }
        } else {
            rsx! {
                div { class: "player-view-tab player-view-tab--creatures",
                    p { "Overview for {player_id}" }
                    h3 { "Your Creatures:" }
                    div {
                        class: "space-y-3",
                        for creature_id in {
                            let mut sorted_creatures: Vec<_> = player.creatures.iter().collect();
                            sorted_creatures.sort_by(|&id_a, &id_b| {
                                let name_a = game.creatures.get(id_a).map(|c| &c.name);
                                let name_b = game.creatures.get(id_b).map(|c| &c.name);
                                name_a.cmp(&name_b)
                            });
                            sorted_creatures
                        } {
                            if let Some(creature) = game.creatures.get(creature_id) {
                                div { key: "{creature_id}",
                                    CreatureCard { creature: creature.clone() }
                                    div { class: "ml-4 mt-2",
                                        CollapsibleInventory { creature: creature.clone() }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    } else {
        rsx! {
            div { class: "player-view-tab player-view-tab--creatures",
                p { "Player {player_id} not found" }
            }
        }
    }
}

#[component]
fn Notes(player_id: PlayerID) -> Element {
    let mut draft_content = use_signal(|| String::new());
    let ws = use_ws();

    let game = GAME.read();
    let existing_note = game.notes.get("Scratch").cloned();

    // Initialize draft content from existing note on first load
    use_effect({
        let existing_note = existing_note.clone();
        move || {
            if let Some(ref note) = existing_note.clone() {
                draft_content.set(note.content.clone());
            }
        }
    });

    let has_existing_note = existing_note.is_some();
    let mut save_action = use_action({
        move |content: String| async move {
            if content.trim().is_empty() {
                return Ok(());
            }

            let note = Note {
                name: "Scratch".to_string(),
                content,
            };
            let note_path: FolderPath = vec!["Notes".to_string()].into();
            let command = if has_existing_note {
                PlayerCommand::EditNote {
                    path: note_path,
                    original_name: "Scratch".to_string(),
                    note,
                }
            } else {
                PlayerCommand::CreateNote {
                    path: note_path,
                    note,
                }
            };

            let request = RPIGameRequest::PlayerCommand { command };

            send_request::<()>(request, ws).await
        }
    });

    let current_content = draft_content();
    let original_content = existing_note
        .as_ref()
        .map(|n| n.content.as_str())
        .unwrap_or("");
    let has_changes = current_content != original_content;
    let is_saving = save_action.pending();

    rsx! {
        div { class: "player-view-tab player-view-tab--notes flex flex-col h-full",
            div { class: "flex justify-between items-center mb-4 p-2 border-b",
                div { class: "flex flex-col",
                    span { class: "text-xs text-gray-500", "Players/Notes" }
                    span { class: "font-semibold", "Scratch" }
                }
                button {
                    class: if has_changes && !is_saving {
                        "px-3 py-1 bg-blue-500 text-white rounded hover:bg-blue-600"
                    } else {
                        "px-3 py-1 bg-gray-300 text-gray-500 rounded cursor-not-allowed"
                    },
                    disabled: !has_changes || is_saving,
                    onclick: move |_| {
                        let content = draft_content();
                        save_action.call(content);
                    },
                    if is_saving {
                        "Saving..."
                    } else {
                        "Save"
                    }
                }
            }
            textarea {
                class: "flex-1 w-full p-3 border border-gray-300 rounded resize-none font-mono text-sm",
                placeholder: "Enter your notes here...",
                value: "{current_content}",
                oninput: move |evt| {
                    draft_content.set(evt.value());
                }
            }
        }
    }
}

#[component]
fn CollapsibleInventory(creature: SerializedCreature) -> Element {
    let mut is_expanded = use_signal(|| false);

    rsx! {
        div { class: "border border-gray-200 rounded-lg mt-2",
            div {
                class: "bg-gray-50 px-3 py-2 cursor-pointer hover:bg-gray-100 rounded-t-lg",
                onclick: move |_| is_expanded.toggle(),
                div { class: "flex items-center justify-between",
                    span { class: "font-medium text-sm", "Inventory" }
                    span { class: "text-xs text-gray-500",
                        if is_expanded() { "▼" } else { "▶" }
                    }
                }
            }
            if is_expanded() {
                div { class: "p-3 border-t border-gray-200",
                    CreatureInventory { creature }
                }
            }
        }
    }
}

#[component]
fn CreatureInventory(creature: SerializedCreature) -> Element {
    let game = GAME.read();

    // Get items from creature's inventory
    let mut inventory_items: Vec<(Item, u64)> = creature
        .inventory
        .iter()
        .filter_map(|(item_id, &count)| {
            if count > 0 {
                game.items.get(item_id).map(|item| (item.clone(), count))
            } else {
                None
            }
        })
        .collect();

    // Sort by item name for deterministic rendering
    inventory_items.sort_by(|(item_a, _), (item_b, _)| item_a.name.cmp(&item_b.name));

    if inventory_items.is_empty() {
        rsx! {
            div { class: "text-gray-500 text-sm italic",
                "No items in inventory"
            }
        }
    } else {
        rsx! {
            div { class: "space-y-2",
                for (item, count) in inventory_items {
                    div {
                        key: "{item.id}",
                        class: "flex items-center justify-between py-2 px-3 bg-white border border-gray-200 rounded",
                        div { class: "flex-1 text-sm font-medium", "{item.name}" }
                        div { class: "flex items-center space-x-2",
                            span {
                                class: "inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-blue-100 text-blue-800",
                                "{count}"
                            }
                            InventoryItemMenu { creature: creature.clone(), item: item.clone(), count }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn InventoryItemMenu(creature: SerializedCreature, item: Item, count: u64) -> Element {
    let mut show_menu = use_signal(|| false);
    let mut show_give_modal = use_signal(|| false);
    let mut show_drop_modal = use_signal(|| false);

    rsx! {
        div { class: "relative",
            button {
                class: "p-1 text-gray-400 hover:text-gray-600 rounded",
                onclick: move |_| show_menu.toggle(),
                "⋮"
            }
            if show_menu() {
                div {
                    class: "absolute right-0 mt-2 w-32 bg-white border border-gray-200 rounded-md shadow-lg z-10",
                    div { class: "py-1",
                        button {
                            class: "block w-full px-4 py-2 text-sm text-left text-gray-700 hover:bg-gray-100",
                            onclick: move |_| {
                                show_menu.set(false);
                                show_give_modal.set(true);
                            },
                            "Give"
                        }
                        button {
                            class: "block w-full px-4 py-2 text-sm text-left text-gray-700 hover:bg-gray-100",
                            onclick: move |_| {
                                show_menu.set(false);
                                show_drop_modal.set(true);
                            },
                            "Drop"
                        }
                    }
                }
            }
        }

        if show_give_modal() {
            GiveItemModal {
                giver: creature.clone(),
                item: item.clone(),
                count,
                on_close: move || show_give_modal.set(false)
            }
        }

        if show_drop_modal() {
            DropItemModal {
                creature: creature.clone(),
                item: item.clone(),
                count,
                on_close: move || show_drop_modal.set(false)
            }
        }
    }
}

#[component]
fn DropItemModal(
    creature: SerializedCreature,
    item: Item,
    count: u64,
    on_close: EventHandler<()>,
) -> Element {
    let mut drop_count = use_signal(|| 1u64);
    let mut drop_action = use_action({
        let ws = use_ws();
        let creature_id = creature.id.clone();
        let item_id = item.id.clone();
        move || async move {
            let request = RPIGameRequest::PlayerCommand {
                command: arptypes::PlayerCommand::DropItem {
                    creature_id,
                    item_id,
                    count: drop_count(),
                },
            };

            send_request::<()>(request, ws).await
        }
    });

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            div { class: "mb-4",
                h3 { class: "text-lg font-semibold", "Drop {item.name}" }
                p { class: "text-sm text-gray-600 mt-2",
                    "You have {count} of this item. How many would you like to drop?"
                }
            }

            div { class: "mb-4",
                label { class: "block text-sm font-medium text-gray-700 mb-2",
                    "Count to drop:"
                }
                input {
                    class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                    r#type: "number",
                    min: "1",
                    max: "{count}",
                    value: "{drop_count()}",
                    oninput: move |evt| {
                        if let Ok(val) = evt.value().parse::<u64>() {
                            if val <= count {
                                drop_count.set(val);
                            }
                        }
                    }
                }
            }

            div { class: "flex justify-end space-x-3",
                button {
                    class: "px-4 py-2 text-gray-600 hover:text-gray-800",
                    onclick: move |_| on_close.call(()),
                    "Cancel"
                }
                button {
                    class: "px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700 disabled:opacity-50",
                    disabled: drop_count() == 0 || drop_count() > count || drop_action.pending(),
                    onclick: move |_| drop_action.call(),
                    if drop_action.pending() {
                        "Dropping..."
                    } else {
                        "Drop"
                    }
                }
            }
        }
    }
}

#[component]
fn GiveItemModal(
    giver: SerializedCreature,
    item: Item,
    count: u64,
    on_close: EventHandler<()>,
) -> Element {
    let mut give_count = use_signal(|| 1u64);
    let mut selected_recipient = use_signal(|| None::<arptypes::CreatureID>);
    let mut give_action = use_action({
        let ws = use_ws();
        let giver_id = giver.id.clone();
        let item_id = item.id.clone();
        move || async move {
            let Some(recipient_id) = selected_recipient() else {
                return Ok(());
            };
            info!(?recipient_id, count = give_count(), "Async GIVING!");
            let request = RPIGameRequest::PlayerCommand {
                command: arptypes::PlayerCommand::GiveItem {
                    from_creature_id: giver_id,
                    to_creature_id: recipient_id,
                    item_id,
                    count: give_count(),
                },
            };

            send_request::<()>(request, ws).await
        }
    });

    // Get other creatures in the scene that are visible to the player
    let game = GAME.read();
    let available_recipients: Vec<SerializedCreature> = {
        // Find which player owns this creature

        if let Some(scene) = &game.active_scene {
            scene
                .creatures
                .iter()
                .filter_map(|(cid, (_, visibility))| {
                    if *cid != giver.id && *visibility == arptypes::Visibility::AllPlayers {
                        game.creatures.get(cid).cloned()
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    };

    rsx! {
        Modal {
            open: true,
            on_close: move |_| on_close.call(()),
            class: "p-6",
            div { class: "mb-4",
                h3 { class: "text-lg font-semibold", "Give {item.name}" }
                p { class: "text-sm text-gray-600 mt-2",
                    "You have {count} of this item."
                }
            }

            if available_recipients.is_empty() {
                div { class: "text-center py-4",
                    p { class: "text-gray-500", "No other creatures available in this scene to give items to." }
                    button {
                        class: "mt-4 px-4 py-2 bg-gray-300 text-gray-700 rounded hover:bg-gray-400",
                        onclick: move |_| on_close.call(()),
                        "Close"
                    }
                }
            } else {
                div { class: "space-y-4",
                    div {
                        label { class: "block text-sm font-medium text-gray-700 mb-2",
                            "Give to:"
                        }
                        select {
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            onchange: move |evt| {
                                if let Ok(creature_id) = evt.value().parse::<arptypes::CreatureID>() {
                                    selected_recipient.set(Some(creature_id));
                                }
                            },
                            option { value: "", "Select a creature..." }
                            for recipient in available_recipients.iter() {
                                option {
                                    value: "{recipient.id}",
                                    "{recipient.name}"
                                }
                            }
                        }
                    }

                    div {
                        label { class: "block text-sm font-medium text-gray-700 mb-2",
                            "Count to give:"
                        }
                        input {
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            r#type: "number",
                            min: "1",
                            max: "{count}",
                            value: "{give_count()}",
                            oninput: move |evt| {
                                if let Ok(val) = evt.value().parse::<u64>() {
                                    if val <= count {
                                        give_count.set(val);
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "flex justify-end space-x-3 mt-6",
                    button {
                        class: "px-4 py-2 text-gray-600 hover:text-gray-800",
                        onclick: move |_| on_close.call(()),
                        "Cancel"
                    }
                    button {
                        class: "px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50",
                        disabled: selected_recipient().is_none() || give_count() == 0 || give_count() > count || give_action.pending(),
                        onclick: move |_| {
                            give_action.call()
                        },
                        if give_action.pending() {
                            "Giving..."
                        } else {
                            "Give"
                        }
                    }
                }
            }
        }
    }
}
