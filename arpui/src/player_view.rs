use arptypes::{
    Item, Note, PlayerCommand, PlayerID, SceneID, SerializedCreature, SerializedPlayerGame,
    multitenant::{GameID, InvitationID, PlayerGameAndMetadata, RPIGameRequest, Role},
};
use dioxus::prelude::*;

use foldertree::FolderPath;
use tracing::{error, info};

use crate::{
    GAME_LOGS, GAME_NAME, GAME_SOURCE, GameSource,
    Route,
    chat::PlayerChat,
    components::{
        button::{Button, ButtonVariant},
        creature::CreatureCard,
        modal::Modal,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    },
    grid::{CreatureMenuAction, SceneGrid},
    rpi::{self, Connector, InvitationCheck, send_request, use_ws},
};

#[derive(Clone, Copy)]
struct PlayerGameContext(Memo<SerializedPlayerGame>);

fn use_player_game() -> SerializedPlayerGame {
    let game = use_context::<PlayerGameContext>().0;
    game()
}

#[component]
pub fn PlayerGamePage(id: GameID, player_id: PlayerID) -> Element {
    rsx! {
      Connector {
        role: Role::Player,
        game_id: id,
        player_id: Some(player_id.clone()),

        GameLoader { player_id }
      }
    }
}

#[component]
fn GameLoader(player_id: PlayerID) -> Element {
    let ws = use_ws();
    let player_id_for_load = player_id.clone();
    let future: Resource<anyhow::Result<SerializedPlayerGame>> = use_resource(move || {
        let player_id_for_load = player_id_for_load.clone();
        async move {
            info!("fetching game state for player view");
            let response =
                send_request::<PlayerGameAndMetadata>(RPIGameRequest::PlayerGetGame, ws).await?;
            *GAME_SOURCE.write() = GameSource::Player {
                player_id: player_id_for_load.clone(),
                game: response.game.clone(),
            };
            *GAME_LOGS.write() = response.logs;
            *GAME_NAME.write() = response.metadata.name.clone();
            Ok(response.game)
        }
    });

    match &*future.read_unchecked() {
        Some(Ok(game)) => {
            let scene_id = game.players.get(&player_id).and_then(|p| p.scene);
            rsx! {
                PlayerGameProvider {
                    Shell {
                        player_id: player_id.clone(),
                        scene_id
                    }
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
fn PlayerGameProvider(children: Element) -> Element {
    let game = use_memo(move || match GAME_SOURCE() {
        GameSource::Player { game, .. } => game,
        GameSource::GM(_) => {
            panic!("Player game context used while current game source is GM")
        }
    });
    use_context_provider(move || PlayerGameContext(game));
    children
}

#[component]
fn Shell(player_id: PlayerID, scene_id: Option<SceneID>) -> Element {
    let tabs = rsx! {Tabs {
        class: "player-view-tabs h-full min-h-0 flex flex-col overflow-hidden".to_string(),
        default_value: "creatures".to_string(),
        TabList {
            TabTrigger { value: "creatures".to_string(), index: 0usize, "Creatures" }
            TabTrigger { value: "notes".to_string(), index: 1usize, "Notes" }
        }
        TabContent {
            class: "h-full min-h-0 overflow-hidden".to_string(),
            index: 0usize,
            value: "creatures".to_string(),
            div {
                class: "h-full min-h-0 overflow-y-auto p-4",
                Creatures { player_id: player_id.clone() }
            }
        }
        TabContent {
            class: "h-full min-h-0 overflow-hidden".to_string(),
            index: 1usize,
            value: "notes".to_string(),
            div {
                class: "h-full min-h-0 overflow-y-auto p-4",
                Notes { player_id: player_id.clone() }
            }
        }
    }};
    let chat = rsx! {
        PlayerChat {}
    };
    let game = use_player_game();

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
        class: "player-view-shell flex h-full min-h-0 w-full overflow-hidden",
        div {
          class: "player-view-shell__main grow min-h-0 min-w-0",
            SceneGrid {
                scene: game.active_scene,
                get_creature_actions: get_creature_actions,
            }
        }
        div {
          class: "player-view-shell__sidebar w-[30rem] h-full min-h-0 overflow-hidden border-l border-gray-200 bg-white flex flex-col",
          style: "min-height: min(800px, 100%);",
          SplitPane {
            direction: SplitDirection::Vertical,
            initial_size: 70.0,
            min_size: 35.0,
            max_size: 90.0,
            first: tabs,
            second: rsx! {
                div {
                    class: "h-full min-h-0 p-4",
                    {chat}
                }
            },
          }
        }
      }
    }
}

#[component]
fn Creatures(player_id: PlayerID) -> Element {
    let game = use_player_game();

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

    let game = use_player_game();
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
    let game = use_player_game();

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
    let game = use_player_game();
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

#[component]
pub fn AcceptInvitationPage(game_id: GameID, invitation_id: InvitationID) -> Element {
    let mut profile_name = use_signal(|| String::new());
    let mut accept_error: Signal<Option<String>> = use_signal(|| None);
    let navigator = navigator();

    let check =
        use_resource(move || async move { rpi::check_invitation(game_id, invitation_id).await });

    let mut accept_action = use_action(move |name: String| {
        let navigator = navigator.clone();
        async move {
            match rpi::accept_invitation(game_id, invitation_id, name).await {
                Ok(()) => {
                    navigator.push(Route::GameListPage);
                }
                Err(e) => {
                    error!(?e, "Failed to accept invitation");
                    accept_error.set(Some("Something went wrong. Please try again.".to_string()));
                }
            }
            Ok::<(), anyhow::Error>(())
        }
    });

    let mut do_accept = move || {
        let name = profile_name().trim().to_string();
        if !name.is_empty() {
            accept_error.set(None);
            accept_action.call(name);
        }
    };

    match &*check.read_unchecked() {
        None => rsx! {
            div {
                class: "flex h-full items-center justify-center",
                p { class: "text-gray-500", "Checking invitation..." }
            }
        },
        Some(Err(e)) => rsx! {
            div {
                class: "flex h-full items-center justify-center",
                p { class: "text-red-600", "Error checking invitation: {e}" }
            }
        },
        Some(Ok(InvitationCheck {
            invitation_valid: false,
            ..
        })) => rsx! {
            div {
                class: "flex h-full items-center justify-center",
                div {
                    class: "bg-white rounded-lg shadow-md p-6 text-center",
                    p { class: "text-gray-700", "Sorry, that invitation doesn't seem to exist." }
                    Link {
                        to: Route::GameListPage {},
                        class: "text-blue-700 text-sm mt-4 inline-block",
                        "Back to Game List"
                    }
                }
            }
        },
        Some(Ok(
            check @ InvitationCheck {
                already_member: true,
                ..
            },
        )) => {
            let name = check
                .game_name
                .clone()
                .unwrap_or_else(|| "this game".to_string());
            let game_link =
                check
                    .member_profile_name
                    .as_ref()
                    .map(|profile_name| Route::PlayerGamePage {
                        id: game_id,
                        player_id: profile_name.clone(),
                    });
            rsx! {
                div {
                    class: "flex h-full items-center justify-center",
                    div {
                        class: "bg-white rounded-lg shadow-md p-6 flex flex-col gap-4 text-center",
                        h2 {
                            class: "text-lg font-semibold text-gray-800",
                            "Already a member"
                        }
                        p {
                            class: "text-sm text-gray-600",
                            "You're already a member of {name}."
                        }
                        if let Some(route) = game_link {
                            Link {
                                to: route,
                                class: "text-blue-700 text-sm",
                                "Go to {name} →"
                            }
                        } else {
                            Link {
                                to: Route::GameListPage {},
                                class: "text-blue-700 text-sm",
                                "Go to Game List"
                            }
                        }
                    }
                }
            }
        }
        Some(Ok(
            check @ InvitationCheck {
                invitation_valid: true,
                already_member: false,
                ..
            },
        )) => {
            let name = check
                .game_name
                .clone()
                .unwrap_or_else(|| "a game".to_string());
            rsx! {
                div {
                    class: "flex h-full items-center justify-center",
                    div {
                        class: "bg-white rounded-lg shadow-md p-6 flex flex-col gap-4",
                        h2 {
                            class: "text-lg font-semibold text-gray-800",
                            "You've been invited to {name}!"
                        }
                        p {
                            class: "text-sm text-gray-600",
                            "Enter a name to join as a player."
                        }
                        input {
                            class: "border rounded px-3 py-2 w-full",
                            r#type: "text",
                            placeholder: "Your player name",
                            value: "{profile_name}",
                            autofocus: true,
                            oninput: move |evt| profile_name.set(evt.value()),
                            onkeydown: move |evt| {
                                if evt.key() == Key::Enter && !accept_action.pending() {
                                    do_accept();
                                }
                            },
                        }
                        if let Some(err) = accept_error() {
                            p {
                                class: "text-red-600 text-sm",
                                "{err}"
                            }
                        }
                        div {
                            class: "flex justify-end gap-2",
                            Link {
                                to: Route::GameListPage {},
                                class: "text-sm text-gray-500 py-2 px-3",
                                "Cancel"
                            }
                            Button {
                                variant: ButtonVariant::Primary,
                                disabled: accept_action.pending() || profile_name().trim().is_empty(),
                                onclick: move |_| do_accept(),
                                if accept_action.pending() {
                                    "Joining..."
                                } else {
                                    "Join as a Player"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
