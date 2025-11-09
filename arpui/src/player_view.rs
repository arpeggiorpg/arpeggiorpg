use std::collections::VecDeque;

use arptypes::{
    multitenant::{GameAndMetadata, GameID, GameIndex, RPIGameRequest, Role},
    Game, GameLog, Note, PlayerCommand, PlayerID, SceneID,
};
use dioxus::prelude::*;

use foldertree::FolderPath;
use tracing::{error, info};

use crate::{
    chat::PlayerChat,
    components::{
        creature::CreatureCard,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    },
    rpi::{send_request, use_ws, Connector},
};

pub static GAME: GlobalSignal<Game> = Signal::global(|| Default::default());
pub static GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(|| String::new());

#[component]
pub fn PlayerGamePage(id: GameID, player_id: PlayerID) -> Element {
    rsx! {
      Connector {
        role: Role::Player,
        game_id: id,
        game_signal: GAME.resolve(),
        game_logs_signal: GAME_LOGS.resolve(),

        GameLoader { player_id }
      }
    }
}

#[component]
fn GameLoader(player_id: PlayerID) -> Element {
    let ws = use_ws();
    let future: Resource<Result<Game, anyhow::Error>> = use_resource(move || async move {
        info!("fetching game state for player view");
        let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
        let game = Game::from_serialized_game(response.game);
        *GAME.write() = game.clone();
        *GAME_LOGS.write() = response.logs;
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(game)
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
    let active_scene = scene_id.map(|sid| sid.to_string());

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

    rsx! {
      div {
        class: "player-view-shell flex w-full",
        div {
          class: "player-view-shell__main grow",
            if let Some(ref sid) = active_scene {
              p { "Active scene: {sid}" }
            } else {
              p { "Ask your GM to put you in a scene." }
            }
            div { class: "player-view-shell__map-placeholder", "Scene grid coming soon." }
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
                        for creature_id in &player.creatures {
                            if let Some(creature) = game.creatures.get(creature_id) {
                                div { key: "{creature_id}",
                                    CreatureCard { creature: creature.clone() }
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
    let player_notes_folder_path: FolderPath = vec![
        "Players".to_string(),
        player_id.0.clone(),
        "Notes".to_string(),
    ]
    .into();
    let existing_note = game
        .campaign
        .get(&player_notes_folder_path)
        .ok()
        .and_then(|notes_folder| notes_folder.notes.get("Scratch"))
        .cloned();

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
        let ws = ws.clone();
        move |content: String| {
            let ws = ws.clone();
            async move {
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
