use arptypes::{
    multitenant::{GameAndMetadata, GameID, RPIGameRequest, Role},
    Game, PlayerID, SceneID,
};
use dioxus::prelude::*;
use tracing::{error, info};

use crate::{
    chat::PlayerChat, components::{
        creature::CreatureCard,
        split_pane::{SplitDirection, SplitPane},
        tabs::{TabContent, TabList, TabTrigger, Tabs},
    }, rpi::{Connector, send_request, use_ws}
};

pub static GAME: GlobalSignal<Game> = Signal::global(|| Default::default());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(|| String::new());

#[component]
pub fn PlayerGamePage(id: GameID, player_id: PlayerID) -> Element {
    rsx! {
      Connector {
        role: Role::Player,
        game_id: id,
        PlayerGameView { player_id }
      }
    }
}

#[component]
fn PlayerGameView(player_id: PlayerID) -> Element {
    let ws = use_ws();
    let future: Resource<Result<Game, anyhow::Error>> = use_resource(move || async move {
        info!("fetching game state for player view");
        let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
        let game = Game::from_serialized_game(response.game);
        *GAME.write() = game.clone();
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(game)
    });

    match &*future.read_unchecked() {
        Some(Ok(game)) => {
            let scene_id = game.players.get(&player_id).and_then(|p| p.scene);
            rsx! {
              PlayerGameScaffold {
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
fn PlayerGameScaffold(player_id: PlayerID, scene_id: Option<SceneID>) -> Element {
    let active_scene = scene_id.map(|sid| sid.to_string());

    let tabs = rsx! {Tabs {
        class: "player-view-tabs".to_string(),
        default_value: "creatures".to_string(),
        TabList {
            TabTrigger { value: "creatures".to_string(), index: 0usize, "Creatures" }
            TabTrigger { value: "notes".to_string(), index: 1usize, "Notes" }
        }
        TabContent { index: 0usize, value: "creatures".to_string(),
            PlayerCreaturesTab { player_id: player_id.clone() }
        }
        TabContent { index: 1usize, value: "notes".to_string(),
            PlayerNotesTab {}
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
fn PlayerCreaturesTab(player_id: PlayerID) -> Element {
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
fn PlayerNotesTab() -> Element {
    rsx! {
        div { class: "player-view-tab player-view-tab--notes",
            p { "Player notes will appear here." }
        }
    }
}
