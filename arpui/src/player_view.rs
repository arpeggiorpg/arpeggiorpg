use arptypes::{
  multitenant::{GameAndMetadata, GameID, RPIGameRequest, Role},
  Game, PlayerID, SceneID,
};
use dioxus::prelude::*;
use tracing::{error, info};

use crate::rpi::{send_request, use_ws, Connector};

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
  let player_label = player_id.to_string();
  let active_scene = scene_id.map(|sid| sid.to_string());

  rsx! {
    div {
      class: "player-view-shell",
      div {
        class: "player-view-shell__header",
        h1 { "Player View" }
        p { "Logged in as {player_label}" }
      }
      div {
        class: "player-view-shell__body",
        section {
          class: "player-view-shell__map",
          h2 { "Map" }
          if let Some(ref sid) = active_scene {
            p { "Active scene: {sid}" }
          } else {
            p { "Ask your GM to put you in a scene." }
          }
          div { class: "player-view-shell__map-placeholder", "Scene grid coming soon." }
        }
        section {
          class: "player-view-shell__sidebar",
          h2 { "Details" }
          ul {
            li { "Creatures panel coming soon." }
            li { "Notes panel coming soon." }
          }
        }
      }
      section {
        class: "player-view-shell__chat",
        h2 { "Chat" }
        p { "Player chat will appear here." }
      }
    }
  }
}
