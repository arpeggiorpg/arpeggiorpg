#![allow(non_snake_case)]

use arptypes::{
  multitenant::{self, GameAndMetadata, GameID, RPIGameRequest, Role},
  Game, PlayerID, Scene, SceneID,
};
use dioxus::prelude::*;
use log::{error, info, LevelFilter};

mod rpi;
mod components;
use rpi::{auth_token, list_games, Connector, AUTH_TOKEN};

use crate::{components::button::{Button,ButtonVariant}, rpi::{send_request, use_ws}};

static COMPONENT_THEME_CSS: Asset = asset!("/assets/dx-components-theme.css");


#[derive(Clone, Routable, Debug)]
#[rustfmt::skip]
enum Route {
  #[layout(Layout)]
    #[route("/")]
    GameListPage,
    #[route("/gm/:id")]
    GMGamePage { id: GameID },
    #[route("/player/:id/:player_id")]
    PlayerGamePage { id: GameID, player_id: PlayerID },
}

fn main() {
  launch(App);
}

fn App() -> Element {
  rsx! {
      document::Stylesheet { href: COMPONENT_THEME_CSS }
      Router::<Route> {}
  }
}

#[component]
fn Layout() -> Element {
  // Mirror the cookie to the GlobalSignal so things can reactively respond to it changing.
  if AUTH_TOKEN() != auth_token() {
    *AUTH_TOKEN.write() = auth_token();
  }

  rsx! {
    div {
      style: "display: flex; flex-direction: column; height: 100%",
      div {
        style: "display: flex; justify-content: space-between;",
        h1 {
          Link { to: Route::GameListPage {}, "ArpeggioRPG" }
          " — {GAME_NAME}"
        }
        div {
          class: "rightNavThing",
          Link { to: Route::GameListPage {}, "Game List"}
          Button {
            variant: ButtonVariant::Ghost,
            onclick: move |_event| {
              info!("Log Off");
              *AUTH_TOKEN.write() = String::new();
              wasm_cookies::set("arpeggio-token", "", &Default::default())
            },
            "Log Off"
          }
        }
      }
      "Enter your auth token:"
      input {
        value: "{AUTH_TOKEN}",
        oninput: move |event| {
          *AUTH_TOKEN.write() = event.value();
          wasm_cookies::set("arpeggio-token", &event.value(), &Default::default())
        }
      }
      Outlet::<Route> {}
    }
  }
}

#[component]
fn GameListPage() -> Element {
  let list = use_resource(move || list_games());

  match &*list.read_unchecked() {
    Some(Ok(list)) => rsx! {
      GameList {list: list.clone()}
    },
    Some(Err(e)) => rsx! { "Error! {e:?}" },
    None => rsx! {"Loading games!"},
  }
}

#[component]
fn GameList(list: multitenant::GameList) -> Element {
  let gm_games = list.games.iter().filter(|(profile, _)| profile.role == Role::GM);
  let player_games = list.games.iter().filter(|(profile, _)| profile.role == Role::Player);

  rsx! {
    h1 { "You are GM of these games" }
    ul {
      for (profile, metadata) in gm_games {
        li {
          Link { to: Route::GMGamePage {id: profile.game_id}, "{metadata.name} (as {profile.profile_name})"}
        }
      }
    }
    h1 { "You are a Player of these games" }
    ul {
      for (profile, metadata) in player_games {
        li {
          a {
            href: "/{profile.role.to_string().to_lowercase()}/{profile.game_id}/{profile.profile_name}",
            "{metadata.name} (as {profile.profile_name})"
          }
        }
      }
    }
  }
}

#[component]
fn GMGamePage(id: GameID) -> Element {
  rsx! {
    "id: {id:?}"
    Connector {
      role: Role::GM, game_id: id,
      GMGame {}
    }
  }
}

#[component]
fn PlayerGamePage(id: GameID, player_id: PlayerID) -> Element {
  rsx! {
    "id: {id:?}"
    Connector {
      role: Role::Player, game_id: id,
      PlayerGame {player_id}
    }
  }
}

#[component]
fn GMGame() -> Element {
  rsx! {
    "Hi GM"
  }
}

pub static GAME: GlobalSignal<Game> = Signal::global(|| Default::default());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(|| String::new());

#[component]
fn PlayerGame(player_id: PlayerID) -> Element {
  let ws = use_ws();
  let future: Resource<Result<Game, anyhow::Error>> = use_resource(move || async move {
    info!("GMGetGame!!!!!!!!");
    let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
    let game = response.game;
    let game = Game::from_serialized_game(game);
    *GAME.write() = game.clone();
    *GAME_NAME.write() = response.metadata.name.clone();
    Ok(game)
  });

  match &*future.read_unchecked() {
    Some(Ok(game)) => {
      let scene_id = game.players.get(&player_id).and_then(|p| p.scene);

      rsx! {
       h1 { "A game! {GAME_NAME}" }
       if let Some(scene_id) = scene_id {
         SceneView {scene_id}
       }
      }
    }
    Some(Err(err)) => {
      error!("Got a FAILING future {:?}", err);
      rsx! { "Got a FAILING future", "{err:?}"}
    }
    None => {
      rsx! { "Future is None"}
    }
  }
}

#[component]
fn SceneView(scene_id: SceneID) -> Element {
  match GAME().get_scene(scene_id) {
    Ok(scene) => rsx! {
      h2 {
        "{scene.name}"
      }
      div {
        "A cool scene!"
      }
    },
    Err(_) => rsx! {
      div { "Couldn't find scene!" }
    },
  }
}
