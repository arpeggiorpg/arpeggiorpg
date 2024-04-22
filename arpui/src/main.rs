#![allow(non_snake_case)]

use arptypes::{
  multitenant::{self, GameAndMetadata, RPIGameRequest, Role}, Game, Scene, SceneID, SerializedGame
};
use dioxus::prelude::*;
use log::{error, info, LevelFilter};
use uuid::Uuid;

mod rpi;
use rpi::{auth_token, list_games, Connector, AUTH_TOKEN};

use crate::rpi::{send_request, use_ws};

#[derive(Clone, Routable, Debug, PartialEq)]
#[rustfmt::skip]
enum Route {
  #[layout(Layout)]
    #[route("/")]
    GameListPage,
    #[route("/gm/:id")]
    GMGamePage { id: Uuid },
    #[route("/player/:id")]
    PlayerGamePage { id: Uuid },
}

fn main() {
  // Init debug
  dioxus_logger::init(LevelFilter::Trace).expect("failed to init logger");
  console_error_panic_hook::set_once();

  launch(App);
}

fn App() -> Element {
  rsx! { Router::<Route> {} }
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
          button {
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
          a {
            href: "/{profile.role.to_string().to_lowercase()}/{profile.game_id}",
            "{metadata.name} (as {profile.profile_name})"
          }
        }
      }
    }
    h1 { "You are a Player of these games" }
    ul {
      for (profile, metadata) in player_games {
        li {
          a {
            href: "/{profile.role.to_string().to_lowercase()}/{profile.game_id}",
            "{metadata.name} (as {profile.profile_name})"
          }
        }
      }
    }
  }
}

#[component]
fn GMGamePage(id: Uuid) -> Element {
  rsx! {
    "id: {id:?}"
    Connector {
      role: Role::GM, game_id: id,
      GMGame {}
    }
  }
}

#[component]
fn PlayerGamePage(id: Uuid) -> Element {
  rsx! {
    "id: {id:?}"
    Connector {
      role: Role::Player, game_id: id,
      PlayerGame {}
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
fn PlayerGame() -> Element {
  let ws = use_ws();
  let future: Resource<Result<SerializedGame, anyhow::Error>> = use_resource(move || async move {
    info!("GMGetGame!!!!!!!!");
    let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
    let game = response.game;
    *GAME.write() = Game::from_serialized_game(game.clone());
    *GAME_NAME.write() = response.metadata.name.clone();
    Ok(game)
  });

  let mut current_scene_id = use_signal(|| SceneID(uuid::Uuid::default()));
  let current_scene = use_signal(|| {
    if current_scene_id() != SceneID(uuid::Uuid::default()) {
      Some(GAME().get_scene(current_scene_id()).expect("no scene found!?").clone())
    } else {
      None
    }
  });

  match &*future.read_unchecked() {
    Some(Ok(game)) => {
      info!("got a SUCCESSFUL future {:?}", game);
      let scenes = &game.scenes;
      rsx! {
        h1 { "A game! {GAME_NAME}" }
        ul {
          for scene in scenes {
            button {
              onclick: move |_| {*current_scene_id.write() = scene.id},
              "Scene: {scene.name}"
            }
          }
        }
        if let Some(current_scene) = current_scene() {
          SceneView {scene: current_scene.clone()}
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
fn SceneView(scene: Scene) -> Element {
  rsx! {
    h2 {
      "{scene.name}"
    }
    div {
      "A cool scene!"
    }
  }

}
