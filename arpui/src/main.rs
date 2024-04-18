#![allow(non_snake_case)]

use arptypes::multitenant::{self, Role};
use dioxus::prelude::*;
use log::{info, LevelFilter};
use reqwest_websocket::RequestBuilderExt;
use uuid::Uuid;

mod rpi;
use rpi::{auth_token, list_games, Connector, AUTH_TOKEN};

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

#[component]
fn PlayerGame() -> Element {
  rsx! { "Hi Player" }
}
