#![allow(non_snake_case)]

use arptypes::multitenant::{self, Role};
use dioxus::prelude::*;
use log::{info, LevelFilter};
use uuid::Uuid;

mod rpi;
use rpi::{auth_token, list_games, AUTH_TOKEN};

#[derive(Clone, Routable, Debug, PartialEq)]
#[rustfmt::skip]
enum Route {
  #[layout(Layout)]
    #[route("/")]
    GameList,
    #[route("/gm/:id")]
    GMGame { id: Uuid },
    #[route("/player/:id")]
    PlayerGame { id: Uuid },
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
          Link { to: Route::GameList {}, "ArpeggioRPG" }
        }
        div {
          class: "rightNavThing",
          Link { to: Route::GameList {}, "Game List"}
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
fn GameList() -> Element {
  let list = use_resource(move || list_games());

  match &*list.read_unchecked() {
    Some(Ok(list)) => rsx! {
      GameListList {list: list.clone()}
    },
    Some(Err(e)) => rsx! { "Error! {e:?}" },
    None => rsx! {"Loading games!"},
  }
}

#[component]
fn GameListList(list: multitenant::GameList) -> Element {
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
fn GMGame(id: Uuid) -> Element {
  rsx! { "id: {id:?}"}
}

#[component]
fn PlayerGame(id: Uuid) -> Element {
  rsx! { "id: {id:?}"}
}
