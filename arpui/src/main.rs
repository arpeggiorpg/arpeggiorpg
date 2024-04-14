#![allow(non_snake_case)]

use dioxus::prelude::*;
use log::{info, LevelFilter};
use uuid::Uuid;

mod rpi;
use rpi::{auth_token, AUTH_TOKEN, list_games};

#[derive(Clone, Routable, Debug, PartialEq)]
enum Route {
  #[route("/")]
  Main,
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
fn Main() -> Element {

  // Mirror the cookie to the GlobalSignal so things can reactively respond to it changing.
  if AUTH_TOKEN() != auth_token() {
    *AUTH_TOKEN.write() = auth_token();
  }

  rsx! {
    div { "Oh no, the cookie {AUTH_TOKEN():?} "}
    div {
      "Enter your auth token:"
      input {
        value: "{AUTH_TOKEN}",
        oninput: move |event| {
          *AUTH_TOKEN.write() = event.value();
          wasm_cookies::set("arpeggio-token", &event.value(), &Default::default())
        }
      }
    }
    GameList {}
  }
}


#[component]
fn GameList() -> Element {
  let list = use_resource(move || list_games());

  match &*list.read_unchecked() {
    Some(Ok(list)) => rsx! {
      "got a list!"
      ul {
        for (profile, metadata) in &list.games {
          li {
            a {
              href: "/{profile.role.to_string().to_lowercase()}/{profile.game_id}",
              "{profile.profile_name} @ {metadata.name} (as {profile.role})" } }
        }
      }
    },
    Some(Err(e)) => rsx! { "Error! {e:?}" },
    None => rsx! {"Loading games!"}
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
