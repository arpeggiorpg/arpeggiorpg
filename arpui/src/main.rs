#![allow(non_snake_case)]

use arptypes::{
  multitenant::{self, GameID, Role},
  PlayerID,
};
use dioxus::prelude::*;
use js_sys::encode_uri_component;
use tracing::info;

mod components;
mod player_view;
mod rpi;
use player_view::{PlayerGamePage, GAME_NAME};
use rpi::{auth_token, list_games, Connector, AUTH_TOKEN};

use crate::{
  components::button::{Button, ButtonVariant},
  rpi::rpi_url,
};

static COMPONENT_THEME_CSS: Asset = asset!("/assets/dx-components-theme.css");
const GOOGLE_CLIENT_ID: &str =
  "328154234071-c7una5er0n385sdgvih81ngbkgp1l7nj.apps.googleusercontent.com";

#[derive(Clone, Routable, Debug)]
#[rustfmt::skip]
enum Route {
  #[route("/auth-success?:id_token")]
  AuthSuccessPage {
    id_token: String
  },
  #[layout(AuthRequiredLayout)]
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
fn AuthRequiredLayout() -> Element {
  // Mirror the cookie to the GlobalSignal so things can reactively respond to it changing.
  if AUTH_TOKEN() != auth_token() {
    *AUTH_TOKEN.write() = auth_token();
  }

  let google_oauth_url = {
    let redirect_uri = format!("{}/oauth/redirect", rpi_url());
    let encoded_redirect =
      encode_uri_component(&redirect_uri).as_string().unwrap_or_else(|| redirect_uri.clone());

    format!(
      "https://accounts.google.com/o/oauth2/v2/auth?client_id={}&redirect_uri={}&response_type=code&scope=openid%20email%20profile&prompt=consent",
      GOOGLE_CLIENT_ID,
      encoded_redirect,
    )
  };

  let navigator = navigator();
  let has_auth_token = !AUTH_TOKEN().is_empty();

  rsx! {
    if !has_auth_token {
      div {
        style: "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; gap: 1rem;",
        Button {
          variant: ButtonVariant::Primary,
          onclick: move |_e| {
            let target: NavigationTarget<Route> = google_oauth_url.parse().unwrap();
            navigator.push(target);
          },
          "Log in with Google"
        }
      }
    } else {
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
                wasm_cookies::set("arpeggio-token", "", &Default::default());
              },
              "Log Off"
            }
          }
        }
        Outlet::<Route> {}
      }
    }
  }
}

#[component]
fn AuthSuccessPage(id_token: String) -> Element {
  let navigator = navigator();
  use_effect(move || {
    info!(id_token, "OAuth redirect returned id_token");
    *AUTH_TOKEN.write() = id_token.clone();
    wasm_cookies::set("arpeggio-token", &id_token, &Default::default());
    navigator.push(Route::GameListPage);
  });

  rsx! { "Completing authentication..." }
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
fn GMGame() -> Element {
  rsx! {
    "Hi GM"
  }
}
