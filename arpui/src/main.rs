#![allow(non_snake_case)]

use arptypes::{
    PlayerID,
    multitenant::{self, GameID, Role},
};
use dioxus::prelude::*;
use js_sys::encode_uri_component;
use tracing::{error, info};

mod chat;
mod components;
mod grid;
mod player_view;
mod rpi;
use player_view::{GAME_NAME, PlayerGamePage};
use rpi::{AUTH_TOKEN, auth_token, list_games};
use wasm_cookies::CookieOptions;

use crate::{
    components::button::{Button, ButtonVariant},
    components::modal::Modal,
    rpi::rpi_url,
};

const GOOGLE_CLIENT_ID: &str =
    "328154234071-c7una5er0n385sdgvih81ngbkgp1l7nj.apps.googleusercontent.com";

pub static PLAYER_SPEC: GlobalSignal<Option<PlayerSpec>> = Signal::global(|| None);

#[derive(Clone, PartialEq)]
pub enum PlayerSpec {
    GM,
    Player(PlayerID),
}

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
    dioxus_logger::init(tracing::Level::INFO).expect("failed to init logger");
    launch(App);
}

fn App() -> Element {
    let components_css = asset!("/assets/dx-components-theme.css");
    info!("What is this? {components_css:?}  display: {components_css}");
    rsx! {
        document::Stylesheet { href: components_css }
        document::Stylesheet { href: asset!("/assets/tailwind.css") }

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
        let encoded_redirect = encode_uri_component(&redirect_uri)
            .as_string()
            .unwrap_or_else(|| redirect_uri.clone());

        format!(
            "https://accounts.google.com/o/oauth2/v2/auth?client_id={}&redirect_uri={}&response_type=code&scope=openid%20email%20profile&prompt=consent",
            GOOGLE_CLIENT_ID, encoded_redirect,
        )
    };

    let navigator = navigator();
    let has_auth_token = !AUTH_TOKEN().is_empty();

    rsx! {
      if !has_auth_token {
        div {
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
          class: "flex h-full flex-col",
          div {
            class: "flex justify-between",
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
                  set_auth_token_cookie("".to_string());
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

fn set_auth_token_cookie(id_token: String) {
    *AUTH_TOKEN.write() = id_token.clone();
    let options: CookieOptions = Default::default();
    wasm_cookies::set("arpeggio-token", &id_token, &options.with_path("/"));
}

#[component]
fn AuthSuccessPage(id_token: String) -> Element {
    let navigator = navigator();
    use_effect(move || {
        info!(id_token, "OAuth redirect returned id_token");
        set_auth_token_cookie(id_token.clone());
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
    let mut show_create_modal = use_signal(|| false);

    info!(create = ?show_create_modal(), "Showing create modal?");
    let gm_games = list
        .games
        .iter()
        .filter(|(profile, _)| profile.role == Role::GM);
    let player_games = list
        .games
        .iter()
        .filter(|(profile, _)| profile.role == Role::Player);

    rsx! {
      h1 { "You are GM of these games" }
      ul {
        for (profile, metadata) in gm_games {
          li {
            Link { to: Route::GMGamePage {id: profile.game_id}, "{metadata.name} (as {profile.profile_name})"}
          }
        }
        li {
          Button {
            variant: ButtonVariant::Primary,
            onclick: move |_| {
              info!("Create New button clicked");
              show_create_modal.set(true);
            },
            "Create New"
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

      CreateGameModal {
        open: show_create_modal(),
        on_close: move |_| show_create_modal.set(false),
      }
    }
}

#[component]
fn CreateGameModal(open: bool, on_close: EventHandler<()>) -> Element {
    let mut game_name = use_signal(|| "Name of your Game".to_string());
    let mut error_msg: Signal<Option<String>> = use_signal(|| None);
    let navigator = navigator();

    info!(?open, "CreateGameModal");
    let mut create_action = use_action(move |name: String| {
        let navigator = navigator.clone();
        async move {
            match rpi::create_game(name).await {
                Ok(game_id) => {
                    on_close.call(());
                    navigator.push(Route::GMGamePage { id: game_id });
                }
                Err(e) => {
                    error!(?e, "Failed to create game");
                    error_msg.set(Some(format!("Failed to create game: {e}")));
                }
            }
            Ok::<(), anyhow::Error>(())
        }
    });

    let mut do_submit = move || {
        info!("Create game button clicked, name: {}", game_name());
        create_action.call(game_name().clone());
    };

    rsx! {
      Modal {
        open,
        on_close: move |_| on_close.call(()),
        class: "p-6",
        h2 {
          class: "text-lg font-semibold mb-4",
          "Create Game"
        }
        div {
          class: "flex flex-col gap-4",
          input {
            class: "border rounded px-3 py-2 w-full",
            r#type: "text",
            value: "{game_name}",
            autofocus: true,
            oninput: move |evt| game_name.set(evt.value()),
            onkeydown: move |evt| {
              if evt.key() == Key::Enter && !create_action.pending() {
                do_submit();
              }
            },
          }
          if let Some(err) = error_msg() {
            p {
              class: "text-red-600 text-sm",
              "{err}"
            }
          }
          div {
            class: "flex justify-end gap-2",
            Button {
              variant: ButtonVariant::Outline,
              onclick: move |_| on_close.call(()),
              "Cancel"
            }
            Button {
              variant: ButtonVariant::Primary,
              disabled: create_action.pending(),
              onclick: move |_| do_submit(),
              if create_action.pending() {
                "Creating..."
              } else {
                "Create"
              }
            }
          }
        }
      }
    }
}

#[component]
fn GMGamePage(id: GameID) -> Element {
    use_effect(move || *PLAYER_SPEC.write() = Some(PlayerSpec::GM));
    rsx! {
      "id: {id:?}"
      div { "NYI"}
    }
}

#[component]
fn GMGame() -> Element {
    rsx! {
      "Hi GM"
    }
}
