#![allow(non_snake_case)]

use std::collections::VecDeque;

use arptypes::{
    Game, GameLog, PlayerID, SerializedPlayerGame,
    multitenant::{self, GameID, GameIndex, InvitationID, Role},
};
use dioxus::prelude::*;
use js_sys::encode_uri_component;
use tracing::{error, info};

mod chat;
mod components;
mod admin_view;
mod gfx;
mod gm_view;
mod grid;
mod player_view;
mod rpi;
use admin_view::AdminPage;
use gm_view::{GMGamePage, GMGameScenePage};
use player_view::{AcceptInvitationPage, PlayerGamePage};
use rpi::{AUTH_TOKEN, auth_token, list_games};
use wasm_cookies::CookieOptions;

use crate::{
    components::button::{Button, ButtonVariant},
    components::modal::Modal,
    rpi::rpi_url,
};

const GOOGLE_CLIENT_ID: &str =
    "328154234071-c7una5er0n385sdgvih81ngbkgp1l7nj.apps.googleusercontent.com";

pub static GAME_SOURCE: GlobalSignal<GameSource> = Signal::global(GameSource::default);
pub static GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(|| String::new());

#[derive(Clone, PartialEq)]
pub enum GameSource {
    GM(Game),
    Player {
        player_id: PlayerID,
        game: SerializedPlayerGame,
    },
}

impl Default for GameSource {
    fn default() -> Self {
        GameSource::Player {
            player_id: PlayerID(String::new()),
            game: Default::default(),
        }
    }
}

#[derive(Clone, Routable, Debug)]
#[rustfmt::skip]
pub(crate) enum Route {
  #[route("/auth-success?:id_token")]
  AuthSuccessPage {
    id_token: String
  },
  #[layout(AuthRequiredLayout)]
    #[route("/")]
    GameListPage,
    #[route("/admin")]
    AdminPage,
    #[route("/gm/:id")]
    GMGamePage { id: GameID },
    #[route("/gm/:id/scenes/:..scene_path")]
    GMGameScenePage { id: GameID, scene_path: Vec<String> },
    #[route("/player/:id/:player_id")]
    PlayerGamePage { id: GameID, player_id: PlayerID },
    #[route("/invitations/:game_id/:invitation_id")]
    AcceptInvitationPage { game_id: GameID, invitation_id: InvitationID },
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
          class: "flex h-screen items-center justify-center",
          div {
            class: "flex flex-col items-center gap-4",
            h1 {
              class: "text-xl font-bold text-gray-800",
              "ArpeggioRPG"
            }
            p {
              class: "text-gray-500",
              "Sign in to manage your games"
            }
            Button {
              variant: ButtonVariant::Primary,
              onclick: move |_e| {
                let target: NavigationTarget<Route> = google_oauth_url.parse().unwrap();
                navigator.push(target);
              },
              "Log in with Google"
            }
          }
        }
      } else {
        div {
          class: "flex h-screen min-h-0 flex-col overflow-hidden",
          div {
            class: "flex items-center justify-between px-4 py-2 border-b border-gray-200 bg-white",
            h1 {
              class: "font-semibold text-gray-800",
              Link { to: Route::GameListPage {}, "ArpeggioRPG" }
              span {
                class: "font-medium text-gray-500",
                " — {GAME_NAME}"
              }
            }
            div {
              class: "flex items-center gap-3",
              Link { to: Route::GameListPage {},
                class: "text-sm text-gray-600 hover:text-gray-800",
                "Game List"
              }
              Link { to: Route::AdminPage {},
                class: "text-sm text-gray-600 hover:text-gray-800",
                "Admin"
              }
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
          div {
            class: "flex-1 min-h-0 overflow-y-auto",
            Outlet::<Route> {}
          }
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
        Some(Err(e)) => rsx! {
          div {
            class: "p-6 text-red-600",
            "Error! {e:?}"
          }
        },
        None => rsx! {
          div {
            class: "p-6 text-gray-500",
            "Loading games..."
          }
        },
    }
}

#[component]
fn GameList(list: multitenant::GameList) -> Element {
    let mut show_create_modal = use_signal(|| false);

    info!(create = ?show_create_modal(), "Showing create modal?");
    let gm_games_vec: Vec<_> = list
        .games
        .iter()
        .filter(|(profile, _)| profile.role == Role::GM)
        .collect();
    let player_games_vec: Vec<_> = list
        .games
        .iter()
        .filter(|(profile, _)| profile.role == Role::Player)
        .collect();

    rsx! {
      div {
        class: "p-6 mx-auto space-y-4",
        // GM Games section
        div {
          class: "bg-white rounded-lg shadow-md p-4",
          div {
            class: "flex items-center justify-between mb-3",
            h2 {
              class: "text-lg font-semibold text-gray-800",
              "Your Games (GM)"
            }
            Button {
              variant: ButtonVariant::Primary,
              onclick: move |_| {
                info!("Create New button clicked");
                show_create_modal.set(true);
              },
              "Create New"
            }
          }
          if gm_games_vec.is_empty() {
            p {
              class: "text-sm text-gray-500 italic",
              "You haven't created any games yet."
            }
          } else {
            ul {
              class: "space-y-1",
              for (profile, metadata) in &gm_games_vec {
                li {
                  class: "px-3 py-2 rounded-md hover:bg-gray-100",
                  Link {
                    to: Route::GMGamePage {id: profile.game_id},
                    class: "text-blue-700 font-medium",
                    "{metadata.name}"
                  }
                  span {
                    class: "text-sm text-gray-500 ml-4",
                    "(as {profile.profile_name})"
                  }
                }
              }
            }
          }
        }

        // Player Games section
        div {
          class: "bg-white rounded-lg shadow-md p-4",
          h2 {
            class: "text-lg font-semibold text-gray-800 mb-3",
            "Games You Play In"
          }
          if player_games_vec.is_empty() {
            p {
              class: "text-sm text-gray-500 italic",
              "You haven't joined any games as a player yet."
            }
          } else {
            ul {
              class: "space-y-1",
              for (profile, metadata) in &player_games_vec {
                li {
                  class: "px-3 py-2 rounded-md hover:bg-gray-100",
                  a {
                    href: "/{profile.role.to_string().to_lowercase()}/{profile.game_id}/{profile.profile_name}",
                    class: "text-blue-700 font-medium",
                    "{metadata.name}"
                  }
                  span {
                    class: "text-sm text-gray-500 ml-4",
                    "(as {profile.profile_name})"
                  }
                }
              }
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
