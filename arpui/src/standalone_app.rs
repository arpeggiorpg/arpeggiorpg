use arptypes::{GMCommand, GameLog, PlayerID, protocol::GameRequest};
use dioxus::prelude::*;
use js_sys::encode_uri_component;
use tracing::info;

use crate::{
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
    },
    gm_view::GMGameView,
    player_view::PlayerGameView,
    rpi::{self, send_request, use_ws},
};

#[derive(Clone, Routable, Debug, PartialEq)]
#[rustfmt::skip]
enum Route {
    #[route("/")]
    GMPage {},
    #[route("/scenes/:..scene_path")]
    GMScenePage { scene_path: Vec<String> },
    #[route("/Player/:player_id")]
    PlayerPage { player_id: PlayerID },
}

pub fn App() -> Element {
    rsx! {
        document::Stylesheet { href: asset!("/assets/dx-components-theme.css") }
        document::Stylesheet { href: asset!("/assets/tailwind.css") }
        Router::<Route> {}
    }
}

fn standalone_websocket_url(path: &str) -> anyhow::Result<String> {
    Ok(format!("{}/ws/{path}", rpi::websocket_base_url()?))
}

#[component]
fn GMPage(#[props(default)] scene_path: Option<Vec<String>>) -> Element {
    let websocket_url = match standalone_websocket_url("GM") {
        Ok(url) => url,
        Err(error) => return rsx! { ConnectionConfigurationError { error: error.to_string() } },
    };
    let navigator = navigator();

    rsx! {
        GMGameView {
            websocket_url,
            scene_path,
            on_scene_path_change: move |scene_path: Option<Vec<String>>| {
                if let Some(scene_path) = scene_path {
                    navigator.push(Route::GMScenePage { scene_path });
                } else {
                    navigator.push(Route::GMPage {});
                }
            },
            toolbar: rsx! { AddPlayerButton {} },
        }
    }
}

#[component]
fn GMScenePage(scene_path: Vec<String>) -> Element {
    rsx! { GMPage { scene_path: Some(scene_path) } }
}

#[component]
fn PlayerPage(player_id: PlayerID) -> Element {
    let encoded_player = encode_uri_component(&player_id.0)
        .as_string()
        .unwrap_or_else(|| player_id.0.clone());
    let websocket_url = match standalone_websocket_url(&format!("Player/{encoded_player}")) {
        Ok(url) => url,
        Err(error) => return rsx! { ConnectionConfigurationError { error: error.to_string() } },
    };

    rsx! { PlayerGameView { websocket_url, player_id } }
}

#[component]
fn ConnectionConfigurationError(error: String) -> Element {
    rsx! {
        div {
            class: "flex h-screen items-center justify-center p-6 text-red-600",
            "Unable to configure the Arpeggio server connection: {error}"
        }
    }
}

#[component]
fn AddPlayerButton() -> Element {
    let ws = use_ws();
    let navigator = navigator();
    let mut open = use_signal(|| false);
    let mut player_name = use_signal(String::new);
    let mut error_message: Signal<Option<String>> = use_signal(|| None);

    let mut register_action = use_action(move |name: String| {
        let navigator = navigator.clone();
        async move {
            let player_id = PlayerID(name);
            let result = send_request::<Result<Vec<GameLog>, String>>(
                GameRequest::GMCommand {
                    command: Box::new(GMCommand::RegisterPlayer {
                        id: player_id.clone(),
                    }),
                },
                ws,
            )
            .await;
            match result {
                Ok(Ok(_)) => {
                    info!(%player_id, "Registered standalone player");
                    open.set(false);
                    navigator.push(Route::PlayerPage { player_id });
                }
                Ok(Err(error)) => error_message.set(Some(error)),
                Err(error) => error_message.set(Some(error.to_string())),
            }
            Ok::<(), anyhow::Error>(())
        }
    });

    let mut submit = move || {
        let name = player_name().trim().to_string();
        if !name.is_empty() {
            error_message.set(None);
            register_action.call(name);
        }
    };

    rsx! {
        Button {
            variant: ButtonVariant::Primary,
            onclick: move |_| {
                player_name.set(String::new());
                error_message.set(None);
                open.set(true);
            },
            "+ Player"
        }
        Modal {
            open: open(),
            on_close: move |_| open.set(false),
            class: "p-6",
            h2 { class: "text-lg font-semibold mb-4", "Add a player" }
            div {
                class: "flex flex-col gap-4",
                input {
                    class: "border rounded px-3 py-2 w-full",
                    r#type: "text",
                    placeholder: "Player name",
                    value: "{player_name}",
                    autofocus: true,
                    oninput: move |event| player_name.set(event.value()),
                    onkeydown: move |event| {
                        if event.key() == Key::Enter && !register_action.pending() {
                            submit();
                        }
                    },
                }
                if let Some(error) = error_message() {
                    p { class: "text-red-600 text-sm", "{error}" }
                }
                div {
                    class: "flex justify-end gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        onclick: move |_| open.set(false),
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        disabled: register_action.pending() || player_name().trim().is_empty(),
                        onclick: move |_| submit(),
                        if register_action.pending() { "Adding..." } else { "Add Player" }
                    }
                }
            }
        }
    }
}
