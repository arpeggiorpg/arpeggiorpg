use arptypes::{
    GMCommand, multitenant::RPIGameRequest, CreatureID, CreatureLog, GameLog, PlayerCommand,
};
use dioxus::prelude::*;
use tracing::warn;

use crate::{
    GAME_SOURCE, GameSource,
    gm_view::GM_GAME_LOGS,
    player_view::GAME_LOGS,
    rpi::{UIRequest, send_request, use_ws},
};

#[derive(Clone, Copy, PartialEq)]
enum ChatMode {
    Player,
    GM,
}

const CHAT_HISTORY_ID: &str = "chat-history";

#[component]
pub fn PlayerChat() -> Element {
    rsx! {
        ChatPanel {
            mode: ChatMode::Player,
        }
    }
}

#[component]
pub fn GMChat() -> Element {
    rsx! {
        ChatPanel {
            mode: ChatMode::GM,
        }
    }
}

#[component]
fn ChatPanel(mode: ChatMode) -> Element {
    let ws = use_ws();
    let mut message = use_signal(|| String::new());
    let chat_logs = match mode {
        ChatMode::Player => GAME_LOGS.read(),
        ChatMode::GM => GM_GAME_LOGS.read(),
    };
    let send_message_handler = move |_| {
        async move {
            let _ = maybe_send_chat(mode, ws, message).await;
        }
    };

    let handle_keydown = {
        move |evt: KeyboardEvent| {
            async move {
                if evt.key() == dioxus::prelude::Key::Enter && !evt.modifiers().shift() {
                    let _ = maybe_send_chat(mode, ws, message).await;
                }
            }
        }
    };

    rsx! {
        section {
            class: chat_section_class(&mode),
            h2 { class: "text-lg font-semibold mb-2", "Chat" }

            div {
                class: chat_history_class(&mode),
                style: chat_history_style(&mode),
                id: "{CHAT_HISTORY_ID}",
                div { class: "p-3 space-y-2",
                    if chat_logs.is_empty() {
                        if mode == ChatMode::Player {
                            div { class: "flex items-center justify-center h-32 text-center",
                                div {
                                    p { class: "text-gray-400 text-sm mb-1", "💬" }
                                    p { class: "text-gray-500 text-sm italic", "No chat messages yet..." }
                                    p { class: "text-gray-400 text-xs", "Start a conversation!" }
                                }
                            }
                        } else {
                            div { class: "flex items-center justify-center h-32 text-center",
                                div {
                                    p { class: "text-gray-400 text-sm mb-1", "No chat messages yet..." }
                                }
                            }
                        }
                    } else {
                        for (index, (_i, log)) in chat_logs.iter().enumerate() {
                            div { key: "{index}",
                                {render_chat_log(log)}
                            }
                        }
                    }
                }
            }

            div { class: "flex gap-2 border-t border-gray-200 pt-3",
                input {
                    r#type: "text",
                    class: "flex-1 px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                    placeholder: "Type your message...",
                    value: "{message}",
                    oninput: move |evt| message.set(evt.value()),
                    onkeydown: handle_keydown,
                }
                button {
                    class: "px-4 py-2 bg-blue-500 text-white rounded-lg hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                    onclick: send_message_handler,
                    disabled: message().trim().is_empty(),
                    "Send"
                }
            }
        }
    }
}

fn render_chat_log(log: &GameLog) -> Option<Element> {
    match log {
        GameLog::ChatFromPlayer { player_id, message } => Some(rsx! {
            div { class: "text-sm p-2 border-l-4 border-blue-400 bg-blue-50 rounded-r-lg",
                div { class: "flex items-baseline gap-2",
                    span { class: "font-semibold text-blue-700 text-xs uppercase tracking-wide", "{player_id}" }
                    span { class: "text-gray-600 text-xs", "•" }
                }
                div { class: "mt-1 text-gray-800", "{message}" }
            }
        }),
        GameLog::ChatFromGM { message } => Some(rsx! {
            div { class: "text-sm p-2 border-l-4 border-purple-400 bg-purple-50 rounded-r-lg",
                div { class: "flex items-baseline gap-2",
                    span { class: "font-semibold text-purple-700 text-xs uppercase tracking-wide", "GM" }
                    span { class: "text-gray-600 text-xs", "•" }
                }
                div { class: "mt-1 text-gray-800", "{message}" }
            }
        }),
        GameLog::CreatureLog { creature_id, log } => render_creature_log(creature_id, log),
        _ => None,
    }
}

fn render_creature_log(creature_id: &CreatureID, log: &CreatureLog) -> Option<Element> {
    let game_source = GAME_SOURCE();
    let creature_name = match &game_source {
        GameSource::Player { game, .. } => game
            .creatures
            .get(creature_id)
            .map(|c| c.name.clone())
            .unwrap_or_else(|| creature_id.to_string()),
        GameSource::GM(game) => game
            .creatures
            .get(creature_id)
            .map(|c| c.name.clone())
            .unwrap_or_else(|| creature_id.to_string()),
    };

    let log_message = match log {
        CreatureLog::Damage { hp, rolls } => {
            format!("takes {:?} damage (rolls: {:?})", hp, rolls)
        }
        CreatureLog::Heal { hp, rolls } => {
            format!("heals {:?} HP (rolls: {:?})", hp, rolls)
        }
        CreatureLog::GenerateEnergy { energy } => {
            format!("generates {:?} energy", energy)
        }
        CreatureLog::ReduceEnergy { energy } => {
            format!("loses {:?} energy", energy)
        }
        CreatureLog::ApplyCondition {
            condition,
            duration,
            ..
        } => {
            format!("gains condition: {:?} ({:?})", condition, duration)
        }
        _ => return None, // Handle other creature log types as needed
    };

    Some(rsx! {
        div { class: "text-sm p-2 border-l-4 border-green-400 bg-green-50 rounded-r-lg",
            div { class: "flex items-baseline gap-2",
                span { class: "font-semibold text-green-700 text-xs uppercase tracking-wide", "{creature_name}" }
                span { class: "text-gray-600 text-xs", "•" }
            }
            div { class: "mt-1 text-green-700 italic", "{log_message}" }
        }
    })
}

/// Automatically scroll the chat history to the bottom
///
/// Used to ensure the latest messages are visible when new ones arrive.
fn scroll_chat_to_bottom() {
    if let Some(window) = web_sys::window() {
        if let Some(document) = window.document() {
            if let Some(chat_div) = document.get_element_by_id(CHAT_HISTORY_ID) {
                chat_div.set_scroll_top(chat_div.scroll_height());
            } else {
                warn!("Chat history element not found for auto-scroll");
            }
        }
    }
}

fn chat_section_class(mode: &ChatMode) -> &'static str {
    match mode {
        ChatMode::Player => "player-view-shell__chat flex flex-col h-full",
        ChatMode::GM => "h-full flex flex-col min-h-0",
    }
}

fn chat_history_class(mode: &ChatMode) -> &'static str {
    match mode {
        ChatMode::Player => {
            "flex-1 overflow-y-auto mb-3 space-y-2 bg-white border border-gray-200 rounded-lg shadow-inner min-h-[180px]"
        }
        ChatMode::GM => {
            "flex-1 min-h-0 overflow-y-auto mb-3 space-y-2 bg-white border border-gray-200 rounded-lg shadow-inner"
        }
    }
}

fn chat_history_style(mode: &ChatMode) -> &'static str {
    match mode {
        ChatMode::Player => "max-height: 280px;",
        ChatMode::GM => "",
    }
}

fn chat_request(mode: &ChatMode, message: String) -> RPIGameRequest {
    match mode {
        ChatMode::Player => RPIGameRequest::PlayerCommand {
            command: PlayerCommand::ChatFromPlayer { message },
        },
        ChatMode::GM => RPIGameRequest::GMCommand {
            command: Box::new(GMCommand::ChatFromGM { message }),
        },
    }
}

async fn maybe_send_chat(
    mode: ChatMode,
    ws: Coroutine<UIRequest>,
    mut message: Signal<String>,
) -> anyhow::Result<()> {
    let msg = message();
    if msg.trim().is_empty() {
        return Ok(());
    }

    let request = chat_request(&mode, msg.clone());
    send_request::<serde_json::Value>(request, ws).await?;
    message.set(String::new());
    scroll_chat_to_bottom();
    Ok(())
}
