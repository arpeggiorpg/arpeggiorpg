use arptypes::{
    multitenant::RPIGameRequest, CreatureID, CreatureLog, GameLog, PlayerCommand, PlayerID,
};
use dioxus::prelude::*;
use tracing::warn;

use crate::{
    player_view::{GAME, GAME_LOGS},
    rpi::{send_request, use_ws},
};

#[component]
pub fn PlayerChat(player_id: PlayerID) -> Element {
    let ws = use_ws();
    let mut message = use_signal(|| String::new());
    let chat_logs = GAME_LOGS.read();

    // TODO: Replace this with actual log fetching from RPI when available
    let _log_fetcher = use_resource(move || async move {
        // Placeholder for fetching recent chat logs
        // This will be replaced when RPI supports log fetching
        Vec::<GameLog>::new()
    });

    let send_message = move || async move {
        let msg = message();
        if !msg.trim().is_empty() {
            let command = PlayerCommand::ChatFromPlayer {
                message: msg.clone(),
            };
            if let Err(err) =
                send_request::<serde_json::Value>(RPIGameRequest::PlayerCommand { command }, ws)
                    .await
            {
                error!("Failed to send chat message: {:?}", err);
            }
            message.set(String::new());
            // Auto-scroll to bottom after sending message
            scroll_chat_to_bottom();
        }
    };

    let send_message_handler = move |_| send_message();

    let handle_keydown = {
        move |evt: KeyboardEvent| async move {
            if evt.key() == dioxus::prelude::Key::Enter && !evt.modifiers().shift() {
                send_message().await
            }
        }
    };

    rsx! {
        section {
            class: "player-view-shell__chat flex flex-col h-full",
            h2 { class: "text-lg font-semibold mb-2", "Chat" }

            // Chat history
            div {
                class: "flex-1 overflow-y-auto mb-3 space-y-2 bg-white border border-gray-200 rounded-lg shadow-inner min-h-[180px]",
                style: "max-height: 280px;",
                id: "chat-history",
                div { class: "p-3 space-y-2",
                    if chat_logs.is_empty() {
                        div { class: "flex items-center justify-center h-32 text-center",
                            div {
                                p { class: "text-gray-400 text-sm mb-1", "💬" }
                                p { class: "text-gray-500 text-sm italic", "No chat messages yet..." }
                                p { class: "text-gray-400 text-xs", "Start a conversation!" }
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

            // Chat input
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
    let game = GAME.read();
    let creature_name = game
        .creatures
        .get(creature_id)
        .map(|c| c.name.clone())
        .unwrap_or_else(|| creature_id.to_string());

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
            if let Some(chat_div) = document.get_element_by_id("chat-history") {
                chat_div.set_scroll_top(chat_div.scroll_height());
            } else {
                warn!("Chat history element not found for auto-scroll");
            }
        }
    }
}
