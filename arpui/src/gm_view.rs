use std::collections::VecDeque;

use arptypes::{
    Game, GameLog,
    multitenant::{GameAndMetadata, GameID, GameIndex, InvitationID, RPIGameRequest, Role},
};
use dioxus::prelude::*;
use tracing::{error, info};

use crate::{
    PLAYER_SPEC, PlayerSpec,
    components::button::{Button, ButtonVariant},
    player_view::GAME_NAME,
    rpi::{Connector, send_request, use_ws},
};

pub static GM_GAME: GlobalSignal<Game> = Signal::global(|| Default::default());
pub static GM_GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());

#[component]
pub fn GMGamePage(id: GameID) -> Element {
    use_effect(move || *PLAYER_SPEC.write() = Some(PlayerSpec::GM));
    rsx! {
        Connector {
            role: Role::GM,
            game_id: id,
            game_signal: Some(GM_GAME.resolve()),
            player_game_signal: None,
            game_logs_signal: GM_GAME_LOGS.resolve(),

            GameLoader { game_id: id }
        }
    }
}

#[component]
fn GameLoader(game_id: GameID) -> Element {
    let ws = use_ws();
    let future: Resource<anyhow::Result<Game>> = use_resource(move || async move {
        info!("fetching game state for GM view");
        let response = send_request::<GameAndMetadata>(RPIGameRequest::GMGetGame, ws).await?;
        let game = Game::from_serialized_game(response.game);
        *GM_GAME.write() = game.clone();
        *GM_GAME_LOGS.write() = response.logs;
        *GAME_NAME.write() = response.metadata.name.clone();
        Ok(game)
    });

    match &*future.read_unchecked() {
        Some(Ok(game)) => {
            rsx! {
                Shell { game: game.clone(), game_id }
            }
        }
        Some(Err(err)) => {
            error!("GM view failed to load game state: {:?}", err);
            rsx! { div { "Unable to load GM view." } }
        }
        None => {
            rsx! { div { "Loading GM view..." } }
        }
    }
}

#[component]
fn Shell(game: Game, game_id: GameID) -> Element {
    let game = GM_GAME();
    let num_scenes = game.scenes.len();
    let num_creatures = game.creatures.len();
    let num_classes = game.classes.len();
    let active_scene_name = game
        .active_scene
        .and_then(|sid| game.scenes.get(&sid))
        .map(|s| s.name.clone())
        .unwrap_or_else(|| "None".to_string());

    rsx! {
        div {
            class: "flex flex-col gap-4 p-6",
            div {
                class: "bg-white rounded-lg shadow-md p-4",
                h2 {
                    class: "text-lg font-semibold text-gray-800 mb-3",
                    "Game Overview"
                }
                div {
                    class: "space-y-2 text-sm text-gray-700",
                    p { "Active Scene: {active_scene_name}" }
                    p { "Scenes: {num_scenes}" }
                    p { "Creatures: {num_creatures}" }
                    p { "Classes: {num_classes}" }
                    if game.current_combat.is_some() {
                        p {
                            class: "font-medium text-blue-700",
                            "Combat is active"
                        }
                    }
                }
            }

            Invitations { game_id }
        }
    }
}

#[component]
fn Invitations(game_id: GameID) -> Element {
    let ws = use_ws();
    let mut invitations: Signal<Option<Vec<InvitationID>>> = use_signal(|| None);
    let mut load_error: Signal<Option<String>> = use_signal(|| None);

    // Load invitations on mount
    let _loader: Resource<()> = use_resource(move || async move {
        match send_request::<Vec<InvitationID>>(RPIGameRequest::GMListInvitations, ws).await {
            Ok(list) => {
                invitations.set(Some(list));
            }
            Err(e) => {
                error!(?e, "Failed to load invitations");
                load_error.set(Some(format!("{e}")));
            }
        }
    });

    let mut generate_action = use_action(move |_: ()| async move {
        let new_id = send_request::<InvitationID>(RPIGameRequest::GMGenerateInvitation, ws).await?;
        info!(?new_id, "Generated new invitation");
        let mut current = invitations().unwrap_or_default();
        current.push(new_id);
        invitations.set(Some(current));
        Ok::<(), anyhow::Error>(())
    });

    let mut delete_action = use_action(move |invitation_id: InvitationID| async move {
        send_request::<serde_json::Value>(RPIGameRequest::GMDeleteInvitation { invitation_id }, ws)
            .await?;
        info!(?invitation_id, "Deleted invitation");
        let current = invitations()
            .unwrap_or_default()
            .into_iter()
            .filter(|id| *id != invitation_id)
            .collect();
        invitations.set(Some(current));
        Ok::<(), anyhow::Error>(())
    });

    let base_url = {
        let window = web_sys::window().expect("window must exist");
        let location = window.location();
        let origin = location.origin().unwrap_or_default();
        origin
    };

    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4",
            div {
                class: "flex items-center justify-between mb-3",
                h2 {
                    class: "text-lg font-semibold text-gray-800",
                    "Invitations"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    disabled: generate_action.pending(),
                    onclick: move |_| generate_action.call(()),
                    if generate_action.pending() {
                        "Generating..."
                    } else {
                        "Generate New Link"
                    }
                }
            }

            if let Some(err) = load_error() {
                p {
                    class: "text-red-600 text-sm",
                    "Failed to load invitations: {err}"
                }
            }

            match invitations() {
                None => rsx! {
                    p {
                        class: "text-sm text-gray-500",
                        "Loading invitations..."
                    }
                },
                Some(list) if list.is_empty() => rsx! {
                    p {
                        class: "text-sm text-gray-500 italic",
                        "No invitation links yet. Generate one to invite players."
                    }
                },
                Some(list) => rsx! {
                    ul {
                        class: "space-y-2",
                        for invitation_id in list {
                            li {
                                key: "{invitation_id}",
                                class: "flex items-center gap-2",
                                input {
                                    class: "border rounded px-3 py-1 text-sm flex-1 bg-gray-50 text-gray-700",
                                    r#type: "text",
                                    readonly: true,
                                    value: "{base_url}/invitations/{game_id}/{invitation_id}",
                                }
                                Button {
                                    variant: ButtonVariant::Outline,
                                    disabled: delete_action.pending(),
                                    onclick: move |_| delete_action.call(invitation_id),
                                    "Delete"
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}
