use std::collections::VecDeque;

use arptypes::{
    Game, GameLog,
    multitenant::{GameAndMetadata, GameID, GameIndex, RPIGameRequest, Role},
};
use dioxus::prelude::*;
use tracing::{error, info};

use crate::{
    PLAYER_SPEC, PlayerSpec,
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

            GameLoader {}
        }
    }
}

#[component]
fn GameLoader() -> Element {
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
                Shell { game: game.clone() }
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
fn Shell(game: Game) -> Element {
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
        }
    }
}
