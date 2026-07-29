#![allow(non_snake_case)]

use std::collections::VecDeque;

use arptypes::{Game, GameLog, PlayerID, SerializedPlayerGame, protocol::GameIndex};
use dioxus::prelude::*;

#[cfg(feature = "hosted")]
pub mod admin_view;
pub mod catalog;
pub mod chat;
pub mod components;
pub mod gfx;
pub mod gm_view;
pub mod grid;
pub mod history;
#[cfg(feature = "hosted")]
pub mod hosted_app;
#[cfg(feature = "hosted")]
pub mod hosted_rpi;
#[cfg(feature = "hosted")]
pub mod hosted_views;
pub mod player_view;
pub mod rpi;
pub mod standalone_app;

pub static GAME_SOURCE: GlobalSignal<GameSource> = Signal::global(GameSource::default);
pub static GAME_LOGS: GlobalSignal<VecDeque<(GameIndex, GameLog)>> =
    Signal::global(|| VecDeque::new());
pub static GAME_NAME: GlobalSignal<String> = Signal::global(String::new);

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
