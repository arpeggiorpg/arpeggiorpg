use uuid::Uuid;
use serde::{Serialize, Deserialize};

pub type GameID = Uuid;
/// This will be be based on the google JWT `sub` for now.
pub type UserID = String;

#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct GameIndex {
  pub game_idx: usize,
  pub log_idx: usize,
}


#[derive(Clone, Serialize, Deserialize)]
pub struct UserGames {
  pub gm_games: Vec<GameID>,
  pub player_games: Vec<GameID>,
}
