use serde::{Deserialize, Serialize};
use ts_rs::TS;
use uuid::Uuid;

// maybe we should move GameID into pandt
#[derive(
  Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize, Debug, Default, TS,
)]
pub struct GameID(pub Uuid);
impl GameID {
  pub fn gen() -> GameID { GameID(Uuid::new_v4()) }
  pub fn to_string(&self) -> String { self.0.to_hyphenated().to_string() }
}
impl std::str::FromStr for GameID {
  type Err = anyhow::Error;
  fn from_str(s: &str) -> Result<GameID, anyhow::Error> { Ok(GameID(Uuid::parse_str(s)?)) }
}

/// This will be be based on the google JWT `sub` for now.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct UserID(pub String);

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameMetadata {
  pub name: String,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameIndex {
  pub game_idx: usize,
  pub log_idx: usize,
}

#[derive(Clone, Serialize, Deserialize, Debug, TS)]
pub struct UserGames {
  pub gm_games: Vec<GameID>,
  pub player_games: Vec<GameID>,
}

/// The result from listing a game. Includes a name (and maybe other data)
#[derive(Clone, Serialize, Deserialize, Debug, TS)]
pub struct GameList {
  pub gm_games: Vec<(GameID, GameMetadata)>,
  pub player_games: Vec<(GameID, GameMetadata)>,
}
