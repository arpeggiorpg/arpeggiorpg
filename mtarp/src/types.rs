use serde::{Deserialize, Serialize};
use ts_rs::TS;
use uuid::Uuid;

use arpeggio::{types::{PlayerID, GMCommand, PlayerCommand, CreatureID, SceneID}, uuid_id};

uuid_id!(GameID);
uuid_id!(InvitationID);

/// This will be be based on the google JWT `sub` for now.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct UserID(pub String);

impl UserID {
  pub fn to_string(&self) -> String {
    self.0.clone()
  }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameMetadata {
  pub name: String,
}

/// The indices stored by GameIndex are a little weird.
///
/// The game_idx (I should call this the snapshot_index) is the actual current snapshot index. The
/// log_idx is actually the number of logs (or, alternatively: the NEXT index that should be written
/// to when a new command is performed) So a
///
/// e.g., a GameIndex of 0/0 has one snapshot and no logs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameIndex {
  pub game_idx: usize,
  pub log_idx: usize,
}

/// A GameProfile is a specific user's association with a game.
#[derive(Clone, Serialize, Deserialize, Debug, TS)]
pub struct GameProfile {
  pub user_id: UserID,
  pub game_id: GameID,
  pub profile_name: PlayerID,
  pub role: Role,
}

#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS, strum::EnumString, strum::Display)]
pub enum Role {
  GM,
  Player,
}

/// The result from listing a game. Includes a name (and maybe other data)
#[derive(Clone, Serialize, Deserialize, Debug, TS)]
pub struct GameList {
  pub games: Vec<(GameProfile, GameMetadata)>,
}

#[derive(Debug)]
pub struct Invitation {
  pub id: InvitationID,
  pub game_id: GameID,
}

/// The various kinds of requests that a frontend can make of the RPI in the context of a game.
/// These are scoped to a specific game, so you won't see things like "Auth" or "ListGames" here,
/// just the commands that related to one specific game.
#[derive(Deserialize, TS, Debug)]
#[serde(tag = "t")]
pub enum RPIGameRequest {
  GMGetGame,
  GMMovementOptions { scene_id: SceneID, creature_id: CreatureID },
  GMCommand { command: GMCommand },
  PlayerCommand { command: PlayerCommand },
}
