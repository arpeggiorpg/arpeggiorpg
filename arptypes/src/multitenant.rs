use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use strum;
use ts_rs::TS;
use uuid::Uuid;

use crate::{uuid_id, AbilityID, CreatureID, GMCommand, PlayerCommand, PlayerID, Point3, SceneID};

uuid_id!(GameID);
uuid_id!(InvitationID);

/// This will be be based on the google JWT `sub` for now.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct UserID(pub String);

impl std::fmt::Display for UserID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameMetadata {
    pub name: String,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct GameAndMetadata {
    pub game: crate::SerializedGame,
    pub metadata: GameMetadata,
    pub logs: VecDeque<(GameIndex, crate::GameLog)>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct PlayerGameAndMetadata {
    pub game: crate::SerializedPlayerGame,
    pub metadata: GameMetadata,
    pub logs: VecDeque<(GameIndex, crate::GameLog)>, // TODO: filter logs by player/scene relevance
}

/// The indices stored by GameIndex are a little weird.
///
/// The game_idx (I should call this the snapshot_index) is the actual current snapshot index. The
/// log_idx is actually the number of logs (or, alternatively: the NEXT index that should be written
/// to when a new command is performed) So a
///
/// e.g., a GameIndex of 0/0 has one snapshot and no logs.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default, TS, PartialOrd, Ord)]
pub struct GameIndex {
    // ORDER MATTERS since we are deriving PartialOrd
    pub game_idx: usize,
    pub log_idx: usize,
}

/// A GameProfile is a specific user's association with a game.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS)]
pub struct GameProfile {
    pub user_id: UserID,
    pub game_id: GameID,
    pub profile_name: PlayerID,
    pub role: Role,
}

#[derive(
    Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS, strum::EnumString, strum::Display,
)]
pub enum Role {
    GM,
    Player,
}

/// The result from listing a game. Includes a name (and maybe other data)
#[derive(PartialEq, Eq, Clone, Serialize, Deserialize, Debug, TS)]
pub struct GameList {
    pub games: Vec<(GameProfile, GameMetadata)>,
}

#[derive(Debug)]
pub struct Invitation {
    pub id: InvitationID,
    pub game_id: GameID,
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug, TS, strum::EnumString, strum::Display)]
#[serde(tag = "t")]
pub enum ImageType {
    BackgroundImage,
    CreatureIcon,
}

/// The various kinds of requests that a frontend can make of the RPI in the context of a game.
/// These are scoped to a specific game, so you won't see things like "Auth" or "ListGames" here,
/// just the commands that related to one specific game.
#[derive(Serialize, Deserialize, TS, Debug)]
#[serde(tag = "t")]
pub enum RPIGameRequest {
    GMGetGame,
    PlayerGetGame,
    GMCommand {
        command: Box<GMCommand>,
    },

    // GM Commands for managing invitations happen here in the RPIGameRequest, but the check/accept
    // operations happen on regular HTTP endpoints, because you can't get a websocket to a game unless
    // you're authorized already.
    GMGenerateInvitation,
    GMListInvitations,
    GMDeleteInvitation {
        invitation_id: InvitationID,
    },

    // "upload" an image by giving us a URL which will be downloaded and saved to our Image store.
    UploadImageFromURL {
        url: String,
        purpose: ImageType,
    },

    // Request a presigned image upload URL that the browser can send the file data to to store it in
    // our Image store.
    RequestUploadImage {
        purpose: ImageType,
    },

    PlayerCommand {
        command: PlayerCommand,
    },

    // These things *technically* could be split up into GM and Player variants,
    // but it's not really a big deal if players can view movement & target
    // options for other creatures
    MovementOptions {
        scene_id: SceneID,
        creature_id: CreatureID,
    },
    CombatMovementOptions,
    TargetOptions {
        scene_id: SceneID,
        creature_id: CreatureID,
        ability_id: AbilityID,
    },
    PreviewVolumeTargets {
        scene_id: SceneID,
        creature_id: CreatureID,
        ability_id: AbilityID,
        point: Point3,
    },
}
