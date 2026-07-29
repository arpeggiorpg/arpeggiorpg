use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use ts_rs::TS;
use uuid::Uuid;

use crate::{
    uuid_id, AbilityID, CreatureID, GMCommand, GameLog, PlayerCommand, Point3, SceneID,
    SerializedGame, SerializedPlayerGame,
};

uuid_id!(GameID);

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct GameMetadata {
    pub name: String,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct GameAndMetadata {
    pub game: SerializedGame,
    pub metadata: GameMetadata,
    pub logs: VecDeque<(GameIndex, GameLog)>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct PlayerGameAndMetadata {
    pub game: SerializedPlayerGame,
    pub metadata: GameMetadata,
    pub logs: VecDeque<(GameIndex, GameLog)>,
}

/// Identifies one point in the snapshot and log history.
///
/// `game_idx` is the current snapshot index. `log_idx` is the index of a log within that snapshot.
/// A `GameIndex` of `0/0` identifies the initial snapshot before any logs have been written.
#[derive(
    Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default, TS, PartialOrd, Ord,
)]
pub struct GameIndex {
    // ORDER MATTERS since we are deriving PartialOrd.
    pub game_idx: usize,
    pub log_idx: usize,
}

#[derive(
    Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS, strum::EnumString, strum::Display,
)]
pub enum Role {
    GM,
    Player,
}

#[derive(
    Clone, Copy, Eq, PartialEq, Serialize, Deserialize, Debug, TS, strum::EnumString, strum::Display,
)]
#[serde(tag = "t")]
pub enum ImageType {
    BackgroundImage,
    CreatureIcon,
}

/// A platform-neutral request made in the context of one game.
#[derive(Serialize, Deserialize, TS, Debug)]
#[serde(tag = "t")]
pub enum GameRequest {
    GMGetGame,
    PlayerGetGame,
    GMCommand {
        command: Box<GMCommand>,
    },
    GMRollback {
        game_index: GameIndex,
    },
    UploadImageFromURL {
        url: String,
        purpose: ImageType,
    },
    RequestUploadImage {
        purpose: ImageType,
    },
    PlayerCommand {
        command: PlayerCommand,
    },
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

/// A request with a caller-generated ID used to correlate the response.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct RpcRequest<T> {
    pub id: String,
    pub request: T,
}

/// A correlated response. The untagged representation preserves the existing
/// `{ "id", "payload" }` and `{ "id", "error" }` wire formats.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub enum RpcResponse<T> {
    Success { id: String, payload: T },
    Error { id: String, error: String },
}

/// An unsolicited game update sent to connected clients.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "t")]
pub enum GameUpdate {
    RefreshGame {
        game: Box<SerializedGame>,
        logs: Vec<(GameIndex, GameLog)>,
    },
    RefreshPlayerGame {
        game: Box<SerializedPlayerGame>,
        logs: Vec<(GameIndex, GameLog)>,
    },
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn request_envelope_preserves_existing_wire_shape() {
        let request = RpcRequest {
            id: "request-1".to_string(),
            request: GameRequest::CombatMovementOptions,
        };

        assert_eq!(
            serde_json::to_value(request).unwrap(),
            json!({
                "id": "request-1",
                "request": {"t": "CombatMovementOptions"}
            })
        );
    }

    #[test]
    fn response_envelopes_preserve_existing_wire_shapes() {
        let success = RpcResponse::Success {
            id: "request-1".to_string(),
            payload: json!({"answer": 42}),
        };
        let error = RpcResponse::<serde_json::Value>::Error {
            id: "request-2".to_string(),
            error: "not allowed".to_string(),
        };

        assert_eq!(
            serde_json::to_value(success).unwrap(),
            json!({
                "id": "request-1",
                "payload": {"answer": 42}
            })
        );
        assert_eq!(
            serde_json::to_value(error).unwrap(),
            json!({
                "id": "request-2",
                "error": "not allowed"
            })
        );
    }

    #[test]
    fn refresh_update_preserves_existing_wire_shape() {
        let update = GameUpdate::RefreshGame {
            game: Box::new(SerializedGame::default()),
            logs: Vec::new(),
        };
        let value = serde_json::to_value(update).unwrap();

        assert_eq!(value.get("t"), Some(&json!("refresh_game")));
        assert_eq!(value.get("logs"), Some(&json!([])));
        assert!(value.get("game").is_some());
    }
}
