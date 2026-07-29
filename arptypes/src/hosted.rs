use serde::{Deserialize, Serialize};
use ts_rs::TS;
use uuid::Uuid;

use crate::{
    protocol::{GameID, GameMetadata, Role},
    uuid_id, PlayerID,
};

uuid_id!(InvitationID);

/// The hosted-service identity for one authenticated user.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default, TS)]
pub struct UserID(pub String);

impl std::fmt::Display for UserID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CopyToPreprodResult {
    pub storage_version: u32,
    pub game_url: String,
}

/// An internal hosted-service association between an authenticated user and a game.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS)]
pub struct GameProfile {
    pub user_id: UserID,
    pub game_id: GameID,
    pub profile_name: PlayerID,
    pub role: Role,
}

/// The account-free representation returned to the hosted browser game list.
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug, TS)]
pub struct GameSummary {
    pub game_id: GameID,
    pub profile_name: PlayerID,
    pub role: Role,
    pub metadata: GameMetadata,
}

#[derive(PartialEq, Eq, Clone, Serialize, Deserialize, Debug, TS)]
pub struct GameList {
    pub games: Vec<GameSummary>,
}

#[derive(Debug)]
pub struct Invitation {
    pub id: InvitationID,
    pub game_id: GameID,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InvitationCheck {
    pub invitation_valid: bool,
    pub already_member: bool,
    pub game_name: Option<String>,
    pub member_profile_name: Option<PlayerID>,
}

/// Requests implemented only by the hosted service.
#[derive(Serialize, Deserialize, TS, Debug)]
#[serde(tag = "t")]
pub enum HostedGameRequest {
    GMGenerateInvitation,
    GMListInvitations,
    GMDeleteInvitation { invitation_id: InvitationID },
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use crate::protocol::RpcRequest;

    use super::*;

    #[test]
    fn hosted_request_uses_the_shared_envelope_without_changing_its_tag() {
        let request = RpcRequest {
            id: "request-1".to_string(),
            request: HostedGameRequest::GMGenerateInvitation,
        };

        assert_eq!(
            serde_json::to_value(request).unwrap(),
            json!({
                "id": "request-1",
                "request": {"t": "GMGenerateInvitation"}
            })
        );
    }

    #[test]
    fn browser_game_summary_does_not_include_user_identity() {
        let value = serde_json::to_value(GameSummary {
            game_id: GameID(Uuid::nil()),
            profile_name: PlayerID("GM".to_string()),
            role: Role::GM,
            metadata: GameMetadata {
                name: "Test".to_string(),
            },
        })
        .unwrap();

        assert!(value.get("user_id").is_none());
        assert_eq!(value.get("profile_name"), Some(&json!("GM")));
    }
}
