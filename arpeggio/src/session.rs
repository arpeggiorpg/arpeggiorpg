//! Platform-neutral request dispatch for one live game session.
//!
//! This module decides what a core protocol request means. Platform adapters remain responsible
//! for persistence, rollback storage, image transfer, and delivering refreshes to sockets.

use std::collections::VecDeque;

use arptypes::{
    protocol::{
        GameAndMetadata, GameIndex, GameMetadata, GameRequest, GameUpdate, ImageType,
        PlayerGameAndMetadata, Role,
    },
    ChangedGame, Game, GameError, GameLog, PlayerID,
};
use serde_json::Value;
use thiserror::Error;

use crate::{
    game::GameExt,
    types::{serialize_player_game, RPIGame},
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionUser {
    pub role: Role,
    pub player_id: Option<PlayerID>,
}

impl SessionUser {
    pub fn gm() -> Self {
        Self {
            role: Role::GM,
            player_id: None,
        }
    }

    pub fn player(player_id: PlayerID) -> Self {
        Self {
            role: Role::Player,
            player_id: Some(player_id),
        }
    }

    fn required_player_id(&self) -> Result<&PlayerID, SessionError> {
        self.player_id
            .as_ref()
            .ok_or(SessionError::MissingPlayerIdentity)
    }
}

#[derive(Debug)]
pub enum DispatchAction {
    Respond(Value),
    Change(Box<Result<ChangedGame, GameError>>),
    Rollback(GameIndex),
    Image(ImageOperation),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImageOperation {
    UploadFromUrl { url: String, purpose: ImageType },
    RequestUpload { purpose: ImageType },
}

#[derive(Debug, Error)]
pub enum SessionError {
    #[error(transparent)]
    Game(#[from] GameError),
    #[error(transparent)]
    Serialize(#[from] serde_json::Error),
    #[error("a player connection is missing its player identity")]
    MissingPlayerIdentity,
    #[error("{role} connections cannot make this request")]
    Unauthorized { role: Role },
}

pub fn dispatch_game_request(
    game: &Game,
    metadata: &GameMetadata,
    recent_logs: &VecDeque<(GameIndex, GameLog)>,
    user: &SessionUser,
    request: GameRequest,
) -> Result<DispatchAction, SessionError> {
    use GameRequest::*;

    let response = match (user.role, request) {
        (Role::GM, GMGetGame) => DispatchAction::Respond(serde_json::to_value(GameAndMetadata {
            game: RPIGame(game).serialize_game()?,
            metadata: metadata.clone(),
            logs: recent_logs.clone(),
        })?),
        (Role::Player, PlayerGetGame) => {
            let player_id = user.required_player_id()?;
            DispatchAction::Respond(serde_json::to_value(PlayerGameAndMetadata {
                game: serialize_player_game(player_id, game)?,
                metadata: metadata.clone(),
                logs: recent_logs.clone(),
            })?)
        }
        (Role::Player, PlayerCommand { command }) => {
            let player_id = user.required_player_id()?.clone();
            DispatchAction::Change(Box::new(game.perform_player_command(player_id, command)))
        }
        (Role::GM, GMCommand { command }) => {
            DispatchAction::Change(Box::new(game.perform_gm_command(*command)))
        }
        (Role::GM, GMRollback { game_index }) => DispatchAction::Rollback(game_index),
        (
            _,
            MovementOptions {
                scene_id,
                creature_id,
            },
        ) => DispatchAction::Respond(serde_json::to_value(
            game.get_movement_options(scene_id, creature_id)?,
        )?),
        (_, CombatMovementOptions) => DispatchAction::Respond(serde_json::to_value(
            game.get_combat()?.current_movement_options()?,
        )?),
        (
            _,
            TargetOptions {
                scene_id,
                creature_id,
                ability_id,
            },
        ) => DispatchAction::Respond(serde_json::to_value(game.get_target_options(
            scene_id,
            creature_id,
            ability_id,
        )?)?),
        (
            _,
            PreviewVolumeTargets {
                scene_id,
                creature_id,
                ability_id,
                point,
            },
        ) => {
            let scene = game.get_scene(scene_id)?;
            DispatchAction::Respond(serde_json::to_value(game.preview_volume_targets(
                scene,
                creature_id,
                ability_id,
                point,
            )?)?)
        }
        (_, UploadImageFromURL { url, purpose }) => {
            DispatchAction::Image(ImageOperation::UploadFromUrl { url, purpose })
        }
        (_, RequestUploadImage { purpose }) => {
            DispatchAction::Image(ImageOperation::RequestUpload { purpose })
        }
        (role, _) => return Err(SessionError::Unauthorized { role }),
    };

    Ok(response)
}

pub fn gm_refresh(game: &Game, logs: &[(GameIndex, GameLog)]) -> Result<GameUpdate, SessionError> {
    Ok(GameUpdate::RefreshGame {
        game: Box::new(RPIGame(game).serialize_game()?),
        logs: logs.to_vec(),
    })
}

pub fn player_refresh(
    game: &Game,
    player_id: &PlayerID,
    logs: &[(GameIndex, GameLog)],
) -> Result<GameUpdate, SessionError> {
    Ok(GameUpdate::RefreshPlayerGame {
        game: Box::new(serialize_player_game(player_id, game)?),
        logs: logs.to_vec(),
    })
}

#[cfg(test)]
mod tests {
    use arptypes::{
        protocol::{GameAndMetadata, PlayerGameAndMetadata},
        GMCommand, PlayerCommand,
    };

    use super::*;
    use crate::game::test::t_game;

    fn apply(action: DispatchAction) -> Game {
        let DispatchAction::Change(result) = action else {
            panic!("expected a game change");
        };
        (*result).expect("command should succeed").game
    }

    #[test]
    fn one_gm_and_two_players_share_the_same_core_session_protocol() {
        let metadata = GameMetadata {
            name: "Test Session".to_string(),
        };
        let logs = VecDeque::new();
        let gm = SessionUser::gm();
        let alice_id = PlayerID("Alice".to_string());
        let bob_id = PlayerID("Bob".to_string());
        let alice = SessionUser::player(alice_id.clone());
        let bob = SessionUser::player(bob_id.clone());
        let mut game = t_game();

        for player_id in [alice_id.clone(), bob_id.clone()] {
            game = apply(
                dispatch_game_request(
                    &game,
                    &metadata,
                    &logs,
                    &gm,
                    GameRequest::GMCommand {
                        command: Box::new(GMCommand::RegisterPlayer { id: player_id }),
                    },
                )
                .unwrap(),
            );
        }

        let DispatchAction::Respond(gm_payload) =
            dispatch_game_request(&game, &metadata, &logs, &gm, GameRequest::GMGetGame).unwrap()
        else {
            panic!("expected GM game payload");
        };
        let gm_game: GameAndMetadata = serde_json::from_value(gm_payload).unwrap();
        assert!(gm_game.game.players.contains_key(&alice_id));
        assert!(gm_game.game.players.contains_key(&bob_id));

        for user in [&alice, &bob] {
            let DispatchAction::Respond(payload) =
                dispatch_game_request(&game, &metadata, &logs, user, GameRequest::PlayerGetGame)
                    .unwrap()
            else {
                panic!("expected player game payload");
            };
            let player_game: PlayerGameAndMetadata = serde_json::from_value(payload).unwrap();
            assert_eq!(player_game.metadata, metadata);
        }

        let changed = dispatch_game_request(
            &game,
            &metadata,
            &logs,
            &alice,
            GameRequest::PlayerCommand {
                command: PlayerCommand::ChatFromPlayer {
                    message: "Hello from Alice".to_string(),
                },
            },
        )
        .unwrap();
        let DispatchAction::Change(changed) = changed else {
            panic!("expected Alice's chat to change the game");
        };
        let Ok(changed) = *changed else {
            panic!("expected Alice's chat to succeed");
        };
        assert_eq!(changed.logs.len(), 1);

        assert!(matches!(
            gm_refresh(&changed.game, &[]).unwrap(),
            GameUpdate::RefreshGame { .. }
        ));
        assert!(matches!(
            player_refresh(&changed.game, &alice_id, &[]).unwrap(),
            GameUpdate::RefreshPlayerGame { .. }
        ));
        assert!(matches!(
            player_refresh(&changed.game, &bob_id, &[]).unwrap(),
            GameUpdate::RefreshPlayerGame { .. }
        ));
    }

    #[test]
    fn role_restrictions_are_enforced_before_platform_adapters() {
        let game = t_game();
        let metadata = GameMetadata::default();
        let logs = VecDeque::new();
        let player = SessionUser::player(PlayerID("Alice".to_string()));

        let result = dispatch_game_request(
            &game,
            &metadata,
            &logs,
            &player,
            GameRequest::GMCommand {
                command: Box::new(GMCommand::ChatFromGM {
                    message: "not allowed".to_string(),
                }),
            },
        );

        assert!(matches!(
            result,
            Err(SessionError::Unauthorized { role: Role::Player })
        ));
    }
}
