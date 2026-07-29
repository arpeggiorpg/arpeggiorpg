use std::collections::VecDeque;

use arpeggio::session::{
    dispatch_game_request, gm_refresh, player_refresh, DispatchAction, SessionUser,
};
use arptypes::{
    protocol::{
        GameAndMetadata, GameIndex, GameMetadata, GameRequest, GameUpdate, PlayerGameAndMetadata,
        RpcRequest, RpcResponse,
    },
    GMCommand, Game, GameLog, PlayerCommand, PlayerID,
};

struct TestSession {
    game: Game,
    metadata: GameMetadata,
    logs: VecDeque<(GameIndex, GameLog)>,
    request_id: usize,
}

impl TestSession {
    fn new() -> Self {
        Self {
            game: Game::default(),
            metadata: GameMetadata {
                name: "Protocol Test".to_string(),
            },
            logs: VecDeque::new(),
            request_id: 0,
        }
    }

    fn request(
        &mut self,
        user: &SessionUser,
        request: GameRequest,
    ) -> RpcResponse<serde_json::Value> {
        self.request_id += 1;
        let request_id = format!("request-{}", self.request_id);
        let request_wire = serde_json::to_string(&RpcRequest {
            id: request_id.clone(),
            request,
        })
        .unwrap();
        let parsed: RpcRequest<GameRequest> = serde_json::from_str(&request_wire).unwrap();

        let response = match dispatch_game_request(
            &self.game,
            &self.metadata,
            &self.logs,
            user,
            parsed.request,
        ) {
            Ok(DispatchAction::Respond(payload)) => RpcResponse::Success {
                id: parsed.id,
                payload,
            },
            Ok(DispatchAction::Change(changed)) => match *changed {
                Ok(changed) => {
                    for log in &changed.logs {
                        let index = GameIndex {
                            game_idx: 0,
                            log_idx: self.logs.len() + 1,
                        };
                        self.logs.push_back((index, log.clone()));
                    }
                    self.game = changed.game;
                    RpcResponse::Success {
                        id: parsed.id,
                        payload: serde_json::to_value(Ok::<_, String>(changed.logs)).unwrap(),
                    }
                }
                Err(error) => RpcResponse::Success {
                    id: parsed.id,
                    payload: serde_json::to_value(Err::<Vec<GameLog>, _>(format!("{error:?}")))
                        .unwrap(),
                },
            },
            Ok(DispatchAction::Rollback(_)) | Ok(DispatchAction::Image(_)) => {
                panic!("this protocol scenario does not use platform I/O")
            }
            Err(error) => RpcResponse::Error {
                id: parsed.id,
                error: error.to_string(),
            },
        };

        let response_wire = serde_json::to_string(&response).unwrap();
        serde_json::from_str(&response_wire).unwrap()
    }
}

fn payload(response: RpcResponse<serde_json::Value>) -> serde_json::Value {
    match response {
        RpcResponse::Success { payload, .. } => payload,
        RpcResponse::Error { error, .. } => panic!("unexpected protocol error: {error}"),
    }
}

#[test]
fn one_gm_and_two_players_complete_a_serialized_protocol_session() {
    let mut session = TestSession::new();
    let gm = SessionUser::gm();
    let alice_id = PlayerID("Alice".to_string());
    let bob_id = PlayerID("Bob".to_string());
    let alice = SessionUser::player(alice_id.clone());
    let bob = SessionUser::player(bob_id.clone());

    for player_id in [alice_id.clone(), bob_id.clone()] {
        let response = payload(session.request(
            &gm,
            GameRequest::GMCommand {
                command: Box::new(GMCommand::RegisterPlayer { id: player_id }),
            },
        ));
        let result: Result<Vec<GameLog>, String> = serde_json::from_value(response).unwrap();
        assert!(result.is_ok());
    }

    let gm_game: GameAndMetadata =
        serde_json::from_value(payload(session.request(&gm, GameRequest::GMGetGame))).unwrap();
    assert!(gm_game.game.players.contains_key(&alice_id));
    assert!(gm_game.game.players.contains_key(&bob_id));

    for player in [&alice, &bob] {
        let player_game: PlayerGameAndMetadata =
            serde_json::from_value(payload(session.request(player, GameRequest::PlayerGetGame)))
                .unwrap();
        assert_eq!(player_game.metadata.name, "Protocol Test");
    }

    let response = payload(session.request(
        &alice,
        GameRequest::PlayerCommand {
            command: PlayerCommand::ChatFromPlayer {
                message: "Hello Bob".to_string(),
            },
        },
    ));
    let result: Result<Vec<GameLog>, String> = serde_json::from_value(response).unwrap();
    assert_eq!(result.unwrap().len(), 1);

    let indexed_logs: Vec<_> = session.logs.iter().cloned().collect();
    assert!(matches!(
        gm_refresh(&session.game, &indexed_logs).unwrap(),
        GameUpdate::RefreshGame { .. }
    ));
    for player_id in [&alice_id, &bob_id] {
        assert!(matches!(
            player_refresh(&session.game, player_id, &indexed_logs).unwrap(),
            GameUpdate::RefreshPlayerGame { .. }
        ));
    }

    assert!(matches!(
        session.request(
            &bob,
            GameRequest::GMCommand {
                command: Box::new(GMCommand::ChatFromGM {
                    message: "not allowed".to_string(),
                }),
            },
        ),
        RpcResponse::Error { .. }
    ));
}
