use std::rc::Rc;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use serde_json::json;
use tracing::{error, info};
use worker::{State, WebSocket};

use arpeggio::{
    game::GameExt,
    types::{serialize_player_game, ChangedGame, GMCommand, GameError, PlayerID, RPIGame},
};
use arptypes::{
    multitenant::{
        GameAndMetadata, GameIndex, GameMetadata, PlayerGameAndMetadata, RPIGameRequest, Role,
    },
    Game, GameLog,
};

use crate::{
    durablegame::{get_tag, WSUser},
    durablestorage::GameStorage,
    images::CFImageService,
};

/// A representation of a request received from a websocket. It has an ID so we can send a response
/// and the client can match them up.
#[derive(Deserialize, Debug)]
struct WSRequest {
    request: RPIGameRequest,
    id: String,
}

pub struct GameSession {
    image_service: CFImageService,
    game_storage: Rc<GameStorage>,
    socket: WebSocket,
    ws_user: WSUser,
    metadata: GameMetadata,
    state: Rc<State>,
}

impl GameSession {
    pub fn new(
        image_service: CFImageService,
        game_storage: Rc<GameStorage>,
        socket: WebSocket,
        ws_user: WSUser,
        metadata: GameMetadata,
        state: Rc<State>,
    ) -> Self {
        Self {
            image_service,
            game_storage,
            socket,
            ws_user,
            metadata,
            state,
        }
    }

    pub async fn ensure_player(&self) -> anyhow::Result<()> {
        // If this a new player, let's make sure they're registered in the Game state.
        if self.ws_user.role == Role::GM {
            return Ok(());
        }
        let changed_game = {
            let game = self.game_storage.game();
            if !game.players.contains_key(&self.ws_user.player_id) {
                let changed_game = game.perform_gm_command(GMCommand::RegisterPlayer {
                    id: self.ws_user.player_id.clone(),
                })?;
                Some(changed_game)
            } else {
                None
            }
        };
        if let Some(changed_game) = changed_game {
            self.game_storage.store_game(changed_game).await?;
        }
        Ok(())
    }

    pub async fn handle_event(&self, text: String) -> anyhow::Result<()> {
        info!(event = "handle-event", text);
        let request: serde_json::Result<WSRequest> = serde_json::from_str(&text);
        match request {
            Ok(request) => {
                let request_id = request.id.clone();
                info!(event = "handling-request", ?request);
                let response = self.handle_request(request).await;
                match response {
                    Ok(result) => {
                        self.send(&json!({"id": request_id, "payload": &result}))?
                    }
                    Err(e) => {
                        error!(event = "error-handling-request", ?e);
                        self.send(
                            &json!({"id": request_id, "error": format!("{e:?}")}),
                        )?
                    }
                }
            }
            Err(e) => {
                // This is a little involved because we try to send the request ID back with the error
                // response, so we have to retry parsing it as a Value.
                let error_response =
                    json!({"error": format!("Couldn't parse as a WSRequest: {e}")});
                let mut error_response = error_response.as_object().unwrap().clone();
                if let Ok(value) = serde_json::from_str::<
                    serde_json::Map<String, serde_json::Value>,
                >(&text)
                {
                    error_response.insert(
                        "id".to_string(),
                        value.get("id").unwrap_or(&serde_json::Value::Null).clone(),
                    );
                }
                error!(event = "error-response", ?error_response);
                self.send(&error_response)?;
            }
        }
        Ok(())
    }

    async fn handle_request(&self, request: WSRequest) -> anyhow::Result<serde_json::Value> {
        // TODO: we should not need to load the game on every operation; we should instead just store an
        // Arc<RefCell(?)<Game>>  in-memory in the durable object.
        let game = self.game_storage.game();
        use RPIGameRequest::*;
        match (self.ws_user.role, request.request) {
            (Role::GM, GMGetGame) => {
                let rpi_game = RPIGame(&game);
                let result = GameAndMetadata {
                    game: rpi_game.serialize_game()?,
                    metadata: self.metadata.clone(),
                    logs: self.game_storage.recent_logs(),
                };
                Ok(serde_json::to_value(result)?)
            }
            (Role::Player, PlayerGetGame) => {
                let player_game = serialize_player_game(&self.ws_user.player_id, &game)?;
                let result = PlayerGameAndMetadata {
                    game: player_game,
                    metadata: self.metadata.clone(),
                    logs: self.game_storage.recent_logs(), // TODO: filter logs by player/scene relevance
                };
                Ok(serde_json::to_value(result)?)
            }
            (Role::Player, PlayerCommand { command }) => {
                let changed_game =
                    game.perform_player_command(self.ws_user.player_id.clone(), command);
                self.change_game(changed_game).await
            }
            (Role::GM, GMCommand { command }) => {
                let changed_game = game.perform_gm_command(*command);
                self.change_game(changed_game).await
            }
            (
                _,
                MovementOptions {
                    scene_id,
                    creature_id,
                },
            ) => {
                let options = game.get_movement_options(scene_id, creature_id)?;
                Ok(serde_json::to_value(options)?)
            }
            (_, CombatMovementOptions) => {
                let options = game.get_combat()?.current_movement_options()?;
                Ok(serde_json::to_value(options)?)
            }
            (
                _,
                TargetOptions {
                    scene_id,
                    creature_id,
                    ability_id,
                },
            ) => {
                let options = game.get_target_options(scene_id, creature_id, ability_id)?;
                Ok(serde_json::to_value(options)?)
            }
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
                let result = game.preview_volume_targets(scene, creature_id, ability_id, point)?;
                Ok(serde_json::to_value(result)?)
            }

            (Role::GM, GMGenerateInvitation) => {
                let invitation_id = self.game_storage.create_invitation().await?;
                Ok(serde_json::to_value(invitation_id)?)
            }
            (Role::GM, GMListInvitations) => {
                let invitations = self.game_storage.list_invitations().await?;
                Ok(serde_json::to_value(invitations)?)
            }
            (Role::GM, GMDeleteInvitation { invitation_id }) => {
                let invitations = self.game_storage.delete_invitation(invitation_id).await?;
                Ok(serde_json::to_value(invitations)?)
            }
            (_, UploadImageFromURL { url, purpose }) => {
                let url = self.image_service.upload_from_url(&url, purpose).await?;
                self.game_storage.register_image(&url, purpose).await?;

                let response = json!({"image_url": url.to_string()});
                Ok(serde_json::to_value(response)?)
            }
            (_, RequestUploadImage { purpose }) => {
                let pending_image = self.image_service.request_upload_image(purpose).await?;
                self.game_storage
                    .register_image(&pending_image.final_url, purpose)
                    .await?;
                let response = json!({
                  "upload_url": pending_image.upload_url.to_string(),
                  "final_url": pending_image.final_url.to_string()
                });
                Ok(serde_json::to_value(response)?)
            }
            (Role::GM, PlayerGetGame) => {
                Err(anyhow!("GMs should use GMGetGame instead of PlayerGetGame"))
            }
            _ => Err(anyhow!("You can't run that command as that role.")),
        }
    }

    async fn change_game(
        &self,
        changed_game: Result<ChangedGame, GameError>,
    ) -> anyhow::Result<serde_json::Value> {
        let result = match changed_game {
            Ok(changed_game) => {
                let logs_with_indices = self.game_storage.store_game(changed_game.clone()).await?;
                self.broadcast_refresh_game(&changed_game.game, &logs_with_indices)?;
                Ok(changed_game.logs)
            }
            Err(e) => Err(e),
        };
        // TODO: render GameError better
        Ok(serde_json::to_value(result.map_err(|e| format!("{e:?}")))?)
    }

    fn broadcast_refresh_game(
        &self,
        game: &Game,
        logs_with_indices: &[(GameIndex, GameLog)],
    ) -> anyhow::Result<()> {
        // Broadcast role-specific game data using hibernatable WebSocket API
        let rpi_game = RPIGame(game);
        let full_game = rpi_game.serialize_game()?;

        // Get GM WebSockets
        let gm_websockets = self.state.get_websockets_with_tag("role:GM");
        for ws in gm_websockets {
            let message = json!({
                "t": "refresh_game", 
                "game": full_game, 
                "logs": logs_with_indices
            });
            if let Err(e) = self.send_to_websocket(&ws, &message) {
                error!(event = "gm-broadcast-error", ?e);
            }
        }

        // Get Player WebSockets and send role-specific data
        let player_websockets = self.state.get_websockets_with_tag("role:Player");
        for ws in player_websockets {
            // Get the player ID from the WebSocket tags
            if let Some(player_id_str) = get_tag("player_id:", &self.state, &ws) {
                let player_id = PlayerID(player_id_str);
                
                if let Ok(player_game) = serialize_player_game(&player_id, game) {
                    let message = json!({
                        "t": "refresh_player_game", 
                        "game": player_game, 
                        "logs": logs_with_indices
                    });
                    if let Err(e) = self.send_to_websocket(&ws, &message) {
                        error!(event = "player-broadcast-error", ?e, player_id = ?player_id);
                    }
                }
            }
        }

        Ok(())
    }

    fn send<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
        let s = serde_json::to_string::<T>(value)?;
        self.socket
            .send_with_str(s)
            .map_err(|e| anyhow!(format!("{e:?}")))
    }

    fn send_to_websocket<T: Serialize>(&self, ws: &WebSocket, value: &T) -> anyhow::Result<()> {
        let s = serde_json::to_string::<T>(value)?;
        ws.send_with_str(s)
            .map_err(|e| anyhow!(format!("{e:?}")))
    }
}

