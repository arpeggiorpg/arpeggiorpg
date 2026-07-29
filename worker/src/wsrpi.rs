use std::rc::Rc;

use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use serde_json::json;
use tracing::{error, info};
use worker::{State, WebSocket};

use arpeggio::{
    game::GameExt,
    session::{
        DispatchAction, ImageOperation, SessionUser, dispatch_game_request, gm_refresh,
        player_refresh,
    },
    types::{ChangedGame, GMCommand, GameError, PlayerID},
};
use arptypes::{
    hosted::HostedGameRequest,
    protocol::{GameIndex, GameMetadata, GameRequest, Role, RpcRequest, RpcResponse},
    Game, GameLog,
};

use crate::{
    durablegame::{get_tag, WSUser},
    durablestorage::GameStorage,
    images::CFImageService,
};

#[derive(Deserialize, Debug)]
#[serde(untagged)]
enum SessionRequest {
    Game(GameRequest),
    Hosted(HostedGameRequest),
}

type WSRequest = RpcRequest<SessionRequest>;

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
            self.game_storage.update_game(changed_game).await?;
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
                    Ok(payload) => self.send(&RpcResponse::Success {
                        id: request_id,
                        payload,
                    })?,
                    Err(e) => {
                        error!(event = "error-handling-request", ?e);
                        self.send(&RpcResponse::<serde_json::Value>::Error {
                            id: request_id,
                            error: format!("{e:?}"),
                        })?
                    }
                }
            }
            Err(e) => {
                let error = format!("Couldn't parse as a WSRequest: {e}");
                if let Ok(request) = serde_json::from_str::<RpcRequest<serde_json::Value>>(&text) {
                    self.send(&RpcResponse::<serde_json::Value>::Error {
                        id: request.id,
                        error,
                    })?;
                } else {
                    self.send(&json!({"error": error}))?;
                }
            }
        }
        Ok(())
    }

    async fn handle_request(&self, request: WSRequest) -> anyhow::Result<serde_json::Value> {
        match request.request {
            SessionRequest::Game(request) => self.handle_game_request(request).await,
            SessionRequest::Hosted(request) => self.handle_hosted_request(request),
        }
    }

    async fn handle_game_request(&self, request: GameRequest) -> anyhow::Result<serde_json::Value> {
        let game = self.game_storage.game();
        let recent_logs = match &request {
            GameRequest::GMGetGame | GameRequest::PlayerGetGame => {
                self.game_storage.recent_logs()?
            }
            _ => Default::default(),
        };
        let user = match self.ws_user.role {
            Role::GM => SessionUser::gm(),
            Role::Player => SessionUser::player(self.ws_user.player_id.clone()),
        };
        match dispatch_game_request(&game, &self.metadata, &recent_logs, &user, request)? {
            DispatchAction::Respond(payload) => Ok(payload),
            DispatchAction::Change(changed_game) => self.change_game(changed_game).await,
            DispatchAction::Rollback(game_index) => {
                let restored_game = self.game_storage.rollback(game_index).await?;
                self.broadcast_refresh_game(&restored_game, &[])?;
                Ok(serde_json::to_value(Vec::<GameLog>::new())?)
            }
            DispatchAction::Image(ImageOperation::UploadFromUrl { url, purpose }) => {
                let url = self.image_service.upload_from_url(&url, purpose).await?;
                self.game_storage.register_image(&url, purpose)?;

                let response = json!({"image_url": url.to_string()});
                Ok(serde_json::to_value(response)?)
            }
            DispatchAction::Image(ImageOperation::RequestUpload { purpose }) => {
                let pending_image = self.image_service.request_upload_image(purpose).await?;
                self.game_storage
                    .register_image(&pending_image.final_url, purpose)?;
                let response = json!({
                  "upload_url": pending_image.upload_url.to_string(),
                  "final_url": pending_image.final_url.to_string()
                });
                Ok(serde_json::to_value(response)?)
            }
        }
    }

    fn handle_hosted_request(
        &self,
        request: HostedGameRequest,
    ) -> anyhow::Result<serde_json::Value> {
        use HostedGameRequest::*;
        match (self.ws_user.role, request) {
            (Role::GM, GMGenerateInvitation) => {
                let invitation_id = self.game_storage.create_invitation()?;
                Ok(serde_json::to_value(invitation_id)?)
            }
            (Role::GM, GMListInvitations) => {
                let invitations = self.game_storage.list_invitations()?;
                Ok(serde_json::to_value(invitations)?)
            }
            (Role::GM, GMDeleteInvitation { invitation_id }) => {
                let invitations = self.game_storage.delete_invitation(invitation_id)?;
                Ok(serde_json::to_value(invitations)?)
            }
            _ => Err(anyhow!("You can't run that hosted command as that role.")),
        }
    }

    async fn change_game(
        &self,
        changed_game: Result<ChangedGame, GameError>,
    ) -> anyhow::Result<serde_json::Value> {
        let result = match changed_game {
            Ok(changed_game) => {
                let logs_with_indices = self.game_storage.update_game(changed_game.clone()).await?;
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
        let gm_message = gm_refresh(game, logs_with_indices)?;

        // Get GM WebSockets
        let gm_websockets = self.state.get_websockets_with_tag("role:GM");
        for ws in gm_websockets {
            if let Err(e) = self.send_to_websocket(&ws, &gm_message) {
                error!(event = "gm-broadcast-error", ?e);
            }
        }

        // Get Player WebSockets and send role-specific data
        let player_websockets = self.state.get_websockets_with_tag("role:Player");
        for ws in player_websockets {
            // Get the player ID from the WebSocket tags
            if let Some(player_id_str) = get_tag("player_id:", &self.state, &ws) {
                let player_id = PlayerID(player_id_str);

                if let Ok(message) = player_refresh(game, &player_id, logs_with_indices) {
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
        ws.send_with_str(s).map_err(|e| anyhow!(format!("{e:?}")))
    }
}
