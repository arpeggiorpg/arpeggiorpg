//! Durable Object implementation using Hibernatable WebSockets API
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::anyhow;
use arpeggio::types::PlayerID;
use arptypes::multitenant::{GameID, GameMetadata, Role};
use serde_json::json;
use tracing::{error, info};
use uuid::Uuid;
use worker::durable::WebSocketIncomingMessage;
use worker::{
    durable_object, wasm_bindgen, DurableObject, Env, Method, Request, Response, Result, State,
    WebSocket, WebSocketPair,
};

use crate::durablestorage::{
    test_fresh_game_initialization, test_snapshot_creation, test_snapshot_creation_multilog,
};
use crate::{
    dump, durablestorage::GameStorage, images::CFImageService, rust_error, storage, wsrpi,
};

/// Durable Object for Arpeggio Games, using SQLite-backend storage.
#[durable_object]
pub struct ArpeggioGameSql {
    state: Rc<State>,
    game_storage: RefCell<Option<Rc<GameStorage>>>,
    ws_tokens: RefCell<HashMap<Uuid, WSUser>>,
    metadata: RefCell<Option<GameMetadata>>,
    env: Env,
}

#[derive(Debug, Clone)]
pub struct WSUser {
    pub role: Role,
    pub player_id: PlayerID,
}

impl DurableObject for ArpeggioGameSql {
    fn new(state: State, env: Env) -> Self {
        Self {
            game_storage: RefCell::new(None),
            state: Rc::new(state),
            ws_tokens: RefCell::new(HashMap::new()),
            metadata: RefCell::new(None),
            env,
        }
    }

    #[tracing::instrument(name = "DO", skip(self, req))]
    async fn fetch(&self, req: Request) -> Result<Response> {
        self.route(req).await.map_err(rust_error)
    }

    async fn websocket_message(
        &self,
        ws: WebSocket,
        message: WebSocketIncomingMessage,
    ) -> Result<()> {
        let game_id = if let Some(game_id) = get_tag("game:", &self.state, &ws) {
            Uuid::parse_str(&game_id).map_err(rust_error)?.into()
        } else {
            return Err("No game ID found in WebSocket tags".into());
        };
        // Get metadata if not already loaded
        let metadata = self.metadata.borrow().clone();
        let metadata = match metadata {
            Some(metadata) => metadata,
            None => {
                let metadata = storage::get_game_metadata(&self.env, game_id)
                    .await
                    .map_err(rust_error)?
                    .ok_or(anyhow!("No metadata!?"))
                    .map_err(rust_error)?;
                *self.metadata.borrow_mut() = Some(metadata.clone());
                metadata
            }
        };
        let game_storage = self.get_game_storage(game_id).await.map_err(rust_error)?;

        let player_id =
            get_tag("player_id:", &self.state, &ws).ok_or("No player_id tag found on WebSocket")?;
        let player_id = PlayerID(player_id);

        let role_str =
            get_tag("role:", &self.state, &ws).ok_or("No role tag found on WebSocket")?;
        let role = role_str
            .parse::<Role>()
            .map_err(|e| format!("Invalid role tag: {}", e))?;

        let ws_user = WSUser { role, player_id };

        let account_id = self.env.var("CF_ACCOUNT_ID")?.to_string();
        let image_delivery_prefix = self.env.var("CF_IMAGE_DELIVERY_PREFIX")?.to_string();
        let images_token = self.env.var("CF_IMAGES_TOKEN")?.to_string();

        // Extract game_id from websocket tags
        let game_id = if let Some(game_id) = get_tag("game:", &self.state, &ws) {
            GameID(Uuid::parse_str(&game_id).map_err(rust_error)?)
        } else {
            return Err("No game ID found in WebSocket tags".into());
        };
        let image_service =
            CFImageService::new(account_id, images_token, &image_delivery_prefix, game_id)
                .map_err(rust_error)?;

        // Create a GameSession and handle the event directly
        let session = wsrpi::GameSession::new(
            image_service,
            game_storage,
            ws.clone(),
            ws_user.clone(),
            metadata,
            self.state.clone(),
        );

        // Ensure player is registered
        session.ensure_player().await.map_err(rust_error)?;

        // Handle the WebSocket message
        match message {
            WebSocketIncomingMessage::String(text) => {
                session.handle_event(text).await.map_err(rust_error)?;
            }
            WebSocketIncomingMessage::Binary(_) => {
                // We don't handle binary messages currently
                info!(event = "binary-message-ignored");
            }
        }

        Ok(())
    }

    async fn websocket_close(
        &self,
        ws: WebSocket,
        _code: usize,
        _reason: String,
        _was_clean: bool,
    ) -> Result<()> {
        info!(event = "websocket-close", tags = ?self.state.get_tags(&ws));
        Ok(())
    }
}

/// Helper function to extract a tag value by prefix from WebSocket tags
pub fn get_tag(prefix: &str, state: &State, ws: &WebSocket) -> Option<String> {
    let tags = state.get_tags(ws);
    tags.iter()
        .find(|tag| tag.starts_with(prefix))
        .map(|tag| tag.strip_prefix(prefix).unwrap().to_string())
}

impl ArpeggioGameSql {
    async fn get_game_storage(&self, game_id: GameID) -> anyhow::Result<Rc<GameStorage>> {
        crate::domigrations::migrate(self.env.clone(), &self.state, game_id)
            .await
            .map_err(rust_error)?;
        let game_storage = self.game_storage.borrow().clone();
        match game_storage {
            Some(game_storage) => Ok(game_storage),
            None => {
                let storage = GameStorage::load(self.state.clone()).map_err(rust_error)?;
                let rc_storage = Rc::new(storage);
                *self.game_storage.borrow_mut() = Some(rc_storage.clone());
                Ok(rc_storage)
            }
        }
    }

    async fn route(&self, req: Request) -> anyhow::Result<Response> {
        let path = req.path();
        info!(event="request", method=?req.method(), path=?path, "Request (MESSAGE)");

        match path.split('/').collect::<Vec<_>>()[1..] {
            ["superuser", "dump", game_id] => {
                let game_id = game_id.parse::<GameID>()?;
                dump::dump_storage(&self.state, &self.env, game_id).await
            }
            ["superuser", "destroy", _game_id] => {
                self.state.storage().delete_all().await?;
                Ok(Response::from_json(
                    &serde_json::json!({"status": "deleted"}),
                )?)
            }
            ["test"] => self.run_tests().await,
            ["request-websocket", _game_id, role, player_id] => {
                // The worker has already authenticated & authorized the user, so we just need to store &
                // return a token.
                let token = Uuid::new_v4();
                let role = role.parse()?;
                // Unfortunately, neither `worker` nor `url` have accessors for path segments that do URL
                // decoding. WTF?
                let player_id = percent_encoding::percent_decode_str(player_id).decode_utf8()?;
                let player_id: PlayerID = PlayerID(player_id.to_string());
                info!(event = "request-websocket", ?player_id);
                self.ws_tokens
                    .borrow_mut()
                    .insert(token, WSUser { role, player_id });
                Ok(Response::from_json(&json!({"token": token}))?)
            }
            ["ws", game_id, ws_token] => {
                let ws_token: Uuid = ws_token.parse()?;
                let Some(ws_user) = self.ws_tokens.borrow_mut().remove(&ws_token) else {
                    error!(event = "invalid-ws-token", ?ws_token);
                    return Ok(Response::error("Bad WS token", 404)?);
                };
                info!(event = "ws-connect", ?path, ?game_id);
                let game_id = game_id.parse::<GameID>()?;

                let pair = WebSocketPair::new()?;
                let server = pair.server;

                // Use hibernatable WebSocket API
                let game_tag = format!("game:{}", game_id.0);
                let player_id_tag = format!("player_id:{}", ws_user.player_id.0);
                let role_tag = format!("role:{}", ws_user.role);

                // Accept the WebSocket with tags for role-based broadcasting and user data
                self.state
                    .accept_websocket_with_tags(&server, &[&role_tag, &game_tag, &player_id_tag]);

                Ok(Response::from_websocket(pair.client)?)
            }
            ["g", "invitations", game_id, invitation_id] if req.method() == Method::Get => {
                let game_id = game_id.parse::<GameID>()?;
                self.check_invitation(game_id, invitation_id).await
            }
            _ => {
                error!(event = "unknown-route", ?path);
                Ok(Response::error(format!("bad URL to DO: {path:?}"), 404)?)
            }
        }
    }

    async fn check_invitation(
        &self,
        game_id: GameID,
        invitation_id: &str,
    ) -> anyhow::Result<Response> {
        let invitation_id = invitation_id.parse()?;
        let game_storage = self.get_game_storage(game_id).await?;
        let exists = game_storage.check_invitation(invitation_id)?;
        Ok(Response::from_json(&json!(exists))?)
    }

    async fn run_tests(&self) -> anyhow::Result<Response> {
        Ok(Response::from_json(&json!({
            "results": {
                "test_snapshot_creation": report(test_snapshot_creation(self.state.clone()).await),
                "test_snapshot_creation_multilog": report(test_snapshot_creation_multilog(self.state.clone()).await),
                "test_fresh_game_initialization": report(test_fresh_game_initialization(self.state.clone(), self.env.clone()).await),
            },
            "status": "completed"
        }))?)
    }
}
fn report(r: anyhow::Result<()>) -> String {
    match r {
        Ok(()) => "Ok!".to_string(),
        Err(e) => format!("Test failed: {e:?}"),
    }
}
