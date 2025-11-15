use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, Context};
use arpeggio::types::PlayerID;
use arptypes::multitenant::{GameID, GameMetadata, InvitationID, Role};
use serde_json::json;
use tracing::{error, info};
use uuid::Uuid;
use worker::{
    durable_object, wasm_bindgen, wasm_bindgen_futures, Env, Method, Request, Response, Result,
    State, WebSocket, WebSocketPair,
};

use crate::{
    anydbg, durablestorage::GameStorage, images::CFImageService, rust_error, storage, wsrpi,
};

#[durable_object]
pub struct ArpeggioGame {
    state: Rc<State>,
    game_storage: RefCell<Option<Rc<GameStorage>>>,
    sessions: Sessions,
    ws_tokens: RefCell<HashMap<Uuid, WSUser>>,
    metadata: RefCell<Option<GameMetadata>>,
    env: Env,
}

#[derive(Debug, Clone)]
pub struct WSUser {
    pub role: Role,
    pub player_id: PlayerID,
}

#[derive(Debug, Clone)]
pub struct SessionInfo {
    pub socket: WebSocket,
    pub user: WSUser,
}

pub type Sessions = Rc<RefCell<Vec<SessionInfo>>>;

impl DurableObject for ArpeggioGame {
    fn new(state: State, env: Env) -> Self {
        Self {
            game_storage: RefCell::new(None),
            state: Rc::new(state),
            sessions: Rc::new(RefCell::new(vec![])),
            ws_tokens: RefCell::new(HashMap::new()),
            metadata: RefCell::new(None),
            env,
        }
    }

    #[tracing::instrument(name = "DO", skip(self, req))]
    async fn fetch(&self, req: Request) -> Result<Response> {
        crate::domigrations::migrate(self.state.storage())
            .await
            .map_err(rust_error)?;
        let game_storage = self.game_storage.borrow().clone();
        let game_storage = match game_storage {
            Some(game_storage) => game_storage,
            None => {
                let storage = GameStorage::load(self.state.clone())
                    .await
                    .map_err(rust_error)?;
                let rc_storage = Rc::new(storage);
                *self.game_storage.borrow_mut() = Some(rc_storage.clone());
                rc_storage
            }
        };
        self.route(req, game_storage).await.map_err(rust_error)
    }
}

impl ArpeggioGame {
    async fn route(&self, req: Request, game_storage: Rc<GameStorage>) -> anyhow::Result<Response> {
        let path = req.path();
        info!(event="request", method=?req.method(), path=?path);

        match path.split('/').collect::<Vec<_>>()[1..] {
            ["superuser", "dump", _game_id] => dump_storage(&self.state).await,
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
                let metadata = self.metadata.borrow().clone();
                let metadata = match metadata {
                    Some(metadata) => metadata,
                    None => {
                        let metadata = storage::get_game_metadata(&self.env, game_id)
                            .await?
                            .ok_or(anyhow!("No metadata!?"))?;
                        *self.metadata.borrow_mut() = Some(metadata.clone());
                        metadata
                    }
                };
                info!(event = "ws-game-metadata", ?metadata);

                let pair = WebSocketPair::new()?;
                let server = pair.server;
                // To avoid racking up DO bills, let's close the socket after a while in case someone
                // leaves their browser on the page. This should NOT be necessary! We should be using
                // Hibernatable Websockets!

                server.accept()?;

                // We have *two* asynchronous tasks here:
                // 1. listen for messages from the client and act on the game
                // 2. listen for broadcasts from the first task and sends a message to all sessions
                // Maybe there's a simpler way to do this that doesn't involve a channel and two tasks?

                self.sessions.borrow_mut().push(SessionInfo {
                    socket: server.clone(),
                    user: ws_user.clone(),
                });

                let account_id = self.env.var("CF_ACCOUNT_ID")?.to_string();
                let image_delivery_prefix = self.env.var("CF_IMAGE_DELIVERY_PREFIX")?.to_string();
                let images_token = self.env.var("CF_IMAGES_TOKEN")?.to_string();
                let image_service =
                    CFImageService::new(account_id, images_token, &image_delivery_prefix, game_id)?;
                let session = wsrpi::GameSession::new(
                    image_service,
                    game_storage,
                    server,
                    self.sessions.clone(),
                    ws_user,
                    metadata,
                );
                wasm_bindgen_futures::spawn_local(async move {
                    session.run().await;
                    info!(event = "ws-session-done");
                });

                Ok(Response::from_websocket(pair.client)?)
            }
            ["g", "invitations", _game_id, invitation_id] if req.method() == Method::Get => {
                self.check_invitation(req, invitation_id).await
            }
            _ => {
                error!(event = "unknown-route", ?path);
                Ok(Response::error(format!("bad URL to DO: {path:?}"), 404)?)
            }
        }
    }

    async fn check_invitation(
        &self,
        _req: Request,
        invitation_id: &str,
    ) -> anyhow::Result<Response> {
        let invitation_id = invitation_id.parse()?;
        let storage = self.state.storage();
        let invitations = storage.get("invitations").await;
        let invitations: Vec<InvitationID> = match invitations {
            Ok(r) => r,
            Err(e) => {
                error!(event = "invitation-fetch-error", ?e);
                vec![]
            }
        };
        Ok(Response::from_json(&json!(
            invitations.contains(&invitation_id)
        ))?)
    }
}

async fn dump_storage(state: &State) -> anyhow::Result<Response> {
    // TODO: STREAM!
    let mut result = HashMap::new();
    let items = state.storage().list().await?;
    for key in items.keys() {
        let key = key.map_err(anydbg).context("just resolving the key...")?;
        let value = items.get(&key);
        info!(event = "dump-storage-key-value", ?key, ?value);
        let value: serde_json::Value = serde_wasm_bindgen::from_value(value)
            .map_err(anydbg)
            .context("parsing the value as a Value")?;
        let key: String = serde_wasm_bindgen::from_value(key)
            .map_err(anydbg)
            .context("parsing the key as a string")?;
        result.insert(key, value);
    }
    Ok(Response::from_json(&result)?)
}
