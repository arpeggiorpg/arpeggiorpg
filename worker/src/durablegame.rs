use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::{anyhow, Context};
use arpeggio::types::PlayerID;
use mtarp::types::{GameID, GameMetadata, InvitationID, Role};
use serde_json::json;
use tracing::{error, info};
use uuid::Uuid;
use worker::{
  async_trait, durable_object, js_sys, wasm_bindgen, wasm_bindgen_futures, worker_sys, Env, Method,
  Request, Response, Result, State, WebSocket, WebSocketPair,
};

use crate::{
  anyhow_str, durablestorage::GameStorage, images::CFImageService, rust_error, storage, wsrpi,
};

#[durable_object]
pub struct ArpeggioGame {
  state: Rc<State>,
  game_storage: Option<Rc<GameStorage>>,
  sessions: Sessions,
  ws_tokens: HashMap<Uuid, WSUser>,
  metadata: Option<GameMetadata>,
  env: Env,
}

#[derive(Debug, Clone)]
pub struct WSUser {
  pub role: Role,
  pub player_id: PlayerID,
}

pub type Sessions = Rc<RefCell<Vec<WebSocket>>>;

#[durable_object]
impl DurableObject for ArpeggioGame {
  fn new(state: State, env: Env) -> Self {
    Self {
      game_storage: None,
      state: Rc::new(state),
      sessions: Rc::new(RefCell::new(vec![])),
      ws_tokens: HashMap::new(),
      metadata: None,
      env,
    }
  }

  #[tracing::instrument(name = "DO", skip(self, req))]
  async fn fetch(&mut self, req: Request) -> Result<Response> {
    crate::domigrations::migrate(self.state.storage()).await.map_err(rust_error)?;
    let game_storage = match self.game_storage {
      Some(ref game_storage) => game_storage.clone(),
      None => {
        let storage = GameStorage::load(self.state.clone()).await.map_err(rust_error)?;
        let rc_storage = Rc::new(storage);
        self.game_storage = Some(rc_storage.clone());
        rc_storage
      }
    };
    self.route(req, game_storage).await.map_err(rust_error)
  }
}

impl ArpeggioGame {
  async fn route(
    &mut self, req: Request, game_storage: Rc<GameStorage>,
  ) -> anyhow::Result<Response> {
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
        self.ws_tokens.insert(token, WSUser { role, player_id });
        Response::from_json(&json!({"token": token})).map_err(anyhow_str)
      }
      ["ws", game_id, ws_token] => {
        let ws_token: Uuid = ws_token.parse()?;
        if let Some(ws_user) = self.ws_tokens.remove(&ws_token) {
          info!(event = "ws-connect", ?path, ?game_id);
          let game_id = game_id.parse::<GameID>()?;
          let metadata = match &self.metadata {
            Some(metadata) => metadata.clone(),
            None => {
              let metadata = storage::get_game_metadata(&self.env, game_id)
                .await
                .map_err(anyhow_str)?
                .ok_or(anyhow!("No metadata!?"))?;
              self.metadata = Some(metadata.clone());
              metadata
            }
          };
          info!(event = "ws-game-metadata", ?metadata);

          let pair = WebSocketPair::new().map_err(anyhow_str)?;
          let server = pair.server;
          // To avoid racking up DO bills, let's close the socket after a while in case someone
          // leaves their browser on the page. This should NOT be necessary! We should be using
          // Hibernatable Websockets!

          server.accept().map_err(anyhow_str)?;

          // We have *two* asynchronous tasks here:
          // 1. listen for messages from the client and act on the game
          // 2. listen for broadcasts from the first task and sends a message to all sessions
          // Maybe there's a simpler way to do this that doesn't involve a channel and two tasks?

          self.sessions.borrow_mut().push(server.clone());

          let account_id = self.env.var("CF_ACCOUNT_ID").map_err(anyhow_str)?.to_string();
          let image_delivery_prefix =
            self.env.var("CF_IMAGE_DELIVERY_PREFIX").map_err(anyhow_str)?.to_string();
          let images_token =
            self.env.var("CF_IMAGES_TOKEN").map_err(anyhow_str)?.to_string();
          let image_service = CFImageService::new(account_id, images_token, &image_delivery_prefix, game_id)?;
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

          Response::from_websocket(pair.client).map_err(anyhow_str)
        } else {
          error!(event = "invalid-ws-token", ?ws_token);
          Response::error("Bad WS token", 404).map_err(anyhow_str)
        }
      }
      ["g", "invitations", _game_id, invitation_id] if req.method() == Method::Get => {
        self.check_invitation(req, invitation_id).await
      }
      _ => {
        error!(event = "unknown-route", ?path);
        Response::error(format!("bad URL to DO: {path:?}"), 404).map_err(anyhow_str)
      }
    }
  }

  async fn check_invitation(&self, _req: Request, invitation_id: &str) -> anyhow::Result<Response> {
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
    Response::from_json(&json!(invitations.contains(&invitation_id))).map_err(anyhow_str)
  }
}

async fn dump_storage(state: &State) -> anyhow::Result<Response> {
  // TODO: STREAM!
  let mut result = HashMap::new();
  let items = state.storage().list().await.map_err(anyhow_str)?;
  for key in items.keys() {
    let key = key.map_err(anyhow_str).context("just resolving the key...")?;
    let value = items.get(&key);
    info!(event = "dump-storage-key-value", ?key, ?value);
    let value: serde_json::Value = serde_wasm_bindgen::from_value(value)
      .map_err(anyhow_str)
      .context("parsing the value as a Value")?;
    let key: String = serde_wasm_bindgen::from_value(key)
      .map_err(anyhow_str)
      .context("parsing the key as a string")?;
    result.insert(key, value);
  }
  Response::from_json(&result).map_err(anyhow_str)
}
