use std::{
  collections::HashSet,
  sync::{Arc, RwLock},
};

use mtarp::types::InvitationID;
use serde_json::json;
use uuid::Uuid;
use worker::{
  async_trait, console_log, durable_object, js_sys, wasm_bindgen, wasm_bindgen_futures, worker_sys,
  Env, Method, Request, Response, Result, State, WebSocket, WebSocketPair,
};

use crate::{rust_error, wsrpi};

#[durable_object]
pub struct ArpeggioGame {
  state: Arc<State>,
  sessions: Sessions,
  ws_tokens: HashSet<Uuid>,
}

pub type Sessions = Arc<RwLock<Vec<WebSocket>>>;

#[durable_object]
impl DurableObject for ArpeggioGame {
  fn new(state: State, _env: Env) -> Self {
    Self {
      state: Arc::new(state),
      sessions: Arc::new(RwLock::new(vec![])),
      ws_tokens: HashSet::new(),
    }
  }

  async fn fetch(&mut self, req: Request) -> Result<Response> {
    let path = req.path();
    console_log!("[DO] method={:?} path={path:?}", req.method());
    match path.split("/").collect::<Vec<_>>()[1..] {
      ["request-websocket", _game_id, _role] => {
        // Theoretically, the worker should have already authenticated & authorized the user, so we
        // just need to store & return a token.
        // TODO: remember `role` so we can have Player vs GM stuff
        let token = Uuid::new_v4();
        self.ws_tokens.insert(token);
        Response::from_json(&json!({"token": token}))
      }
      ["ws", _, ws_token] => {
        let ws_token: Uuid = ws_token.parse().map_err(rust_error)?;
        if self.ws_tokens.remove(&ws_token) {
          console_log!("[DO] GAME {path}");
          console_log!("[worker] WEBSOCKET");
          let pair = WebSocketPair::new()?;
          let server = pair.server;
          server.accept()?;

          // We have *two* asynchronous tasks here:
          // 1. listen for messages from the client and act on the game
          // 2. listen for broadcasts from the first task and sends a message to all sessions
          // Maybe there's a simpler way to do this that doesn't involve a channel and two tasks?

          // TODO: ignore poison
          self.sessions.write().expect("poison").push(server.clone());
          let session = wsrpi::GameSession::new(self.state.clone(), server, self.sessions.clone());
          wasm_bindgen_futures::spawn_local(async move {
            session.run().await;
          });

          Response::from_websocket(pair.client)
        } else {
          console_log!("Bad WS token {path:?}");
          Response::error("Bad WS token", 404)
        }
      }
      ["g", "invitations", _game_id, invitation_id] if req.method() == Method::Get => {
        self.check_invitation(req, invitation_id).await
      }
      _ => {
        console_log!("Bad URL to DO: {path:?}");
        Response::error(format!("bad URL to DO: {path:?}"), 404)
      }
    }
  }
}

impl ArpeggioGame {
  async fn check_invitation(&self, req: Request, invitation_id: &str) -> Result<Response> {
    let invitation_id = invitation_id.parse().map_err(rust_error)?;
    let storage = self.state.storage();
    let invitations = storage.get("invitations").await;
    let invitations: Vec<InvitationID> = match invitations {
      Ok(r) => r,
      Err(e) => vec![],
    };
    Response::from_json(&json!(invitations.contains(&invitation_id)))
  }
}
