use std::{cell::RefCell, collections::HashMap, rc::Rc};

use arpeggio::types::PlayerID;
use mtarp::types::{InvitationID, Role};
use serde_json::json;
use uuid::Uuid;
use worker::{
  async_trait, console_log, durable_object, js_sys, wasm_bindgen, wasm_bindgen_futures, worker_sys,
  Env, Method, Request, Response, Result, State, WebSocket, WebSocketPair,
};

use crate::{rust_error, wsrpi};

#[durable_object]
pub struct ArpeggioGame {
  state: Rc<State>,
  sessions: Sessions,
  ws_tokens: HashMap<Uuid, WSUser>,
}

struct WSUser {
  role: Role,
  player_id: PlayerID,
}

pub type Sessions = Rc<RefCell<Vec<WebSocket>>>;

#[durable_object]
impl DurableObject for ArpeggioGame {
  fn new(state: State, _env: Env) -> Self {
    Self {
      state: Rc::new(state),
      sessions: Rc::new(RefCell::new(vec![])),
      ws_tokens: HashMap::new(),
    }
  }

  async fn fetch(&mut self, mut req: Request) -> Result<Response> {
    let path = req.path();
    console_log!("[DO] method={:?} path={path:?}", req.method());
    match path.split("/").collect::<Vec<_>>()[1..] {
      ["request-websocket", _game_id, role, player_id] => {
        // The worker has already authenticated & authorized the user, so we just need to store &
        // return a token.
        let token = Uuid::new_v4();
        let role = role.parse().map_err(rust_error)?;
        // Unfortunately, neither `worker` nor `url` have accessors for path segments that do URL
        // decoding. WTF?
        let player_id =
          percent_encoding::percent_decode_str(player_id).decode_utf8().map_err(rust_error)?;
        let player_id: PlayerID = PlayerID(player_id.to_string());
        console_log!("Ok what the heck is this player ID {player_id:?}");
        self.ws_tokens.insert(token, WSUser { role, player_id });
        Response::from_json(&json!({"token": token}))
      }
      ["ws", _, ws_token] => {
        let ws_token: Uuid = ws_token.parse().map_err(rust_error)?;
        if let Some(ws_user) = self.ws_tokens.remove(&ws_token) {
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
          self.sessions.borrow_mut().push(server.clone());
          let session = wsrpi::GameSession::new(
            self.state.clone(),
            server,
            self.sessions.clone(),
            ws_user.role,
            ws_user.player_id,
          );
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
