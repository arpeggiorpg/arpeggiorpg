use std::{
  collections::HashMap,
  sync::{Arc, Mutex, RwLock},
};

use anyhow::anyhow;
use mtarp::types::{GameID, UserID};
use once_cell::sync::Lazy;
use serde::Deserialize;
use serde_json::json;
use uuid::Uuid;
use worker::*;

use google_signin;

mod wsrpi;

// Things I've learned about error-handling in workers-rs:
// - any Err returned from the main worker doesn't seem to do anything other than "Error: The script
//   will never generate a response.". So there *must* be an error handler in main that produces a
//   custom Response. (turns out I'm wrong: see `respond_with_errors`)
//   - The same is not true for the Durable Object. When I return an Err from the DO, it ends up in
//     the HTTP response.

// - Panics in the worker do not get printed anywhere, as far as I can tell, and they result in the
//   same "The script will never generate a response" message.
//   - Panics in the Durable Object are even worse; the DO seems to disappear from the face of the
//     net and the Worker just hangs waiting from a response instead of getting some sort of error.

// So, we can just use a panic hook to log the panics. However, I'm still concerned about the fact
// that a panicking DO does not immediately return a 500 or even seem to drop the connection to the
// waiting Worker. I'll have to see what the behavior is in actual production; maybe this is just a
// behavior of the local dev environment.

#[event(start)]
fn start() { console_error_panic_hook::set_once(); }

#[event(fetch, respond_with_errors)]
async fn main(req: Request, env: Env, ctx: Context) -> Result<Response> {
  console_log!("[worker] Start {} {:?}", req.path(), req.headers());

  if req.path().starts_with("/ws/") {
    // WebSocket requests can't go through the entire HTTP rigmarole
    return forward_websocket(req, env).await;
  }

  let cors = Cors::new().with_origins(vec!["*"]).with_allowed_headers(vec!["*"]);
  return http_routes(req, env).await.and_then(|r| r.with_cors(&cors));
}

async fn http_routes(req: Request, env: Env) -> Result<Response> {
  let path = req.path();
  let id_token = req.headers().get("x-arpeggio-auth")?;
  if id_token.is_none() {
    console_error!("No arpeggio auth header!");
    return Response::from_json(&json!({"error": "need x-arpeggio-auth"}));
  }

  let id_token = id_token.unwrap();
  let client_id = env.var("GOOGLE_CLIENT_ID")?.to_string();

  let validation_result = validate_google_token(&id_token, client_id).await;
  if let Err(e) = validation_result {
    console_log!("token is invalid; returning 401 {e:?}");
    return Response::error("Invalid token", 401);
  }
  let user_id = validation_result.unwrap();

  if path.starts_with("/request-websocket") {
    return request_websocket(req, env, user_id).await;
  } else if path == "/g/create" {
    // TODO: obviously this needs to *actually* create a
    let json = json!({"game_id": GameID::gen().to_string()});
    return Response::from_json(&json);
  } else if path == "/g/list" {
    let games: Vec<String> = vec![];
    let json = json!({"games": games});
    return Response::from_json(&json);
  } else {
    return Response::error("No route matched", 404);
  }
}

async fn request_websocket(req: Request, env: Env, user_id: UserID) -> Result<Response> {
  let path = req.path();
  let game_id = path.split("/").nth(2).ok_or(Error::RustError("bad path".to_string()))?;
  let uuid = Uuid::new_v4();
  // TODO: Authorize the user's ability to access this game.
  let game_id: GameID = game_id.parse().map_err(rust_error)?;
  let db = env.d1("DB")?;
  let statement =
    db.prepare("INSERT INTO websocket_tokens (token, user_id, game_id) VALUES (?, ?, ?)");
  let statement = statement.bind(&[
    uuid.to_string().into(),
    user_id.to_string().into(),
    game_id.to_string().into(),
  ])?;
  statement.run().await?;

  // WS_TOKENS.lock().expect("poison").insert(uuid, (user_id.clone().to_owned(), game_id));
  return Response::from_json(&json!({"token": uuid.to_string()}));
}


#[derive(Deserialize)]
struct WsToken {
  user_id: UserID,
  game_id: GameID,
}

async fn forward_websocket(req: Request, env: Env) -> Result<Response> {
  // This is a WebSocket request. CORS & authentication are meaningless here. We use a token
  // system where the client sends a regular HTTP request to /request-websocket/{game_id} to do
  // authn & authz, which returns a temporary token that is then passed here, in the request that
  // creates the websocket.

  let path = req.path();
  let ws_token = path.split("/").nth(2).ok_or(rust_error("bad path"))?;

  let ws_token: Uuid = ws_token.parse().map_err(rust_error)?;
  let (user_id, game_id) = {
    let db = env.d1("DB")?;
    // TODO: we need to clean up old tokens if people hit /request-websocket without following it up
    // with a request to /ws/.
    // TODO: Consider, instead of using D1 for this, we could probably just use in-memory storage in the Durable Object...
    let statement = db.prepare("SELECT user_id, game_id FROM websocket_tokens WHERE token = ?");
    let statement = statement.bind(&[ws_token.to_string().into()])?;
    let row = statement.first::<WsToken>(None).await?;
    match row {
      Some(row) => {
        let statement = db.prepare("DELETE FROM websocket_tokens WHERE token = ?");
        let statement = statement.bind(&[ws_token.to_string().into()])?;
        statement.run().await?;
        (row.user_id, row.game_id)
      }
      None => return Response::error("Couldn't find WS token", 404),
    }
  };

  console_log!("[worker] GAME {game_id:?}");
  let namespace = env.durable_object("ARPEGGIOGAME")?;
  // We should probably make GameIDs actually be the Durable Object ID and use
  // namespace.id_from_string? This requires us to allocate the IDs with namespace.unique_id()
  // and would mean the type of GameID would have to change from wrapping UUIDs to instead
  // wrapping u256s (or more likely, [u8; 32]. or, more likely, String :P).
  let stub = namespace.id_from_name(&game_id.to_string())?.get_stub()?;
  return Ok(stub.fetch_with_request(req).await?);
}

#[durable_object]
pub struct ArpeggioGame {
  state: Arc<State>,
  env: Env,
  sessions: Sessions,
}

pub type Sessions = Arc<RwLock<Vec<WebSocket>>>;

#[durable_object]
impl DurableObject for ArpeggioGame {
  fn new(state: State, env: Env) -> Self {
    Self { state: Arc::new(state), env, sessions: Arc::new(RwLock::new(vec![])) }
  }

  async fn fetch(&mut self, mut req: Request) -> Result<Response> {
    let path = req.path();
    console_log!("[DO] method={:?} path={path:?}", req.method());
    if path.starts_with("/ws") {
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
      return Response::error("bad URL to DO", 404);
    }
  }
}

async fn validate_google_token(id_token: &str, client_id: String) -> anyhow::Result<UserID> {
  let mut certs = google_signin::CachedCerts::new();
  // let mut certs = self.cached_certs.lock().await;
  certs.refresh_if_needed().await?;
  let mut client = google_signin::Client::new();
  client.audiences.push(client_id);
  let claims = client.verify(id_token, &certs).await?;
  // let expiry = UNIX_EPOCH + Duration::from_secs(id_info.exp);
  // let time_until_expiry = expiry.duration_since(std::time::SystemTime::now());
  let custom = claims.custom;
  console_log!(
    "validate-token: email={:?} name={:?} sub={:?} expires={:?} ",
    custom.email,
    custom.name,
    custom.sub,
    claims.expiration,
    // time_until_expiry
  );
  Ok(UserID(format!("google_{}", custom.sub)))
}

/// For some reason I can't just convert a workers::Error to an anyhow::Error because I get crazy
/// errors about how a *mut u8 might escape an async closure or something. So this converts the
/// error to a string before converting it to an anyhow Error.
pub fn anyhow_str<T: std::fmt::Debug>(e: T) -> anyhow::Error { anyhow!("{e:?}") }

fn rust_error<T: std::fmt::Debug>(e: T) -> Error { Error::RustError(format!("{e:?}")) }
