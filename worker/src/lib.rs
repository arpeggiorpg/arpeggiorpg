use std::{
  panic,
  sync::{Arc, RwLock},
};

use mtarp::types::GameID;
use serde_json::json;
use worker::*;

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
fn start() { panic::set_hook(Box::new(console_error_panic_hook::hook)); }

#[event(fetch, respond_with_errors)]
async fn main(req: Request, env: Env, ctx: Context) -> Result<Response> {
  console_log!("[worker] Start");
  let result = Router::new()
    .on_async("/g/create", |req, ctx| async move {
      let json = json!({"game_id": GameID::gen().to_string()});
      Response::from_json(&json)
    })
    .on_async("/g/list", |req, ctx| async move {
      let games: Vec<String> = vec![];
      let json = json!({"games": games});
      Response::from_json(&json)
    })
    .on_async("/game/:id", |req, ctx| async move {
      // TODO: Authenticate the user!
      // TODO: Authorize that the user has access to the game!
      let id = ctx.param("id").expect("id should exist because it's in the route");
      console_log!("[worker] GAME {id}");
      let namespace = ctx.durable_object("ARPEGGIOGAME")?;
      // We should probably make GameIDs actually be the Durable Object ID and use
      // namespace.id_from_string? This requires us to allocate the IDs with namespace.unique_id()
      // and would mean the type of GameID would have to change from wrapping UUIDs to instead
      // wrapping u256s (or more likely, [u8; 32]. or, more likely, String :P).
      let stub = namespace.id_from_name(id)?.get_stub()?;
      Ok(stub.fetch_with_request(req).await?)
    })
    .run(req, env)
    .await;
  console_log!("[worker] Done");
  let cors = Cors::new().with_origins(vec!["*"]).with_allowed_headers(vec!["*"]);
  result.and_then(move |r| r.with_cors(&cors))
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
    Self {
      state: Arc::new(state),
      env,
      sessions: Arc::new(RwLock::new(vec![])),
    }
  }

  async fn fetch(&mut self, mut req: Request) -> Result<Response> {
    let path = req.path();
    console_log!("[DO] method={:?} path={path:?}", req.method());
    if path.starts_with("/game") {
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
