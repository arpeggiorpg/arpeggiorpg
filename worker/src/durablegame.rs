use std::{
  cell::{Cell, RefCell},
  collections::{HashMap, VecDeque},
  rc::Rc,
};

use anyhow::{anyhow, Context};
use arpeggio::types::{ChangedGame, Game, GameLog, PlayerID};
use mtarp::types::{GameIndex, InvitationID, Role};
use serde_json::json;
use uuid::Uuid;
use wasm_bindgen::JsError;
use worker::{
  async_trait, console_error, console_log, durable_object, js_sys, wasm_bindgen,
  wasm_bindgen_futures, worker_sys, Env, ListOptions, Method, Request, Response, Result, State,
  WebSocket, WebSocketPair,
};

use crate::{anyhow_str, rust_error, wsrpi};

#[durable_object]
pub struct ArpeggioGame {
  state: Rc<State>,
  game_storage: Option<Rc<GameStorage>>,
  sessions: Sessions,
  ws_tokens: HashMap<Uuid, WSUser>,
}

#[derive(Debug, Clone)]
pub struct WSUser {
  pub role: Role,
  pub player_id: PlayerID,
}

pub type Sessions = Rc<RefCell<Vec<WebSocket>>>;

#[durable_object]
impl DurableObject for ArpeggioGame {
  fn new(state: State, _env: Env) -> Self {
    Self {
      game_storage: None,
      state: Rc::new(state),
      sessions: Rc::new(RefCell::new(vec![])),
      ws_tokens: HashMap::new(),
    }
  }

  async fn fetch(&mut self, req: Request) -> Result<Response> {
    let game_storage = match self.game_storage {
      Some(ref game_storage) => game_storage.clone(),
      None => {
        let storage = GameStorage::load(self.state.clone()).await.map_err(rust_error)?;
        let rc_storage = Rc::new(storage);
        self.game_storage = Some(rc_storage.clone());
        rc_storage
      }
    };
    Ok(self.route(req, game_storage).await.map_err(rust_error)?)
  }
}

impl ArpeggioGame {
  async fn route(
    &mut self, req: Request, game_storage: Rc<GameStorage>,
  ) -> anyhow::Result<Response> {
    let path = req.path();
    console_log!("[DO] method={:?} path={path:?}", req.method());

    match path.split("/").collect::<Vec<_>>()[1..] {
      ["request-websocket", _game_id, role, player_id] => {
        // The worker has already authenticated & authorized the user, so we just need to store &
        // return a token.
        let token = Uuid::new_v4();
        let role = role.parse()?;
        // Unfortunately, neither `worker` nor `url` have accessors for path segments that do URL
        // decoding. WTF?
        let player_id = percent_encoding::percent_decode_str(player_id).decode_utf8()?;
        let player_id: PlayerID = PlayerID(player_id.to_string());
        console_log!("Ok what the heck is this player ID {player_id:?}");
        self.ws_tokens.insert(token, WSUser { role, player_id });
        Response::from_json(&json!({"token": token})).map_err(anyhow_str)
      }
      ["ws", _, ws_token] => {
        let ws_token: Uuid = ws_token.parse()?;
        if let Some(ws_user) = self.ws_tokens.remove(&ws_token) {
          console_log!("[DO] GAME {path}");
          console_log!("[worker] WEBSOCKET");
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
          let session =
            wsrpi::GameSession::new(game_storage, server, self.sessions.clone(), ws_user);
          wasm_bindgen_futures::spawn_local(async move {
            session.run().await;
            console_log!("Okayyyy. I'm done with my taaaaask.");
          });

          Response::from_websocket(pair.client).map_err(anyhow_str)
        } else {
          console_log!("Bad WS token {path:?}");
          Response::error("Bad WS token", 404).map_err(anyhow_str)
        }
      }
      ["g", "invitations", _game_id, invitation_id] if req.method() == Method::Get => {
        self.check_invitation(req, invitation_id).await
      }
      _ => {
        console_log!("Bad URL to DO: {path:?}");
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
        console_error!("Error getting invitations: {e:?}");
        vec![]
      }
    };
    Response::from_json(&json!(invitations.contains(&invitation_id))).map_err(anyhow_str)
  }
}

type RecentGameLogs = VecDeque<(GameIndex, GameLog)>;
/// The state of the game.
///
/// The cool thing that Durable Objects give us is that we can keep the Game in memory, just loading
/// it when the DO wakes up. Of course, during normal play the DO will go to sleep and wake up many
/// times, but while it *is* awake we only need to save logs and update the in-memory game.
///
/// This is really the whole reason I wanted to use CF Durable Objects for Arpeggio. I don't need to
/// worry about distributed caching or distributed event queues for notifications when a game object
/// changes; everyone's connected to the same live game object.
pub struct GameStorage {
  state: Rc<State>,
  current_snapshot_idx: Cell<usize>,
  next_log_idx: Cell<usize>,
  cached_game: Rc<RefCell<Game>>,
  recent_logs: Rc<RefCell<RecentGameLogs>>,
}

const RECENT_LOGS_SIZE: usize = 100;

/// ## Storage in Durable Objects
/// DO gives us a KV store, where the size of values is pretty significantly limited (128kB). It
/// turns out that CloudFlare is (hopefully) going to be releasing local a local SQLite-based
/// storage API for Durable Objects within a few months, so I am not going to bother trying to make
/// this storage system too robust for now; it's far too onerous.
impl GameStorage {
  pub fn game(&self) -> Game { self.cached_game.borrow().clone() }
  pub fn recent_logs(&self) -> RecentGameLogs { self.recent_logs.borrow().clone() }

  async fn load(state: Rc<State>) -> anyhow::Result<Self> {
    // TODO: support muiltple snapshots? Or maybe just wait until SQLite support exists...

    let (game, recent_logs) = match state.storage().get::<String>("snapshot-0-chunk-0").await {
      Ok(game_str) => {
        let game = serde_json::from_str(&game_str)?;
        Self::load_logs(state.clone(), game).await?
      }
      Err(e) => {
        console_error!("ERROR: Loading game failed: {e:?}");
        match e {
          worker::Error::JsError(e) if e == "No such value in storage." => {
            console_log!("No initial snapshot. This is a new game!");
            let default_game = Default::default();
            state
              .storage()
              .put("snapshot-0-chunk-0", serde_json::to_string(&default_game)?)
              .await
              .map_err(anyhow_str)?;
            (default_game, VecDeque::new())
          }
          _ => return Err(anyhow_str(e)),
        }
      }
    };
    let next_log_idx = recent_logs.iter().last().map(|l| l.0.log_idx + 1).unwrap_or(0);
    let game_storage = Self {
      state,
      current_snapshot_idx: Cell::new(0),
      next_log_idx: Cell::new(next_log_idx),
      cached_game: Rc::new(RefCell::new(game)),
      recent_logs: Rc::new(RefCell::new(recent_logs)),
    };
    Ok(game_storage)
  }

  /// log keys are like "log-{snapshot_idx}-idx-{log_idx}"
  async fn load_logs(state: Rc<State>, mut game: Game) -> anyhow::Result<(Game, RecentGameLogs)> {
    // Here's another super annoying deficiency of the DO "list" API: it doesn't return an iterator,
    // but the entire result set all at once as a javascript Map! So, we have to manually do
    // batching to avoid loading too much stuff into memory at once.

    let storage = state.storage();
    let list_options = ListOptions::new().prefix("log-");
    let items = storage.list_with_options(list_options).await.map_err(anyhow_str)?;
    let mut recent_logs = VecDeque::new();
    if items.size() == 0 {
      return Ok((game, recent_logs));
    }
    console_log!("Loading Logs: Found {} items", items.size());
    for key in items.keys() {
      let key = key.map_err(anyhow_str)?;
      let value = items.get(&key);
      let value: String = serde_wasm_bindgen::from_value(value).map_err(anyhow_str)?;
      let key: String = serde_wasm_bindgen::from_value(key).map_err(anyhow_str)?;
      console_log!("found a log {key:?}");
      if let ["log", _, "idx", log_idx_str] = key.split("-").collect::<Vec<_>>()[..] {
        let log: GameLog = serde_json::from_str(&value).map_err(|e| {
          anyhow!("Failed parsing GameLog as JSON:\ncontent: {value:?}\nerror: {e:?}")
        })?;
        game = game.apply_log(&log)?;
        let log_idx = log_idx_str.parse()?;
        if recent_logs.len() >= RECENT_LOGS_SIZE {
          recent_logs.pop_front();
        }
        recent_logs.push_back((GameIndex { game_idx: 0, log_idx }, log));
      } else {
        console_log!("Don't know what this log is: {key:?}");
      }
    }

    Ok((game, recent_logs))
  }

  /// Upate Game storage with changes from a changed_game. Updates the locally cached Game as well
  /// as writing new logs to storage.
  pub async fn store_game(
    &self, changed_game: ChangedGame,
  ) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
    // NOTE: Durable Objects support "write coalescing", where you can run multiple PUT operations
    // without awaiting and they will be merged into one "transaction". Here's the thing: I'm not
    // sure this works with the Rust bindings. But we should try something like that. There is also
    // a method called "transaction" which takes a closure to do this explicitly, but that's not
    // exposed in the Rust workers API yet.

    // we could probably use something more compact than JSON...

    // TODO: We could occasionally produce a new snapshot... or just wait for SQLite
    let mut logs_with_indices = vec![];
    for log in changed_game.logs {
      let serialized_log = serde_json::to_string(&log)?;
      let key =
        format!("log-{:09}-idx-{:09}", self.current_snapshot_idx.get(), self.next_log_idx.get());
      console_log!("Storing log {key:?}");
      self.state.storage().put(&key, serialized_log).await.map_err(anyhow_str)?;
      logs_with_indices.push((GameIndex { game_idx: 0, log_idx: self.next_log_idx.get() }, log));

      self.next_log_idx.set(self.next_log_idx.get() + 1);
    }
    *self.cached_game.borrow_mut() = changed_game.game;
    let mut recent_logs = self.recent_logs.borrow_mut();
    recent_logs.extend(logs_with_indices.iter().cloned());
    let drain_to = recent_logs.len().saturating_sub(RECENT_LOGS_SIZE);
    recent_logs.drain(0..drain_to);

    Ok(logs_with_indices)
  }

  pub async fn create_invitation(&self) -> anyhow::Result<InvitationID> {
    let invitation_id = InvitationID::gen();

    let mut invitations = self.list_invitations().await?;
    invitations.push(invitation_id);
    self.state.storage().put("invitations", invitations).await.map_err(anyhow_str)?;
    Ok(invitation_id)
  }

  pub async fn list_invitations(&self) -> anyhow::Result<Vec<InvitationID>> {
    let invitations = self.state.storage().get("invitations").await;
    let invitations: Vec<InvitationID> = match invitations {
      Ok(invitations) => invitations,
      Err(e) => {
        console_error!("Error listing invitations: {e:?}");
        vec![]
      }
    };
    Ok(invitations)
  }

  pub async fn delete_invitation(
    &self, invitation_id: InvitationID,
  ) -> anyhow::Result<Vec<InvitationID>> {
    let mut invitations = self.list_invitations().await?;
    invitations.retain_mut(|inv| inv != &invitation_id);
    self.state.storage().put("invitations", invitations.clone()).await.map_err(anyhow_str)?;
    Ok(invitations)
  }
}
