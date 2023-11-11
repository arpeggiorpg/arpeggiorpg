use std::{
  cell::{Cell, RefCell},
  collections::VecDeque,
  rc::Rc,
};

use anyhow::anyhow;
use tracing::{error, info, warn};
use worker::{ListOptions, State};

use arpeggio::types::{ChangedGame, Game, GameLog};
use mtarp::types::{GameIndex, ImageType, InvitationID};

use crate::anyhow_str;

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

  pub async fn load(state: Rc<State>) -> anyhow::Result<Self> {
    // TODO: support muiltple snapshots? Or maybe just wait until SQLite support exists...
    let (game, recent_logs) = match state.storage().get::<String>("snapshot-0-chunk-0").await {
      Ok(game_str) => {
        let game = serde_json::from_str(&game_str)?;
        Self::load_logs(state.clone(), game).await?
      }
      Err(e) => match e {
        worker::Error::JsError(e) if e == "No such value in storage." => {
          info!(event = "new-game");
          let default_game = Default::default();
          state
            .storage()
            .put("snapshot-0-chunk-0", serde_json::to_string(&default_game)?)
            .await
            .map_err(anyhow_str)?;
          (default_game, VecDeque::new())
        }
        _ => {
          error!(event = "error-fetching-game", ?e);
          return Err(anyhow_str(e));
        }
      },
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
    info!(event = "loading-logs", num = items.size());
    for key in items.keys() {
      let key = key.map_err(anyhow_str)?;
      let value = items.get(&key);
      let value: String = serde_wasm_bindgen::from_value(value).map_err(anyhow_str)?;
      let key: String = serde_wasm_bindgen::from_value(key).map_err(anyhow_str)?;
      info!(event = "found-log", ?key);
      if let ["log", _, "idx", log_idx_str] = key.split('-').collect::<Vec<_>>()[..] {
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
        warn!(event = "unknown-log-key", ?key);
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
      info!(event = "storing-log", ?key);
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
        error!(event = "error-listing-invitations", ?e);
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

  pub async fn register_image(
    &self, url: &worker::Url, image_type: ImageType,
  ) -> anyhow::Result<()> {
    // stupid for now; please give me SQL cloudflare!
    let images_key = format!("images-{image_type}");
    let mut storage = self.state.storage();
    let mut images: Vec<String> = storage.get(&images_key).await.map_err(anyhow_str)?;
    images.push(url.to_string());
    storage.put(&images_key, images).await.map_err(anyhow_str)?;
    Ok(())
  }
}
