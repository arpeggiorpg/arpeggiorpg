use std::{collections::HashMap, time::Duration};

use anyhow;
use futures::channel::oneshot;
use tokio::{sync::Mutex, time::timeout};
use tracing::{error, instrument};

use mtarp::types::{GameID, GameIndex};

/// The PingService coordinates the notification of all players in a game session so that they get
/// instantly updated whenever a change happens to the game they're playing.
// This should go away and be replaced with a CloudFlare Workers Durable Object using Hibernatable
// WebSockets.
pub struct PingService {
  waiters: Mutex<HashMap<GameID, Vec<oneshot::Sender<()>>>>,
}

impl PingService {
  pub fn new() -> PingService { PingService { waiters: Mutex::new(HashMap::new()) } }

  pub async fn register_waiter(&self, game_id: &GameID, sender: oneshot::Sender<()>) {
    let mut waiters = self.waiters.lock().await;
    let game_waiters = waiters.entry(*game_id);
    game_waiters.and_modify(|v| v.push(sender)).or_insert(vec![]);
  }

  pub async fn ping(&self, game_id: &GameID) -> anyhow::Result<()> {
    let mut waiters = self.waiters.lock().await;

    if let Some(waiters) = waiters.get_mut(game_id) {
      for sender in waiters.drain(0..) {
        if let Err(e) = sender.send(()) {
          error!("game_changed:receiver-unavailable when sending {:?}", e);
        }
      }
    }
    Ok(())
  }

  #[instrument(level = "debug", skip(self))]
  pub async fn poll_game(&self, game_id: GameID, game_index: GameIndex) -> anyhow::Result<()> {
    // First, if the app has already changed, return it immediately.
    if game_index != game_index {
      return Ok(());
    }
    // Now, we wait.
    let (sender, receiver) = oneshot::channel();
    self.register_waiter(&game_id, sender).await;
    let event = timeout(Duration::from_secs(30), receiver).await;
    match event {
      Ok(_) => {
        // The oneshot was canceled. I'm not really sure what this means or why it happens.
      }
      Err(_) => {
        // Timeout; just return the state of the app
      }
    }
    Ok(())
  }
}
