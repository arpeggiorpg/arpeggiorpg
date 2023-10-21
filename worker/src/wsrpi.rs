use std::sync::Arc;

use anyhow::{anyhow, Context};
use futures_util::stream::StreamExt;
use serde::{Deserialize, Serialize};
use serde_json::json;
use worker::{console_error, console_log, ListOptions, State, WebSocket, WebsocketEvent};

use arpeggio::types::{ChangedGame, Game, GameError, RPIGame};
use mtarp::types::RPIGameRequest;

use crate::{anyhow_str, durablegame::Sessions};

/// A representation of a request received from a websocket. It has an ID so we can send a response
/// and the client can match them up.
#[derive(Deserialize, Debug)]
struct WSRequest {
  request: RPIGameRequest,
  id: String,
}

pub struct GameSession {
  state: Arc<State>,
  socket: WebSocket,
  sessions: Sessions,
}

impl GameSession {
  pub fn new(state: Arc<State>, socket: WebSocket, sessions: Sessions) -> Self {
    Self { state, socket, sessions }
  }

  pub async fn run(&self) {
    let mut event_stream = self.socket.events().expect("could not open stream");

    while let Some(event) = event_stream.next().await {
      if let Err(e) = self.handle_event(event.expect("received error in websocket")).await {
        console_error!("Error handling event. Disconnecting. {e:?}");
        if let Err(e) = self.socket.close(Some(4000), Some(format!("{e:?}"))) {
          console_error!("error disconnecting websocket? {e:?}");
        }
      }
    }
  }

  pub async fn handle_event(&self, event: WebsocketEvent) -> anyhow::Result<()> {
    match event {
      WebsocketEvent::Message(msg) => {
        if let Some(text) = msg.text() {
          console_log!("[wsrpi] handling event: {text:?}");
          let request: serde_json::Result<WSRequest> = serde_json::from_str(&text);
          match request {
            Ok(request) => {
              let request_id = request.id.clone();
              console_log!("About to handle request {request:?}");
              let response = self.handle_request(request).await;
              // console_log!("Handled request: {response:?}");
              match response {
                Ok(result) => self.send(&json!({"id": request_id, "payload": &result}))?,
                Err(e) => {
                  console_error!("Error while handling request: {e:?}");
                  self.send(&json!({"id": request_id, "error": format!("{e:?}")}))?
                }
              }
            }
            Err(e) => self.send(
              // If I separated parsing of the frames-with-ids from the WSRequest JSON, I would be
              // able to send this error back with the ID of the request (assuming it had one).
              &json!({"websocket_error": format!("Couldn't parse as a WSRequest: {e:?}")}),
            )?,
          }
        }
      }
      WebsocketEvent::Close(_) => {
        console_log!("[worker] Closed WebSocket");
      }
    }
    Ok(())
  }

  async fn handle_request(&self, request: WSRequest) -> anyhow::Result<serde_json::Value> {
    let game = self.load_game().await?;

    use RPIGameRequest::*;
    match request.request {
      GMGetGame => {
        let game = game.clone();
        let rpi_game = RPIGame(&game);
        Ok(serde_json::to_value(&rpi_game)?)
      }
      PlayerCommand { command } => {
        let changed_game = { game.perform_player_command(todo!("player ID"), command) };
        self.change_game(changed_game).await
      }
      GMCommand { command } => {
        let changed_game = { game.perform_gm_command(command) };
        self.change_game(changed_game).await
      }
      MovementOptions { scene_id, creature_id } => {
        let options = game.get_movement_options(scene_id, creature_id)?;
        Ok(serde_json::to_value(options)?)
      }
      CombatMovementOptions => {
        let options = game.get_combat()?.current_movement_options()?;
        Ok(serde_json::to_value(options)?)
      }
      TargetOptions { scene_id, creature_id, ability_id } => {
        let options = game.get_target_options(scene_id, creature_id, ability_id)?;
        Ok(serde_json::to_value(options)?)
      }
      PreviewVolumeTargets { scene_id, creature_id, ability_id, point } => {
        let scene = game.get_scene(scene_id)?;
        let result = game.preview_volume_targets(scene, creature_id, ability_id, point)?;
        Ok(serde_json::to_value(result)?)
      }
    }
  }

  async fn change_game(
    &self, changed_game: Result<ChangedGame, GameError>,
  ) -> anyhow::Result<serde_json::Value> {
    let changed_game = changed_game.map_err(|e| format!("{e:?}"));
    let result = match changed_game {
      Ok(ChangedGame { logs, game: new_game }) => {
        self.store_game(new_game.clone()).await?;
        let rpi_game = RPIGame(&new_game);
        self.broadcast(&json!({"t": "refresh_game", "game": rpi_game}))?;
        Ok(logs)
      }
      Err(e) => Err(format!("{e:?}")),
    };
    Ok(serde_json::to_value(result)?)
  }

  // ## Storage in Durable Objects
  // DO gives us a KV store, where the size of values is pretty significantly limited (128kB). This
  // requires us to break up a game into little pieces. I considered splitting up each object in a
  // game (scenes, notes, etc) into their own key (e.g. "{snapshot_idx}-scene-{scene_id}"), but that
  // seems like unnecessarily complexity when we can just serialize the entire game and split it
  // into 128kB chunks, with keys like "snap-{snapshot_idx}-chunk-{chunk_idx}". dealing with smaller chunks
  // might be less memory intensive but I doubt that will be a problem.

  async fn load_game(&self) -> anyhow::Result<Game> {
    let snapshot_idx = 0;
    let items = self
      .state
      .storage()
      .list_with_options(ListOptions::new().prefix(&format!("snapshot-{snapshot_idx}-chunk-")))
      .await
      .map_err(anyhow_str).context("Listing DO storage")?;
    if items.size() == 0 {
      // This is a new game!
      return Ok(Default::default());
    }
    let mut serialized_game = String::new();
    for key in items.keys() {
      let key = key.map_err(anyhow_str)?;
      let value = items.get(&key);
      let chunk: String = serde_wasm_bindgen::from_value(value).map_err(anyhow_str)?;
      serialized_game.push_str(&chunk);
    }
    let game: Game = serde_json::from_str(&serialized_game)?;
    Ok(game)
  }

  /// Write an entire Game to Durable Object storage.
  async fn store_game(&self, game: Game) -> anyhow::Result<()> {
    // TODO: snapshots & logs! We could automatically look up the current snapshots in the database,
    // but unfortunately there is no way to list the keys in the database without also retrieving
    // the values.
    //
    // NOTE: Durable Objects support "write coalescing", where you can run multiple PUT operations
    // without awaiting and they will be merged into one "transaction". Here's the thing: I'm not
    // sure this works with the Rust bindings. But we should try something like that. There is also
    // a method called "transaction" which takes a closure to do this explicitly, but that's not
    // exposed in the Rust workers API yet.
    let snapshot_idx = 0; // TODO: calculate next snapshot IDX
    let mut storage = self.state.storage();
    // we should probably use something other than serde_json.
    let serialized_game = serde_json::to_string(&game)?;
    storage
      .put(&format!("snapshot-{snapshot_idx}-chunk-0"), serialized_game)
      .await
      .map_err(anyhow_str)?;
    Ok(())
  }

  fn broadcast<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    // TODO: ignore poison
    let sessions = self.sessions.read().map_err(anyhow_str)?;
    console_log!("Broadcasting a message to {:?} clients", sessions.len());
    for socket in sessions.iter() {
      socket.send_with_str(s.clone()).map_err(anyhow_str)?;
    }
    Ok(())
  }

  fn send<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    Ok(self.socket.send_with_str(s).map_err(|e| anyhow!(format!("{e:?}")))?)
  }
}
