use std::sync::{Arc, RwLock};

use anyhow::anyhow;
use futures_util::stream::StreamExt;
use serde::{Deserialize, Serialize};
use serde_json::json;
use worker::{console_error, console_log, State, WebSocket, WebsocketEvent};

use arpeggio::types::{ChangedGame, Game, GameError, RPIGame};
use mtarp::types::RPIGameRequest;

use crate::Sessions;

/// A representation of a request received from a websocket. It has an ID so we can send a response
/// and the client can match them up.
#[derive(Deserialize)]
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
  pub fn new(
    state: Arc<State>, socket: WebSocket, sessions: Sessions,
  ) -> Self {
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
              console_log!("About to handle request");
              let response = self.handle_request(request).await;
              console_log!("Handled request: {response:?}");
              match response {
                Ok(result) => self.send(&json!({"id": request_id, "payload": &result}))?,
                Err(e) => self.send(&json!({"id": request_id, "error": format!("{e:?}")}))?,
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
    let game: Game = self.state.storage().get("THE_GAME").await.map_err(anyhow_str)?;

    use RPIGameRequest::*;
    match request.request {
      GMGetGame => {
        let game = game.clone();
        let rpi_game = RPIGame(&game);
        Ok(serde_json::to_value(&rpi_game)?)
      }
      PlayerCommand { command } => {
        let changed_game = {
          game.perform_player_command(todo!("player ID"), command)
        };
        self.change_game(changed_game).await
      }
      GMCommand { command } => {
        let changed_game = {
          game.perform_gm_command(command)
        };
        self.change_game(changed_game).await
      }
      GMMovementOptions { scene_id, creature_id } => {
        let options = game.get_movement_options(scene_id, creature_id)?;
        Ok(serde_json::to_value(options)?)
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

  async fn store_game(&self, game: Game) -> anyhow::Result<()>{
    self.state.storage().put("THE_GAME", game).await.map_err(anyhow_str)?;
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

/// For some reason I can't just convert a workers::Error to an anyhow::Error because I get crazy
/// errors about how a *mut u8 might escape an async closure or something. So this converts the
/// error to a string before converting it to an anyhow Error.
fn anyhow_str<T: std::fmt::Debug>(e: T) -> anyhow::Error { anyhow!("{e:?}") }
