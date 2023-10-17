use std::sync::{Arc, Mutex};

use anyhow::anyhow;
use futures_util::stream::StreamExt;
use serde::{Deserialize, Serialize};
use serde_json::json;
use worker::{console_error, console_log, WebSocket, WebsocketEvent};

use arpeggio::types::{ChangedGame, GMCommand, Game};
use mtarp::actor::GMService;

#[derive(Deserialize)]
struct WSRequest {
  command: WSCommand,
  id: String,
}

#[derive(Deserialize)]
#[serde(tag = "t")]
enum WSCommand {
  GetGame,
  GMCommand { command: GMCommand },
}

pub struct GameSession {
  game: Arc<Mutex<Game>>,
  socket: WebSocket,
}

impl GameSession {
  pub fn new(game: Arc<Mutex<Game>>, socket: WebSocket) -> Self { Self { game, socket } }

  pub async fn run(&self) {
    let mut event_stream = self.socket.events().expect("could not open stream");

    while let Some(event) = event_stream.next().await {
      if let Err(e) = self.handle_event(event.expect("received error in websocket")).await {
        console_error!("Error handling event. Disconnecting. {e:?}");
        if let Err(e) = self.socket.close(Some(1011), Some(format!("{e:?}"))) {
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
              match self.handle_request(request).await {
                Ok(result) => self.send(&json!({"id": request_id, "payload": &result}))?,
                Err(e) => self.send(&json!({"id": request_id, "error": format!("{e:?}")}))?,
              }
            }
            Err(e) => self.send(
              // If I separated parsing of the frames-with-ids from the WSCommand JSON, I would be
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
    match request.command {
      WSCommand::GetGame => Ok(serde_json::to_value(&self.game.lock().expect("no poison").clone())?),
      WSCommand::GMCommand { command } => {
        let mut game = self.game.lock().expect("no poison");
        let changed_game = game.perform_gm_command(command).map_err(|e| format!("{e:?}"));
        let result = match changed_game {
          Ok(ChangedGame { logs, game: new_game }) => {
            *game = new_game;
            Ok(logs)
          }
          Err(e) => Err(format!("{e:?}")),
        };
        Ok(serde_json::to_value(result)?)
      }
    }
  }

  fn send<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    Ok(self.socket.send_with_str(s).map_err(|e| anyhow!(format!("{e:?}")))?)
  }
}
