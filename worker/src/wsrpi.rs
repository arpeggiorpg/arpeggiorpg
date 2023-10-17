use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use serde_json::json;
use worker::{console_log, WebSocket, WebsocketEvent};

use arpeggio::types::{GMCommand, Game};
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

pub async fn handle_event(socket: &WebSocket, event: WebsocketEvent) -> anyhow::Result<()> {
  match event {
    WebsocketEvent::Message(msg) => {
      if let Some(text) = msg.text() {
        console_log!("[wsrpi] handling event: {text:?}");
        let request: serde_json::Result<WSRequest> = serde_json::from_str(&text);
        match request {
          Ok(request) => {
            let request_id = request.id.clone();
            match handle_request(socket, request).await {
              Ok(result) => send(socket, &json!({"id": request_id, "payload": &result}))?,
              Err(e) => send(socket, &json!({"id": request_id, "error": format!("{e:?}")}))?
            }
          }
          Err(e) => send(
            socket,
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

async fn handle_request(socket: &WebSocket, request: WSRequest) -> anyhow::Result<serde_json::Value> {
  match request.command {
    WSCommand::GetGame => {
      let game: Game = Default::default();
      Ok(serde_json::to_value(&game)?)
    }
    WSCommand::GMCommand { command } => {
      let game: Game = Default::default(); // LOL RADIX
      let changed_game = game.perform_gm_command(command).map_err(|e| format!("{e:?}"));
      Ok(serde_json::to_value(changed_game)?)
    }
  }
}

fn send<T: Serialize>(socket: &WebSocket, value: &T) -> anyhow::Result<()> {
  let s = serde_json::to_string::<T>(value)?;
  Ok(socket.send_with_str(s).map_err(|e| anyhow!(format!("{e:?}")))?)
}
