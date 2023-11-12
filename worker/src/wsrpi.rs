use std::{cell::RefCell, rc::Rc};

use anyhow::anyhow;
use futures_util::stream::StreamExt;
use gloo_timers::callback::Timeout;
use serde::{Deserialize, Serialize};
use serde_json::json;
use tracing::{error, info};
use worker::{WebSocket, WebsocketEvent};

use arpeggio::types::{ChangedGame, GMCommand, GameError, RPIGame};
use mtarp::types::{GameMetadata, RPIGameRequest, Role};

use crate::{
  anyhow_str,
  durablegame::{Sessions, WSUser},
  durablestorage::GameStorage,
  images::CFImageService,
};

const IDLE_TIMEOUT: u32 = 10 * 60;

/// A representation of a request received from a websocket. It has an ID so we can send a response
/// and the client can match them up.
#[derive(Deserialize, Debug)]
struct WSRequest {
  request: RPIGameRequest,
  id: String,
}

pub struct GameSession {
  image_service: CFImageService,
  game_storage: Rc<GameStorage>,
  socket: WebSocket,
  sessions: Sessions,
  ws_user: WSUser,
  timeout: RefCell<Timeout>,
  // We will want this to be Rc<RefCell<GameMetadata>> once we start supporting renaming games
  metadata: GameMetadata,
}

impl GameSession {
  pub fn new(
    image_service: CFImageService, game_storage: Rc<GameStorage>, socket: WebSocket,
    sessions: Sessions, ws_user: WSUser, metadata: GameMetadata,
  ) -> Self {
    let timeout = mk_timeout(socket.clone(), ws_user.clone());
    Self {
      image_service,
      game_storage,
      socket,
      sessions,
      ws_user,
      timeout: RefCell::new(timeout),
      metadata,
    }
  }

  #[tracing::instrument(name = "GameSession", skip(self))]
  pub async fn run(&self) {
    if let Err(e) = self.handle_stream().await {
      error!(event = "handle-stream-error", ?e);
    }
  }

  pub async fn handle_stream(&self) -> anyhow::Result<()> {
    self.ensure_player().await?;
    let mut event_stream = self.socket.events().map_err(anyhow_str)?;

    while let Some(event) = event_stream.next().await {
      let event = event.map_err(anyhow_str)?;
      // every time we receive an event, we need to restart the timeout
      self.reset_timeout();
      match self.handle_event(event).await {
        Ok(true) => {
          info!(event = "graceful-shutdown");
          return Ok(());
        }
        Ok(false) => {}
        Err(e) => {
          error!(event = "error-handling-event", ?e);
          if let Err(e) = self.socket.close(Some(4000), Some(format!("{e:?}"))) {
            error!(event = "error-disconnecting", ?e);
          }
        }
      }
    }
    Ok(())
  }

  fn reset_timeout(&self) {
    let new_timeout = mk_timeout(self.socket.clone(), self.ws_user.clone());
    let old_timeout = self.timeout.replace(new_timeout);
    info!(event="refresh-timeout", ?self.ws_user);
    old_timeout.cancel();
  }

  async fn ensure_player(&self) -> anyhow::Result<()> {
    // If this a new player, let's make sure they're registered in the Game state.
    if self.ws_user.role == Role::GM {
      return Ok(());
    }
    let changed_game = {
      let game = self.game_storage.game();
      if !game.players.contains_key(&self.ws_user.player_id) {
        let changed_game = game
          .perform_gm_command(GMCommand::RegisterPlayer { id: self.ws_user.player_id.clone() })?;
        Some(changed_game)
      } else {
        None
      }
    };
    if let Some(changed_game) = changed_game {
      self.game_storage.store_game(changed_game).await?;
    }
    Ok(())
  }

  pub async fn handle_event(&self, event: WebsocketEvent) -> anyhow::Result<bool> {
    match event {
      WebsocketEvent::Message(msg) => {
        if let Some(text) = msg.text() {
          info!(event = "handle-event", text);
          let request: serde_json::Result<WSRequest> = serde_json::from_str(&text);
          match request {
            Ok(request) => {
              let request_id = request.id.clone();
              info!(event = "handling-request", ?request);
              let response = self.handle_request(request).await;
              // console_log!("Handled request: {response:?}");
              match response {
                Ok(result) => self.send(&json!({"id": request_id, "payload": &result}))?,
                Err(e) => {
                  error!(event = "error-handling-request", ?e);
                  self.send(&json!({"id": request_id, "error": format!("{e:?}")}))?
                }
              }
            }
            Err(e) => {
              // This is a little involved because we try to send the request ID back with the error
              // response, so we have to retry parsing it as a Value.
              let error_response = json!({"error": format!("Couldn't parse as a WSRequest: {e}")});
              let mut error_response = error_response.as_object().unwrap().clone();
              if let Ok(value) =
                serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(&text)
              {
                error_response.insert(
                  "id".to_string(),
                  value.get("id").unwrap_or(&serde_json::Value::Null).clone(),
                );
              }
              error!(event = "error-response", ?error_response);
              self.send(&error_response)?;
            }
          }
        }
      }
      WebsocketEvent::Close(close_event) => {
        info!(event = "reciprocating-close", ?close_event);
        self.socket.close(Some(1000), Some("closing as requested")).map_err(anyhow_str)?;
        return Ok(true);
      }
    }
    Ok(false)
  }

  async fn handle_request(&self, request: WSRequest) -> anyhow::Result<serde_json::Value> {
    // TODO: we should not need to load the game on every operation; we should instead just store an
    // Arc<RefCell(?)<Game>>  in-memory in the durable object.
    let game = self.game_storage.game();
    use RPIGameRequest::*;
    match (self.ws_user.role, request.request) {
      (_, GMGetGame) => {
        // RADIX: TODO: we need separate GMGetGame and PlayerGetGame commands, where the player only
        // gets information about the current scene. This is going to be a big change, though.

        // RADIX TODO! Include the last 100 GameLogs here, keyed by GameIndexes. I guess this means
        // we should probably store the last 100 GameLogs in GameStorage, probably in a VecDeque.
        let rpi_game = RPIGame(&game);
        let json = json!({"game": rpi_game, "logs": self.game_storage.recent_logs(), "metadata": self.metadata});
        Ok(serde_json::to_value(json)?)
      }
      (Role::Player, PlayerCommand { command }) => {
        let changed_game = game.perform_player_command(self.ws_user.player_id.clone(), command);
        self.change_game(changed_game).await
      }
      (Role::GM, GMCommand { command }) => {
        let changed_game = game.perform_gm_command(command);
        self.change_game(changed_game).await
      }
      (_, MovementOptions { scene_id, creature_id }) => {
        let options = game.get_movement_options(scene_id, creature_id)?;
        Ok(serde_json::to_value(options)?)
      }
      (_, CombatMovementOptions) => {
        let options = game.get_combat()?.current_movement_options()?;
        Ok(serde_json::to_value(options)?)
      }
      (_, TargetOptions { scene_id, creature_id, ability_id }) => {
        let options = game.get_target_options(scene_id, creature_id, ability_id)?;
        Ok(serde_json::to_value(options)?)
      }
      (_, PreviewVolumeTargets { scene_id, creature_id, ability_id, point }) => {
        let scene = game.get_scene(scene_id)?;
        let result = game.preview_volume_targets(scene, creature_id, ability_id, point)?;
        Ok(serde_json::to_value(result)?)
      }

      (Role::GM, GMGenerateInvitation) => {
        let invitation_id = self.game_storage.create_invitation().await?;
        Ok(serde_json::to_value(invitation_id)?)
      }
      (Role::GM, GMListInvitations) => {
        let invitations = self.game_storage.list_invitations().await?;
        Ok(serde_json::to_value(invitations)?)
      }
      (Role::GM, GMDeleteInvitation { invitation_id }) => {
        let invitations = self.game_storage.delete_invitation(invitation_id).await?;
        Ok(serde_json::to_value(invitations)?)
      }
      (_, UploadImageFromURL { url, purpose }) => {
        let url = self.image_service.upload_from_url(&url, purpose).await?;
        self.game_storage.register_image(&url, purpose).await?;

        let response = json!({"image_url": url.to_string()});
        Ok(serde_json::to_value(response)?)
      }
      (_, RequestUploadImage { purpose }) => {
        let pending_image = self.image_service.request_upload_image(purpose).await?;
        self.game_storage.register_image(&pending_image.final_url, purpose).await?;
        let response = json!({
          "upload_url": pending_image.upload_url.to_string(),
          "final_url": pending_image.final_url.to_string()
        });
        Ok(serde_json::to_value(response)?)
      }
      _ => Err(anyhow!("You can't run that command as that role.")),
    }
  }

  async fn change_game(
    &self, changed_game: Result<ChangedGame, GameError>,
  ) -> anyhow::Result<serde_json::Value> {
    let changed_game = changed_game.map_err(|e| format!("{e:?}"));
    let result = match changed_game {
      Ok(changed_game) => {
        let logs_with_indices = self.game_storage.store_game(changed_game.clone()).await?;
        let rpi_game = RPIGame(&changed_game.game);
        self
          .broadcast(&json!({"t": "refresh_game", "game": rpi_game, "logs": logs_with_indices}))?;
        Ok(changed_game.logs)
      }
      Err(e) => Err(format!("{e:?}")),
    };
    Ok(serde_json::to_value(result)?)
  }

  fn broadcast<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    let mut sessions = self.sessions.borrow_mut();
    info!(event = "broadcast", num_clients = sessions.len());
    sessions.retain_mut(|socket| match socket.send_with_str(s.clone()) {
      Ok(_) => true,
      Err(e) => {
        error!(event = "broadcast-error", ?e);
        false
      }
    });
    Ok(())
  }

  fn send<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    self.socket.send_with_str(s).map_err(|e| anyhow!(format!("{e:?}")))
  }
}

fn mk_timeout(socket: WebSocket, ws_user: WSUser) -> Timeout {
  Timeout::new(IDLE_TIMEOUT * 1000, move || {
    info!(event = "idle-close", ?ws_user);
    if let Err(e) = socket.close(Some(4000), Some("idle timeout")) {
      error!(event = "timeout-close-error", ?e);
    }
  })
}
