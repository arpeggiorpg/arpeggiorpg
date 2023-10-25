use std::{cell::RefCell, rc::Rc};

use anyhow::{anyhow, Context};
use futures_util::stream::StreamExt;
use serde::{Deserialize, Serialize};
use serde_json::json;
use worker::{console_error, console_log, ListOptions, State, WebSocket, WebsocketEvent};

use arpeggio::types::{ChangedGame, GMCommand, Game, GameError, PlayerID, RPIGame};
use mtarp::types::{InvitationID, RPIGameRequest, Role};

use crate::{
  anyhow_str,
  durablegame::{GameStorage, Sessions},
};

/// A representation of a request received from a websocket. It has an ID so we can send a response
/// and the client can match them up.
#[derive(Deserialize, Debug)]
struct WSRequest {
  request: RPIGameRequest,
  id: String,
}

pub struct GameSession {
  game_storage: Rc<GameStorage>,
  socket: WebSocket,
  sessions: Sessions,
  role: Role,
  player_id: PlayerID,
}

impl GameSession {
  pub fn new(
    game_storage: Rc<GameStorage>, socket: WebSocket, sessions: Sessions, role: Role,
    player_id: PlayerID,
  ) -> Self {
    Self { game_storage, socket, sessions, role, player_id }
  }

  pub async fn run(&self) {
    if let Err(e) = self.ensure_player().await {
      console_error!("Error ensuring player: {e:?}");
    }
    // TODO: get rid of panics, they will kill the game for everyone!
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

  async fn ensure_player(&self) -> anyhow::Result<()> {
    // If this a new player, let's make sure they're registered in the Game state.
    if self.role == Role::GM {
      return Ok(());
    }
    let changed_game = {
      let game = self.game_storage.game();
      if !game.players.contains_key(&self.player_id) {
        let changed_game =
          game.perform_gm_command(GMCommand::RegisterPlayer(self.player_id.clone()))?;
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
    // TODO: we should not need to load the game on every operation; we should instead just store an
    // Arc<RefCell(?)<Game>>  in-memory in the durable object.
    let game = self.game_storage.game();
    use RPIGameRequest::*;
    match (self.role, request.request) {
      (_, GMGetGame) => {
        // RADIX: TODO: we need separate GMGetGame and PlayerGetGame commands, where the player only
        // gets information about the current scene. This is going to be a big change, though.
        let rpi_game = RPIGame(&game);
        Ok(serde_json::to_value(&rpi_game)?)
      }
      (Role::Player, PlayerCommand { command }) => {
        let changed_game = game.perform_player_command(self.player_id.clone(), command);
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
      _ => Err(anyhow!("You can't run that command as that role.")),
    }
  }

  async fn change_game(
    &self, changed_game: Result<ChangedGame, GameError>,
  ) -> anyhow::Result<serde_json::Value> {
    let changed_game = changed_game.map_err(|e| format!("{e:?}"));
    let result = match changed_game {
      Ok(changed_game) => {
        self.game_storage.store_game(changed_game.clone()).await?;
        let rpi_game = RPIGame(&changed_game.game);
        self.broadcast(&json!({"t": "refresh_game", "game": rpi_game}))?;
        Ok(changed_game.logs)
      }
      Err(e) => Err(format!("{e:?}")),
    };
    Ok(serde_json::to_value(result)?)
  }

  fn broadcast<T: Serialize>(&self, value: &T) -> anyhow::Result<()> {
    let s = serde_json::to_string::<T>(value)?;
    // TODO: ignore poison
    let sessions = self.sessions.borrow();
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
