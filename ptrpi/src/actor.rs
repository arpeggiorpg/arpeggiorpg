use std::{collections::HashMap, sync::Arc, time::Duration};

use anyhow::{anyhow, Context, Result as AEResult};
use futures::channel::oneshot;
use tokio::{sync::Mutex, time::timeout};
use tracing::{debug, error, info, instrument};

use crate::{
  storage::{load_game, Storage},
  types::{GameID, GameIndex, GameList, UserID, Role},
};

use pandt::types::{self, Game, GameCommand};

#[derive(thiserror::Error, Debug)]
#[error("Authentication Error")]
pub struct AuthenticationError {
  #[from]
  pub from: anyhow::Error,
}

/// AuthenticatableService is a capability layer that hands out AuthenticatedServices to users who
/// authenticate.
#[derive(Clone)]
pub struct AuthenticatableService {
  pub storage: Arc<dyn Storage>,

  ping_service: Arc<PingService>,

  /// This is google client ID
  pub google_client_id: String,
  /// Cached certs for use by google_signin
  pub cached_certs: Arc<Mutex<google_signin::CachedCerts>>,
}

impl AuthenticatableService {
  pub fn new(storage: Arc<dyn Storage>, google_client_id: String) -> AuthenticatableService {
    AuthenticatableService {
      storage,
      google_client_id,
      cached_certs: Arc::new(Mutex::new(google_signin::CachedCerts::new())),
      ping_service: Arc::new(PingService::new()),
    }
  }

  /// Verify a google ID token and return an AuthenticatedService if it's valid.
  pub async fn authenticate(
    &self, google_id_token: String,
  ) -> Result<AuthenticatedService, AuthenticationError> {
    let user_id = self
      .validate_google_token(&google_id_token)
      .await
      .context(format!("Validating Google ID Token"))
      .map_err(|e| AuthenticationError { from: e })?;
    return Ok(AuthenticatedService {
      user_id,
      storage: self.storage.clone(),
      ping_service: self.ping_service.clone(),
    });
  }

  async fn validate_google_token(&self, id_token: &str) -> AEResult<UserID> {
    let mut certs = self.cached_certs.lock().await;
    certs.refresh_if_needed().await?;
    let mut client = google_signin::Client::new();
    client.audiences.push(self.google_client_id.clone());
    let id_info = client.verify(id_token, &certs).await?;
    let expiry = std::time::UNIX_EPOCH + Duration::from_secs(id_info.exp);
    let time_until_expiry = expiry.duration_since(std::time::SystemTime::now());
    debug!(
      "validate-token: email={:?} name={:?} sub={:?} expires={:?} expires IN: {:?}",
      id_info.email, id_info.name, id_info.sub, id_info.exp, time_until_expiry
    );
    Ok(UserID(format!("google_{}", id_info.sub)))
  }
}

/// AuthenticatedService is a capability layer that exposes functionality to authenticated users.
/// One important responsibility is that this layer *authorizes* users to access specific games and
/// hands out PlayerServices and GMServices.
pub struct AuthenticatedService {
  pub user_id: UserID,
  pub storage: Arc<dyn Storage>,
  ping_service: Arc<PingService>,
}

impl AuthenticatedService {
  pub async fn new_game(&self, name: String) -> AEResult<GameID> {
    let game: Game = Default::default();
    let game_id = self.storage.create_game(&self.user_id, &game, &name).await?;
    Ok(game_id.clone())
  }

  pub async fn list_games(&self) -> AEResult<GameList> {
    let usergames = self.storage.list_user_games(&self.user_id).await?;
    let mut games = vec![];
    for profile in usergames {
      games.push((profile.clone(), self.storage.load_game_metadata(&profile.game_id).await?));
    }
    Ok(GameList { games })
  }

  async fn find_game(&self, game_id: &GameID, role: Role) -> AEResult<(Game, GameIndex)> {
    let games = self.storage.list_user_games(&self.user_id).await?;
    for game in games {
      if game.user_id == self.user_id && game.role == Role::GM {
        return Ok(load_game(&*self.storage, game_id).await.context(format!("Loading game {game_id:?}"))?);
      }
    }
    Err(anyhow!("User {:?} is not a {role:?} of game {game_id:?}", self.user_id))
  }

  pub async fn gm(&self, game_id: &GameID) -> AEResult<GameService> {
    let (game, game_index) = self.find_game(game_id, Role::GM).await?;
    // TODO Actually return a GMService!!!
    Ok(GameService {
      storage: self.storage.clone(),
      game_id: game_id.clone(),
      game,
      game_index,
      ping_service: self.ping_service.clone(),
    })
  }

  pub async fn player(&self, game_id: &GameID) -> AEResult<GameService> {
    let (game, game_index) = self.find_game(game_id, Role::Player).await?;
    // TODO Actually return a PlayerService!!!
    Ok(GameService {
      storage: self.storage.clone(),
      game_id: game_id.clone(),
      game,
      game_index,
      ping_service: self.ping_service.clone(),
    })
  }
}

// TODO: GameService should not exist - it should be split into PlayerService and GMService.
pub struct GameService {
  pub storage: Arc<dyn Storage>,
  pub game: Game,
  pub game_index: GameIndex,
  pub game_id: GameID,
  ping_service: Arc<PingService>,
}

impl GameService {
  // Okay, so all these methods have returned JSON strings. This kind of sucks. Ideally we could
  // return references to live objects, but I have not been able to figure out how to do this. I am
  // pretty sure the answer involves MappedMutexGuard, but combining that with RPIGame has been very
  // difficult for me.

  /// Wait for a Game to change and then return it.
  #[instrument(level = "debug", skip(self))]
  pub async fn poll_game(&self, game_index: GameIndex) -> AEResult<()> {
    // First, if the app has already changed, return it immediately.
    if self.game_index != game_index {
      return Ok(());
    }
    // Now, we wait.
    let (sender, receiver) = oneshot::channel();
    self.ping_service.register_waiter(&self.game_id, sender).await;
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

  pub async fn perform_command(&self, command: GameCommand) -> AEResult<types::ChangedGame> {
    let log_cmd = command.clone();
    info!("perform_command:start: {:?}", &log_cmd);
    let changed_game = self.game.perform_command(command)?;
    self.storage.apply_game_logs(&self.game_id, &changed_game.logs).await?;
    self.ping_service.ping(&self.game_id).await?;
    debug!("perform_command:done: {:?}", &log_cmd);
    Ok(changed_game)
  }

  pub async fn movement_options(
    &self, scene_id: types::SceneID, creature_id: types::CreatureID,
  ) -> AEResult<Vec<types::Point3>> {
    let options = self.game.get_movement_options(scene_id, creature_id)?;
    Ok(options)
  }

  pub async fn combat_movement_options(&self) -> AEResult<Vec<types::Point3>> {
    let options = self.game.get_combat()?.current_movement_options()?;
    Ok(options)
  }

  pub async fn target_options(
    &self, scene_id: types::SceneID, creature_id: types::CreatureID, ability_id: types::AbilityID,
  ) -> AEResult<types::PotentialTargets> {
    let options = self.game.get_target_options(scene_id, creature_id, ability_id)?;
    Ok(options)
  }

  pub async fn preview_volume_targets(
    &self, scene_id: types::SceneID, actor_id: types::CreatureID, ability_id: types::AbilityID,
    point: types::Point3,
  ) -> AEResult<(Vec<types::CreatureID>, Vec<types::Point3>)> {
    let scene = self.game.get_scene(scene_id)?;
    let targets = self.game.preview_volume_targets(scene, actor_id, ability_id, point)?;
    Ok(targets)
  }

  pub async fn load_into_folder(
    &self, game_id_to_load: &GameID, folder_path: foldertree::FolderPath,
  ) -> AEResult<String> {
    let game_to_load = load_game(&*self.storage, game_id_to_load).await?;
    Ok("".to_string())
    // RADIX FIXME TODO: update the GameCommand for LoadModule.
    // let command = GameCommand::LoadModule {
    //   game: game_to_load,
    //   path: folder_path,
    // };
    // self.perform_command(command).await
  }
}

struct PingService {
  waiters: Mutex<HashMap<GameID, Vec<oneshot::Sender<()>>>>,
}

impl PingService {
  pub fn new() -> PingService { PingService { waiters: Mutex::new(HashMap::new()) } }

  pub async fn register_waiter(&self, game_id: &GameID, sender: oneshot::Sender<()>) {
    let mut waiters = self.waiters.lock().await;
    let game_waiters = waiters.entry(game_id.clone());
    game_waiters.and_modify(|v| v.push(sender)).or_insert(vec![]);
  }

  pub async fn ping(&self, game_id: &GameID) -> AEResult<()> {
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
}
