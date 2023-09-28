use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::Error;
use futures::channel::oneshot;
use log::{debug, error, info};
use thiserror;

use tokio::sync::Mutex;
use tokio::time::timeout;

use pandt::game::load_app_from_path;
use pandt::types;

/// Not really an actor for now, we're just pretending.
#[derive(Clone)]
pub struct AppActor {
  pub app: Arc<Mutex<types::App>>,
  pub waiters: Arc<Mutex<Vec<oneshot::Sender<()>>>>,
  pub saved_game_path: PathBuf,
  pub module_path: Option<PathBuf>,
}

impl AppActor {
  pub fn new(app: types::App, saved_game_path: PathBuf, module_path: Option<PathBuf>) -> AppActor {
    AppActor {
      app: Arc::new(Mutex::new(app)),
      saved_game_path,
      module_path,
      waiters: Arc::new(Mutex::new(vec![])),
    }
  }

  /// The methods on this type return Strings containing JSON data.
  /// That's because these responses are generated while a mutex is locked,
  /// and we can't return a reference to the locked data outside of the guarded code.
  pub async fn get_app(&self) -> Result<String, Error> {
    let app = self.app.lock().await;
    app_to_string(&app)
  }

  /// Wait for an app to change and then return it.
  pub async fn poll_app(&self, snapshot_len: usize, log_len: usize) -> Result<String, Error> {
    // First, if the app has already changed, return it immediately.
    debug!("poll_app:start");
    {
      let app = self.app.lock().await;
      if app.snapshots.len() != snapshot_len
        || app.snapshots.back().map(|(_, ls)| ls.len()).unwrap_or(0) != log_len
      {
        return app_to_string(&app);
      }
    }
    // Now, we wait.
    let (sender, receiver) = oneshot::channel();
    {
      let mut waiters = self.waiters.lock().await;
      waiters.push(sender);
    }
    let event = timeout(Duration::from_secs(30), receiver).await;
    match event {
      Ok(x) => {
        // propagate whatever error may have occurred from receiving the event... not sure what
        // would cause this.
        x?
      }
      Err(_) => {
        // Timeout; just return the state of the app
      }
    }
    self.get_app().await
  }

  async fn ping_waiters(&self) {
    for sender in self.waiters.lock().await.drain(0..) {
      if let Err(e) = sender.send(()) {
        error!("ping_waiters:receiver-unavailable when sending {:?}", e);
      }
    }
  }

  pub async fn perform_command(&self, command: types::GameCommand) -> Result<String, Error> {
    let module_path = self.module_path.as_deref();
    let log_cmd = command.clone();
    info!("perform_command:start: {:?}", &log_cmd);
    let result = {
      let mut app = self.app.lock().await;
      let result = app.perform_command(command, &self.saved_game_path, module_path);
      // Convert the rich error into a generic string error to serialize back to the client
      let result = result.map_err(|e| format!("Error: {}", e));
      let result = result.map(|(g, l)| (types::RPIGame(g), l));
      serde_json::to_string(&result)?
    };
    self.ping_waiters().await;
    debug!("perform_command:done: {:?}", &log_cmd);
    Ok(result)
  }

  pub async fn movement_options(
    &self, scene_id: types::SceneID, creature_id: types::CreatureID,
  ) -> Result<String, Error> {
    let app = self.app.lock().await;
    let options = app.get_movement_options(scene_id, creature_id)?;
    Ok(serde_json::to_string(&options)?)
  }

  pub async fn combat_movement_options(&self) -> Result<String, Error> {
    let app = self.app.lock().await;
    let options = app.get_combat_movement_options()?;
    Ok(serde_json::to_string(&options)?)
  }

  pub async fn target_options(
    &self, scene_id: types::SceneID, creature_id: types::CreatureID, ability_id: types::AbilityID,
  ) -> Result<String, Error> {
    let app = self.app.lock().await;
    let options = app.get_target_options(scene_id, creature_id, ability_id)?;
    Ok(serde_json::to_string(&options)?)
  }

  pub async fn preview_volume_targets(
    &self, scene_id: types::SceneID, actor_id: types::CreatureID, ability_id: types::AbilityID,
    point: types::Point3,
  ) -> Result<(Vec<types::CreatureID>, Vec<types::Point3>), Error> {
    let app = self.app.lock().await;
    let targets = app.preview_volume_targets(scene_id, actor_id, ability_id, point)?;
    Ok(targets)
  }

  pub async fn load_saved_game(
    &self, name: String, source: types::ModuleSource,
  ) -> Result<String, Error> {
    let module_path = self.module_path.as_deref();
    let app = load_app_from_path(&self.saved_game_path, module_path, source, &name)?;
    let result = app_to_string(&app);
    *self.app.lock().await = app;
    self.ping_waiters().await;
    result
  }

  pub async fn save_game(&self, name: String) -> Result<String, Error> {
    save_app(&*self.app.lock().await, &name, &self.saved_game_path)?;
    Ok("{}".to_string())
  }

  pub async fn save_module(
    &self, name: String, folder_path: foldertree::FolderPath,
  ) -> Result<String, Error> {
    let new_game = self.app.lock().await.current_game.export_module(&folder_path)?;
    let new_app = types::App::new(new_game);
    save_app(&new_app, &name, &self.saved_game_path)?;
    Ok("{}".to_string())
  }

  pub async fn new_game(&self) -> Result<String, Error> {
    let new_game = Default::default();
    let mut app = self.app.lock().await;
    *app = types::App::new(new_game);
    self.ping_waiters().await;
    app_to_string(&app)
  }
}

fn app_to_string(app: &types::App) -> Result<String, Error> {
  Ok(serde_json::to_string(&types::RPIApp(app))?)
}

fn save_app(app: &types::App, name: &str, file_path: &PathBuf) -> Result<(), Error> {
  let new_path = child_path(file_path, name)?;
  // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
  // without the extra magic that decorates the data with dynamic data for clients.
  let yaml = serde_yaml::to_string(app)?;
  fs::File::create(new_path)?.write_all(yaml.as_bytes())?;
  Ok(())
}

#[derive(PartialEq, Eq, PartialOrd, Ord, thiserror::Error, Debug)]
#[error("Path is insecure: {name}")]
struct InsecurePathError {
  name: String,
}

fn child_path(parent: &Path, name: &str) -> Result<PathBuf, InsecurePathError> {
  if name.contains('/') || name.contains(':') || name.contains('\\') {
    return Err(InsecurePathError { name: name.to_string() });
  }
  let new_path = parent.join(name);
  for p in &new_path {
    if p == "." || p == ".." {
      return Err(InsecurePathError { name: name.to_string() });
    }
  }
  Ok(new_path)
}
