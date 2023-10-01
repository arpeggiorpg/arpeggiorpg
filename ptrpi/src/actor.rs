use std::{
  fs,
  io::{Read, Write},
  path::{Path, PathBuf},
  sync::Arc,
  time::Duration,
};

use anyhow::{anyhow, Error};
use futures::channel::oneshot;
use google_cloud_storage::{
  client::Client as StorageClient,
  http::objects::{
    download::Range,
    get::GetObjectRequest,
    list::ListObjectsRequest,
    upload::{Media, UploadObjectRequest, UploadType},
  },
};
use log::{debug, error, info};
use thiserror;

use tokio::{sync::Mutex, time::timeout};

use pandt::types;

/// Not really an actor for now, we're just pretending.
#[derive(Clone)]
pub struct AppActor {
  pub app: Arc<Mutex<types::App>>,
  pub storage_client: Option<(String, Arc<Mutex<StorageClient>>)>,
  pub waiters: Arc<Mutex<Vec<oneshot::Sender<()>>>>,
  pub saved_game_path: PathBuf,
  pub module_path: Option<PathBuf>,
}

impl AppActor {
  pub fn new(
    app: types::App, saved_game_path: PathBuf, module_path: Option<PathBuf>,
    storage_client: Option<(String, StorageClient)>,
  ) -> AppActor {
    AppActor {
      app: Arc::new(Mutex::new(app)),
      // TODO: this probably needs to move inside the same Mutex that protects app.
      storage_client: storage_client
        .map(|(bucket, sclient)| (bucket, Arc::new(Mutex::new(sclient)))),
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
    let log_cmd = command.clone();
    info!("perform_command:start: {:?}", &log_cmd);
    let result = {
      let mut app = self.app.lock().await;
      let result = app.perform_command(command);
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

  /// List the items in a particular prefix, returning a Vec<String> that *don't* contain the prefix
  async fn list_bucket_items(
    &self, bucket: &str, sclient: &StorageClient, prefix: &str,
  ) -> Result<Vec<String>, Error> {
    let list_response = sclient
      .list_objects(&ListObjectsRequest {
        bucket: bucket.to_string(),
        prefix: Some(prefix.to_string()),
        ..ListObjectsRequest::default()
      })
      .await?;
    let items = list_response
      .items
      .unwrap_or(vec![])
      .into_iter()
      .map(|item| {
        item
          .name
          .strip_prefix(prefix)
          .expect("google cloud storage should only return objects with the given prefix")
          .to_string()
      })
      .collect();
    Ok(items)
  }

  pub async fn list_saved_games(&self) -> Result<(Vec<String>, Vec<String>), Error> {
    let result = if let Some((bucket, sclient)) = &self.storage_client {
      let sclient = sclient.lock().await;
      let modules = self.list_bucket_items(bucket, &sclient, "modules/").await?;
      let games = self.list_bucket_items(bucket, &sclient, "games/public/").await?;
      (modules, games)
    } else {
      fn list_dir_into_strings(path: &Path) -> Result<Vec<String>, Error> {
        let mut result = vec![];
        for mpath in fs::read_dir(path)? {
          let path = mpath?;
          if path.file_type()?.is_file() {
            match path.file_name().into_string() {
              Ok(s) => result.push(s),
              Err(x) => error!("Couldn't parse filename as unicode: {:?}", x),
            }
          }
        }
        Ok(result)
      }

      let modules = match self.module_path {
        Some(ref path) => list_dir_into_strings(path.as_ref())?,
        None => vec![],
      };
      (modules, list_dir_into_strings(&self.saved_game_path)?)
    };
    Ok(result)
  }

  pub async fn load_saved_game(
    &self, name: &str, source: types::ModuleSource,
  ) -> Result<String, Error> {
    let app = self.load_app_from_path(source, name).await?;
    let result = app_to_string(&app);
    *self.app.lock().await = app;
    self.ping_waiters().await;
    result
  }

  pub async fn load_into_folder(
    &self, source: types::ModuleSource, name: String, folder_path: foldertree::FolderPath,
  ) -> Result<String, Error> {
    let app = self.load_app_from_path(source, &name).await?;
    let command = types::GameCommand::LoadModule {
      name,
      source,
      game: app.current_game,
      path: folder_path,
    };
    self.perform_command(command).await
  }

  pub async fn save_game(&self, name: String) -> Result<String, Error> {
    self.save_app(&*self.app.lock().await, &name, types::ModuleSource::SavedGame).await?;
    Ok("{}".to_string())
  }

  pub async fn save_module(
    &self, name: String, folder_path: foldertree::FolderPath,
  ) -> Result<String, Error> {
    let new_game = self.app.lock().await.current_game.export_module(&folder_path)?;
    let new_app = types::App::new(new_game);
    self.save_app(&new_app, &name, types::ModuleSource::Module).await?;
    Ok("{}".to_string())
  }

  pub async fn new_game(&self) -> Result<String, Error> {
    let new_game = Default::default();
    let mut app = self.app.lock().await;
    *app = types::App::new(new_game);
    self.ping_waiters().await;
    app_to_string(&app)
  }

  async fn save_app(
    &self, app: &types::App, name: &str, target: types::ModuleSource,
  ) -> Result<(), Error> {
    let new_path = match (target, self.module_path.as_deref()) {
      (types::ModuleSource::SavedGame, _) => child_path(&self.saved_game_path, name)?,
      (types::ModuleSource::Module, Some(module_path)) => child_path(module_path, name)?,
      // this is dumb
      (types::ModuleSource::Module, None) => child_path(&self.saved_game_path, name)?,
    };
    // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the
    // app, without the extra magic that decorates the data with dynamic data for clients.
    let yaml = serde_yaml::to_string(app)?;
    fs::File::create(new_path)?.write_all(yaml.as_bytes())?;

    if let Some((bucket, sclient)) = &self.storage_client {
      let sclient = sclient.lock().await;
      // TODO: this will need to scope saved games per-user in the future
      let prefix = match target {
        types::ModuleSource::SavedGame => "games/public",
        types::ModuleSource::Module => "modules",
      };
      let upload_type = UploadType::Simple(Media {
        name: format!("{prefix}/{name}").into(),
        content_type: "text/vnd.yaml".to_string().into(),
        content_length: None,
      });
      sclient
        .upload_object(
          &UploadObjectRequest { bucket: bucket.clone(), ..Default::default() },
          yaml,
          &upload_type,
        )
        .await?;
    }
    Ok(())
  }

  pub async fn load_app_from_path(
    &self, source: types::ModuleSource, filename: &str,
  ) -> Result<types::App, Error> {
    let app: types::App = if let Some((bucket, sclient)) = &self.storage_client {
      let sclient = sclient.lock().await;
      let filename = match source {
        types::ModuleSource::Module => format!("modules/{filename}"),
        types::ModuleSource::SavedGame => format!("games/public/{filename}"),
      };
      let data = sclient
        .download_object(
          &GetObjectRequest { object: filename, bucket: bucket.to_string(), ..Default::default() },
          &Range::default(),
        )
        .await?;
      serde_yaml::from_slice(&data)?
    } else {
      let filename = match (source, &self.module_path) {
        (types::ModuleSource::Module, Some(module_path)) => module_path.join(filename),
        (types::ModuleSource::Module, None) => return Err(anyhow!("No module source")),
        (types::ModuleSource::SavedGame, _) => self.saved_game_path.join(filename),
      };
      let app_string = {
        let mut appf = fs::File::open(filename.clone())
          .map_err(|_e| anyhow!("Could not open game file {filename:?}"))?;
        let mut apps = String::new();
        appf.read_to_string(&mut apps).unwrap();
        apps
      };
      if filename.extension() == Some(std::ffi::OsStr::new("json")) {
        println!("{filename:?} is JSON");
        serde_json::from_str(&app_string).map_err(|_e| anyhow!("Could not parse JSON"))?
      } else {
        println!("{filename:?} is YAML");
        serde_yaml::from_str(&app_string).map_err(|_e| anyhow!("Could not parse YAML"))?
      }
    };
    app.current_game.validate_campaign()?;
    Ok(app)
  }
}

fn app_to_string(app: &types::App) -> Result<String, Error> {
  Ok(serde_json::to_string(&types::RPIApp(app))?)
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
