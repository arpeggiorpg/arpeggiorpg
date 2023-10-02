use std::{fs, io::Read, path::{PathBuf, Path}};

use async_trait::async_trait;
use anyhow::Result as AEResult;
use google_cloud_storage::{
    client::{ClientConfig as StorageClientConfig, Client as StorageClient},
    http::objects::{
      download::Range,
      get::GetObjectRequest,
      list::ListObjectsRequest,
      upload::{Media, UploadObjectRequest, UploadType},
    },
  };
use log::info;

use crate::types::{GameID, UserID, GameIndex, UserGames};

use pandt::types::{Game, GameLog};


#[async_trait]
pub trait PTStorage {

  // User management

  // we might not need create_user; we can just have get_user_games or the others create it if it
  // doesn't exist.
  // async fn create_user(&self, u: UserID, name: String) -> AEResult<()>;
  async fn get_user_games(&self, u: &UserID) -> AEResult<UserGames>;
  async fn add_user_gm_game(&self, u: &UserID, g: &GameID) -> AEResult<()>;
  async fn add_user_player_game(&self, u: &UserID, g: &GameID) -> AEResult<()>;

  // Game management
  async fn create_game(&self, g: &Game) -> AEResult<GameID>;

  /// Load the current state of a game
  async fn load_game(&self, g: &GameID) -> AEResult<(Game, GameIndex)>;
  async fn apply_game_logs(&self, g: &GameID, log: &[GameLog]) -> AEResult<GameIndex>;

  /// Get recent logs for a game so we can show them to the user
  // I am pretty skeptical that this is the API we will end up with.
  async fn get_recent_logs(&self, g: &GameID) -> AEResult<Vec<(GameIndex, GameLog)>>;
  /// Roll back to a specific log index.
  async fn roll_back(&self, g: &GameID, game_idx: GameIndex) -> AEResult<Game>;
}



pub struct CloudStorage {
  bucket: String,
  storage_client: StorageClient,
}

impl CloudStorage {

  pub async fn new(bucket: String) -> AEResult<CloudStorage> {
    let config = StorageClientConfig::default().with_auth().await?;
    let storage_client = StorageClient::new(config);

    Ok(CloudStorage { bucket, storage_client })
  }
}

// impl PTStorage for CloudStorage {}

pub struct FSStorage {
  path: PathBuf,
}

impl FSStorage {
  pub fn new(path: PathBuf) -> FSStorage {
    let users_path = path.join("users");
    let games_path = path.join("games");
    fs::create_dir_all(&users_path).expect("Couldn't create users path");
    fs::create_dir_all(&games_path).expect("Couldn't create games path");
    FSStorage { path }
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

  fn user_game_path(&self, user_id: &UserID) -> PathBuf {
    self.path.join("users").join(format!("{user_id}.json"))
  }

  fn get_game_index(&self, game_id: &GameID) -> AEResult<GameIndex> {
    let game_path = self.path.join(&game_id.to_string());

    // First, figure out what the latest snapshot index is in path/games/{game_id}/*
    let mut snapshot_paths = fs::read_dir(game_path)?.map(|res| res.map(|e| e.path())).collect::<Result<Vec<PathBuf>, _>>()?;
    let mut snapshot_indices: Vec<usize> = snapshot_paths.iter().filter_map(|path| {
      path.file_name()?.to_str()?.parse::<usize>().ok()
    }).collect();
    snapshot_indices.sort();
    let game_idx = *snapshot_indices.last().unwrap_or(&0);
    let log_indices = self.get_log_indices(game_id, game_idx)?;

    // Then, figure out what the latest log index is in path/games/{game_id}/{latest_snapshot_idx}/log-*.json
    let log_idx = *log_indices.last().unwrap_or(&0);
    Ok(GameIndex { game_idx, log_idx })
  }

  fn get_log_indices(&self, game_id: &GameID, snapshot_idx: usize) -> AEResult<Vec<usize>> {
    let snapshot_path = self.path.join(&game_id.to_string()).join(&snapshot_idx.to_string());
    let log_paths = fs::read_dir(snapshot_path)?.map(|res| res.map(|e| e.path())).collect::<Result<Vec<PathBuf>, _>>()?;
    let mut log_indices: Vec<usize> = log_paths.iter().filter_map(|path| {
      path.file_name()?.to_str()?.strip_prefix("log-")?.strip_suffix(".json")?.parse::<usize>().ok()
    }).collect();
    log_indices.sort();
    Ok(log_indices)
  }

}

#[derive(PartialEq, Eq, PartialOrd, Ord, thiserror::Error, Debug)]
#[error("Path is insecure: {name}")]
struct InsecurePathError {
  name: String,
}

/*

Directory layout:

root/
  users/
    {user_id}.json - type UserGames
  games/
    {game_id}/
      {snapshot_index}/
        game.json - type Game
        log-{log_index}.json - type GameLog
*/

#[async_trait]
impl PTStorage for FSStorage {

  async fn get_user_games(&self, user_id: &UserID) -> AEResult<UserGames> {
    let json_file_path = self.user_game_path(user_id);
    let file = fs::File::open(json_file_path);
    let user_games = match file {
      Ok(file) => serde_json::from_reader(file)?,
      Err(err) => {
        info!("Couldn't load file {json_file_path:?}: {err:?}");
        UserGames { gm_games: vec![], player_games: vec![]}
      }
    };
    Ok(user_games)
  }

  async fn add_user_gm_game(&self, user_id: &UserID, game_id: &GameID) -> AEResult<()> {
    let user_games = self.get_user_games(user_id).await?;
    user_games.gm_games.push(*game_id);
    let json_file_path = self.user_game_path(user_id);
    serde_json::to_writer(fs::File::create(json_file_path)?, &user_games)?;
    Ok(())
  }
  async fn add_user_player_game(&self, user_id: &UserID, game_id: &GameID) -> AEResult<()> {
    let user_games = self.get_user_games(user_id).await?;
    user_games.player_games.push(*game_id);
    let json_file_path = self.user_game_path(user_id);
    serde_json::to_writer(fs::File::create(json_file_path)?, &user_games)?;
    Ok(())
  }

  // Game management
  async fn create_game(&self, game: &Game) -> AEResult<GameID> {
    let game_id = uuid::Uuid::new_v4();
    let games_path = self.path.join("games").join(&game_id.to_string()).join("0");
    fs::create_dir_all(games_path);
    let game_file = fs::File::create(games_path.join("game.json"))?;
    serde_json::to_writer(game_file, game)?;

    Ok(game_id)
  }

  /// Load the current state of a game
  async fn load_game(&self, game_id: &GameID) -> AEResult<(Game, GameIndex)> {

    let game_index = self.get_game_index(game_id)?;
    let game_path = self.path.join("games").join(&game_id.to_string());
    let snapshot_path = game_path.join(&game_index.game_idx.to_string());

    let file = fs::File::open(snapshot_path.join("game.json"))?;
    let mut game: Game = serde_json::from_reader(file)?;
    let log_indices = self.get_log_indices(game_id, game_index.game_idx)?;
    for log_idx in log_indices {
      let file = fs::File::open(snapshot_path.join(&format!("log-{log_idx}.json")))?;
      let game_log: GameLog = serde_json::from_reader(file)?;
      game = game.apply_log(&game_log)?;
    }
    Ok((game, game_index))
  }

  async fn apply_game_logs(&self, game_id: &GameID, logs: &[GameLog]) -> AEResult<GameIndex> {
    let game_index = self.get_game_index(game_id)?;
    let snapshot_path = self.path.join("games").join(&game_id.to_string()).join(&game_index.game_idx.to_string());

    // TODO: Actually implement snapshotting when we hit a limit on logs!
    let mut log_idx = game_index.log_idx;
    for log in logs {
      log_idx += 1;
      let log_path = snapshot_path.join(&format!("log-{log_idx}.json"));
      let file = fs::File::create(log_path)?;
      serde_json::to_writer(file, log)?;
    }

    Ok(GameIndex { game_idx: game_index.game_idx, log_idx: log_idx})
  }

  /// Get recent logs for a game so we can show them to the user
  // I am pretty skeptical that this is the API we will end up with.
  async fn get_recent_logs(&self, g: &GameID) -> AEResult<Vec<(GameIndex, GameLog)>> {
    Ok(vec![])
  }
  /// Roll back to a specific log index.
  async fn roll_back(&self, g: &GameID, game_idx: GameIndex) -> AEResult<Game> {
    Ok(Default::default())
  }

}


// impl CloudStorage {
//   /// List the items in a particular prefix, returning a Vec<String> that *don't* contain the prefix
//   async fn list_bucket_items(
//     &self, bucket: &str, sclient: &StorageClient, prefix: &str,
//   ) -> AEResult<Vec<String>> {
//     let list_response = sclient
//       .list_objects(&ListObjectsRequest {
//         bucket: bucket.to_string(),
//         prefix: Some(prefix.to_string()),
//         ..ListObjectsRequest::default()
//       })
//       .await?;
//     let items = list_response
//       .items
//       .unwrap_or(vec![])
//       .into_iter()
//       .map(|item| {
//         item
//           .name
//           .strip_prefix(prefix)
//           .expect("google cloud storage should only return objects with the given prefix")
//           .to_string()
//       })
//       .collect();
//     Ok(items)
//   }

//   async fn save_app(
//     &self, app: &types::App, name: &str, target: types::ModuleSource,
//   ) -> AEResult<()> {

//     let yaml = serde_yaml::to_string(app)?;

//     if let Some(saved_game_path) = &self.saved_game_path {
//       let new_path = match (target, self.module_path.as_deref()) {
//         (types::ModuleSource::SavedGame, _) => child_path(saved_game_path, name)?,
//         (types::ModuleSource::Module, Some(module_path)) => child_path(module_path, name)?,
//         // this is dumb
//         (types::ModuleSource::Module, None) => child_path(saved_game_path, name)?,
//       };
//       // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the
//       // app, without the extra magic that decorates the data with dynamic data for clients.
//       fs::File::create(new_path)?.write_all(yaml.as_bytes())?;
//     }

//     if let Some((bucket, sclient)) = &self.storage_client {
//       let sclient = sclient.lock().await;
//       // TODO: this will need to scope saved games per-user in the future
//       let prefix = match target {
//         types::ModuleSource::SavedGame => "games/public",
//         types::ModuleSource::Module => "modules",
//       };
//       let upload_type = UploadType::Simple(Media {
//         name: format!("{prefix}/{name}").into(),
//         content_type: "text/vnd.yaml".to_string().into(),
//         content_length: None,
//       });
//       sclient
//         .upload_object(
//           &UploadObjectRequest { bucket: bucket.clone(), ..Default::default() },
//           yaml,
//           &upload_type,
//         )
//         .await?;
//     }
//     Ok(())
//   }

//   pub async fn load_app_from_path(
//     &self, source: types::ModuleSource, filename: &str,
//   ) -> AEResult<types::App> {
//     let app: types::App = if let Some((bucket, sclient)) = &self.storage_client {
//       let sclient = sclient.lock().await;
//       let filename = match source {
//         types::ModuleSource::Module => format!("modules/{filename}"),
//         types::ModuleSource::SavedGame => format!("games/public/{filename}"),
//       };
//       let data = sclient
//         .download_object(
//           &GetObjectRequest { object: filename, bucket: bucket.to_string(), ..Default::default() },
//           &Range::default(),
//         )
//         .await?;
//       serde_yaml::from_slice(&data)?
//     } else if let Some(saved_game_path) = &self.saved_game_path {
//       let filename = match (source, &self.module_path) {
//         (types::ModuleSource::Module, Some(module_path)) => module_path.join(filename),
//         (types::ModuleSource::Module, None) => return Err(anyhow!("No module source")),
//         (types::ModuleSource::SavedGame, _) => saved_game_path.join(filename),
//       };
//       let app_string = {
//         let mut appf = fs::File::open(filename.clone())
//           .map_err(|_e| anyhow!("Could not open game file {filename:?}"))?;
//         let mut apps = String::new();
//         appf.read_to_string(&mut apps).unwrap();
//         apps
//       };
//       if filename.extension() == Some(std::ffi::OsStr::new("json")) {
//         println!("{filename:?} is JSON");
//         serde_json::from_str(&app_string).map_err(|_e| anyhow!("Could not parse JSON"))?
//       } else {
//         println!("{filename:?} is YAML");
//         serde_yaml::from_str(&app_string).map_err(|_e| anyhow!("Could not parse YAML"))?
//       }
//     } else {
//       return Err(anyhow!("No saved_game_path or google cloud configured"));
//     };
//     app.current_game.validate_campaign()?;
//     Ok(app)
//   }
// }

