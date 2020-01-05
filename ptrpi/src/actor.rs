use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use failure::Error;
use futures::channel::oneshot;
use futures::{future, Future};
use serde_json;
use serde_yaml;
use tokio::sync::Mutex;
use tokio_core::reactor::Timeout;

use foldertree;
use pandt::game::load_app_from_path;
use pandt::types;

/// Not really an actor for now, we're just pretending.
#[derive(Clone)]
pub struct AppActor {
  app: Arc<Mutex<types::App>>,
  // waiters: Vec<oneshot::Sender<()>>,
  saved_game_path: PathBuf,
  module_path: Option<PathBuf>,
}

impl AppActor {
  pub fn new(app: types::App, saved_game_path: PathBuf, module_path: Option<PathBuf>) -> AppActor {
    AppActor { app: Arc::new(Mutex::new(app)), saved_game_path, module_path }
  }
}

fn app_to_string(app: &types::App) -> Result<String, Error> {
  Ok(serde_json::to_string(&types::RPIApp(app))?)
}

/// The methods on this type return Strings containing JSON data.
/// That's because these responses are generated while a mutex is locked,
/// and we can't return a reference to the locked data outside of the guarded code.
impl AppActor {
  pub async fn get_app(&self) -> Result<String, Error> {
    let app = self.app.lock().await;
    app_to_string(&app)
  }
}

// pub struct PerformCommand(pub types::GameCommand);
// handle_actor! {
//   PerformCommand => String, Error;
//   fn handle(&mut self, command: PerformCommand, _: &mut Context<Self>) -> Self::Result {
//     let module_path = self.module_path.as_ref().map(|b| b.as_path());
//     let result = self.app.perform_command(command.0, &self.saved_game_path, module_path);
//     for sender in self.waiters.drain(0..) {
//       if let Err(e) = sender.send(()) {
//         error!("Unexpected failure while notifying a waiter: {:?}", e);
//       }
//     }
//     // Convert the rich error into a generic string error to serialize back to the client
//     let result = result.map_err(|e| format!("Error: {}", e));
//     let result = result.map(|(g, l)| (types::RPIGame(g), l));
//     Ok(serde_json::to_string(&result)?)
//   }
// }

// pub struct PollApp {
//   pub snapshot_len: usize,
//   pub log_len: usize,
// }
// handle_actor! {
//   async PollApp => String, Error;
//   fn handle(&mut self, cmd: PollApp, ctx: &mut Context<Self>) -> Self::Result {
//     if let Some(r) = try_fut!(get_current_app(&self.app, cmd.snapshot_len, cmd.log_len)) {
//       return Box::new(future::ok(r));
//     }
//     let (sender, receiver) = oneshot::channel();
//     self.waiters.push(sender);

//     let handle = actix::Arbiter::handle();
//     let timeout = Timeout::new(Duration::from_secs(30), handle).expect("Timeout::new panic!");
//     let me = ctx.sync_address();

//     let fut = timeout
//       .select2(receiver)
//       .map_err(|e| match e {
//         future::Either::A((err, _)) => err.into(),
//         future::Either::B((err, _)) => err.into(),
//       })
//       .and_then(move |_| me.send(GetApp).from_err().and_then(|s| s));
//     Box::new(fut)
//   }
// }

// fn get_current_app(
//   app: &types::App, snapshot_len: usize, log_len: usize
// ) -> Result<Option<String>, Error> {
//   if app.snapshots.len() != snapshot_len
//     || app.snapshots.back().map(|&(_, ref ls)| ls.len()).unwrap_or(0) != log_len
//   {
//     app_to_string(app).map(Some)
//   } else {
//     Ok(None)
//   }
// }

// pub struct MovementOptions {
//   pub creature_id: types::CreatureID,
//   pub scene_id: types::SceneID,
// }
// handle_actor! {
//   MovementOptions => String, Error;
//   fn handle(&mut self, cmd: MovementOptions, _: &mut Context<Self>) -> Self::Result {
//     let options = &self.app.get_movement_options(cmd.scene_id, cmd.creature_id)?;
//     serde_json::to_string(options).map_err(From::from)
//   }
// }

// pub struct CombatMovementOptions;
// handle_actor! {
//   CombatMovementOptions => String, Error;
//   fn handle(&mut self, _: CombatMovementOptions, _: &mut Context<AppActor>) -> Self::Result {
//     Ok(serde_json::to_string(&self.app.get_combat_movement_options()?)?)
//   }
// }

// pub struct TargetOptions {
//   pub creature_id: types::CreatureID,
//   pub scene_id: types::SceneID,
//   pub ability_id: types::AbilityID,
// }
// handle_actor! {
//   TargetOptions => String, Error;
//   fn handle(&mut self, cmd: TargetOptions, _: &mut Context<AppActor>) -> Self::Result {
//     Ok(serde_json::to_string(&self.app.get_target_options(
//       cmd.scene_id,
//       cmd.creature_id,
//       cmd.ability_id,
//     )?)?)
//   }
// }

// pub struct PreviewVolumeTargets {
//   pub scene_id: types::SceneID,
//   pub actor_id: types::CreatureID,
//   pub ability_id: types::AbilityID,
//   pub point: types::Point3,
// }
// handle_actor! {
//   PreviewVolumeTargets => String, Error;
//   fn handle(&mut self, cmd: PreviewVolumeTargets, _: &mut Context<AppActor>) -> Self::Result {
//     Ok(serde_json::to_string(&self.app.preview_volume_targets(
//       cmd.scene_id,
//       cmd.actor_id,
//       cmd.ability_id,
//       cmd.point,
//     )?)?)
//   }
// }

// pub struct LoadSavedGame {
//   pub name: String,
//   pub source: types::ModuleSource,
// }
// handle_actor! {
//   LoadSavedGame => String, Error;
//   fn handle(&mut self, cmd: LoadSavedGame, _: &mut Context<AppActor>) -> Self::Result {
//     let module_path = self.module_path.as_ref().map(|b| b.as_path());
//     let app = load_app_from_path(&self.saved_game_path, module_path, cmd.source, &cmd.name)?;
//     self.app = app;
//     app_to_string(&self.app)
//   }
// }

// pub struct SaveGame(pub String);
// handle_actor! {
//   SaveGame => String, Error;
//   fn handle(&mut self, cmd: SaveGame, _: &mut Context<AppActor>) -> Self::Result {
//     save_app(&self.app, &cmd.0, &self.saved_game_path)?;
//     Ok("{}".to_string())
//   }
// }

// pub struct SaveModule {
//   pub name: String,
//   pub path: foldertree::FolderPath,
// }
// handle_actor! {
//   SaveModule => String, Error;
//   fn handle(&mut self, cmd: SaveModule, _: &mut Context<AppActor>) -> Self::Result {
//     let new_game = self.app.current_game.export_module(&cmd.path)?;
//     let new_app = types::App::new(new_game);
//     save_app(&new_app, &cmd.name, &self.saved_game_path)?;
//     Ok("{}".to_string())
//   }
// }

// fn save_app(app: &types::App, name: &str, file_path: &PathBuf) -> Result<(), Error> {
//   let new_path = child_path(file_path, name)?;
//   // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
//   // without the extra magic that decorates the data with dynamic data for clients.
//   let yaml = serde_yaml::to_string(app)?;
//   fs::File::create(new_path)?.write_all(yaml.as_bytes())?;
//   Ok(())
// }

// pub struct NewGame;
// handle_actor! {
//   NewGame => String, Error;
//   fn handle(&mut self, _: NewGame, _: &mut Context<AppActor>) -> Self::Result {
//     let new_game = Default::default();
//     self.app = types::App::new(new_game);
//     app_to_string(&self.app)
//   }
// }

// #[derive(PartialEq, Eq, PartialOrd, Ord, Fail, Debug)]
// #[fail(display = "Path is insecure: {}", name)]
// struct InsecurePathError {
//   name: String,
// }

// fn child_path(parent: &Path, name: &str) -> Result<PathBuf, InsecurePathError> {
//   if name.contains('/') || name.contains(':') || name.contains('\\') {
//     return Err(InsecurePathError { name: name.to_string() });
//   }
//   let new_path = parent.join(name);
//   for p in &new_path {
//     if p == "." || p == ".." {
//       return Err(InsecurePathError { name: name.to_string() });
//     }
//   }
//   Ok(new_path)
// }
