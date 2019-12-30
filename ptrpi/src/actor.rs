// use std::fs;
// use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
// use std::time::Duration;


use async_std::sync::Mutex;

use failure::Error;
// use futures::{future, Future};
// use futures::sync::oneshot;
// use serde_json;
// use serde_yaml;

use foldertree;
use pandt::types;
use pandt::game::load_app_from_path;


#[derive(Clone)]
pub struct PT {
  saved_game_path: PathBuf,
  module_path: Option<PathBuf>,
  appm: Arc<Mutex<types::App>>,
}

impl PT {
  pub fn new(saved_game_path: PathBuf, module_path: Option<PathBuf>, app: types::App) -> PT {
    PT {
      saved_game_path, module_path,
      appm: Arc::new(Mutex::new(app)),
    }
  }

  pub async fn get_app(&self) -> String {
    app_to_string(&*self.appm.lock().await).unwrap()
  }
}

// pub struct AppActor {
//   app: types::App,
//   waiters: Vec<oneshot::Sender<()>>,
//   saved_game_path: PathBuf,
//   module_path: Option<PathBuf>,
// }

// impl AppActor {
//   pub fn new(app: types::App, saved_game_path: PathBuf, module_path: Option<PathBuf>) -> AppActor {
//     AppActor { app, saved_game_path, module_path, waiters: vec![] }
//   }
// }

// impl Actor for AppActor {
//   type Context = Context<Self>;
// }

// #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
// pub enum Void {}

fn app_to_string(app: &types::App) -> Result<String, Error> {
  Ok(serde_json::to_string(&types::RPIApp(app))?)
}

// macro_rules! handle_actor {
//   ($type:ty => $success:ty, $error:ty; result: $result:ty; $($handler:tt)*) => (
//     impl Message for $type {
//       type Result = Result<$success, $error>;
//     }
//     impl Handler<$type> for AppActor {
//       type Result = $result;
//       $($handler)*
//     }
//   );
//   (async $type:ty => $success:ty, $error: ty; $($handler:tt)*) => (
//     handle_actor! {
//       $type => $success, $error;
//       result: Box<Future<Item = $success, Error = $error>>;
//       $($handler)*
//     }
//   );
//   ($type:ty => $success:ty, $error:ty; $($handler:tt)*) => (
//     handle_actor! {
//       $type => $success, $error;
//       result: Result<$success, $error>;
//       $($handler)*
//     }
//   );
// }

// pub struct GetApp;
// handle_actor! {
//   GetApp => String, Error;
//   fn handle(&mut self, _: GetApp, _: &mut Context<Self>) -> Self::Result {
//     app_to_string(&self.app)
//   }
// }

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

// /// Return the current app, but only if you give the current snapshot/log revisions.
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
