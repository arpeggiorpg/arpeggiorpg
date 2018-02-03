use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::time::Duration;

use actix;
use actix::{Actor, AsyncContext};
use actix::fut::WrapFuture;
use futures::{future, Future};
use futures::sync::oneshot;
use tokio_core::reactor::Timeout;
use serde_json;
use serde_yaml;

use pandt::{foldertree, types};

pub struct AppActor {
  app: types::App,
  waiters: Vec<oneshot::Sender<()>>,
  saved_game_path: PathBuf,
}

impl AppActor {
  pub fn new(app: types::App, saved_game_path: PathBuf) -> AppActor {
    AppActor {
      app,
      saved_game_path,
      waiters: vec![],
    }
  }
}

impl actix::Actor for AppActor {
  type Context = actix::Context<Self>;
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Void {}

fn app_to_string(app: &types::App) -> Result<String, ::RPIError> {
  Ok(serde_json::to_string(&types::RPIApp(app))?)
}

pub struct GetApp;

impl actix::ResponseType for GetApp {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<GetApp> for AppActor {
  type Result = actix::MessageResult<GetApp>;
  fn handle(&mut self, _: GetApp, _: &mut actix::Context<Self>) -> Self::Result {
    app_to_string(&self.app)
  }
}

pub struct PerformCommand(pub types::GameCommand);

impl actix::ResponseType for PerformCommand {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PerformCommand> for AppActor {
  type Result = actix::MessageResult<PerformCommand>;
  fn handle(&mut self, command: PerformCommand, _: &mut actix::Context<Self>) -> Self::Result {
    let result = self
      .app
      .perform_command(command.0, self.saved_game_path.clone());
    for sender in self.waiters.drain(0..) {
      if let Err(e) = sender.send(()) {
        error!("Unexpected failure while notifying a waiter: {:?}", e);
      }
    }
    let result = result.map_err(|e| format!("Error: {}", e));
    let result = result.map(|(g, l)| (types::RPIGame(g), l));
    Ok(serde_json::to_string(&result)?)
  }
}

pub struct PollApp {
  pub snapshot_len: usize,
  pub log_len: usize,
}

impl actix::ResponseType for PollApp {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PollApp> for AppActor {
  type Result = actix::Response<Self, PollApp>;
  fn handle(&mut self, cmd: PollApp, ctx: &mut actix::Context<Self>) -> Self::Result {
    // In actix master we can specify the result directly as a Box<Future>> so we won't need this inner function!
    fn handle(
      actor: &mut AppActor, cmd: PollApp, ctx: &mut actix::Context<AppActor>
    ) -> Box<Future<Item = String, Error = ::RPIError>> {
      if let Some(r) = try_fut!(get_current_app(&actor.app, cmd.snapshot_len, cmd.log_len)) {
        return Box::new(future::ok(r));
      }
      let (sender, receiver) = oneshot::channel();
      actor.waiters.push(sender);

      let handle = actix::Arbiter::handle();
      let timeout = Timeout::new(Duration::from_secs(30), handle).expect("Timeout::new panic!");
      let me: actix::Address<AppActor> = ctx.address();

      let fut = timeout
        .select2(receiver)
        .map_err(|e| {
          error!("Error while polling: {:?}", e);
          ::RPIError::MessageError("Error while polling".to_string())
        })
        .and_then(move |_| me.call_fut(GetApp).map_err(|_| panic!()).and_then(|s| s));
      Box::new(fut)
    }
    Self::async_reply(handle(self, cmd, ctx).into_actor(self))
  }
}

fn get_current_app(
  app: &types::App, snapshot_len: usize, log_len: usize
) -> Result<Option<String>, ::RPIError> {
  if app.snapshots.len() != snapshot_len
    || app
      .snapshots
      .back()
      .map(|&(_, ref ls)| ls.len())
      .unwrap_or(0) != log_len
  {
    Ok(Some(app_to_string(app)?))
  } else {
    Ok(None)
  }
}

pub struct MovementOptions {
  pub creature_id: types::CreatureID,
  pub scene_id: types::SceneID,
}
impl actix::ResponseType for MovementOptions {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<MovementOptions> for AppActor {
  type Result = actix::MessageResult<MovementOptions>;

  fn handle(&mut self, cmd: MovementOptions, _: &mut actix::Context<AppActor>) -> Self::Result {
    Ok(serde_json::to_string(&self
      .app
      .get_movement_options(cmd.scene_id, cmd.creature_id)?)?)
  }
}

pub struct CombatMovementOptions;
impl actix::ResponseType for CombatMovementOptions {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<CombatMovementOptions> for AppActor {
  type Result = actix::MessageResult<CombatMovementOptions>;

  fn handle(&mut self, _: CombatMovementOptions, _: &mut actix::Context<AppActor>) -> Self::Result {
    Ok(serde_json::to_string(&self
      .app
      .get_combat_movement_options()?)?)
  }
}

pub struct TargetOptions {
  pub creature_id: types::CreatureID,
  pub scene_id: types::SceneID,
  pub ability_id: types::AbilityID,
}
impl actix::ResponseType for TargetOptions {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<TargetOptions> for AppActor {
  type Result = actix::MessageResult<TargetOptions>;

  fn handle(&mut self, cmd: TargetOptions, _: &mut actix::Context<AppActor>) -> Self::Result {
    Ok(serde_json::to_string(&self.app.get_target_options(
      cmd.scene_id,
      cmd.creature_id,
      cmd.ability_id,
    )?)?)
  }
}

pub struct PreviewVolumeTargets {
  pub scene_id: types::SceneID,
  pub actor_id: types::CreatureID,
  pub ability_id: types::AbilityID,
  pub point: types::Point3,
}
impl actix::ResponseType for PreviewVolumeTargets {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PreviewVolumeTargets> for AppActor {
  type Result = actix::MessageResult<PreviewVolumeTargets>;

  fn handle(
    &mut self, cmd: PreviewVolumeTargets, _: &mut actix::Context<AppActor>
  ) -> Self::Result {
    Ok(serde_json::to_string(&self.app.preview_volume_targets(
      cmd.scene_id,
      cmd.actor_id,
      cmd.ability_id,
      cmd.point,
    )?)?)
  }
}

pub struct LoadSavedGame(pub String);
impl actix::ResponseType for LoadSavedGame {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<LoadSavedGame> for AppActor {
  type Result = actix::MessageResult<LoadSavedGame>;

  fn handle(&mut self, cmd: LoadSavedGame, _: &mut actix::Context<AppActor>) -> Self::Result {
    let path = child_path(&self.saved_game_path, &cmd.0)?;
    let mut buffer = String::new();
    fs::File::open(path)?.read_to_string(&mut buffer)?;
    self.app = serde_yaml::from_str(&buffer)?;
    app_to_string(&self.app)
  }
}

pub struct SaveGame(pub String);
impl actix::ResponseType for SaveGame {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<SaveGame> for AppActor {
  type Result = actix::MessageResult<SaveGame>;

  fn handle(&mut self, cmd: SaveGame, _: &mut actix::Context<AppActor>) -> Self::Result {
    save_app(&self.app, &cmd.0, &self.saved_game_path)?;
    Ok("{}".to_string())
  }
}

pub struct SaveModule {
  pub name: String,
  pub path: foldertree::FolderPath,
}
impl actix::ResponseType for SaveModule {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<SaveModule> for AppActor {
  type Result = actix::MessageResult<SaveModule>;

  fn handle(&mut self, cmd: SaveModule, _: &mut actix::Context<AppActor>) -> Self::Result {
    let new_game = self.app.current_game.export_module(&cmd.path)?;
    let new_app = types::App::new(new_game);
    save_app(&new_app, &cmd.name, &self.saved_game_path)?;
    Ok("{}".to_string())
  }
}

fn save_app(app: &types::App, name: &str, file_path: &PathBuf) -> Result<(), ::RPIError> {
  let new_path = child_path(file_path, name)?;
  // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
  // without the extra magic that decorates the data with dynamic data for clients.
  let yaml = serde_yaml::to_string(app)?;
  fs::File::create(new_path)?.write_all(yaml.as_bytes())?;
  Ok(())
}

fn child_path(parent: &PathBuf, name: &str) -> Result<PathBuf, ::RPIError> {
  if name.contains('/') || name.contains(':') || name.contains('\\') {
    bail!(::RPIError::InsecurePath(name.to_string()));
  }
  let new_path = parent.join(name);
  for p in &new_path {
    if p == "." || p == ".." {
      bail!(::RPIError::InsecurePath(name.to_string()));
    }
  }
  Ok(new_path)
}
