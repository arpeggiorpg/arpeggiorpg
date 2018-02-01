use std::path::PathBuf;
use std::time::Duration;

use actix;
use actix::{Actor, AsyncContext};
use actix::fut::WrapFuture;
use futures::{Future, future};
use futures::sync::oneshot;
use tokio_core::reactor::Timeout;
use serde_json;

use pandt::types;

pub struct AppActor {
  app: types::App,
  waiters: Vec<oneshot::Sender<()>>,
  saved_game_path: PathBuf,
}

impl AppActor {
  pub fn new(app: types::App, saved_game_path: PathBuf) -> AppActor {
    AppActor { app, saved_game_path, waiters: vec![] }
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


pub struct PerformCommand(types::GameCommand);

impl actix::ResponseType for PerformCommand {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PerformCommand> for AppActor {
  type Result = actix::MessageResult<PerformCommand>;
  fn handle(&mut self, command: PerformCommand, _: &mut actix::Context<Self>) -> Self::Result {
    let _result = self.app.perform_command(command.0, self.saved_game_path.clone())?;
    for sender in self.waiters.drain(0..) {
      if let Err(e) = sender.send(()) {
        error!("Unexpected failure while notifying a waiter: {:?}", e);
      }
    }
    Ok(())
  }
}


pub struct PollApp {
  pub snapshot_len: usize,
  pub log_len: usize
}

impl actix::ResponseType for PollApp {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PollApp> for AppActor {
  type Result = actix::Response<Self, PollApp>;
  fn handle(&mut self, cmd: PollApp, ctx: &mut actix::Context<Self>) -> Self::Result {
    // In actix master we can specify the result directly as a Box<Future>> so we won't need this inner function!
    fn handle(actor: &mut AppActor, cmd: PollApp, ctx: &mut actix::Context<AppActor>)
      -> Box<Future<Item=String, Error=::RPIError>> {
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
        .and_then(move |_| me.call_fut(GetApp).map_err(|_| panic!()).and_then(|s| s))
        ;
      Box::new(fut)
    }
    Self::async_reply(handle(self, cmd, ctx).into_actor(self))
  }
}

fn get_current_app(app: &types::App, snapshot_len: usize, log_len: usize)
  -> Result<Option<String>, ::RPIError>
{
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

