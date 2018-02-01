use std::path::PathBuf;
use std::time::Duration;

use actix;
use actix::{Actor};
use actix::fut::WrapFuture;
use futures::{Future};
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


pub struct GetApp;

impl actix::ResponseType for GetApp {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<GetApp> for AppActor {
  type Result = actix::MessageResult<GetApp>;
  fn handle(&mut self, _: GetApp, _: &mut actix::Context<Self>) -> Self::Result {
    let body = serde_json::to_string(&self.app)?;
    Ok(body)
  }
}


pub struct PerformCommand(types::GameCommand);

impl actix::ResponseType for PerformCommand {
  type Item = ();
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


pub struct PollApp;

impl actix::ResponseType for PollApp {
  type Item = String;
  type Error = ::RPIError;
}

impl actix::Handler<PollApp> for AppActor {
  type Result = actix::Response<Self, PollApp>;
  fn handle(&mut self, _: PollApp, _: &mut actix::Context<Self>) -> Self::Result {
    let (sender, receiver) = oneshot::channel();
    self.waiters.push(sender);

    let handle = actix::Arbiter::handle();
    let timeout = Timeout::new(Duration::from_secs(30), handle).unwrap();

    let fut = timeout
      .select2(receiver)
      .and_then(move |_|
        // TODO: Return app!
        Ok("".to_string()))
      .map_err(|e| {
        error!("Error while polling: {:?}", e);
        ::RPIError::MessageError("Error while polling".to_string())
      });

    Self::async_reply(fut.into_actor(self))
  }
}
