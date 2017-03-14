#![feature(slice_patterns)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate bus;
#[macro_use]
extern crate error_chain;
extern crate rocket;
extern crate rocket_contrib;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

extern crate pandt;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time;

use bus::Bus;

use rocket::State;
use rocket_contrib::JSON;
use rocket::http::Method;

mod cors;
use cors::{CORS, PreflightCORS};

use pandt::types::{App, RPIApp, AbilityID, CreatureID, SceneName, GameCommand, GameError, Point3,
                   PotentialTarget};


error_chain! {
  types { RPIError, RPIErrorKind, RPIResultExt; }

  foreign_links {
    GameError(GameError); // TODO: make GameError use error-chain
    JSONError(serde_json::error::Error);
  }

  errors {
    LockError(resource: String) {
      description("Some locked resource is poisoned. The application probably needs restarted.")
      display("The lock on {} is poisoned. The application probably needs restarted.", resource)
    }
  }
}


type PTResult<X> = Result<CORS<JSON<X>>, RPIError>;

#[derive(Clone)]
struct PT {
  app: Arc<Mutex<App>>,
  pollers: Arc<Mutex<bus::Bus<()>>>,
}

impl PT {
  fn app(&self) -> Result<MutexGuard<App>, RPIError> {
    self.app.lock().map_err(|_| RPIErrorKind::LockError("app".to_string()).into())
  }

  fn pollers(&self) -> Result<MutexGuard<bus::Bus<()>>, RPIError> {
    self.pollers.lock().map_err(|_| RPIErrorKind::LockError("pollers".to_string()).into())
  }
}

#[route(OPTIONS, "/")]
fn options_handler<'a>() -> PreflightCORS {
  CORS::preflight("*").methods(vec![Method::Options, Method::Post]).headers(vec!["Content-Type"])
}

#[get("/")]
fn get_app(pt: State<PT>) -> Result<CORS<String>, RPIError> {
  let app = pt.app()?;
  let result = serde_json::to_string(&RPIApp(&*app))?;
  Ok(CORS::any(result))
}

/// If the client is polling with a non-current app "version", then immediately return the current
/// App. Otherwise, wait 30 seconds for any new changes.
#[get("/poll/<snapshot_len>/<log_len>")]
fn poll_app(pt: State<PT>, snapshot_len: usize, log_len: usize) -> Result<CORS<String>, RPIError> {
  {
    let app = pt.app()?;
    if app.snapshots.len() != snapshot_len ||
       app.snapshots.back().map(|&(_, ref ls)| ls.len()).unwrap_or(0) != log_len {
      let result = serde_json::to_string(&RPIApp(&*app))?;
      return Ok(CORS::any(result));
    }
  }

  let mut reader = pt.pollers()?.add_rx();
  // this will either return a timeout or (); in any case we'll just return the App to the client.
  let _ = reader.recv_timeout(time::Duration::from_secs(30));
  get_app(pt)
}

#[post("/", format = "application/json", data = "<command>")]
fn post_app(command: JSON<GameCommand>, pt: State<PT>) -> Result<CORS<String>, RPIError> {
  let json = {
    let mut app = pt.app()?;
    let result = app.perform_unchecked(command.0);
    serde_json::to_string(&result)
  };
  pt.pollers()?.broadcast(());
  Ok(CORS::any(json?))
}

#[get("/combat_movement_options")]
fn combat_movement_options(pt: State<PT>) -> PTResult<Vec<Point3>> {
  let app = pt.app()?;
  Ok(CORS::any(JSON(app.get_combat_movement_options()?)))
}

#[get("/movement_options/<scene_name>/<cid>")]
fn movement_options(pt: State<PT>, scene_name: String, cid: &str) -> PTResult<Vec<Point3>> {
  let app = pt.app()?;
  let cid = CreatureID::from_str(cid)?;
  let scene = SceneName(scene_name);
  Ok(CORS::any(JSON(app.get_movement_options(scene, cid)?)))
}

#[get("/target_options/<scene>/<cid>/<abid>")]
fn target_options(pt: State<PT>, scene: String, cid: &str, abid: &str)
                  -> PTResult<Vec<PotentialTarget>> {
  let app = pt.app()?;
  let scene = SceneName(scene);
  let cid = CreatureID::from_str(cid)?;
  let abid = AbilityID::new(abid)?;
  Ok(CORS::any(JSON(app.get_target_options(scene, cid, abid)?)))
}

fn main() {
  let filename = env::args().nth(1).unwrap_or("samplegame.yaml".to_string());
  let app: App = {
    let mut appf = File::open(filename).unwrap();
    let mut apps = String::new();
    appf.read_to_string(&mut apps).unwrap();
    serde_yaml::from_str(&apps).unwrap()
  };

  let pt = PT {
    app: Arc::new(Mutex::new(app)),
    pollers: Arc::new(Mutex::new(Bus::new(1000))),
  };

  rocket::ignite()
    .mount("/",
           routes![get_app,
                   poll_app,
                   options_handler,
                   post_app,
                   combat_movement_options,
                   movement_options,
                   target_options])
    .manage(pt)
    .launch();
}
