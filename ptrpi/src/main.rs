#![feature(slice_patterns)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate bus;
#[macro_use]
extern crate error_chain;
extern crate owning_ref;
extern crate rocket;
extern crate rocket_contrib;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

extern crate pandt;

use std::env;
use std::path::{Path, PathBuf};
use std::fs::File;
use std::fs;
use std::io::prelude::*;
use std::sync::{mpsc, Arc, Mutex, MutexGuard, PoisonError};
use std::thread;
use std::time;


use bus::Bus;
use owning_ref::MutexGuardRefMut;

use rocket::State;
use rocket_contrib::Json;
use rocket::http::Method;

mod cors;
use cors::{PreflightCORS, CORS};

use pandt::types::{App, CreatureID, GameCommand, GameError, GameErrorEnum, Point3,
                   PotentialTargets, RPIApp, RPIGame, Runtime};


error_chain! {
  types { RPIError, RPIErrorKind, RPIResultExt; }

  links {
    GameError(GameError, GameErrorEnum);
  }
  foreign_links {
    JSONError(serde_json::error::Error);
    IOError(::std::io::Error);
    YAMLError(serde_yaml::Error);
  }

  errors {
    UnexpectedResponse {
      description("Unexpected response. This is a bug.")
      display("Unexpected response. This is a bug.")
    }
    LockError(resource: String) {
      description("Some locked resource is poisoned. The application probably needs restarted.")
      display("The lock on {} is poisoned. The application probably needs restarted.", resource)
    }
    InsecurePath(name: String) {
      description("A path cannot contain certain characters or elements for security reasons.")
      display("The path {} is insecure.", name)
    }
  }
}

type PTResult<X> = Result<CORS<Json<X>>, RPIError>;

#[derive(Clone)]
struct PT {
  actor: Arc<Mutex<Actor<PTRequest, PTResponse>>>,
  pollers: Arc<Mutex<bus::Bus<()>>>,
  saved_game_path: PathBuf,
}

impl PT {
  fn pollers(&self) -> Result<MutexGuard<bus::Bus<()>>, RPIError> {
    self.pollers.lock().map_err(|_| RPIErrorKind::LockError("pollers".to_string()).into())
  }

  fn clone_app(&self) -> Result<App, RPIError> {
    if let PTResponse::App(app) = self.actor()?.send(PTRequest::GetReadOnlyApp) {
      Ok(app)
    } else {
      bail!(RPIErrorKind::UnexpectedResponse)
    }
  }
  fn actor(&self) -> Result<Actor<PTRequest, PTResponse>, RPIError> {
    self
      .actor
      .lock()
      .map_err(|_| RPIErrorKind::LockError("actor".to_string()).into())
      .map(|ac| ac.clone())
  }
}

#[route(OPTIONS, "/")]
fn options_handler() -> PreflightCORS {
  CORS::preflight("*").methods(vec![Method::Options, Method::Post]).headers(vec!["Content-Type"])
}

#[get("/")]
fn get_app(pt: State<PT>) -> Result<CORS<String>, RPIError> {
  let app = pt.clone_app()?;
  let json = serde_json::to_string(&RPIApp(&app))?;
  Ok(CORS::any(json))
}

/// If the client is polling with a non-current app "version", then immediately return the current
/// App. Otherwise, wait 30 seconds for any new changes.
#[get("/poll/<snapshot_len>/<log_len>")]
fn poll_app(pt: State<PT>, snapshot_len: usize, log_len: usize) -> Result<CORS<String>, RPIError> {
  {
    let app = pt.clone_app()?;
    if app.snapshots.len() != snapshot_len ||
      app.snapshots.back().map(|&(_, ref ls)| ls.len()).unwrap_or(0) != log_len
    {
      let result = serde_json::to_string(&RPIApp(&app))?;
      return Ok(CORS::any(result));
    }
  }

  let mut reader = pt.pollers()?.add_rx();
  // this will either return a timeout or (); in any case we'll just return the App to the client.
  let _ = reader.recv_timeout(time::Duration::from_secs(30));
  get_app(pt)
}

#[post("/", format = "application/json", data = "<command>")]
fn post_app(command: Json<GameCommand>, pt: State<PT>) -> Result<CORS<String>, RPIError> {
  if let PTResponse::JSON(json) = pt.actor()?.send(PTRequest::Perform(command.0)) {
    pt.pollers()?.broadcast(());
    Ok(CORS::any(json))
  } else {
    bail!(RPIErrorKind::UnexpectedResponse);
  }
}

#[get("/combat_movement_options")]
fn combat_movement_options(pt: State<PT>) -> PTResult<Vec<Point3>> {
  let app = pt.clone_app()?;
  Ok(CORS::any(Json(app.get_combat_movement_options()?)))
}

#[get("/movement_options/<scene_id>/<cid>")]
fn movement_options(pt: State<PT>, scene_id: String, cid: String) -> PTResult<Vec<Point3>> {
  let app = pt.clone_app()?;
  let cid = cid.parse()?;
  let scene = scene_id.parse()?;
  Ok(CORS::any(Json(app.get_movement_options(scene, cid)?)))
}

#[get("/target_options/<scene_id>/<cid>/<abid>")]
fn target_options(
  pt: State<PT>, scene_id: String, cid: String, abid: String
) -> PTResult<PotentialTargets> {
  let app = pt.clone_app()?;
  let scene = scene_id.parse()?;
  let cid = cid.parse()?;
  let abid = abid.parse()?;
  Ok(CORS::any(Json(app.get_target_options(scene, cid, abid)?)))
}

#[route(OPTIONS, "/preview_volume_targets/<scene>/<actor_id>/<ability_id>/<x>/<y>/<z>")]
fn options_creatures_in_volume(
  scene: String, actor_id: String, ability_id: String, x: String, y: String, z: String
) -> PreflightCORS {
  options_handler()
}

#[post("/preview_volume_targets/<scene_id>/<actor_id>/<ability_id>/<x>/<y>/<z>")]
fn preview_volume_targets(
  pt: State<PT>, scene_id: String, actor_id: String, ability_id: String, x: i16, y: i16, z: i16
) -> PTResult<(Vec<CreatureID>, Vec<Point3>)> {
  let app = pt.clone_app()?;
  let sid = scene_id.parse()?;
  let actor_id = actor_id.parse()?;
  let ability_id = ability_id.parse()?;
  let point = (x, y, z);
  Ok(CORS::any(Json(app.preview_volume_targets(sid, actor_id, ability_id, point)?)))
}

#[get("/saved_games")]
fn list_saved_games(pt: State<PT>) -> PTResult<Vec<String>> {
  let mut result = vec![];
  for mpath in fs::read_dir(&pt.saved_game_path)? {
    let path = mpath?;
    if path.file_type()?.is_file() {
      match path.file_name().into_string() {
        Ok(s) => result.push(s),
        Err(x) => println!("Couldn't parse filename as unicode: {:?}", x),
      }
    }
  }
  Ok(CORS::any(Json(result)))
}

#[post("/saved_games/<name>/load")]
fn load_saved_game(pt: State<PT>, name: String) -> Result<CORS<String>, RPIError> {
  let path = child_path(&pt.saved_game_path, name)?;
  let mut buffer = String::new();
  File::open(path)?.read_to_string(&mut buffer)?;
  let app = serde_yaml::from_str(&buffer)?;
  pt.actor()?.send(PTRequest::SetApp(app));
  get_app(pt)
}

#[post("/saved_games/<name>")]
fn save_game(pt: State<PT>, name: String) -> PTResult<()> {
  let new_path = child_path(&pt.saved_game_path, name)?;
  // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
  // without the extra magic that decorates the data with dynamic data for clients.
  let yaml = serde_yaml::to_string(&pt.clone_app()?)?;
  File::create(new_path)?.write_all(yaml.as_bytes())?;
  Ok(CORS::any(Json(())))
}

fn child_path(parent: &PathBuf, name: String) -> Result<PathBuf, RPIError> {
  if name.contains("/") || name.contains(":") || name.contains("\\") {
    bail!(RPIErrorKind::InsecurePath(name));
  }
  let new_path = parent.join(name.clone());
  for p in &new_path {
    if p == "." || p == ".." {
      bail!(RPIErrorKind::InsecurePath(name));
    }
  }
  Ok(new_path)
}

fn load_app_from_path(filename: &Path) -> App {
  let mut appf = File::open(filename).unwrap();
  let mut apps = String::new();
  appf.read_to_string(&mut apps).unwrap();
  serde_yaml::from_str(&apps).unwrap()
}


#[derive(Clone, Debug, PartialEq, Eq)]
enum PTRequest {
  GetReadOnlyApp,
  Perform(GameCommand),
  SetApp(App),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum PTResponse {
  App(App),
  JSON(String),
  Success,
}

#[derive(Clone)]
struct Actor<Req, Resp> {
  request_sender: mpsc::Sender<ActorMsg<Req, Resp>>,
}

enum ActorMsg<Req, Resp> {
  Payload(Req, mpsc::Sender<Resp>),
  Stop,
}

impl<Req, Resp> Actor<Req, Resp>
where
  Req: Send + 'static,
  Resp: Send + 'static,
{
  fn spawn<Init, S, Handler>(init: Init, mut handler: Handler) -> Self
  where
    Init: FnOnce() -> S,
    Init: Send + 'static,
    Handler: FnMut(&mut S, Req) -> Resp,
    Handler: Send + 'static,
  {
    let (request_sender, request_receiver) = mpsc::channel();
    let actor = Actor { request_sender };
    thread::spawn(move || {
      let mut state = init();
      loop {
        let request = request_receiver.recv().unwrap();
        match request {
          ActorMsg::Payload(r, sender) => sender.send(handler(&mut state, r)).unwrap(),
          ActorMsg::Stop => break,
        }
      }
    });
    actor
  }

  fn send(&self, message: Req) -> Resp {
    let (response_sender, response_receiver) = mpsc::channel();
    self.request_sender.send(ActorMsg::Payload(message, response_sender)).unwrap();
    return response_receiver.recv().unwrap();
  }

  fn stop(&self) {
    self.request_sender.send(ActorMsg::Stop).unwrap();
  }
}

fn handle_request(runtime: &mut Runtime, req: PTRequest) -> PTResponse {
  match req {
    PTRequest::GetReadOnlyApp => PTResponse::App(runtime.app.clone()),
    PTRequest::SetApp(app) => {
      runtime.app = app;
      PTResponse::Success
    }
    PTRequest::Perform(command) => {
      let game_and_logs =
        runtime.app.perform_unchecked(command).map_err(|e| format!("Error: {}", e));
      if let Ok((g, _)) = game_and_logs {
        runtime.world = g.get_world();
      }
      let rpi_game_and_logs = game_and_logs.map(|(g, l)| (RPIGame(g), l));
      PTResponse::JSON(
        serde_json::to_string(&rpi_game_and_logs).expect("Must be able to serialize Games"),
      )
    }
  }
}

fn main() {
  let game_dir = env::args().nth(1).unwrap_or_else(|| {
    env::current_dir()
      .expect("couldn't get curdir")
      .into_os_string()
      .into_string()
      .expect("Couldn't parse curdir as string")
  });
  let game_dir = PathBuf::from(game_dir);
  let initial_file = env::args().nth(2).unwrap_or("samplegame.yaml".to_string());

  let app: App = load_app_from_path(game_dir.join(initial_file).as_path());
  let actor = Actor::spawn(
    move || Runtime { app, world: None },
    move |runtime, request| handle_request(runtime, request),
  );

  let pt = PT {
    actor: Arc::new(Mutex::new(actor)),
    pollers: Arc::new(Mutex::new(Bus::new(1000))),
    saved_game_path: fs::canonicalize(game_dir).expect("Couldn't canonicalize game dir"),
  };

  rocket::ignite()
    .mount(
      "/",
      routes![
        get_app,
        poll_app,
        options_handler,
        post_app,
        combat_movement_options,
        movement_options,
        target_options,
        options_creatures_in_volume,
        preview_volume_targets,
        list_saved_games,
        load_saved_game,
        save_game,
      ],
    )
    .manage(pt)
    .launch();
}

#[cfg(test)]
mod test {
  use std::path::Path;
  use Actor;

  #[test]
  fn load_samplegame_yaml() {
    ::load_app_from_path(Path::new("sample_games/samplegame.yaml"));
  }

  #[test]
  fn actors() {
    fn handler(_: &mut (), i: usize) -> usize {
      i + 1
    }
    let actor = Actor::spawn(|| (), handler);
    assert_eq!(actor.send(1), 2);
    assert_eq!(actor.send(2), 3);
    actor.stop()
  }

  #[test]
  fn actor_clone() {
    fn handler(_: &mut (), i: usize) -> usize {
      i + 1
    }
    let actor = Actor::spawn(|| (), handler);
    assert_eq!(actor.send(1), 2);
    assert_eq!(actor.clone().send(2), 3);
    actor.stop()
  }

}
