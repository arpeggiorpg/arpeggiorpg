// Disable `needless_pass_by_value` because it is triggered by all the uses of Rocket's `State`.
#![cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

extern crate actix;
extern crate actix_web;
extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate futures;
extern crate http;
extern crate hyper;
#[macro_use] extern crate log;
extern crate mime;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

extern crate pandt;

pub mod actor;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use futures::sync::oneshot;

use pandt::game::load_app_from_path;
use pandt::types::{App, GameError};

mod webapp {

  use actix_web;
  use actix_web::middleware::cors;
  use actix_web::{Application, HttpRequest, HttpResponse};

  use futures;
  use futures::{Future, future, Stream};
  use futures::sync::oneshot;
  use http::{header, Method};

  use mime;
  use serde_json;

  use pandt::types::{GameCommand, RPIApp};

  use super::PT;

  fn enable_cors(r: &mut actix_web::Resource<PT>) {
    let mut corsm = cors::Cors::build();
    corsm.send_wildcard().allowed_header(header::CONTENT_TYPE);
    corsm.finish().unwrap().register(r);
  }

  pub fn router(pt: PT) -> Application<PT> {
    let mut corsm = cors::Cors::build();
    corsm.send_wildcard().allowed_header(header::CONTENT_TYPE);
    let corsm = corsm.finish().unwrap();

    Application::with_state(pt)
      .middleware(corsm)
      .resource("/", |r| {
        r.method(Method::GET).f(get_app);
        r.method(Method::POST).a(post_app);
      })
      .resource("/poll/{snapshot_len}/{log_len}", |r| r.route().a(poll_app))
  }

  fn post_app(req: HttpRequest<PT>) -> Box<Future<Item=HttpResponse, Error=actix_web::error::Error>> {
    let f = req.json().from_err().and_then(move |command: GameCommand| {
      {
        let pt = req.state();
        let mut app = pt.app.lock().unwrap();
        if let Err(e) = app.perform_command(command, pt.saved_game_path.clone()) {
          error!("Error running GameCommand! {:?}", e);
        }
      }
      {
        let mut waiters = req.state().waiters.lock().unwrap();
        for sender in waiters.drain(0..) {
          if let Err(e) = sender.send(()) {
            error!("Random failure notifying a waiter: {:?}", e);
          }
        }
      }

      // TODO:
      // 1. Return proper response
      // 2. handle Err from perform_command
      // 3. broadcast to waiters
      json_response(&())
    });
    Box::new(f)
  }

  pub fn get_app(req: HttpRequest<PT>) -> actix_web::Result<HttpResponse> {
    let app = req.state().app.lock().unwrap();
    json_response(&RPIApp(&*app))
  }

  /// If the client is polling with a non-current app "version", then immediately return the current
  /// App. Otherwise, wait 30 seconds for any new changes.
  fn poll_app(req: HttpRequest<PT>) -> Box<Future<Item=HttpResponse, Error=actix_web::error::Error>> {
    // TODO: Nothing is *stopping* the polling when a browser is reloaded or whatever.
    // We can work around this by just putting a 30 second timeout on the poll.
    let snapshot_len: usize = req.match_info().query("snapshot_len").unwrap();
    let log_len: usize = req.match_info().query("log_len").unwrap();

    let updated = {
      let app = req.state().app.lock().unwrap();
      if app.snapshots.len() != snapshot_len
        || app
          .snapshots
          .back()
          .map(|&(_, ref ls)| ls.len())
          .unwrap_or(0) != log_len
      {
        Some(json_response(&RPIApp(&app)))
      } else {
        None
      }
    };
    if let Some(r) = updated {
      return Box::new(futures::done(r));
    }
    let (sender, receiver) = oneshot::channel();
    req.state().waiters.lock().unwrap().push(sender);
    let fut = receiver.and_then(move |()| {
      get_app(req).map_err(|_| panic!())
    }).map_err(|_| panic!());
    Box::new(fut)
  }

  fn json_response<T: ::serde::Serialize>(b: &T) -> actix_web::Result<HttpResponse> {
    let body = serde_json::to_string(b)?;
    Ok(HttpResponse::Ok()
      .content_type("application/json")
      .body(body)?)
  }
}


#[derive(Debug, Fail)]
enum RPIError {
  #[fail(display = "Game Error")] GameError(#[cause] GameError),
  #[fail(display = "JSON Error")] JSONError(#[cause] serde_json::error::Error),
  #[fail(display = "IO Error")] IOError(#[cause] ::std::io::Error),
  #[fail(display = "YAML Error")] YAMLError(#[cause] serde_yaml::Error),

  #[fail(display = "The lock on {} is poisoned. The application probably needs restarted.", _0)]
  LockError(String),
  #[fail(display = "The path {} is insecure.", _0)] InsecurePath(String),
}

impl From<GameError> for RPIError {
  fn from(error: GameError) -> Self {
    RPIError::GameError(error)
  }
}
impl From<serde_json::error::Error> for RPIError {
  fn from(error: serde_json::error::Error) -> Self {
    RPIError::JSONError(error)
  }
}

impl From<::std::io::Error> for RPIError {
  fn from(error: ::std::io::Error) -> Self {
    RPIError::IOError(error)
  }
}

impl From<serde_yaml::Error> for RPIError {
  fn from(error: serde_yaml::Error) -> Self {
    RPIError::YAMLError(error)
  }
}

// type PTResult<X> = Result<Json<X>, RPIError>;

#[derive(Clone)]
pub struct PT {
  app: Arc<Mutex<App>>,
  waiters: Arc<Mutex<Vec<oneshot::Sender<()>>>>,
  saved_game_path: PathBuf,
}

// #[get("/combat_movement_options")]
// fn combat_movement_options(pt: State<PT>) -> PTResult<Vec<Point3>> {
//   let app = pt.clone_app()?;
//   Ok(Json(app.get_combat_movement_options()?))
// }

// #[get("/movement_options/<scene_id>/<cid>")]
// fn movement_options(pt: State<PT>, scene_id: String, cid: String) -> PTResult<Vec<Point3>> {
//   let app = pt.clone_app()?;
//   let cid = cid.parse()?;
//   let scene = scene_id.parse()?;
//   Ok(Json(app.get_movement_options(scene, cid)?))
// }

// #[get("/target_options/<scene_id>/<cid>/<abid>")]
// fn target_options(
//   pt: State<PT>, scene_id: String, cid: String, abid: String
// ) -> PTResult<PotentialTargets> {
//   let app = pt.clone_app()?;
//   let scene = scene_id.parse()?;
//   let cid = cid.parse()?;
//   let abid = abid.parse()?;
//   Ok(Json(app.get_target_options(scene, cid, abid)?))
// }

// #[post("/preview_volume_targets/<scene_id>/<actor_id>/<ability_id>/<x>/<y>/<z>")]
// fn preview_volume_targets(
//   pt: State<PT>, scene_id: String, actor_id: String, ability_id: String, x: i16, y: i16, z: i16
// ) -> PTResult<(Vec<CreatureID>, Vec<Point3>)> {
//   let app = pt.clone_app()?;
//   let sid = scene_id.parse()?;
//   let actor_id = actor_id.parse()?;
//   let ability_id = ability_id.parse()?;
//   let point = Point3::new(x, y, z);
//   Ok(Json(app.preview_volume_targets(
//     sid,
//     actor_id,
//     ability_id,
//     point,
//   )?))
// }

// #[get("/saved_games")]
// fn list_saved_games(pt: State<PT>) -> PTResult<Vec<String>> {
//   let mut result = vec![];
//   for mpath in fs::read_dir(&pt.saved_game_path)? {
//     let path = mpath?;
//     if path.file_type()?.is_file() {
//       match path.file_name().into_string() {
//         Ok(s) => result.push(s),
//         Err(x) => println!("Couldn't parse filename as unicode: {:?}", x),
//       }
//     }
//   }
//   Ok(Json(result))
// }

// #[post("/saved_games/<name>/load")]
// fn load_saved_game(pt: State<PT>, name: String) -> Result<String, RPIError> {
//   let path = child_path(&pt.saved_game_path, &name)?;
//   let mut buffer = String::new();
//   File::open(path)?.read_to_string(&mut buffer)?;
//   let app = serde_yaml::from_str(&buffer)?;
//   pt.actor()?.send(PTRequest::SetApp(app));
//   get_app(pt)
// }

// #[post("/saved_games/<name>")]
// fn save_game(pt: State<PT>, name: String) -> PTResult<()> {
//   let app = pt.clone_app()?; // hey! A NLL usecase!
//   save_app(pt, &name, &app)
// }

// #[post("/modules/<name>", data = "<path>")]
// fn save_module(pt: State<PT>, name: String, path: Json<FolderPath>) -> PTResult<()> {
//   let app = pt.clone_app()?;
//   let new_game = app.current_game.export_module(&path)?;
//   let new_app = App::new(new_game);
//   save_app(pt, &name, &new_app)
// }

// fn save_app(pt: State<PT>, name: &str, app: &App) -> PTResult<()> {
//   let new_path = child_path(&pt.saved_game_path, name)?;
//   // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
//   // without the extra magic that decorates the data with dynamic data for clients.
//   let yaml = serde_yaml::to_string(&app)?;
//   File::create(new_path)?.write_all(yaml.as_bytes())?;
//   Ok(Json(()))
// }

// fn child_path(parent: &PathBuf, name: &str) -> Result<PathBuf, RPIError> {
//   if name.contains('/') || name.contains(':') || name.contains('\\') {
//     bail!(RPIError::InsecurePath(name.to_string()));
//   }
//   let new_path = parent.join(name.clone());
//   for p in &new_path {
//     if p == "." || p == ".." {
//       bail!(RPIError::InsecurePath(name.to_string()));
//     }
//   }
//   Ok(new_path)
// }

fn main() {
  let game_dir = env::args().nth(1).unwrap_or_else(|| {
    env::current_dir()
      .expect("couldn't get curdir")
      .into_os_string()
      .into_string()
      .expect("Couldn't parse curdir as string")
  });
  let initial_file = env::args()
    .nth(2)
    .unwrap_or_else(|| "samplegame.yaml".to_string());

  let server = actix_web::HttpServer::new(move || {
    let game_dir = PathBuf::from(game_dir.clone());
    let app: App = load_app_from_path(&game_dir, &initial_file).expect("Couldn't load app from file");
    let pt = PT {
      app: Arc::new(Mutex::new(app)),
      waiters: Arc::new(Mutex::new(vec![])),
      saved_game_path: fs::canonicalize(game_dir).expect("Couldn't canonicalize game dir"),
    };
    webapp::router(pt)
  });
  server.bind("0.0.0.0:1337").expect("Couldn't bind to 1337").run();
}

#[cfg(test)]
mod test {
  use std::path::Path;
  use Actor;

  #[test]
  fn load_samplegame_yaml() {
    ::load_app_from_path(Path::new("sample_games"), "samplegame.yaml").unwrap();
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
