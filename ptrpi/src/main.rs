// Disable `needless_pass_by_value` because it is triggered by all the uses of Rocket's `State`.
#![cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate futures;
extern crate gotham;
#[macro_use] extern crate gotham_derive;
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

use futures::{Future, future};
use futures::sync::oneshot;
use gotham::handler::HandlerFuture;
use gotham::middleware::{Middleware, NewMiddleware};
use gotham::state::{State};

use pandt::game::load_app_from_path;
use pandt::types::{App, GameError};

mod webapp {

  use futures::{Future, future, Stream};
  use futures::sync::oneshot;

  use gotham;
  use gotham::handler::{HandlerError, HandlerFuture, IntoHandlerError};
  use gotham::http::response::create_response;
  use gotham::middleware::pipeline::new_pipeline;
  use gotham::router::Router;
  use gotham::router::builder::*;
  use gotham::router::route::dispatch::{new_pipeline_set, finalize_pipeline_set};
  use gotham::state::{FromState, State};

  use hyper;
  use hyper::{Body, Response, StatusCode};
  use mime;
  use serde_json;

  use pandt::types::{GameCommand, RPIApp};

  use super::PT;

  #[derive(StateData, PathExtractor, StaticResponseExtender)]
  struct PollExtractor {
    snapshot_idx: usize,
    log_idx: usize,
  }

  pub fn router(pt: PT) -> Router {
    let pipelines = new_pipeline_set();
    let (pipelines, global) = pipelines.add(
      new_pipeline().add(pt)
      .build());
    let default_pipeline_chain = (global, ());
    let pipelines = finalize_pipeline_set(pipelines);

    build_router(default_pipeline_chain, pipelines, |route| {
      route.options("/").to(options);
      route.get("/").to(get_app);
      route.post("/").to(post_app);
      route
        .get("/poll/:snapshot_idx/:log_idx")
        .with_path_extractor::<PollExtractor>()
        .to(poll_app);
    })
  }

  fn options(state: State) -> (State, Response) {
    let response = create_response(&state, StatusCode::Ok, Some((vec![], mime::APPLICATION_JSON)));
    (state, response)
  }

  fn decode_body<T: ::serde::de::DeserializeOwned>(chunk: hyper::Chunk) -> Result<T, HandlerError> {
    let s = ::std::str::from_utf8(&chunk).map_err(|e| e.into_handler_error())?;
    serde_json::from_str(s).map_err(|e| e.into_handler_error())
  }

  fn with_body<T: ::serde::de::DeserializeOwned + 'static>(mut state: State, f: fn(&mut State, T) -> Result<Response, HandlerError>) -> Box<HandlerFuture> {
    let f = Body::take_from(&mut state).concat2().then(move |body| {
      match body {
        Ok(body) => {
          match decode_body::<T>(body) {
            Ok(obj) => {
              match f(&mut state, obj) {
                Ok(r) => future::ok((state, r)),
                Err(e) => future::err((state, e.into_handler_error()))
              }
            }
            Err(e) => future::err((state, e.into_handler_error()))
          }
        }
        Err(e) => future::err((state, e.into_handler_error()))
      }
    });
    Box::new(f)
  }

  fn post_gamecommand(state: &mut State, command: GameCommand) -> Result<Response, HandlerError> {
    {
      let pt = state.borrow::<PT>();
      let mut app = pt.app.lock().unwrap();
      app.perform_command(command, pt.saved_game_path.clone());
    }
    {
      let mut waiters = state.borrow::<PT>().waiters.lock().unwrap();
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
    Ok(json_response(state, &()))
  }

  fn post_app(state: State) -> Box<HandlerFuture> {
    with_body::<GameCommand>(state, post_gamecommand)
  }

  pub fn get_app(state: State) -> (State, Response) {
    let response = {
      let app = state.borrow::<PT>().app.lock().unwrap();
      json_response(&state, &RPIApp(&*app))
    };
    (state, response) 
  }

  /// If the client is polling with a non-current app "version", then immediately return the current
  /// App. Otherwise, wait 30 seconds for any new changes.
  fn poll_app(mut state: State) -> Box<HandlerFuture> {
    // TODO: Nothing is *stopping* the polling when a browser is reloaded or whatever.
    // We can work around this by just putting a 30 second timeout on the poll.
    let PollExtractor {snapshot_idx: snapshot_len, log_idx: log_len} = PollExtractor::take_from(&mut state);
    let updated: Option<Response> = {
      let app = state.borrow::<PT>().app.lock().unwrap();
      if app.snapshots.len() != snapshot_len
        || app
          .snapshots
          .back()
          .map(|&(_, ref ls)| ls.len())
          .unwrap_or(0) != log_len
      {
        Some(json_response(&state, &RPIApp(&app)))
      } else {
        None
      }
    };
    if let Some(r) = updated {
      return Box::new(future::ok((state, r)));
    }
    let (sender, receiver) = oneshot::channel();
    state.borrow::<PT>().waiters.lock().unwrap().push(sender);
    let fut = receiver.and_then(move |()| {
      Ok(get_app(state))
    }).map_err(|_| panic!()); // TODO: real error handling!
    Box::new(fut)
  }

  fn json_response<T: ::serde::Serialize>(state: &State, b: &T) -> Response {
    let s = serde_json::to_string(b).unwrap();
    create_response(state, StatusCode::Ok, Some((s.into_bytes(), mime::APPLICATION_JSON)))
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

#[derive(Clone, StateData)]
pub struct PT {
  app: Arc<Mutex<App>>,
  waiters: Arc<Mutex<Vec<oneshot::Sender<()>>>>,
  saved_game_path: PathBuf,
}

impl NewMiddleware for PT {
  type Instance = PT;

  fn new_middleware(&self) -> ::std::io::Result<Self::Instance> {
    Ok(self.clone())
  }
}

impl Middleware for PT {
  /// - insert CORS headers
  /// - insert the `PT` object into the State
  fn call<Chain>(self , mut state: State, chain: Chain) -> Box<HandlerFuture>
  where Chain: FnOnce(State) -> Box<HandlerFuture> {
    state.put(self);

    let result = chain(state);

    let f = result.and_then(move |(state, mut response)| {
      {
        let headers = response.headers_mut();
        headers.set_raw("Access-Control-Allow-Origin", "*");
        headers.set_raw("Access-Control-Allow-Headers", "Content-Type");
      }

      future::ok((state, response))
    });
    Box::new(f)
  }
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
  let game_dir = PathBuf::from(game_dir);
  let initial_file = env::args()
    .nth(2)
    .unwrap_or_else(|| "samplegame.yaml".to_string());

  let app: App = load_app_from_path(&game_dir, &initial_file).expect("Couldn't load app from file");

  let pt = PT {
    app: Arc::new(Mutex::new(app)),
    waiters: Arc::new(Mutex::new(vec![])),
    saved_game_path: fs::canonicalize(game_dir).expect("Couldn't canonicalize game dir"),
  };

  gotham::start("0.0.0.0:1337", webapp::router(pt));

  // let cors_opts = rocket_cors::Cors {
  //   allowed_origins: AllowedOrigins::all(),
  //   allowed_methods: vec![Method::Get, Method::Post]
  //     .into_iter()
  //     .map(From::from)
  //     .collect(),
  //   allowed_headers: AllowedHeaders::all(),
  //   allow_credentials: true,
  //   ..Default::default()
  // };

  // rocket::ignite()
  //   .mount(
  //     "/",
  //     routes![
  //       post_app,
  //       combat_movement_options,
  //       movement_options,
  //       target_options,
  //       preview_volume_targets,
  //       list_saved_games,
  //       load_saved_game,
  //       save_game,
  //       save_module,
  //     ],
  //   )
  //   .manage(pt)
  //   .attach(cors_opts)
  //   .launch();
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
