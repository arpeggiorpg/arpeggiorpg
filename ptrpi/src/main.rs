// Disable `needless_pass_by_value` because it is triggered by all the uses of Rocket's `State`.
#![cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

extern crate bus;
extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate futures;
extern crate gotham;
#[macro_use] extern crate gotham_derive;
extern crate hyper;
extern crate mime;
extern crate serde_json;
extern crate serde_yaml;

extern crate pandt;

pub mod actor;

use std::env;
use std::path::PathBuf;
use std::fs;
use std::sync::{Arc, Mutex, MutexGuard};

use bus::Bus;

use futures::{Future, future};
use gotham::handler::HandlerFuture;
use gotham::middleware::{Middleware, NewMiddleware};
use gotham::state::{State};

use pandt::game::load_app_from_path;
use pandt::types::{App, GameError};

// use actor::Actor;

mod webapp {
  use gotham::http::response::create_response;
  use gotham::middleware::pipeline::new_pipeline;
  use gotham::router::Router;
  use gotham::router::builder::*;
  use gotham::router::route::dispatch::{new_pipeline_set, finalize_pipeline_set};
  use gotham::state::{State};

  use hyper::{Response, StatusCode};
  use mime;
  use serde_json;

  use pandt::types::{RPIApp};

  use super::PT;

  pub fn router(pt: PT) -> Router {
    let pipelines = new_pipeline_set();
    let (pipelines, global) = pipelines.add(
      new_pipeline().add(pt)
      .build());
    let default_pipeline_chain = (global, ());
    let pipelines = finalize_pipeline_set(pipelines);


    build_router(default_pipeline_chain, pipelines, |route| {
      route.get("/").to(get_app);
    })
  }

  pub fn get_app(state: State) -> (State, Response) {
    let json = {
      let app = state.borrow::<PT>().app.lock().unwrap();
      serde_json::to_string(&RPIApp(&*app)).unwrap_or("{'error': 'serialize'}".to_string())
    };

    let res = create_response(
      &state,
      StatusCode::Ok,
      Some((json.into_bytes(), mime::APPLICATION_JSON)),
    );
    (state, res)
  }
}


#[derive(Debug, Fail)]
enum RPIError {
  #[fail(display = "Game Error")] GameError(#[cause] GameError),
  #[fail(display = "JSON Error")] JSONError(#[cause] serde_json::error::Error),
  #[fail(display = "IO Error")] IOError(#[cause] ::std::io::Error),
  #[fail(display = "YAML Error")] YAMLError(#[cause] serde_yaml::Error),

  #[fail(display = "Unexpected response. This is a bug.")] UnexpectedResponse,
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
  pollers: Arc<Mutex<bus::Bus<()>>>,
  saved_game_path: PathBuf,
}

impl PT {
  fn pollers(&self) -> Result<MutexGuard<bus::Bus<()>>, RPIError> {
    self
      .pollers
      .lock()
      .map_err(|_| RPIError::LockError("pollers".to_string()).into())
  }

}

impl NewMiddleware for PT {
  type Instance = PT;

  fn new_middleware(&self) -> ::std::io::Result<Self::Instance> {
    Ok(self.clone())
  }
}

impl Middleware for PT {
  /// 1. insert CORS headers
  /// 2. insert the `PT` object into the State
  fn call<Chain>(self , mut state: State, chain: Chain) -> Box<HandlerFuture>
  where Chain: FnOnce(State) -> Box<HandlerFuture> {
    state.put(self);

    let result = chain(state);

    let f = result.and_then(move |(state, mut response)| {
        {
            let headers = response.headers_mut();
            headers.set_raw("Access-Control-Allow-Origin", "*");
        }

        future::ok((state, response))
    });
    Box::new(f)
  }
}

// /// If the client is polling with a non-current app "version", then immediately return the current
// /// App. Otherwise, wait 30 seconds for any new changes.
// #[get("/poll/<snapshot_len>/<log_len>")]
// fn poll_app(pt: State<PT>, snapshot_len: usize, log_len: usize) -> Result<String, RPIError> {
//   {
//     let app = pt.clone_app()?;
//     if app.snapshots.len() != snapshot_len
//       || app
//         .snapshots
//         .back()
//         .map(|&(_, ref ls)| ls.len())
//         .unwrap_or(0) != log_len
//     {
//       let result = serde_json::to_string(&RPIApp(&app))?;
//       return Ok(result);
//     }
//   }

//   let mut reader = pt.pollers()?.add_rx();
//   // this will either return a timeout or (); in any case we'll just return the App to the client.
//   let _ = reader.recv_timeout(time::Duration::from_secs(30));
//   get_app(pt)
// }

// #[post("/", format = "application/json", data = "<command>")]
// fn post_app(command: Json<GameCommand>, pt: State<PT>) -> Result<String, RPIError> {
//   if let PTResponse::JSON(json) = pt.actor()?
//     .send(PTRequest::Perform(command.0, pt.saved_game_path.clone()))
//   {
//     pt.pollers()?.broadcast(());
//     Ok(json)
//   } else {
//     bail!(RPIError::UnexpectedResponse);
//   }
// }

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
  // let actor = Actor::spawn(
  //   move || Runtime { app, world: None },
  //   move |runtime, request| handle_request(runtime, request),
  // );

  let pt = PT {
    app: Arc::new(Mutex::new(app)),
    pollers: Arc::new(Mutex::new(Bus::new(1000))),
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
  //       get_app,
  //       poll_app,
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
