// Actix-web passes requests by value even though we don't consume them. Ignore this in clippy.
#![cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]

extern crate actix;
extern crate actix_web;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate futures;
extern crate http;
extern crate hyper;
#[macro_use]
extern crate log;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate tokio_core;

extern crate pandt;

pub mod actor;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use actix::Actor;
use futures::sync::oneshot;

use pandt::game::load_app_from_path;
use pandt::types::{App, GameError};

mod webapp {
  use std::fs;
  use std::io::{Read, Write};
  use std::path::PathBuf;
  use std::time::Duration;

  use actix;
  use actix_web::dev::FromParam;
  use actix_web::middleware::cors;
  use actix_web::{Application, HttpRequest, HttpResponse, Json};
  use futures::{future, Future};
  use futures::sync::oneshot;
  use http::{header, Method};
  use serde_json;
  use serde_yaml;
  use tokio_core::reactor::Timeout;

  use pandt::types::{App, CreatureID, GameCommand, Point3, PotentialTargets, RPIApp, RPIGame,
                     SceneID};

  use super::{RPIError, PT};
  use actor;


  type PTResult<X> = Result<Json<X>, RPIError>;

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
      .resource("/movement_options/{scene_id}/{cid}", |r| {
        r.f(movement_options)
      })
      .resource("/combat_movement_options", |r| r.f(combat_movement_options))
      .resource("/target_options/{scene_id}/{cid}/{abid}", |r| {
        r.f(target_options)
      })
      .resource(
        "/preview_volume_targets/{scene_id}/{actor_id}/{ability_id}/{x}/{y}/{z}",
        |r| r.f(preview_volume_targets),
      )
      .resource("/saved_games", |r| r.f(list_saved_games))
      .resource("/saved_games/{name}/load", |r| {
        r.method(Method::POST).f(load_saved_game)
      })
      .resource("/saved_games/{name}", |r| {
        r.method(Method::POST).f(save_game)
      })
      .resource("/modules/{name}", |r| r.method(Method::POST).a(save_module))
  }

  fn get_app(req: HttpRequest<PT>) -> Box<Future<Item=HttpResponse, Error=RPIError>> {
    let fut = req.state().app_address.call_fut(actor::GetApp);
    let fut = fut
      .map_err(|e| RPIError::MessageError(format!("Actor request failed: {:?}", e)))
      .and_then(|s| s)
      .and_then(string_json_response) ;
    Box::new(fut)
  }

  fn post_app(req: HttpRequest<PT>) -> Box<Future<Item = HttpResponse, Error = RPIError>> {
    let f = req.json().from_err().and_then(
      move |command: GameCommand| -> Result<HttpResponse, RPIError> {
        let response = {
          let pt = req.state();
          let mut app = pt.app()?;
          let result = app.perform_command(command, pt.saved_game_path.clone());
          let result = result.map_err(|e| format!("Error: {}", e));
          let result = result.map(|(g, l)| (RPIGame(g), l));
          json_response(&result)
        };
        {
          let mut waiters = req.state().waiters()?;
          for sender in waiters.drain(0..) {
            if let Err(e) = sender.send(()) {
              error!("Random failure notifying a waiter: {:?}", e);
            }
          }
        }
        response
      },
    );
    Box::new(f)
  }

  /// If the client is polling with a non-current app "version", then immediately return the current
  /// App. Otherwise, wait 30 seconds for any new changes.
  fn poll_app(req: HttpRequest<PT>) -> Box<Future<Item = HttpResponse, Error = RPIError>> {
    let result = get_current_app(&req);
    match result {
      Ok(Some(r)) => return Box::new(future::ok(r)),
      Err(e) => return Box::new(future::err(e)),
      Ok(None) => {}
    }

    let (sender, receiver) = oneshot::channel();
    req.state().waiters.lock().unwrap().push(sender);

    let handle = actix::Arbiter::handle();
    let timeout = Timeout::new(Duration::from_secs(30), handle).unwrap();

    let fut = timeout
      .select2(receiver)
      .and_then(move |_| get_app(req).map_err(|_| panic!()))
      .map_err(|_| panic!());

    Box::new(fut)
  }

  fn get_current_app(req: &HttpRequest<PT>) -> Result<Option<HttpResponse>, RPIError> {
    let snapshot_len: usize = get_arg(req, "snapshot_len")?;
    let log_len: usize = get_arg(req, "log_len")?;
    let app = req.state().app()?;
    if app.snapshots.len() != snapshot_len
      || app
        .snapshots
        .back()
        .map(|&(_, ref ls)| ls.len())
        .unwrap_or(0) != log_len
    {
      json_response(&RPIApp(&app)).map(Some)
    } else {
      Ok(None)
    }
  }

  fn movement_options(req: HttpRequest<PT>) -> PTResult<Vec<Point3>> {
    let app = req.state().app()?;
    let cid: CreatureID = parse_arg(&req, "cid")?;
    let scene_id: SceneID = parse_arg(&req, "scene_id")?;
    Ok(Json(app.get_movement_options(scene_id, cid)?))
  }

  fn combat_movement_options(req: HttpRequest<PT>) -> PTResult<Vec<Point3>> {
    let app = req.state().app()?;
    Ok(Json(app.get_combat_movement_options()?))
  }

  fn target_options(req: HttpRequest<PT>) -> PTResult<PotentialTargets> {
    let scene_id = parse_arg(&req, "scene_id")?;
    let cid = parse_arg(&req, "cid")?;
    let abid = parse_arg(&req, "abid")?;
    let app = req.state().app()?;
    Ok(Json(app.get_target_options(scene_id, cid, abid)?))
  }

  fn preview_volume_targets(req: HttpRequest<PT>) -> PTResult<(Vec<CreatureID>, Vec<Point3>)> {
    let scene_id = parse_arg(&req, "scene_id")?;
    let actor_id = parse_arg(&req, "actor_id")?;
    let ability_id = parse_arg(&req, "ability_id")?;
    let x = get_arg(&req, "x")?;
    let y = get_arg(&req, "y")?;
    let z = get_arg(&req, "z")?;
    let point = Point3::new(x, y, z);
    let app = req.state().app()?;
    let preview = app.preview_volume_targets(scene_id, actor_id, ability_id, point)?;
    Ok(Json(preview))
  }

  fn list_saved_games(req: HttpRequest<PT>) -> PTResult<Vec<String>> {
    let mut result = vec![];
    for mpath in fs::read_dir(&req.state().saved_game_path)? {
      let path = mpath?;
      if path.file_type()?.is_file() {
        match path.file_name().into_string() {
          Ok(s) => result.push(s),
          Err(x) => println!("Couldn't parse filename as unicode: {:?}", x),
        }
      }
    }
    Ok(Json(result))
  }

  fn _load_saved_game(req: &HttpRequest<PT>) -> Result<(), RPIError> {
    let state = req.state();
    let name: String = get_arg(req, "name")?;
    let path = child_path(&state.saved_game_path, &name)?;
    let mut buffer = String::new();
    fs::File::open(path)?.read_to_string(&mut buffer)?;
    let mut app = state.app()?;
    *app = serde_yaml::from_str(&buffer)?;
    Ok(())
  }

  fn load_saved_game(req: HttpRequest<PT>) -> Box<Future<Item=HttpResponse, Error=RPIError>> {
    Box::new(future::result(_load_saved_game(&req)).and_then(|_| get_app(req)))
  }

  fn save_game(req: HttpRequest<PT>) -> PTResult<()> {
    let name: String = get_arg(&req, "name")?;
    let app = req.state().app()?;
    save_app(&req, &name, &app)
  }

  fn save_app(req: &HttpRequest<PT>, name: &str, app: &App) -> PTResult<()> {
    let new_path = child_path(&req.state().saved_game_path, name)?;
    // Note that we *don't* use RPIApp here, so we're getting plain-old-data serialization of the app,
    // without the extra magic that decorates the data with dynamic data for clients.
    let yaml = serde_yaml::to_string(&app)?;
    fs::File::create(new_path)?.write_all(yaml.as_bytes())?;
    Ok(Json(()))
  }

  fn save_module(req: HttpRequest<PT>) -> Box<Future<Item = Json<()>, Error = RPIError>> {
    let f = req.json().from_err().and_then(move |path| -> PTResult<()> {
      let name: String = get_arg(&req, "name")?;
      let app = req.state().app()?;
      let new_game = app.current_game.export_module(&path)?;
      let new_app = App::new(new_game);
      save_app(&req, &name, &new_app)
    });
    Box::new(f)
  }

  fn string_json_response(body: String) -> Result<HttpResponse, RPIError> {
    Ok(HttpResponse::Ok()
      .content_type("application/json")
      .body(body)?)
  }

  fn json_response<T: ::serde::Serialize>(b: &T) -> Result<HttpResponse, RPIError> {
    let body = serde_json::to_string(b)?;
    string_json_response(body)
  }

  fn get_arg<T>(req: &HttpRequest<PT>, key: &str) -> Result<T, RPIError>
  where
    T: FromParam,
  {
    req
      .match_info()
      .query::<T>(key)
      .map_err(RPIError::from_response_error)
  }

  fn parse_arg<T>(req: &HttpRequest<PT>, key: &str) -> Result<T, RPIError>
  where
    T: ::std::str::FromStr,
    RPIError: From<<T as ::std::str::FromStr>::Err>, // I dunno man
  {
    let s = req.match_info().query::<String>(key);
    Ok(s.map_err(RPIError::from_response_error)?.parse()?)
  }

  fn child_path(parent: &PathBuf, name: &str) -> Result<PathBuf, RPIError> {
    if name.contains('/') || name.contains(':') || name.contains('\\') {
      bail!(RPIError::InsecurePath(name.to_string()));
    }
    let new_path = parent.join(name);
    for p in &new_path {
      if p == "." || p == ".." {
        bail!(RPIError::InsecurePath(name.to_string()));
      }
    }
    Ok(new_path)
  }
}

#[derive(Debug, Fail)]
pub enum RPIError {
  #[fail(display = "Internal Error")] MessageError(String),
  #[fail(display = "Game Error")] GameError(#[cause] GameError),
  #[fail(display = "JSON Error")] JSONError(#[cause] serde_json::error::Error),
  #[fail(display = "IO Error")] IOError(#[cause] ::std::io::Error),
  #[fail(display = "YAML Error")] YAMLError(#[cause] serde_yaml::Error),
  #[fail(display = "Web Error")] WebError(Box<actix_web::ResponseError>),
  #[fail(display = "JSON Payload Error")]
  JSONPayloadError(#[cause] actix_web::error::JsonPayloadError),
  #[fail(display = "HTTP Error")] HTTPError(#[cause] http::Error),
  #[fail(display = "The lock on {} is poisoned. The application probably needs restarted.", _0)]
  LockError(String),
  #[fail(display = "The path {} is insecure.", _0)] InsecurePath(String),
}

impl From<GameError> for RPIError {
  fn from(error: GameError) -> Self { RPIError::GameError(error) }
}
impl From<serde_json::error::Error> for RPIError {
  fn from(error: serde_json::error::Error) -> Self { RPIError::JSONError(error) }
}

impl From<::std::io::Error> for RPIError {
  fn from(error: ::std::io::Error) -> Self { RPIError::IOError(error) }
}

impl From<serde_yaml::Error> for RPIError {
  fn from(error: serde_yaml::Error) -> Self { RPIError::YAMLError(error) }
}

impl From<http::Error> for RPIError {
  fn from(error: http::Error) -> Self { RPIError::HTTPError(error) }
}

impl From<actix_web::error::JsonPayloadError> for RPIError {
  fn from(error: actix_web::error::JsonPayloadError) -> Self { RPIError::JSONPayloadError(error) }
}

impl RPIError {
  fn from_response_error<T: actix_web::ResponseError>(e: T) -> Self {
    RPIError::WebError(Box::new(e))
  }
}

impl actix_web::ResponseError for RPIError {
  fn error_response(&self) -> actix_web::HttpResponse {
    match *self {
      RPIError::JSONPayloadError(ref e) => e.error_response(),
      RPIError::WebError(ref e) => e.error_response(),
      RPIError::HTTPError(ref e) => e.error_response(),
      RPIError::IOError(ref e) => e.error_response(),
      _ => actix_web::HttpResponse::new(
        http::StatusCode::INTERNAL_SERVER_ERROR,
        actix_web::Body::Empty,
      ),
    }
  }
}

// TODO: annihilate usage of Mutex, switch to actors
#[derive(Clone)]
pub struct PT {
  app: Arc<Mutex<App>>,
  waiters: Arc<Mutex<Vec<oneshot::Sender<()>>>>,
  saved_game_path: PathBuf,
  app_address: actix::SyncAddress<actor::AppActor>,
}

impl PT {
  fn app(&self) -> Result<::std::sync::MutexGuard<App>, RPIError> {    self
      .app
      .lock()
      .map_err(|_| RPIError::LockError("app".to_string()))
  }

  fn waiters(&self) -> Result<::std::sync::MutexGuard<Vec<oneshot::Sender<()>>>, RPIError> {
    self
      .waiters
      .lock()
      .map_err(|_| RPIError::LockError("pollers".to_string()))
  }
}

fn main() {
  if let Err(_) = env::var("PANDT_LOG") {
    env::set_var("PANDT_LOG", "info");
  }
  let env = env_logger::Env::new()
    .filter("PANDT_LOG")
    .write_style("PANDT_LOG_STYLE");
  env_logger::init_from_env(env);

  info!("Starting up the P&T Remote Programming Interface HTTP server!");
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
  let game_dir = PathBuf::from(game_dir.clone());
  let app: App = load_app_from_path(&game_dir, &initial_file).expect("Couldn't load app from file");

  let actable_app = app.clone();

  let actor = actor::AppActor::new(actable_app, game_dir.clone());

  let actix_system = actix::System::new("P&T-RPI");
  let app_addr: actix::SyncAddress<actor::AppActor> = actor.start();

  let pt = PT {
    app: Arc::new(Mutex::new(app)),
    waiters: Arc::new(Mutex::new(vec![])),
    saved_game_path: fs::canonicalize(game_dir).expect("Couldn't canonicalize game dir"),
    app_address: app_addr,
  };

  let server = actix_web::HttpServer::new(move || webapp::router(pt.clone()));
  server.bind("0.0.0.0:1337").expect("Couldn't bind to 1337").start();
  actix_system.run();
}

#[cfg(test)]
mod test {
  use std::path::Path;

  #[test]
  fn load_samplegame_yaml() {
    ::load_app_from_path(Path::new("sample_games"), "samplegame.yaml").unwrap();
  }

}
