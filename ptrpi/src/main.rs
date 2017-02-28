#![feature(slice_patterns)]
extern crate hyper;
extern crate futures;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate unicase;

extern crate pandt;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::sync::{Arc, Mutex};

use futures::{finished, Stream, Future, BoxFuture};

use hyper::StatusCode;
use hyper::Method::{Get, Post, Options};
use hyper::header::{ContentType, AccessControlAllowOrigin, AccessControlAllowHeaders};
use hyper::server::{Http, Service, Request, Response};
use unicase::UniCase;

use pandt::types::{AbilityID, CreatureID, GameCommand, GameError, Point3, PotentialTarget};

#[derive(Clone)]
struct PT {
  app: Arc<Mutex<pandt::types::App>>,
}

enum Route {
  GetApp,
  PollApp,
  PostApp(hyper::Body),
  MovementOptions(String),
  // Get movement options of the current combat creature, honoring the movement budget
  CombatMovementOptions,
  TargetOptions(String, String),
  Options,
  Unknown,
}

fn route(req: Request) -> Route {
  println!("Handling {:?} {:?}", req.method(), req.path());
  let segments_owned: Vec<String> = req.path().split("/").map(|s| s.to_string()).collect();
  let segments_reffed: Vec<&str> = segments_owned.iter().map(|s| s.as_ref()).collect();
  println!("SEGMENTS: {:?}", segments_reffed);
  match (req.method(), &segments_reffed[1..]) {
    // lol routes
    (&Post, &[""]) => Route::PostApp(req.body()),
    (&Options, &[""]) => Route::Options,
    (&Get, &[""]) => Route::GetApp,
    (&Get, &["poll"]) => Route::PollApp,
    (&Get, &["", "movement_options", cid]) => Route::MovementOptions(cid.to_string()),
    (&Get, &["", "combat_movement_options"]) => Route::CombatMovementOptions,
    (&Get, &["", "target_options", cid, abid]) => {
      Route::TargetOptions(cid.to_string(), abid.to_string())
    }
    _ => Route::Unknown,
  }
}

impl Service for PT {
  type Request = Request;
  type Response = Response;
  type Error = hyper::Error;
  type Future = BoxFuture<Response, hyper::Error>;

  fn call(&self, req: Request) -> Self::Future {
    let route = route(req);
    match route {
      Route::GetApp => respond_string(self.get_app()),
      Route::PollApp => respond_string(self.poll_app()),
      Route::PostApp(body) => self.post_app(body),
      Route::MovementOptions(cid) => respond(self.get_movement_options(&cid)),
      Route::CombatMovementOptions => respond(self.get_combat_movement_options()),
      Route::TargetOptions(cid, abid) => respond(self.get_target_options(&cid, &abid)),
      Route::Options => {
        finished(Response::new()
            .with_header(AccessControlAllowOrigin::Any)
            .with_header(AccessControlAllowHeaders(vec![UniCase("Content-Type".to_owned())]))
            .with_body("Okay"))
          .boxed()
      }
      Route::Unknown => finished(Response::new().with_status(StatusCode::NotFound)).boxed(),
    }
  }
}

impl PT {
  fn get_app(&self) -> Result<String, GameError> {
    let app = self.app.lock().unwrap();
    let json = serde_json::to_string(&pandt::types::RPIApp(&*app)).unwrap();
    Ok(json)
  }

  fn poll_app(&self) -> Result<String, GameError> {
    // TODO: return a Future that only results in an RPIApp when there is a change
    // TODO: take a `version` parameter which causes this function to immediately return the latest
    // App if the version is older than current.
    self.get_app()
  }

  fn get_movement_options(&self, creature_id: &str) -> Result<Vec<Point3>, GameError> {
    let cid = CreatureID::new(creature_id)?;
    self.app.lock().unwrap().get_movement_options(cid)
  }

  fn get_combat_movement_options(&self) -> Result<Vec<Point3>, GameError> {
    self.app.lock().unwrap().get_combat_movement_options()
  }

  fn get_target_options(&self, cid: &str, abid: &str) -> Result<Vec<PotentialTarget>, GameError> {
    let cid = CreatureID::new(cid)?;
    let abid = AbilityID::new(abid)?;
    self.app.lock().unwrap().get_target_options(cid, abid)
  }

  fn post_app(&self, body_stream: hyper::Body) -> BoxFuture<Response, hyper::Error> {
    // we need to clone here so that we don't move a &ref into the closure below, which
    // causes havoc
    let mutable_app = self.app.clone();
    body_stream.collect()
      .and_then(move |chunks| {
        let vecu8: Vec<u8> = chunks.iter().flat_map(|c| c.as_ref().to_vec()).collect();
        let json = String::from_utf8(vecu8);
        println!("Got a POST body: {:?}", json);
        match json {
          Ok(json) => {
            let command: Result<GameCommand, _> = serde_json::from_str(&json);
            match command {
              Ok(command) => {
                let (json, successful) = {
                  let mut app = mutable_app.lock().unwrap();
                  let result = app.perform_unchecked(command);
                  println!("Command result:\n {:?}", result);
                  (http_json(&result), result.is_ok())
                };
                if successful {
                  let mut f = File::create("saved-game.yaml").unwrap();
                  let app_yaml = serde_yaml::to_string(&*mutable_app.lock().unwrap()).unwrap();
                  f.write_all(app_yaml.as_bytes()).unwrap();
                  // TODO: also trigger an event that sends a notification to long-poll clients
                }
                json
              }
              Err(e) => http_error(e),
            }
          }
          Err(_) => http_error("BAD JSON Y'ALL"),
        }
      })
      .boxed()
  }
}

fn respond<S: serde::Serialize, E: std::fmt::Debug>(result: Result<S, E>)
                                                    -> BoxFuture<Response, hyper::Error> {
  match result {
    Ok(r) => http_json(&r),
    Err(e) => http_error(e),
  }
}

fn respond_string<E: std::fmt::Debug>(result: Result<String, E>)
                                      -> BoxFuture<Response, hyper::Error> {
  match result {
    Ok(r) => http_string(r),
    Err(e) => http_error(e),
  }
}

fn http_error<E: std::fmt::Debug>(e: E) -> BoxFuture<Response, hyper::Error> {
  finished(Response::new()
      .with_status(StatusCode::InternalServerError)
      .with_header(AccessControlAllowOrigin::Any)
      .with_header(ContentType::json())
      .with_body(serde_json::to_string(&format!("{:?}", e)).unwrap()))
    .boxed()
}

fn http_json<J: serde::Serialize>(j: &J) -> BoxFuture<Response, hyper::Error> {
  http_string(serde_json::to_string(j).unwrap())
}

fn http_string(s: String) -> BoxFuture<Response, hyper::Error> {
  finished(Response::new()
      .with_header(AccessControlAllowOrigin::Any)
      .with_header(ContentType::json())
      .with_body(s))
    .boxed()
}

fn main() {
  let addr = format!("0.0.0.0:{}",
                     env::args().nth(1).unwrap_or("1337".to_string()))
    .parse()
    .unwrap();
  let app: pandt::types::App = {
    let mut appf = File::open(env::args().nth(2).unwrap_or("samplegame.yaml".to_string())).unwrap();
    let mut apps = String::new();
    appf.read_to_string(&mut apps).unwrap();
    serde_yaml::from_str(&apps).unwrap()
  };

  let pt = PT { app: Arc::new(Mutex::new(app)) };

  let server = Http::new().bind(&addr, move || Ok(pt.clone())).unwrap();
  println!("Listening on http://{}", server.local_addr().unwrap());
  server.run().unwrap();
}
