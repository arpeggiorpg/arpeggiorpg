#![feature(slice_patterns)]
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate bus;
extern crate rocket;
extern crate rocket_contrib;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
extern crate unicase;

extern crate pandt;

use bus::Bus;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::sync::{Arc, Mutex};

use rocket::State;
use rocket_contrib::JSON;
use rocket::http::Method;

mod cors;
use cors::{CORS, PreflightCORS};

use pandt::types::{AbilityID, CreatureID, GameCommand, Game, GameLog, GameError, Point3,
                   PotentialTarget};

type PTResult<X> = Result<CORS<JSON<X>>, GameError>;

#[derive(Clone)]
struct PT {
  app: Arc<Mutex<pandt::types::App>>,
  pollers: Arc<Mutex<bus::Bus<()>>>,
}

#[route(OPTIONS, "/")]
fn options_handler<'a>() -> PreflightCORS {
  CORS::preflight("*").methods(vec![Method::Options, Method::Post]).headers(vec!["Content-Type"])
}

#[get("/")]
fn get_app(pt: State<PT>) -> CORS<String> {
  let app = pt.app.lock().unwrap();
  let result = serde_json::to_string(&pandt::types::RPIApp(&*app)).unwrap();
  CORS::any(result)
}

#[get("/poll")]
fn poll_app(pt: State<PT>) -> CORS<String> {
  let mut reader = {
    let mut bus = pt.pollers.lock().unwrap();
    bus.add_rx()
  };
  reader.recv().expect("Couldn't receive from BusReader");
  get_app(pt)
}

#[post("/", format = "application/json", data = "<command>")]
fn post_app(command: JSON<GameCommand>, pt: State<PT>) -> CORS<String> {
  let mut app = pt.app.lock().unwrap();
  let result = app.perform_unchecked(command.0);
  let json = serde_json::to_string(&result).expect("Couldn't serialize result");
  pt.pollers.lock().unwrap().broadcast(());
  CORS::any(json)
}

#[get("/combat_movement_options")]
fn combat_movement_options(pt: State<PT>) -> PTResult<Vec<Point3>> {
  let app = pt.app.lock().unwrap();
  Ok(CORS::any(JSON(app.get_combat_movement_options()?)))
}

#[get("/movement_options/<cid>")]
fn movement_options(pt: State<PT>, cid: &str) -> PTResult<Vec<Point3>> {
  let app = pt.app.lock().unwrap();
  let cid = CreatureID::new(cid)?;
  Ok(CORS::any(JSON(app.get_movement_options(cid)?)))
}

#[get("/target_options/<cid>/<abid>")]
fn target_options(pt: State<PT>, cid: &str, abid: &str) -> PTResult<Vec<PotentialTarget>> {
  let app = pt.app.lock().unwrap();
  let cid = CreatureID::new(cid)?;
  let abid = AbilityID::new(abid)?;
  Ok(CORS::any(JSON(app.get_target_options(cid, abid)?)))
}

fn main() {
  let filename = env::args().nth(1).unwrap_or("samplegame.yaml".to_string());
  let app: pandt::types::App = {
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
