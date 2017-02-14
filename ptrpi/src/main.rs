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
    PostApp(hyper::Body),
    MovementOptions(String),
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
        (&Get, &["", "movement_options", cid]) => Route::MovementOptions(cid.to_string()),
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
            Route::GetApp => respond(self.get_app()),
            Route::PostApp(body) => self.post_app(body),
            Route::MovementOptions(cid) => respond(self.get_movement_options(&cid)),
            Route::TargetOptions(cid, abid) => respond(self.get_target_options(&cid, &abid)),
            Route::Options => {
                finished(Response::new()
                        .with_header(AccessControlAllowOrigin::Any)
                        .with_header(AccessControlAllowHeaders(vec![UniCase("Content-Type"
                                                                        .to_owned())]))
                        .with_body("Okay"))
                    .boxed()
            }
            Route::Unknown => finished(Response::new().with_status(StatusCode::NotFound)).boxed(),
        }
    }
}

impl PT {
    fn get_app(&self) -> Result<pandt::types::App, GameError> {
        Ok(self.app.lock().unwrap().clone())
    }

    fn get_movement_options(&self, creature_id: &str) -> Result<Vec<Point3>, GameError> {
        let cid = CreatureID::new(creature_id)?;
        self.app.lock().unwrap().get_movement_options(cid)
    }

    fn get_target_options(&self, cid: &str, abid: &str) -> Result<Vec<PotentialTarget>, GameError> {
        let cid = CreatureID::new(cid)?;
        let abid = AbilityID::new(abid)?;
        self.app.lock().unwrap().get_target_options(cid, abid)
    }

    fn post_app(&self, body_stream: hyper::Body) -> BoxFuture<Response, hyper::Error> {
        // we need to clone here so that we don't move a &ref into the closure below, which
        // causes havoc
        let ARMUT = self.app.clone();
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
                                // TODO: is there a Future-based mutex yet? this .unwrap()
                                // sux
                                let mut app = ARMUT.lock().unwrap();
                                let result = app.perform_unchecked(command).clone();
                                println!("Command result:\n {:?}", result);
                                http_json(&result)
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

fn http_error<E: std::fmt::Debug>(e: E) -> BoxFuture<Response, hyper::Error> {
    finished(Response::new()
            .with_status(StatusCode::InternalServerError)
            .with_header(AccessControlAllowOrigin::Any)
            .with_header(ContentType::json())
            .with_body(serde_json::to_string(&format!("{:?}", e)).unwrap()))
        .boxed()
}

fn http_json<J: serde::Serialize>(j: &J) -> BoxFuture<Response, hyper::Error> {
    finished(Response::new()
            .with_header(AccessControlAllowOrigin::Any)
            .with_header(ContentType::json())
            .with_body(serde_json::to_string(j).unwrap()))
        .boxed()
}

fn main() {
    let addr = format!("0.0.0.0:{}",
                       env::args().nth(1).unwrap_or("1337".to_string()))
        .parse()
        .unwrap();
    let mut appf = File::open("samplegame.yaml").unwrap();
    let mut apps = String::new();
    appf.read_to_string(&mut apps).unwrap();
    let app: pandt::types::App = serde_yaml::from_str(&apps).unwrap();

    let pt = PT { app: Arc::new(Mutex::new(app)) };

    let server = Http::new().bind(&addr, move || Ok(pt.clone())).unwrap();
    println!("Listening on http://{}", server.local_addr().unwrap());
    server.run().unwrap();
}
