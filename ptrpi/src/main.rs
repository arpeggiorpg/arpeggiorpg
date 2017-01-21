extern crate hyper;
extern crate futures;
extern crate serde_json;
extern crate serde_yaml;
extern crate unicase;

extern crate pandt;

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::sync::{Arc, Mutex};

use futures::{finished, Stream, Future, BoxFuture};

use hyper::StatusCode;
use hyper::Method::{Get, Post, Options};
use hyper::header::{ContentType, AccessControlAllowOrigin, AccessControlAllowHeaders};
use hyper::server::{Server, Service, Request, Response};
use unicase::UniCase;

use pandt::types::GameCommand;

#[derive(Clone)]
struct PT {
    app: Arc<Mutex<pandt::app::App>>,
}

impl Service for PT {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = BoxFuture<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        println!("Handling {:?} {:?}", req.method(), req.path());
        match (req.method(), req.path()) {
            (&Get, Some("/")) => self.get_app(),
            (&Post, Some("/")) => self.post_app(req),
            (&Options, Some("/")) => {
                finished(Response::new()
                        .with_header(AccessControlAllowOrigin::Any)
                        .with_header(AccessControlAllowHeaders(vec![UniCase("Content-Type"
                                                                        .to_owned())]))
                        .with_body("Okay"))
                    .boxed()
            }
            _ => finished(Response::new().with_status(StatusCode::NotFound)).boxed(),
        }
    }
}

impl PT {
    fn get_app(&self) -> BoxFuture<Response, hyper::Error> {
        finished(Response::new()
                .with_header(ContentType::json())
                .with_header(AccessControlAllowOrigin::Any)
                // TODO: is there a Future-based mutex yet? this .unwrap()
                // sux
                .with_body(serde_json::to_string(&*self.app.lock().unwrap()).unwrap()))
            .boxed()
    }

    fn post_app(&self, req: Request) -> BoxFuture<Response, hyper::Error> {
        let body_stream = req.body();
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
                                finished(Response::new()
                                        .with_header(AccessControlAllowOrigin::Any)
                                        .with_header(ContentType::json())
                                        .with_body(serde_json::to_string(&result).unwrap()))
                                    .boxed()
                            }
                            Err(e) => {
                                finished(Response::new()
                                        .with_status(StatusCode::InternalServerError)
                                        .with_header(AccessControlAllowOrigin::Any)
                                        .with_header(ContentType::json())
                                        .with_body(serde_json::to_string(&format!("{:?}", e))
                                            .unwrap()))
                                    .boxed()
                            }
                        }
                    }
                    Err(_) => {
                        finished(Response::new()
                                .with_header(AccessControlAllowOrigin::Any)
                                .with_body("BAD JSON YALL"))
                            .boxed()
                    }
                }
            })
            .boxed()
    }
}

fn main() {
    let addr = format!("0.0.0.0:{}",
                       env::args().nth(1).unwrap_or("1337".to_string()))
        .parse()
        .unwrap();
    let mut appf = File::open("samplegame.yaml").unwrap();
    let mut apps = String::new();
    appf.read_to_string(&mut apps).unwrap();
    let app: pandt::app::App = serde_yaml::from_str(&apps).unwrap();

    let pt = PT { app: Arc::new(Mutex::new(app)) };
    let (listening, server) = Server::standalone(|tokio| {
            let pt = pt.clone();
            Server::http(&addr, tokio)
                ?
                .handle(move || Ok(pt.clone()), tokio)
        })
        .unwrap();
    println!("Listening on http://{}", listening);
    server.run();
}
