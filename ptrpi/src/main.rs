extern crate hyper;
extern crate futures;
extern crate serde_json;

extern crate pandt;

use std::env;
use std::sync::{Arc, Mutex};

use futures::{finished, Stream, Future};

use hyper::{Get, Post, StatusCode};
use hyper::header::ContentType;
use hyper::server::{Server, Service, Request, Response};

use pandt::types::GameCommand;

#[derive(Clone)]
struct PT(Arc<Mutex<pandt::app::App>>);

impl Service for PT {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = futures::BoxFuture<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        match (req.method(), req.path()) {
            (&Get, Some("/")) => {
                finished(Response::new()
                        .with_header(ContentType::json())
                        // TODO: is there a Future-based mutex yet? this .unwrap()
                        // sux
                        .with_body(serde_json::to_string(&*self.0.lock().unwrap()).unwrap()))
                    .boxed()
            }
            (&Post, Some("/")) => {
                let body_stream = req.body();
                // we need to clone here so that we don't move a &ref into the closure below, which
                // causes havoc
                let ARMUT = self.0.clone();
                body_stream.collect()
                    .and_then(move |chunks| {
                        let vecu8: Vec<u8> =
                            chunks.iter().flat_map(|c| c.as_ref().to_vec()).collect();
                        let json = String::from_utf8(vecu8);
                        match json {
                            Ok(json) => {
                                let command: Result<GameCommand, _> = serde_json::from_str(&json);
                                match command {
                                    Ok(command) => {
                                        // TODO: is there a Future-based mutex yet? this .unwrap()
                                        // sux
                                        ARMUT.lock().unwrap().perform_unchecked(command);
                                        finished(Response::new()
                                                .with_header(ContentType::json())
                                                .with_body("OK GOT THE COMMAND"))
                                            .boxed()
                                    }
                                    Err(_) => {
                                        finished(Response::new().with_body("NOT A COMMAND YALL"))
                                            .boxed()
                                    }
                                }
                            }
                            Err(_) => finished(Response::new().with_body("BAD JSON YALL")).boxed(),
                        }
                    })
                    .boxed()
            }
            _ => finished(Response::new().with_status(StatusCode::NotFound)).boxed(),
        }
    }
}

fn main() {
    let addr = format!("0.0.0.0:{}",
                       env::args().nth(1).unwrap_or(String::from("1337")))
        .parse()
        .unwrap();
    let app = pandt::app::App::new(pandt::game::Game::new());
    let pt = PT(Arc::new(Mutex::new(app)));
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
