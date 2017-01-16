extern crate hyper;
extern crate futures;
extern crate serde_json;

extern crate pandt;

use std::env;
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, Mutex};

use futures::{finished, Stream, Future, BoxFuture};

use hyper::{Get, Post, StatusCode};
use hyper::header::{ContentType, AccessControlAllowOrigin};
use hyper::server::{Server, Service, Request, Response};

use pandt::types::GameCommand;

#[derive(Clone)]
struct PT {
    app: Arc<Mutex<pandt::app::App>>,
    index: String,
    javascript: String,
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
            // lol no router or static file handler
            (&Get, Some("/app/")) => self.serve_index(),
            (&Get, Some("/app/main.js")) => self.serve_javascript(),
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

    fn serve_index(&self) -> BoxFuture<Response, hyper::Error> {
        finished(Response::new().with_body(self.index.clone())).boxed()
    }
    fn serve_javascript(&self) -> BoxFuture<Response, hyper::Error> {
        let result = finished(Response::new().with_body(self.javascript.clone())).boxed();
        println!("Okay, I constructed the result.");
        result
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
                match json {
                    Ok(json) => {
                        let command: Result<GameCommand, _> = serde_json::from_str(&json);
                        match command {
                            Ok(command) => {
                                // TODO: is there a Future-based mutex yet? this .unwrap()
                                // sux
                                ARMUT.lock().unwrap().perform_unchecked(command);
                                finished(Response::new()
                                        .with_header(AccessControlAllowOrigin::Any)
                                        .with_header(ContentType::json())
                                        .with_body("OK GOT THE COMMAND"))
                                    .boxed()
                            }
                            Err(_) => {
                                finished(Response::new().with_body("NOT A COMMAND YALL")).boxed()
                            }
                        }
                    }
                    Err(_) => finished(Response::new().with_body("BAD JSON YALL")).boxed(),
                }
            })
            .boxed()
    }
}

fn read_file(filename: &str) -> String {
    let mut f = File::open(filename).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    s
}

fn main() {
    let addr = format!("0.0.0.0:{}",
                       env::args().nth(1).unwrap_or(String::from("1337")))
        .parse()
        .unwrap();


    let index = read_file("../ptui/dist/index.html");
    let javascript = read_file("../ptui/dist/main.js");

    let app = pandt::app::App::new(pandt::game::Game::new());
    let pt = PT {
        app: Arc::new(Mutex::new(app)),
        index: index,
        javascript: javascript,
    };
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
