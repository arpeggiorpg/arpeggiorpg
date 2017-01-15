extern crate hyper;
extern crate futures;
extern crate serde_json;

extern crate pandt;

use std::env;
use std::cell::RefCell;

use futures::{finished, Stream, Future};

use hyper::{Get, Post, StatusCode};
use hyper::header::ContentType;
use hyper::server::{Server, Service, Request, Response};

#[derive(Clone)]
struct PT(RefCell<pandt::app::App>);

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
                        .with_body(serde_json::to_string(&*self.0.borrow()).unwrap()))
                    .boxed()
            }
            (&Post, Some("/")) => {
                let body_stream = req.body();
                Stream::collect(body_stream)
                    .and_then(|chunks| {
                        let vecu8: Vec<u8> =
                            chunks.iter().flat_map(|c| c.as_ref().to_vec()).collect();
                        let json = String::from_utf8(vecu8);
                        match json {
                            Ok(json) => finished(Response::new().with_body(json)),
                            Err(_) => finished(Response::new().with_body("BAD JSON YALL")),
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
    let pt = PT(RefCell::new(app));
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
