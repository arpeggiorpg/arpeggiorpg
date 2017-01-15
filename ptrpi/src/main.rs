extern crate hyper;
extern crate futures;
extern crate serde_json;

extern crate pandt;

use std::env;
use std::cell::RefCell;
use std::cell::Ref;

use hyper::{Get, StatusCode};
use hyper::header::ContentType;
use hyper::server::{Server, Service, Request, Response};

#[derive(Clone)]
struct PT(RefCell<pandt::app::App>);

impl Service for PT {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = ::futures::Finished<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        ::futures::finished(match (req.method(), req.path()) {
            (&Get, Some("/")) => {
                Response::new()
                    .with_header(ContentType::json())
                    .with_body(serde_json::to_string(&*self.0.borrow()).unwrap())
            }
            _ => Response::new().with_status(StatusCode::NotFound),
        })
    }
}

fn main() {
    let addr = format!("0.0.0.0:{}",
                       env::args().nth(1).unwrap_or(String::from("1337")))
        .parse()
        .unwrap();
    let mut app = pandt::app::App::new(pandt::game::Game::new());
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
