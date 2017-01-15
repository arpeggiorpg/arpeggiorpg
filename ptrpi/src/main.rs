extern crate hyper;
extern crate futures;

extern crate pandt;

use std::env;

use hyper::{Get, StatusCode};
use hyper::header::ContentType;
use hyper::server::{Server, Service, Request, Response};

struct PT;

impl Service for PT {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = ::futures::Finished<Response, hyper::Error>;

    fn call(&self, req: Request) -> Self::Future {
        ::futures::finished(match (req.method(), req.path()) {
            (&Get, Some("/")) => {
                Response::new().with_header(ContentType::html()).with_body("Hello, world!")
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
    let (listening, server) = Server::standalone(|tokio| {
            Server::http(&addr, tokio)
                ?
                .handle(|| Ok(PT), tokio)
        })
        .unwrap();
    println!("Listening on http://{}", listening);
    server.run();
}
