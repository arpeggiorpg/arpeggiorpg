// Actix-web passes requests by value even though we don't consume them. Ignore this in clippy.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::needless_pass_by_value))]

mod actor;
mod web;

use std::env;
use std::fs;
use std::path::PathBuf;

use actix_cors::Cors;
use actix_web::{middleware::Logger, App as WebApp};
use log::info;
use structopt::StructOpt;

use tonic::transport::Server as TonicServer;



pub mod hello_world {
  use tonic::{Request, Response, Status};
  tonic::include_proto!("helloworld");

  pub use greeter_server::{Greeter, GreeterServer};

  #[derive(Debug, Default)]
  pub struct MyGreeter {}

  #[tonic::async_trait]
  impl Greeter for MyGreeter {
    async fn say_hello(
      &self,
      request: Request<HelloRequest>, // Accept request of type HelloRequest
    ) -> Result<Response<HelloReply>, Status> {
      // Return an instance of type HelloReply
      // We must use .into_inner() as the fields of gRPC requests and responses are private
      let x = request.into_inner();
      println!("Got a request: {:?}", x);

      let reply = HelloReply {
        message: format!("Hello {}!", x.name).into(),
      };

      Ok(Response::new(reply)) // Send back our formatted greeting
    }
  }
}

use pandt::game::load_app_from_path;
use pandt::types::{App, ModuleSource};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  if env::var("PANDT_LOG").is_err() {
    env::set_var("PANDT_LOG", "info");
  }
  let env = env_logger::Env::new().filter("PANDT_LOG").write_style("PANDT_LOG_STYLE");
  env_logger::init_from_env(env);

  info!("Starting up the P&T Remote Programming Interface HTTP server!");
  let opts = Opts::from_args();
  let saved_game_path =
    fs::canonicalize(opts.saved_game_path).expect("Couldn't canonicalize game dir");
  let module_path =
    opts.module_path.map(|p| fs::canonicalize(p).expect("Couldn't canonicalize module dir"));

  let app = match opts.load_game {
    Some(initial_file) => {
      load_app_from_path(&saved_game_path, None, ModuleSource::SavedGame, &initial_file)
        .expect("Couldn't load app from file")
    }
    None => App::new(Default::default()),
  };

  let actor = actor::AppActor::new(app, saved_game_path.clone(), module_path.clone());
  let server = actix_web::HttpServer::new(move || {
    WebApp::new()
      .wrap(Logger::default())
      .wrap(Cors::permissive())
      .configure(|c| web::router(actor.clone(), c))
  });
  tokio::spawn(server.bind("0.0.0.0:1337")?.run());
  println!("Started Actix Web server...");

  let greeter = hello_world::GreeterServer::new(hello_world::MyGreeter::default());
  TonicServer::builder()
    .accept_http1(true)
    .add_service(tonic_web::enable(greeter))
    .serve("0.0.0.0:50051".parse().unwrap())
    .await?;
  Ok(())
}

#[derive(StructOpt)]
#[structopt(name = "basic")]
struct Opts {
  /// The directory where saved games should be stored
  #[structopt(long = "saved-games", parse(from_os_str))]
  saved_game_path: PathBuf,

  /// The directory where read-only modules should be loaded from
  #[structopt(long = "modules", parse(from_os_str))]
  module_path: Option<PathBuf>,

  #[structopt(long = "load-game")]
  load_game: Option<String>,
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use pandt::types::ModuleSource;

  #[test]
  fn load_samplegame_yaml() {
    crate::load_app_from_path(
      Path::new("sample_games"),
      None,
      ModuleSource::SavedGame,
      "samplegame.yaml",
    )
    .unwrap();
  }
}
