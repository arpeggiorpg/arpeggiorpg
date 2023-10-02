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

use pandt::types::ModuleSource;

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
    opts.saved_game_path.map(|p| fs::canonicalize(p).expect("Couldn't canonicalize game dir"));
  let module_path =
    opts.module_path.map(|p| fs::canonicalize(p).expect("Couldn't canonicalize module dir"));

  let cloud_storage = if let Some(bucket) = opts.google_bucket {
    let config = google_cloud_storage::client::ClientConfig::default().with_auth().await?;
    Some((bucket, google_cloud_storage::client::Client::new(config)))
  } else {
    None
  };

  let google_oauth_details = opts.google_client.map(|s| {
    let parts = s.split_once(":").expect("google-client should be of format {client_id}:{client_secret}");
    return (parts.0.to_string(), parts.1.to_string())
  });
  let actor = actor::AppActor::new(Default::default(), saved_game_path.clone(), module_path.clone(), cloud_storage, google_oauth_details);
  if let Some(initial_file) = opts.load_game {
    actor.load_saved_game(&initial_file, ModuleSource::SavedGame).await?;
  }

  let webactor = actor.clone();
  let server = actix_web::HttpServer::new(move || {
    WebApp::new()
      .wrap(Logger::default())
      .wrap(Cors::permissive())
      .configure(|c| web::router(webactor.clone(), c))
  });
  println!("Starting Actix Web server on port 1337.");
  let _ = server.bind("0.0.0.0:1337")?.run().await;
  Ok(())
}

#[derive(StructOpt)]
#[structopt(name = "basic")]
struct Opts {
  /// The directory where saved games should be stored
  #[structopt(long = "saved-games", parse(from_os_str))]
  saved_game_path: Option<PathBuf>,

  /// The directory where read-only modules should be loaded from
  #[structopt(long = "modules", parse(from_os_str))]
  module_path: Option<PathBuf>,

  #[structopt(long = "load-game")]
  load_game: Option<String>,

  #[structopt(long = "google-bucket")]
  google_bucket: Option<String>,

  #[structopt(long = "google-client")]
  google_client: Option<String>,
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use pandt::types::ModuleSource;
  use crate::actor;

  #[tokio::test]
  async fn load_samplegame_yaml() {
    let actor = actor::AppActor::new(Default::default(), Some(Path::new("sample_games").to_path_buf()), None, None, None);
    actor.load_saved_game("samplegame.yaml", ModuleSource::SavedGame).await.unwrap();
  }
}
