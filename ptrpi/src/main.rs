// Actix-web passes requests by value even though we don't consume them. Ignore this in clippy.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::needless_pass_by_value))]

mod actor;
mod web;
mod types;
mod storage;

use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::anyhow;
use actix_cors::Cors;
use actix_web::{middleware::Logger, App as WebApp};
use log::info;
use structopt::StructOpt;

use crate::storage::{PTStorage, FSStorage, CloudStorage};

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
  if env::var("PANDT_LOG").is_err() {
    env::set_var("PANDT_LOG", "info");
  }
  let env = env_logger::Env::new().filter("PANDT_LOG").write_style("PANDT_LOG_STYLE");
  env_logger::init_from_env(env);

  info!("Starting up the P&T Remote Programming Interface HTTP server!");
  let opts = Opts::from_args();
  let storage: Arc<dyn PTStorage> =
  if let Some(storage_path) = opts.storage_path {
    Arc::new(FSStorage::new(storage_path))
  // } else if let Some(bucket) = opts.google_bucket {
  //   Arc::new(CloudStorage::new(bucket).await?)
  } else {
    return Err(anyhow!("Need to pass one of storage-path or google-bucket"));
  };

  let actor = actor::AuthenticatableService::new(storage, opts.google_client_id);

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
  #[structopt(long = "storage-path", parse(from_os_str))]
  storage_path: Option<PathBuf>,

  #[structopt(long = "google-bucket")]
  google_bucket: Option<String>,

  #[structopt(long = "google-client-id")]
  google_client_id: String
}
