// Actix-web passes requests by value even though we don't consume them. Ignore this in clippy.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::needless_pass_by_value))]

mod actor;
mod gents;
mod storage;
mod types;
mod web;

use std::{env, path::PathBuf, sync::Arc};

use anyhow::anyhow;
use clap::{Parser, Subcommand};
use tracing::{error, info};
use tracing_subscriber;

use crate::storage::{CachedStorage, CloudStorage, FSStorage, Storage};

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
  if env::var("PANDT_LOG").is_err() {
    env::set_var("PANDT_LOG", "info,ptrpi=debug,tower_http::trace::make_span=debug");
  }
  tracing_subscriber::fmt()
    .with_env_filter(tracing_subscriber::EnvFilter::from_env("PANDT_LOG"))
    .init();

  let opts = Opts::parse();

  return match &opts.command {
    Some(Commands::GenTS) => {
      gents::main()?;
      Ok(())
    }
    Some(Commands::Serve { storage_path, google_bucket, google_client_id }) => {
      serve(storage_path.clone(), google_bucket.clone(), google_client_id.clone()).await
    }

    None => {
      error!("Please provide a subcommand.");
      Ok(())
    }
  };
}

async fn serve(
  storage_path: Option<PathBuf>, google_bucket: Option<String>, google_client_id: String,
) -> anyhow::Result<()> {
  info!("Starting up the P&T Remote Programming Interface HTTP server!");

  let storage: Arc<dyn Storage> = if let Some(storage_path) = storage_path {
    Arc::new(CachedStorage::new(FSStorage::new(storage_path)))
  // } else if let Some(bucket) = opts.google_bucket {
  //   Arc::new(CloudStorage::new(bucket).await?)
  } else {
    return Err(anyhow!("Need to pass one of storage-path or google-bucket"));
  };

  let service = Arc::new(actor::AuthenticatableService::new(storage, google_client_id));

  let router = web::router(service.clone());

  info!(event = "listen-rpi", port = 1337);
  axum::Server::bind(&"0.0.0.0:1337".parse().unwrap())
    .serve(router.into_make_service())
    .await
    .unwrap();
  Ok(())
}

#[derive(Parser)]
#[command()]
struct Opts {
  #[command(subcommand)]
  command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
  /// Generate typescript bindings
  GenTS,

  /// Run the PTRPI server
  Serve {
    #[arg(long, value_name = "FILE")]
    storage_path: Option<PathBuf>,

    #[arg(long)]
    google_bucket: Option<String>,

    #[arg(long)]
    google_client_id: String,
  },
}
