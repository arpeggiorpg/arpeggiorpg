use std::{env, path::PathBuf, sync::Arc};

use anyhow::anyhow;
use clap::Parser;
use tracing::info;

use mtarp::storage::{CachedStorage, FSStorage, Storage};

use rpi::{authn, web};

#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
  if env::var("ARPEGGIO_LOG").is_err() {
    env::set_var("ARPEGGIO_LOG", "info,ptrpi=debug,tower_http::trace::make_span=debug");
  }
  tracing_subscriber::fmt()
    .with_env_filter(tracing_subscriber::EnvFilter::from_env("ARPEGGIO_LOG"))
    .init();

  let Opts { storage_path, google_bucket, google_client_id } = Opts::parse();

  serve(storage_path.clone(), google_bucket.clone(), google_client_id.clone()).await
}

async fn serve(
  storage_path: Option<PathBuf>, _google_bucket: Option<String>, google_client_id: String,
) -> anyhow::Result<()> {
  info!("Starting up the Arpeggio Remote Programming Interface HTTP server!");

  let storage: Arc<dyn Storage> = if let Some(storage_path) = storage_path {
    Arc::new(CachedStorage::new(FSStorage::new(storage_path)))
  // } else if let Some(bucket) = opts.google_bucket {
  //   Arc::new(CloudStorage::new(bucket).await?)
  } else {
    return Err(anyhow!("Need to pass one of storage-path or google-bucket"));
  };

  let service = authn::AuthenticatableService::new(storage, google_client_id);

  let router = web::router(service);

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
  #[arg(long, value_name = "FILE")]
  storage_path: Option<PathBuf>,

  #[arg(long)]
  google_bucket: Option<String>,

  #[arg(long)]
  google_client_id: String,
}
