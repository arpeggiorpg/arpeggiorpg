//! Full Durable Object storage dumps for debugging.
use tracing::info;
use worker::{Response, State};

/// Dump current application storage for debugging.
pub async fn dump_storage(state: &State) -> anyhow::Result<Response> {
    info!(event = "dumping-full-storage");
    let dump = worker_sqlite_dump::export(state.storage()).await?;
    Ok(Response::from_json(&dump)?)
}
