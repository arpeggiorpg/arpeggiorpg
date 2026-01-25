use arptypes::multitenant::GameID;
use tracing::info;
use worker::{Env, State, Storage};

mod sqlite_migration;

const VERSION_KEY: &str = "DURABLEGAME_VERSION";

async fn try_get_version(storage: &Storage) -> anyhow::Result<Option<usize>> {
    let current_version = storage.get::<usize>(VERSION_KEY).await;
    match current_version {
        Ok(current_version) => Ok(current_version),
        Err(worker::Error::JsError(e)) if e == "No such value in storage." => Ok(None),
        Err(e) => Err(e)?,
    }
}

#[tracing::instrument(skip(state))]
pub async fn migrate(env: Env, state: &State, game_id: GameID) -> anyhow::Result<()> {
    let storage = &state.storage();
    let current_version = match try_get_version(storage).await? {
        Some(cv) => cv,
        None => {
            sqlite_migration::migrate_kv_to_sqlite(env, state, game_id).await?;
            try_get_version(&storage).await?.unwrap_or(0)
        }
    };
    migrate_from(state.storage(), current_version).await
}

async fn migrate_from(storage: Storage, current_version: usize) -> anyhow::Result<()> {
    // Rust doesn't allow us to have an array of async function pointers, so... I guess we just have
    // to write some imperative code here instead of iterating through an array of migration
    // functions.
    let latest_migration = 1;
    info!(
        event = "running-migrations",
        current_version, latest_migration
    );

    if current_version < 1 {
        // Migration 1: Initial setup (no-op, just set version)
        info!(event = "running-migration-1");
        storage.put(VERSION_KEY, 1).await?;
        info!(event = "migration-1-complete");
    }

    info!(event = "migrations-done");
    Ok(())
}
