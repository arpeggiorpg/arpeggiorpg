use anyhow::Context;
use tracing::info;
use worker::Storage;

mod v2_sqlite;

const VERSION_KEY: &str = "DURABLEGAME_VERSION";

#[tracing::instrument(name = "do-migrate", skip(storage))]
pub async fn migrate(storage: Storage) -> anyhow::Result<()> {
    let current_version = storage.get::<usize>(VERSION_KEY).await;
    match current_version {
        Ok(current_version) => Ok(migrate_from(storage, current_version).await?),
        Err(worker::Error::JsError(e)) if e == "No such value in storage." => {
            Ok(migrate_from(storage, 0).await?)
        }
        Err(e) => Err(e)?,
    }
}

async fn migrate_from(storage: Storage, current_version: usize) -> anyhow::Result<()> {
    // Rust doesn't allow us to have an array of async function pointers, so... I guess we just have
    // to write some imperative code here instead of iterating through an array of migration
    // functions.
    let latest_migration = 2;
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

    if current_version < 2 {
        // Migration 2: Migrate from key-value to SQLite
        info!(event = "running-migration-2", desc = "kv-to-sqlite");
        v2_sqlite::migrate_kv_to_sqlite(&storage)
            .await
            .with_context(|| "Running migration 2 (KV to SQLite)")?;
        storage.put(VERSION_KEY, 2).await?;
        info!(event = "migration-2-complete");
    }

    info!(event = "migrations-done");
    Ok(())
}
