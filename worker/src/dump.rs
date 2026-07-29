//! Full Durable Object storage dumps for debugging and migration.

use tracing::info;
use worker::{Response, SqlStorageValue, State};
use worker_sqlite_dump::{Dump, SqlOperation};

#[derive(serde::Deserialize)]
struct IdRow {
    id: String,
}

#[derive(serde::Deserialize)]
struct CountRow {
    count: i64,
}

async fn create_dump(state: &State) -> worker::Result<Dump> {
    worker_sqlite_dump::export(state.storage()).await
}

/// Dump all application SQL schema/data and local KV in one storage transaction.
pub async fn dump_storage(state: &State) -> anyhow::Result<Response> {
    info!(event = "dumping-full-storage");
    let dump = create_dump(state).await?;
    Ok(Response::from_json(&dump)?)
}

pub async fn test_full_storage_dump(state: &State) -> anyhow::Result<()> {
    crate::tests::test_init(state).await?;
    state
        .storage()
        .sql()
        .exec("INSERT INTO invitations (id) VALUES ('dump-test')", None)?;
    state.storage().put("dump-test-number", 42).await?;
    state
        .storage()
        .put("dump-test-object", serde_json::json!({"nested": true}))
        .await?;

    let dump = create_dump(state).await?;
    dump.validate_format()?;

    anyhow::ensure!(
        dump.sql.iter().any(|operation| matches!(
            operation,
            SqlOperation::Statement { sql }
                if sql.contains("CREATE TABLE invitations")
        )) && dump.sql.iter().any(|operation| matches!(
            operation,
            SqlOperation::Insert { table, .. } if table == "invitations"
        )),
        "full storage dump omitted SQL schema or data"
    );
    anyhow::ensure!(
        dump.sql.iter().any(|operation| matches!(
            operation,
            SqlOperation::Statement { sql } if sql.contains("CREATE TABLE items")
        )) && dump.sql.iter().any(|operation| matches!(
            operation,
            SqlOperation::Insert { table, .. } if table == "game_state"
        )),
        "full storage dump omitted typed game tables or singleton state"
    );
    anyhow::ensure!(
        dump.kv.len() == 2
            && dump
                .kv
                .iter()
                .any(|entry| entry.key == "dump-test-number" && entry.value == 42)
            && dump.kv.iter().any(|entry| {
                entry.key == "dump-test-object"
                    && entry.value == serde_json::json!({"nested": true})
            }),
        "full storage dump omitted local KV data"
    );

    let large_value = "x".repeat(256 * 1024);
    state.storage().sql().exec(
        "INSERT INTO invitations (id) VALUES (?)",
        Some(vec![SqlStorageValue::String(large_value.clone())]),
    )?;
    let large_blob = vec![0xAB; 256 * 1024];
    state.storage().sql().exec(
        "INSERT INTO game_snapshots (snapshot_idx, game) VALUES (?, ?)",
        Some(vec![
            SqlStorageValue::Integer(999),
            SqlStorageValue::Blob(large_blob.clone()),
        ]),
    )?;
    let dump = create_dump(state).await?;
    state.storage().delete_all().await?;
    worker_sqlite_dump::restore(state.storage(), dump).await?;
    let restored: IdRow = state
        .storage()
        .sql()
        .exec(
            "SELECT id FROM invitations WHERE length(id) = ?",
            Some(vec![SqlStorageValue::Integer(large_value.len() as i64)]),
        )?
        .one()?;
    anyhow::ensure!(
        restored.id == large_value,
        "large SQL value changed during dump and restore"
    );
    let restored_blob: CountRow = state
        .storage()
        .sql()
        .exec(
            "SELECT count(*) AS count FROM game_snapshots WHERE game = ?",
            Some(vec![SqlStorageValue::Blob(large_blob)]),
        )?
        .one()?;
    anyhow::ensure!(
        restored_blob.count == 1,
        "large SQL blob changed during dump and restore"
    );
    Ok(())
}
