//! Full Durable Object storage dumps for debugging and migration.

use tracing::info;
use worker::{Response, State};
use worker_sqlite_dump::Dump;

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
    dump.verify()?;

    anyhow::ensure!(
        dump.sql
            .iter()
            .any(|statement| statement.contains("CREATE TABLE invitations"))
            && dump
                .sql
                .iter()
                .any(|statement| statement.contains("INSERT INTO \"invitations\"")),
        "full storage dump omitted SQL schema or data"
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
    Ok(())
}
