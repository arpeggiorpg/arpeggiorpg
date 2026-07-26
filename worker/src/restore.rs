//! Target-pull restoration for copying game storage between Durable Object namespaces.

use std::rc::Rc;

use arptypes::multitenant::{CopyToPreprodResult, GameID, GameMetadata, UserID};
use serde::{Deserialize, Serialize};
use worker::{Env, Error, State};
use worker_sqlite_dump::{Dump, SqlOperation};

use crate::{domigrations::StorageVersion, durablestorage::GameStorage, storage};

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DumpSource {
    Production,
}

impl DumpSource {
    fn binding(self) -> &'static str {
        match self {
            Self::Production => "PRODUCTION_ARPEGGIOGAME",
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct RestoreFromSourceRequest {
    pub source: DumpSource,
    pub user_id: UserID,
    pub metadata: GameMetadata,
}

async fn fetch_from_source(env: &Env, source: DumpSource, game_id: GameID) -> worker::Result<Dump> {
    let namespace = env.durable_object(source.binding())?;
    let id = namespace.id_from_name(&game_id.to_string())?;
    let stub = id.get_stub()?;
    let mut response = stub
        .fetch_with_str(&format!("https://internal/superuser/dump/{game_id}"))
        .await?;
    if !(200..300).contains(&response.status_code()) {
        let status = response.status_code();
        let body = response.text().await?;
        return Err(Error::RustError(format!(
            "Source dump failed with HTTP {status}: {body}"
        )));
    }
    let dump: Dump = response.json().await?;
    dump.validate_format()?;
    Ok(dump)
}

async fn restore_dump(
    state: Rc<State>,
    dump: Dump,
) -> anyhow::Result<(StorageVersion, Rc<GameStorage>)> {
    worker_sqlite_dump::restore(state.storage(), dump).await?;
    let version = crate::domigrations::migrate_storage_to_current(state.storage())
        .await
        .map_err(crate::anydbg)?;
    let game_storage = Rc::new(GameStorage::load(state)?);
    Ok((version, game_storage))
}

pub async fn restore_from_source(
    env: &Env,
    state: Rc<State>,
    game_id: GameID,
    request: RestoreFromSourceRequest,
) -> anyhow::Result<(CopyToPreprodResult, Rc<GameStorage>)> {
    let source_dump = fetch_from_source(env, request.source, game_id).await?;
    let (version, game_storage) = restore_dump(state.clone(), source_dump).await?;
    let frontend_url = env.var("FRONTEND_URL")?.to_string();
    let game_url = format!("{}/gm/{game_id}", frontend_url.trim_end_matches('/'));

    storage::upsert_copied_game(env, game_id, request.user_id, request.metadata.name).await?;

    Ok((
        CopyToPreprodResult {
            storage_version: version.0,
            game_url,
        },
        game_storage,
    ))
}

pub async fn test_dump_restore_and_load(state: Rc<State>) -> anyhow::Result<()> {
    crate::tests::test_init(&state).await?;
    GameStorage::load(state.clone())?;
    state
        .storage()
        .put("dump-restore-test", serde_json::json!({"value": 42}))
        .await?;
    let source_dump = worker_sqlite_dump::export(state.storage()).await?;

    state.storage().delete_all().await?;
    let mut broken_dump = source_dump.clone();
    broken_dump.sql.push(SqlOperation::Statement {
        sql: "THIS IS NOT VALID SQL;".to_string(),
    });
    anyhow::ensure!(
        restore_dump(state.clone(), broken_dump).await.is_err(),
        "invalid dump unexpectedly restored"
    );
    anyhow::ensure!(
        worker_sqlite_dump::is_empty(&state.storage()).await?,
        "failed restore left partial target data"
    );

    let (version, _) = restore_dump(state.clone(), source_dump.clone()).await?;
    anyhow::ensure!(
        version == crate::domigrations::CURRENT_STORAGE_VERSION,
        "restored storage did not migrate to the current version"
    );
    let target_dump = worker_sqlite_dump::export(state.storage()).await?;
    anyhow::ensure!(
        target_dump == source_dump,
        "restored storage does not match its source dump"
    );
    anyhow::ensure!(
        restore_dump(state.clone(), source_dump).await.is_err(),
        "restore unexpectedly replaced nonempty target storage"
    );
    anyhow::ensure!(
        worker_sqlite_dump::export(state.storage()).await? == target_dump,
        "rejected restore changed target storage"
    );
    Ok(())
}
