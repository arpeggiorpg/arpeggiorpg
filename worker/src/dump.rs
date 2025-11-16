//! Storage dumping functionality for debugging and data export
use arptypes::multitenant::GameID;
use serde_json::json;
use tracing::info;
use worker::{wasm_bindgen, Env, Response, State};

use crate::{legacykv, rust_error};

/// Dump all storage data including both key-value and SQLite data
pub async fn dump_storage(state: &State, env: &Env, game_id: GameID) -> anyhow::Result<Response> {
    // TODO: STREAM!
    info!("Dumping storage!");
    let storage = state.storage();
    let map = storage.list().await?;

    // `list()` returns a JS Map; convert it into a serde_json::Value
    let js_val = wasm_bindgen::JsValue::from(map);
    let mut data: serde_json::Map<String, serde_json::Value> =
        serde_wasm_bindgen::from_value(js_val).map_err(rust_error)?;

    // Use explicit queries to dump SQLite data instead of unsupported .dump command
    let mut sqlite_data = serde_json::Map::new();

    sqlite_data.insert(
        "game_snapshots".to_string(),
        error_to_json(dump_game_snapshots(state).await),
    );
    sqlite_data.insert("logs".to_string(), error_to_json(dump_logs(state).await));
    sqlite_data.insert(
        "invitations".to_string(),
        error_to_json(dump_invitations(state).await),
    );
    sqlite_data.insert(
        "images".to_string(),
        error_to_json(dump_images(state).await),
    );

    data.insert("SQLite".to_string(), serde_json::Value::Object(sqlite_data));

    // Add legacy KV data if available
    if let Some(legacy_data) = legacykv::fetch_legacy_dump(env.clone(), game_id).await? {
        data.insert(
            "legacy-kv-deserialized".to_string(),
            serde_json::to_value(legacy_data)?,
        );
    }
    if let Some(raw_legacy) = legacykv::fetch_raw_legacy_dump(env.clone(), game_id).await? {
        data.insert("legacy-kv-raw".to_string(), raw_legacy);
    }

    Ok(Response::from_json(&data)?)
}

fn error_to_json<E>(r: Result<serde_json::Value, E>) -> serde_json::Value
where
    E: std::fmt::Display,
{
    match r {
        Ok(r) => r,
        Err(e) => serde_json::json!({"error": e.to_string()}),
    }
}

/// Dump all game snapshots from the database
pub async fn dump_game_snapshots(state: &State) -> anyhow::Result<serde_json::Value> {
    #[derive(serde::Deserialize)]
    struct SnapshotRow {
        snapshot_idx: i64,
        game: String,
    }

    let sql = state.storage().sql();
    let rows: Vec<SnapshotRow> = sql
        .exec(
            "SELECT snapshot_idx, json(game) as game FROM game_snapshots ORDER BY snapshot_idx",
            None,
        )?
        .to_array()?;

    let snapshots: Vec<serde_json::Value> = rows
        .into_iter()
        .map(|row| {
            json!({
                "snapshot_idx": row.snapshot_idx,
                "game": serde_json::from_str::<serde_json::Value>(&row.game).unwrap_or(serde_json::Value::String(row.game))
            })
        })
        .collect();

    Ok(serde_json::Value::Array(snapshots))
}

/// Dump all logs from the database
pub async fn dump_logs(state: &State) -> anyhow::Result<serde_json::Value> {
    #[derive(serde::Deserialize)]
    struct LogRow {
        snapshot_idx: i64,
        log_idx: i64,
        game_log: String,
    }

    let sql = state.storage().sql();
    let rows: Vec<LogRow> = sql
        .exec("SELECT snapshot_idx, log_idx, json(game_log) as game_log FROM logs ORDER BY snapshot_idx, log_idx", None)?
        .to_array()?;

    let logs: Vec<serde_json::Value> = rows
        .into_iter()
        .map(|row| {
            json!({
                "snapshot_idx": row.snapshot_idx,
                "log_idx": row.log_idx,
                "game_log": serde_json::from_str::<serde_json::Value>(&row.game_log).unwrap_or(serde_json::Value::String(row.game_log))
            })
        })
        .collect();

    Ok(serde_json::Value::Array(logs))
}

/// Dump all invitations from the database
pub async fn dump_invitations(state: &State) -> anyhow::Result<serde_json::Value> {
    #[derive(serde::Deserialize)]
    struct InvitationRow {
        id: String,
    }

    let sql = state.storage().sql();
    let rows: Vec<InvitationRow> = sql
        .exec("SELECT id FROM invitations ORDER BY id", None)?
        .to_array()?;

    let invitations: Vec<serde_json::Value> =
        rows.into_iter().map(|row| json!({"id": row.id})).collect();

    Ok(serde_json::Value::Array(invitations))
}

/// Dump all images from the database
pub async fn dump_images(state: &State) -> anyhow::Result<serde_json::Value> {
    #[derive(serde::Deserialize)]
    struct ImageRow {
        image_type: String,
        url: String,
    }

    let sql = state.storage().sql();
    let rows: Vec<ImageRow> = sql
        .exec(
            "SELECT image_type, url FROM images ORDER BY image_type, url",
            None,
        )?
        .to_array()?;

    let images: Vec<serde_json::Value> = rows
        .into_iter()
        .map(|row| {
            json!({
                "image_type": row.image_type,
                "url": row.url
            })
        })
        .collect();

    Ok(serde_json::Value::Array(images))
}
