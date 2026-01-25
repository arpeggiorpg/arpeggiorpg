use arptypes::multitenant::{GameID, ImageType};
use tracing::info;
use worker::{Env, SqlStorage, State};

use crate::{
    legacykv::{fetch_legacy_dump, LegacyKVStorage},
    sqlite::initialize_sqlite_tables,
};

/// This is not a "normal" migration in that it fetches data from an entirely separate
/// DurableObject, since switching from KV to SQLite required us to completely recreate our Durable
/// Objects (very annoyingly).
pub async fn migrate_kv_to_sqlite(env: Env, state: &State, game_id: GameID) -> anyhow::Result<()> {
    let storage = &state.storage();
    let sql = storage.sql();

    initialize_sqlite_tables(&sql).await?;

    let dump = fetch_legacy_dump(env, game_id).await?;
    if let Some(dump) = dump {
        migrate_game_snapshot(&sql, &dump).await?;
        migrate_logs(&sql, &dump).await?;
        migrate_invitations(&sql, &dump).await?;
        migrate_images(&sql, &dump).await?;
    }
    info!(event = "sqlite-migration-complete");
    Ok(())
}

async fn migrate_game_snapshot(
    sql: &SqlStorage,
    legacy_kv: &LegacyKVStorage,
) -> anyhow::Result<()> {
    info!(event = "migrating-game-snapshot");

    info!(event = "found-existing-game-snapshot");

    let game_json = serde_json::to_string(&legacy_kv.snapshot_0)?;
    // Insert into SQLite using JSONB format
    sql.exec(
        "INSERT OR REPLACE INTO game_snapshots (snapshot_idx, game) VALUES (?, jsonb(?))",
        Some(vec![(0i64).into(), game_json.into()]),
    )?;

    info!(event = "migrated-game-snapshot");

    Ok(())
}

async fn migrate_logs(sql: &SqlStorage, legacy_kv: &LegacyKVStorage) -> anyhow::Result<()> {
    info!(event = "migrating-logs");

    let log_entries = legacy_kv.logs()?;
    if log_entries.is_empty() {
        info!(event = "no-logs-to-migrate");
        return Ok(());
    }

    info!(event = "found-logs-to-migrate", count = log_entries.len());

    // Insert all logs into SQLite using JSONB format
    for (log_idx, game_log) in log_entries {
        sql.exec(
            "INSERT INTO logs (snapshot_idx, log_idx, game_log) VALUES (?, ?, jsonb(?))",
            Some(vec![
                (log_idx.game_idx as i64).into(),
                (log_idx.log_idx as i64).into(),
                serde_json::to_string(&game_log)?.into(),
            ]),
        )?;
    }

    info!(event = "migrated-logs");
    Ok(())
}

async fn migrate_invitations(sql: &SqlStorage, legacy_kv: &LegacyKVStorage) -> anyhow::Result<()> {
    info!(event = "migrating-invitations");

    info!(
        event = "migrating-invitations",
        count = legacy_kv.invitations.len()
    );

    for invitation_id in &legacy_kv.invitations {
        sql.exec(
            "INSERT INTO invitations (id) VALUES (?)",
            Some(vec![invitation_id.clone().into()]),
        )?;
    }

    info!(event = "migrated-invitations");

    Ok(())
}

async fn migrate_images(sql: &SqlStorage, legacy_kv: &LegacyKVStorage) -> anyhow::Result<()> {
    info!(event = "migrating-images");
    let background_images = &legacy_kv.background_images;
    if background_images.is_empty() {
        info!(event = "no-background-images-to-migrate");
        return Ok(());
    }
    let creature_icons = &legacy_kv.creature_icons;
    if creature_icons.is_empty() {
        info!(event = "no-creature-icons-to-migrate");
        return Ok(());
    }

    info!(
        event = "found-images-to-migrate",
        count = background_images.len() + creature_icons.len()
    );

    for url in background_images {
        sql.exec(
            "INSERT INTO images (image_type, url) VALUES (?, ?)",
            Some(vec![
                ImageType::BackgroundImage.to_string().into(),
                url.clone().into(),
            ]),
        )?;
    }
    for url in creature_icons {
        sql.exec(
            "INSERT INTO images (image_type, url) VALUES (?, ?)",
            Some(vec![
                ImageType::CreatureIcon.to_string().into(),
                url.clone().into(),
            ]),
        )?;
    }

    info!(event = "migrated-images");
    Ok(())
}
