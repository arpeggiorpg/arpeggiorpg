use anyhow::anyhow;
use tracing::info;
use worker::{ListOptions, SqlStorage, Storage};

pub async fn migrate_kv_to_sqlite(storage: &Storage) -> anyhow::Result<()> {
    let sql = storage.sql();

    // Initialize all SQLite tables
    initialize_sqlite_tables(&sql).await?;

    migrate_game_snapshot(storage, &sql).await?;
    migrate_logs(storage, &sql).await?;
    migrate_invitations(storage, &sql).await?;
    migrate_images(storage, &sql).await?;

    info!(event = "sqlite-migration-complete");
    Ok(())
}

async fn initialize_sqlite_tables(sql: &SqlStorage) -> anyhow::Result<()> {
    info!(event = "initializing-sqlite-tables");

    // Create logs table
    sql.exec(
        "CREATE TABLE IF NOT EXISTS logs (
            snapshot_idx INTEGER NOT NULL,
            log_idx INTEGER NOT NULL,
            game_log BLOB NOT NULL,
            PRIMARY KEY (snapshot_idx, log_idx)
        )",
        None,
    )?;

    // Create game_snapshots table
    sql.exec(
        "CREATE TABLE IF NOT EXISTS game_snapshots (
            snapshot_idx INTEGER PRIMARY KEY,
            game BLOB NOT NULL
        )",
        None,
    )?;

    // Create invitations table
    sql.exec(
        "CREATE TABLE IF NOT EXISTS invitations (
            id TEXT PRIMARY KEY
        )",
        None,
    )?;

    // Create images table
    sql.exec(
        "CREATE TABLE IF NOT EXISTS images (
            image_type TEXT NOT NULL,
            url TEXT NOT NULL
        )",
        None,
    )?;

    Ok(())
}

async fn migrate_game_snapshot(storage: &Storage, sql: &SqlStorage) -> anyhow::Result<()> {
    info!(event = "migrating-game-snapshot");

    // Try to get the game snapshot from key-value storage
    match storage.get::<String>("snapshot-0-chunk-0").await {
        Ok(game_json) => {
            info!(event = "found-existing-game-snapshot");

            // Insert into SQLite using JSONB format
            sql.exec(
                "INSERT OR REPLACE INTO game_snapshots (snapshot_idx, game) VALUES (?, jsonb(?))",
                Some(vec![(0i64).into(), game_json.into()]),
            )?;

            info!(event = "migrated-game-snapshot");
        }
        Err(worker::Error::JsError(e)) if e == "No such value in storage." => {
            info!(event = "no-existing-game-snapshot");
            // No existing game, this is fine - new games will be created as needed
        }
        Err(e) => return Err(e)?,
    }

    Ok(())
}

async fn migrate_logs(storage: &Storage, sql: &SqlStorage) -> anyhow::Result<()> {
    info!(event = "migrating-logs");

    // List all log keys
    let list_options = ListOptions::new().prefix("log-");
    let items = storage.list_with_options(list_options).await?;

    if items.size() == 0 {
        info!(event = "no-logs-to-migrate");
        return Ok(());
    }

    info!(event = "found-logs-to-migrate", count = items.size());

    let mut log_entries = Vec::new();

    // Collect all log entries
    for key in items.keys() {
        let key = key.map_err(|e| anyhow!("Failed to get key: {:?}", e))?;
        let value = items.get(&key);
        let value: String = serde_wasm_bindgen::from_value(value)
            .map_err(|e| anyhow!("Failed to deserialize value: {:?}", e))?;
        let key: String = serde_wasm_bindgen::from_value(key)
            .map_err(|e| anyhow!("Failed to deserialize key: {:?}", e))?;

        if let Some((snapshot_idx, log_idx)) = parse_log_key(&key) {
            log_entries.push((snapshot_idx, log_idx, value));
        } else {
            info!(event = "skipping-unknown-log-key", key = %key);
        }
    }

    // Sort by snapshot_idx and log_idx to maintain order
    log_entries.sort_by_key(|(snapshot_idx, log_idx, _)| (*snapshot_idx, *log_idx));

    // Insert all logs into SQLite using JSONB format
    for (snapshot_idx, log_idx, game_log) in log_entries {
        sql.exec(
            "INSERT INTO logs (snapshot_idx, log_idx, game_log) VALUES (?, ?, jsonb(?))",
            Some(vec![
                (snapshot_idx as i64).into(),
                (log_idx as i64).into(),
                game_log.into(),
            ]),
        )?;
    }

    info!(event = "migrated-logs", count = items.size());
    Ok(())
}

async fn migrate_invitations(storage: &Storage, sql: &SqlStorage) -> anyhow::Result<()> {
    info!(event = "migrating-invitations");

    match storage.get::<Vec<String>>("invitations").await {
        Ok(invitations) => {
            info!(
                event = "found-existing-invitations",
                count = invitations.len()
            );

            for invitation_id in invitations {
                sql.exec(
                    "INSERT INTO invitations (id) VALUES (?)",
                    Some(vec![invitation_id.into()]),
                )?;
            }

            info!(event = "migrated-invitations");
        }
        Err(worker::Error::JsError(e)) if e == "No such value in storage." => {
            info!(event = "no-existing-invitations");
        }
        Err(e) => return Err(e)?,
    }

    Ok(())
}

async fn migrate_images(storage: &Storage, sql: &SqlStorage) -> anyhow::Result<()> {
    info!(event = "migrating-images");

    // List all image keys (they start with "images-")
    let list_options = ListOptions::new().prefix("images-");
    let items = storage.list_with_options(list_options).await?;

    if items.size() == 0 {
        info!(event = "no-images-to-migrate");
        return Ok(());
    }

    info!(event = "found-images-to-migrate", count = items.size());

    for key in items.keys() {
        let key = key.map_err(|e| anyhow!("Failed to get key: {:?}", e))?;
        let value = items.get(&key);
        let urls: Vec<String> = serde_wasm_bindgen::from_value(value)
            .map_err(|e| anyhow!("Failed to deserialize image URLs: {:?}", e))?;
        let key: String = serde_wasm_bindgen::from_value(key)
            .map_err(|e| anyhow!("Failed to deserialize key: {:?}", e))?;

        // Parse image key format: "images-{image_type}"
        if let Some(image_type) = key.strip_prefix("images-") {
            for url in urls {
                sql.exec(
                    "INSERT INTO images (image_type, url) VALUES (?, ?)",
                    Some(vec![image_type.to_string().into(), url.into()]),
                )?;
            }
        } else {
            info!(event = "skipping-unknown-image-key", key = %key);
        }
    }

    info!(event = "migrated-images");
    Ok(())
}

pub fn parse_log_key(key: &str) -> Option<(usize, usize)> {
    // Expected format: "log-{snapshot_idx}-idx-{log_idx}"
    let parts: Vec<&str> = key.split('-').collect();
    if parts.len() != 4 || parts[0] != "log" || parts[2] != "idx" {
        return None;
    }

    let snapshot_idx = parts[1].parse().ok()?;
    let log_idx = parts[3].parse().ok()?;

    Some((snapshot_idx, log_idx))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_log_key() {
        let result = parse_log_key("log-000000000-idx-000000001");
        assert!(result.is_some());
        let parts = result.unwrap();
        assert_eq!(parts, (0, 1));

        let result = parse_log_key("log-000000005-idx-000000123");
        assert!(result.is_some());
        let parts = result.unwrap();
        assert_eq!(parts, (5, 123));

        // Invalid formats
        assert!(parse_log_key("invalid-key").is_none());
        assert!(parse_log_key("log-abc-idx-123").is_none());
        assert!(parse_log_key("log-123-invalid-456").is_none());
    }
}
