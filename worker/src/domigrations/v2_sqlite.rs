use std::collections::HashMap;

use serde::{de::Error as _, Deserialize, Deserializer};

use arptypes::{
    multitenant::{GameID, ImageType},
    GameLog,
};
use tracing::info;
use worker::{Env, SqlStorage, State};

#[derive(Deserialize)]
struct LegacyKVStorage {
    #[serde(
        rename = "snapshot-0-chunk-0",
        deserialize_with = "deserialize_double_encoded"
    )]
    snapshot_0: arptypes::Game,

    #[serde(default)]
    invitations: Vec<String>,

    #[serde(rename = "images-BackgroundImage", default)]
    background_images: Vec<String>,

    #[serde(rename = "images-CreatureIcon", default)]
    creature_icons: Vec<String>,
    #[serde(flatten)]
    logs: HashMap<String, DoubleEncoded<GameLog>>,
}

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

async fn fetch_legacy_dump(env: Env, game_id: GameID) -> anyhow::Result<Option<LegacyKVStorage>> {
    let legacy_ns = env.durable_object("ARPEGGIOGAME_LEGACY")?;
    let legacy_id = legacy_ns.id_from_name(&game_id.to_string())?;
    let stub = legacy_id.get_stub()?;
    let mut resp = stub.fetch_with_str("https://dummy/dump").await?;

    if resp.status_code() == 404 {
        return Ok(None);
    }

    let dumped: LegacyKVStorage = resp.json().await?;
    Ok(Some(dumped))
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

    let log_entries: Result<Vec<((usize, usize), GameLog)>, anyhow::Error> = legacy_kv
        .logs
        .iter()
        .filter(|(k, _v)| k.starts_with("log-"))
        .map(|(k, DoubleEncoded(v))| Ok((parse_log_key(k)?, v.clone())))
        .collect();
    let mut log_entries = log_entries?;
    if log_entries.len() == 0 {
        info!(event = "no-logs-to-migrate");
        return Ok(());
    }

    info!(event = "found-logs-to-migrate", count = log_entries.len());

    // Sort by snapshot_idx and log_idx to maintain order
    log_entries.sort_by_key(|li| li.0);

    // Insert all logs into SQLite using JSONB format
    for ((snapshot_idx, log_idx), game_log) in log_entries {
        sql.exec(
            "INSERT INTO logs (snapshot_idx, log_idx, game_log) VALUES (?, ?, jsonb(?))",
            Some(vec![
                (snapshot_idx as i64).into(),
                (log_idx as i64).into(),
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
    if background_images.len() == 0 {
        info!(event = "no-background-images-to-migrate");
        return Ok(());
    }
    let creature_icons = &legacy_kv.creature_icons;
    if creature_icons.len() == 0 {
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

pub fn parse_log_key(key: &str) -> anyhow::Result<(usize, usize)> {
    // Expected format: "log-{snapshot_idx}-idx-{log_idx}"
    let parts: Vec<&str> = key.split('-').collect();
    if parts.len() != 4 || parts[0] != "log" || parts[2] != "idx" {
        return Err(anyhow::anyhow!("not a log key: {key}"));
    }

    let snapshot_idx = parts[1].parse()?;
    let log_idx = parts[3].parse()?;

    Ok((snapshot_idx, log_idx))
}

#[derive(Debug)]
struct DoubleEncoded<T>(T);

impl<'de, T> Deserialize<'de> for DoubleEncoded<T>
where
    T: for<'a> Deserialize<'a>,
{
    fn deserialize<D>(deser: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_double_encoded(deser).map(DoubleEncoded)
    }
}

fn deserialize_double_encoded<'de, D, T>(deser: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: for<'a> Deserialize<'a>,
{
    let s = String::deserialize(deser)?;
    serde_json::from_str(&s).map_err(D::Error::custom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_log_key() {
        let result = parse_log_key("log-000000000-idx-000000001");
        let parts = result.unwrap();
        assert_eq!(parts, (0, 1));

        let result = parse_log_key("log-000000005-idx-000000123");
        let parts = result.unwrap();
        assert_eq!(parts, (5, 123));

        // Invalid formats
        assert!(parse_log_key("invalid-key").is_err());
        assert!(parse_log_key("log-abc-idx-123").is_err());
        assert!(parse_log_key("log-123-invalid-456").is_err());
    }

    #[test]
    fn test_parse_legacy_storage() {
        let json = include_str!("dumped-legacy-kv.json");
        let jd = &mut serde_json::Deserializer::from_str(json);

        let legacy_kv: LegacyKVStorage = serde_path_to_error::deserialize(jd).unwrap();
        assert_eq!(legacy_kv.snapshot_0, Default::default());
        assert_eq!(legacy_kv.logs.len(), 110);
        assert_eq!(
            legacy_kv.background_images,
            vec![
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/7e9868fb-457f-4cc7-9864-8a03cf37a0ce",
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/99e68793-4bb6-4cca-9a08-8a9305a2a432",
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/175349e5-2d31-4bca-8113-82f380d5d109"
        ]);
    }
}
