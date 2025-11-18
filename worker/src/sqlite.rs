use tracing::info;
use worker::SqlStorage;

pub async fn initialize_sqlite_tables(sql: &SqlStorage) -> anyhow::Result<()> {
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
