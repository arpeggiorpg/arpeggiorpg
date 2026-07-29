use arpeggio::game::GameExt;
use arptypes::{Game, GameLog};
use serde::Deserialize;
use worker::{Error, SqlStorage};

use crate::entity_storage;

#[derive(Deserialize)]
struct SnapshotRow {
    snapshot_idx: i64,
    game: String,
}

#[derive(Deserialize)]
struct LogRow {
    log_idx: i64,
    game_log: String,
}

fn migration_error(message: impl Into<String>) -> Error {
    Error::RustError(message.into())
}

pub(super) fn migrate_typed_tables(sql: &SqlStorage) -> worker::Result<()> {
    entity_storage::initialize_tables(sql)
        .map_err(|error| migration_error(format!("Could not create typed game tables: {error}")))?;

    let snapshots: Vec<SnapshotRow> = sql
        .exec(
            "SELECT snapshot_idx, json(game) AS game
             FROM game_snapshots
             ORDER BY snapshot_idx DESC
             LIMIT 1",
            None,
        )?
        .to_array()?;

    let (mut game, snapshot_idx) = match snapshots.into_iter().next() {
        Some(snapshot) => {
            let game = serde_json::from_str(&snapshot.game).map_err(|error| {
                migration_error(format!(
                    "Could not decode latest game snapshot {}: {error}",
                    snapshot.snapshot_idx
                ))
            })?;
            (game, snapshot.snapshot_idx)
        }
        None => {
            let game = Game::default();
            let game_json = serde_json::to_string(&game).map_err(|error| {
                migration_error(format!("Could not encode default game snapshot: {error}"))
            })?;
            sql.exec(
                "INSERT INTO game_snapshots (snapshot_idx, game) VALUES (0, jsonb(?))",
                Some(vec![game_json.into()]),
            )?;
            (game, 0)
        }
    };

    let logs: Vec<LogRow> = sql
        .exec(
            "SELECT log_idx, json(game_log) AS game_log
             FROM logs
             WHERE snapshot_idx = ?
             ORDER BY log_idx",
            Some(vec![snapshot_idx.into()]),
        )?
        .to_array()?;
    for log in logs {
        let game_log: GameLog = serde_json::from_str(&log.game_log).map_err(|error| {
            migration_error(format!(
                "Could not decode log {snapshot_idx}:{} while backfilling typed tables: {error}",
                log.log_idx
            ))
        })?;
        game = game.apply_log(&game_log).map_err(|error| {
            migration_error(format!(
                "Could not replay log {snapshot_idx}:{} while backfilling typed tables: {error}",
                log.log_idx
            ))
        })?;
    }

    entity_storage::replace_game(sql, &game)
        .map_err(|error| migration_error(format!("Could not backfill typed game tables: {error}")))
}
