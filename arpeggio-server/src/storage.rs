use std::{collections::VecDeque, path::Path};

use anyhow::{Context, ensure};
use arpeggio::game::GameExt;
use arptypes::{
    ChangedGame, Game, GameLog,
    protocol::{GameIndex, GameMetadata, ImageType},
};
use sqlx::{
    Row, Sqlite, SqlitePool, Transaction,
    sqlite::{SqliteConnectOptions, SqlitePoolOptions},
};

const LOGS_PER_SNAPSHOT: usize = 100;
const RECENT_LOG_LIMIT: i64 = 100;

pub struct NativeStorage {
    pool: SqlitePool,
    game: Game,
    metadata: GameMetadata,
    current_snapshot_idx: usize,
    next_log_idx: usize,
}

impl NativeStorage {
    pub async fn open(data_dir: &Path) -> anyhow::Result<Self> {
        tokio::fs::create_dir_all(data_dir).await?;
        let database_path = data_dir.join("arpeggio.sqlite3");
        let options = SqliteConnectOptions::new()
            .filename(&database_path)
            .create_if_missing(true);
        let pool = SqlitePoolOptions::new()
            .max_connections(1)
            .connect_with(options)
            .await
            .with_context(|| format!("opening {}", database_path.display()))?;

        sqlx::query(
            "CREATE TABLE IF NOT EXISTS current_state (
                singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
                game_json TEXT NOT NULL,
                metadata_json TEXT NOT NULL,
                snapshot_idx INTEGER NOT NULL,
                next_log_idx INTEGER NOT NULL
            );
            CREATE TABLE IF NOT EXISTS snapshots (
                snapshot_idx INTEGER PRIMARY KEY,
                game_json TEXT NOT NULL,
                cause TEXT NOT NULL
            );
            CREATE TABLE IF NOT EXISTS logs (
                snapshot_idx INTEGER NOT NULL,
                log_idx INTEGER NOT NULL,
                log_json TEXT NOT NULL,
                PRIMARY KEY (snapshot_idx, log_idx)
            );
            CREATE TABLE IF NOT EXISTS images (
                id TEXT PRIMARY KEY,
                purpose TEXT NOT NULL,
                path TEXT NOT NULL
            );",
        )
        .execute(&pool)
        .await?;

        let state = sqlx::query(
            "SELECT game_json, metadata_json, snapshot_idx, next_log_idx
             FROM current_state WHERE singleton = 1",
        )
        .fetch_optional(&pool)
        .await?;

        let (game, metadata, current_snapshot_idx, next_log_idx) = if let Some(row) = state {
            (
                serde_json::from_str(row.try_get("game_json")?)?,
                serde_json::from_str(row.try_get("metadata_json")?)?,
                usize::try_from(row.try_get::<i64, _>("snapshot_idx")?)?,
                usize::try_from(row.try_get::<i64, _>("next_log_idx")?)?,
            )
        } else {
            let game = Game::default();
            let metadata = GameMetadata {
                name: "Arpeggio".to_string(),
            };
            let game_json = serde_json::to_string(&game)?;
            let metadata_json = serde_json::to_string(&metadata)?;
            let mut transaction = pool.begin().await?;
            sqlx::query(
                "INSERT INTO current_state
                    (singleton, game_json, metadata_json, snapshot_idx, next_log_idx)
                 VALUES (1, ?, ?, 0, 0)",
            )
            .bind(&game_json)
            .bind(metadata_json)
            .execute(&mut *transaction)
            .await?;
            sqlx::query(
                "INSERT INTO snapshots (snapshot_idx, game_json, cause)
                 VALUES (0, ?, 'initial')",
            )
            .bind(game_json)
            .execute(&mut *transaction)
            .await?;
            transaction.commit().await?;
            (game, metadata, 0, 0)
        };

        Ok(Self {
            pool,
            game,
            metadata,
            current_snapshot_idx,
            next_log_idx,
        })
    }

    pub fn game(&self) -> &Game {
        &self.game
    }

    pub fn metadata(&self) -> &GameMetadata {
        &self.metadata
    }

    pub async fn recent_logs(&self) -> anyhow::Result<VecDeque<(GameIndex, GameLog)>> {
        let rows = sqlx::query(
            "SELECT snapshot_idx, log_idx, log_json
             FROM logs
             ORDER BY snapshot_idx DESC, log_idx DESC
             LIMIT ?",
        )
        .bind(RECENT_LOG_LIMIT)
        .fetch_all(&self.pool)
        .await?;

        let mut logs = VecDeque::new();
        for row in rows {
            logs.push_front((
                GameIndex {
                    game_idx: usize::try_from(row.try_get::<i64, _>("snapshot_idx")?)?,
                    log_idx: usize::try_from(row.try_get::<i64, _>("log_idx")?)?,
                },
                serde_json::from_str(row.try_get("log_json")?)?,
            ));
        }
        Ok(logs)
    }

    pub async fn update_game(
        &mut self,
        changed: ChangedGame,
    ) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
        let snapshot_idx = self.current_snapshot_idx;
        let first_log_idx = self.next_log_idx;
        let indexed_logs: Vec<_> = changed
            .logs
            .iter()
            .enumerate()
            .map(|(offset, log)| {
                (
                    GameIndex {
                        game_idx: snapshot_idx,
                        log_idx: first_log_idx + offset,
                    },
                    log.clone(),
                )
            })
            .collect();
        let next_log_idx = first_log_idx + changed.logs.len();
        let new_snapshot_idx = (next_log_idx >= LOGS_PER_SNAPSHOT).then_some(snapshot_idx + 1);
        let game_json = serde_json::to_string(&changed.game)?;

        let mut transaction = self.pool.begin().await?;
        for (index, log) in &indexed_logs {
            sqlx::query("INSERT INTO logs (snapshot_idx, log_idx, log_json) VALUES (?, ?, ?)")
                .bind(i64::try_from(index.game_idx)?)
                .bind(i64::try_from(index.log_idx)?)
                .bind(serde_json::to_string(log)?)
                .execute(&mut *transaction)
                .await?;
        }

        let (stored_snapshot_idx, stored_next_log_idx) =
            if let Some(new_snapshot_idx) = new_snapshot_idx {
                sqlx::query(
                    "INSERT INTO snapshots (snapshot_idx, game_json, cause)
                 VALUES (?, ?, 'periodic')",
                )
                .bind(i64::try_from(new_snapshot_idx)?)
                .bind(&game_json)
                .execute(&mut *transaction)
                .await?;
                (new_snapshot_idx, 0)
            } else {
                (snapshot_idx, next_log_idx)
            };
        Self::write_current_state(
            &mut transaction,
            &game_json,
            &self.metadata,
            stored_snapshot_idx,
            stored_next_log_idx,
        )
        .await?;
        transaction.commit().await?;

        self.game = changed.game;
        self.current_snapshot_idx = stored_snapshot_idx;
        self.next_log_idx = stored_next_log_idx;
        Ok(indexed_logs)
    }

    pub async fn rollback(&mut self, index: GameIndex) -> anyhow::Result<Game> {
        let row = sqlx::query("SELECT game_json FROM snapshots WHERE snapshot_idx = ?")
            .bind(i64::try_from(index.game_idx)?)
            .fetch_optional(&self.pool)
            .await?
            .with_context(|| format!("snapshot {} does not exist", index.game_idx))?;
        let mut restored: Game = serde_json::from_str(row.try_get("game_json")?)?;
        let rows = sqlx::query(
            "SELECT log_idx, log_json FROM logs
             WHERE snapshot_idx = ? AND log_idx < ?
             ORDER BY log_idx",
        )
        .bind(i64::try_from(index.game_idx)?)
        .bind(i64::try_from(index.log_idx)?)
        .fetch_all(&self.pool)
        .await?;
        ensure!(
            rows.len() == index.log_idx,
            "rollback target {}/{} does not identify a complete log prefix",
            index.game_idx,
            index.log_idx
        );
        for (expected_idx, row) in rows.into_iter().enumerate() {
            ensure!(
                usize::try_from(row.try_get::<i64, _>("log_idx")?)? == expected_idx,
                "rollback log prefix has a gap"
            );
            let log: GameLog = serde_json::from_str(row.try_get("log_json")?)?;
            restored = restored.apply_log(&log)?;
        }

        let new_snapshot_idx = self.current_snapshot_idx + 1;
        let game_json = serde_json::to_string(&restored)?;
        let cause = format!("rollback:{}/{}", index.game_idx, index.log_idx);
        let mut transaction = self.pool.begin().await?;
        sqlx::query("INSERT INTO snapshots (snapshot_idx, game_json, cause) VALUES (?, ?, ?)")
            .bind(i64::try_from(new_snapshot_idx)?)
            .bind(&game_json)
            .bind(cause)
            .execute(&mut *transaction)
            .await?;
        Self::write_current_state(
            &mut transaction,
            &game_json,
            &self.metadata,
            new_snapshot_idx,
            0,
        )
        .await?;
        transaction.commit().await?;

        self.game = restored.clone();
        self.current_snapshot_idx = new_snapshot_idx;
        self.next_log_idx = 0;
        Ok(restored)
    }

    pub async fn register_image(
        &self,
        id: &str,
        purpose: ImageType,
        path: &Path,
    ) -> anyhow::Result<()> {
        sqlx::query(
            "INSERT INTO images (id, purpose, path) VALUES (?, ?, ?)
             ON CONFLICT(id) DO UPDATE SET purpose = excluded.purpose, path = excluded.path",
        )
        .bind(id)
        .bind(purpose.to_string())
        .bind(path.to_string_lossy().as_ref())
        .execute(&self.pool)
        .await?;
        Ok(())
    }

    async fn write_current_state(
        transaction: &mut Transaction<'_, Sqlite>,
        game_json: &str,
        metadata: &GameMetadata,
        snapshot_idx: usize,
        next_log_idx: usize,
    ) -> anyhow::Result<()> {
        sqlx::query(
            "UPDATE current_state
             SET game_json = ?, metadata_json = ?, snapshot_idx = ?, next_log_idx = ?
             WHERE singleton = 1",
        )
        .bind(game_json)
        .bind(serde_json::to_string(metadata)?)
        .bind(i64::try_from(snapshot_idx)?)
        .bind(i64::try_from(next_log_idx)?)
        .execute(&mut **transaction)
        .await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use arptypes::{GMCommand, PlayerID};

    use super::*;

    #[tokio::test]
    async fn persists_changes_and_rolls_back_to_a_log_prefix() {
        let directory = tempfile::tempdir().unwrap();
        let mut storage = NativeStorage::open(directory.path()).await.unwrap();
        let alice = PlayerID("Alice".to_string());
        let bob = PlayerID("Bob".to_string());

        let first = storage
            .game()
            .perform_gm_command(GMCommand::RegisterPlayer { id: alice.clone() })
            .unwrap();
        assert_eq!(
            storage.update_game(first).await.unwrap()[0].0,
            GameIndex {
                game_idx: 0,
                log_idx: 0
            }
        );
        let second = storage
            .game()
            .perform_gm_command(GMCommand::RegisterPlayer { id: bob.clone() })
            .unwrap();
        storage.update_game(second).await.unwrap();

        drop(storage);
        let mut storage = NativeStorage::open(directory.path()).await.unwrap();
        assert!(storage.game().players.contains_key(&alice));
        assert!(storage.game().players.contains_key(&bob));

        let restored = storage
            .rollback(GameIndex {
                game_idx: 0,
                log_idx: 1,
            })
            .await
            .unwrap();
        assert!(restored.players.contains_key(&alice));
        assert!(!restored.players.contains_key(&bob));

        drop(storage);
        let storage = NativeStorage::open(directory.path()).await.unwrap();
        assert_eq!(*storage.game(), restored);
    }
}
