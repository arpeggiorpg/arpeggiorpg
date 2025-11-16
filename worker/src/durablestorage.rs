use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
    rc::Rc,
};

use anyhow::anyhow;
use tracing::info;
use worker::{SqlStorage, State};

use arpeggio::{
    game::GameExt,
    types::{ChangedGame, Game, GameLog},
};
use arptypes::multitenant::{GameIndex, ImageType, InvitationID};

type RecentGameLogs = VecDeque<(GameIndex, GameLog)>;

/// The state of the game using SQLite storage.
///
/// The cool thing that Durable Objects give us is that we can keep the Game in memory, just loading
/// it when the DO wakes up. Of course, during normal play the DO will go to sleep and wake up many
/// times, but while it *is* awake we only need to save logs and update the in-memory game.
///
/// This is really the whole reason I wanted to use CF Durable Objects for Arpeggio. I don't need to
/// worry about distributed caching or distributed event queues for notifications when a game object
/// changes; everyone's connected to the same live game object.
pub struct GameStorage {
    sql: SqlStorage,
    current_snapshot_idx: Cell<usize>,
    next_log_idx: Cell<usize>,
    cached_game: Rc<RefCell<Game>>,
    recent_logs: Rc<RefCell<RecentGameLogs>>,
}

const RECENT_LOGS_SIZE: usize = 100;

/// Storage using SQLite in Durable Objects
impl GameStorage {
    pub fn game(&self) -> Game {
        self.cached_game.borrow().clone()
    }

    pub fn recent_logs(&self) -> RecentGameLogs {
        self.recent_logs.borrow().clone()
    }

    pub async fn load(state: Rc<State>) -> anyhow::Result<Self> {
        let sql = state.storage().sql();

        // Load game from snapshots table (always using snapshot_idx = 0 for now)
        let game = match Self::load_game_snapshot(&sql, 0).await? {
            Some(game) => game,
            None => {
                info!(event = "new-game");
                let default_game = Game::default();
                Self::store_game_snapshot(&sql, 0, &default_game).await?;
                default_game
            }
        };

        // Load recent logs and apply them to the game
        let (final_game, recent_logs, next_log_idx) =
            Self::load_and_apply_logs(&sql, game, 0).await?;

        let game_storage = Self {
            sql,
            current_snapshot_idx: Cell::new(0),
            next_log_idx: Cell::new(next_log_idx),
            cached_game: Rc::new(RefCell::new(final_game)),
            recent_logs: Rc::new(RefCell::new(recent_logs)),
        };

        Ok(game_storage)
    }

    async fn load_game_snapshot(
        sql: &SqlStorage,
        snapshot_idx: usize,
    ) -> anyhow::Result<Option<Game>> {
        #[derive(serde::Deserialize)]
        struct GameRow {
            game: String,
        }

        let rows: Vec<GameRow> = sql
            .exec(
                "SELECT json(game) as game FROM game_snapshots WHERE snapshot_idx = ?",
                Some(vec![(snapshot_idx as i64).into()]),
            )?
            .to_array()?;

        match rows.first() {
            Some(row) => {
                let game = serde_json::from_str(&row.game)?;
                Ok(Some(game))
            }
            None => Ok(None),
        }
    }

    async fn store_game_snapshot(
        sql: &SqlStorage,
        snapshot_idx: usize,
        game: &Game,
    ) -> anyhow::Result<()> {
        let game_json = serde_json::to_string(game)?;
        sql.exec(
            "INSERT OR REPLACE INTO game_snapshots (snapshot_idx, game) VALUES (?, jsonb(?))",
            Some(vec![(snapshot_idx as i64).into(), game_json.into()]),
        )?;
        Ok(())
    }

    async fn load_and_apply_logs(
        sql: &SqlStorage,
        mut game: Game,
        snapshot_idx: usize,
    ) -> anyhow::Result<(Game, RecentGameLogs, usize)> {
        #[derive(serde::Deserialize)]
        struct LogRow {
            log_idx: i64,
            game_log: String,
        }

        let rows: Vec<LogRow> = sql
            .exec(
                "SELECT log_idx, json(game_log) as game_log FROM logs WHERE snapshot_idx = ? ORDER BY log_idx",
                Some(vec![(snapshot_idx as i64).into()]),
            )?
            .to_array()?;

        let mut recent_logs = VecDeque::new();
        let mut next_log_idx = 0;

        if rows.is_empty() {
            return Ok((game, recent_logs, next_log_idx));
        }

        info!(event = "loading-logs", num = rows.len());

        for row in rows {
            let log: GameLog = serde_json::from_str(&row.game_log).map_err(|e| {
                anyhow!(
                    "Failed parsing GameLog as JSON:\ncontent: {:?}\nerror: {:?}",
                    row.game_log,
                    e
                )
            })?;

            game = game.apply_log(&log)?;

            let log_idx = row.log_idx as usize;
            next_log_idx = log_idx + 1;

            if recent_logs.len() >= RECENT_LOGS_SIZE {
                recent_logs.pop_front();
            }
            recent_logs.push_back((
                GameIndex {
                    game_idx: 0,
                    log_idx,
                },
                log,
            ));
        }

        Ok((game, recent_logs, next_log_idx))
    }

    /// Update Game storage with changes from a changed_game. Updates the locally cached Game as well
    /// as writing new logs to storage.
    pub async fn store_game(
        &self,
        changed_game: ChangedGame,
    ) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
        let mut logs_with_indices = vec![];

        for log in changed_game.logs {
            let serialized_log = serde_json::to_string(&log)?;
            let log_idx = self.next_log_idx.get();

            info!(
                event = "storing-log",
                snapshot_idx = self.current_snapshot_idx.get(),
                log_idx
            );

            self.sql.exec(
                "INSERT INTO logs (snapshot_idx, log_idx, game_log) VALUES (?, ?, jsonb(?))",
                Some(vec![
                    (self.current_snapshot_idx.get() as i64).into(),
                    (log_idx as i64).into(),
                    serialized_log.into(),
                ]),
            )?;

            logs_with_indices.push((
                GameIndex {
                    game_idx: 0,
                    log_idx,
                },
                log,
            ));

            self.next_log_idx.set(log_idx + 1);
        }

        // Update the cached game
        *self.cached_game.borrow_mut() = changed_game.game.clone();

        // Update the snapshot (always at index 0 for now)
        Self::store_game_snapshot(&self.sql, 0, &changed_game.game).await?;

        // Update recent logs
        let mut recent_logs = self.recent_logs.borrow_mut();
        recent_logs.extend(logs_with_indices.iter().cloned());
        let drain_to = recent_logs.len().saturating_sub(RECENT_LOGS_SIZE);
        recent_logs.drain(0..drain_to);

        Ok(logs_with_indices)
    }

    pub async fn create_invitation(&self) -> anyhow::Result<InvitationID> {
        let invitation_id = InvitationID::gen();

        self.sql.exec(
            "INSERT INTO invitations (id) VALUES (?)",
            Some(vec![invitation_id.to_string().into()]),
        )?;

        Ok(invitation_id)
    }

    pub async fn list_invitations(&self) -> anyhow::Result<Vec<InvitationID>> {
        #[derive(serde::Deserialize)]
        struct InvitationRow {
            id: String,
        }

        let rows: Vec<InvitationRow> = self
            .sql
            .exec("SELECT id FROM invitations", None)?
            .to_array()?;

        let mut invitations = Vec::new();
        for row in rows {
            let invitation_id = row
                .id
                .parse()
                .map_err(|e| anyhow!("Failed to parse invitation ID: {}", e))?;
            invitations.push(invitation_id);
        }

        Ok(invitations)
    }

    pub async fn delete_invitation(
        &self,
        invitation_id: InvitationID,
    ) -> anyhow::Result<Vec<InvitationID>> {
        self.sql.exec(
            "DELETE FROM invitations WHERE id = ?",
            Some(vec![invitation_id.to_string().into()]),
        )?;

        // Return the updated list
        self.list_invitations().await
    }

    pub async fn register_image(
        &self,
        url: &worker::Url,
        image_type: ImageType,
    ) -> anyhow::Result<()> {
        self.sql.exec(
            "INSERT INTO images (image_type, url) VALUES (?, ?)",
            Some(vec![image_type.to_string().into(), url.to_string().into()]),
        )?;

        Ok(())
    }
}
