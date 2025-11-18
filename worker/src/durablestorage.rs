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
use arptypes::{
    multitenant::{GameIndex, ImageType, InvitationID},
    GMCommand,
};

use crate::tests::test_init;

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
    state: Rc<State>,
    current_snapshot_idx: Cell<usize>,
    next_log_idx: Cell<usize>,
    cached_game: Rc<RefCell<Game>>,
    recent_logs: Rc<RefCell<RecentGameLogs>>,
}

const NUM_LOGS_PER_SNAPSHOT: usize = 100;

/// Storage using SQLite in Durable Objects
impl GameStorage {
    pub fn game(&self) -> Game {
        self.cached_game.borrow().clone()
    }

    pub fn recent_logs(&self) -> RecentGameLogs {
        self.recent_logs.borrow().clone()
    }

    #[tracing::instrument(skip(state))]
    pub fn load(state: Rc<State>) -> anyhow::Result<Self> {
        info!(event = "game-load-start");
        let sql = state.storage().sql();

        // Find the most recent snapshot
        let (game, latest_snapshot_idx) = match Self::load_latest_game_snapshot(state.clone())? {
            Some(x) => x,
            None => {
                info!(event = "new-game");
                let default_game = Game::default();
                Self::store_game_snapshot(&sql, 0, &default_game)?;
                (default_game, 0)
            }
        };

        // Load recent logs from the most recent 2 snapshot indexes and apply them to the game
        let (final_game, recent_logs, next_log_idx) =
            Self::load_and_apply_recent_logs(&sql, game, latest_snapshot_idx)?;

        let game_storage = Self {
            state,
            current_snapshot_idx: Cell::new(latest_snapshot_idx),
            next_log_idx: Cell::new(next_log_idx),
            cached_game: Rc::new(RefCell::new(final_game)),
            recent_logs: Rc::new(RefCell::new(recent_logs)),
        };

        Ok(game_storage)
    }

    fn load_latest_game_snapshot(state: Rc<State>) -> anyhow::Result<Option<(Game, usize)>> {
        #[derive(serde::Deserialize)]
        struct GameRow {
            game: String, // can't figure out how to just make this `Game`
            snapshot_idx: usize,
        }

        let sql = state.storage().sql();
        let rows: Vec<GameRow> = sql
            .exec(
                "SELECT json(game) as game, snapshot_idx FROM game_snapshots \
                WHERE snapshot_idx = (SELECT MAX(snapshot_idx) FROM game_snapshots)",
                None,
            )?
            .to_array()?;

        match rows.into_iter().next() {
            Some(GameRow { game, snapshot_idx }) => {
                let game = serde_json::from_str(&game)?;
                Ok(Some((game, snapshot_idx)))
            }
            None => Ok(None),
        }
    }

    fn store_game_snapshot(
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

    fn load_and_apply_recent_logs(
        sql: &SqlStorage,
        mut game: Game,
        latest_snapshot_idx: usize,
    ) -> anyhow::Result<(Game, RecentGameLogs, usize)> {
        // Load logs from the most recent 2 snapshot indexes for history
        let snapshot_indices = if latest_snapshot_idx == 0 {
            vec![0]
        } else {
            vec![latest_snapshot_idx - 1, latest_snapshot_idx]
        };

        #[derive(serde::Deserialize)]
        struct LogRow {
            log_idx: i64,
            game_log: String,
            snapshot_idx: i64,
        }

        let snapshot_indices: Vec<worker::SqlStorageValue> = snapshot_indices
            .iter()
            .map(|&idx| (idx as i64).into())
            .collect();

        let placeholders = vec!["?"; snapshot_indices.len()].join(", ");
        let query = format!(
            "SELECT log_idx, json(game_log) as game_log, snapshot_idx FROM logs WHERE snapshot_idx IN ({}) ORDER BY snapshot_idx, log_idx",
            placeholders
        );

        let all_rows: Vec<LogRow> = sql.exec(&query, Some(snapshot_indices))?.to_array()?;

        let mut recent_logs = VecDeque::new();
        let mut next_log_idx = 0;

        if all_rows.is_empty() {
            return Ok((game, recent_logs, next_log_idx));
        }

        info!(event = "loading-logs", num = all_rows.len());

        for row in all_rows {
            let log: GameLog = serde_json::from_str(&row.game_log).map_err(|e| {
                anyhow!(
                    "Failed parsing GameLog as JSON:\ncontent: {:?}\nerror: {:?}",
                    row.game_log,
                    e
                )
            })?;

            let log_idx = row.log_idx as usize;
            let snapshot_idx = row.snapshot_idx as usize;

            // Only apply logs from the latest snapshot to the game state
            if snapshot_idx == latest_snapshot_idx {
                game = game.apply_log(&log)?;
                next_log_idx = log_idx + 1;
            }

            // But add all logs to recent_logs for history browsing
            recent_logs.push_back((
                GameIndex {
                    game_idx: snapshot_idx,
                    log_idx,
                },
                log,
            ));
        }

        Ok((game, recent_logs, next_log_idx))
    }

    /// Update Game storage with changes from a changed_game. Updates the locally cached Game as well
    /// as writing new logs to storage.
    pub fn update_game(
        &self,
        changed_game: ChangedGame,
    ) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
        let mut logs_with_indices = vec![];

        // TODO: verify if the "implicit transactions" thing actually works, because this code
        // *definitely* relies on it:
        // https://developers.cloudflare.com/durable-objects/api/sqlite-storage-api/#transaction
        for log in &changed_game.logs {
            // Update the cached game

            self.maybe_snapshot(changed_game.clone())?;

            let serialized_log = serde_json::to_string(&log)?;
            let log_idx = self.next_log_idx.get();

            info!(
                event = "storing-log",
                snapshot_idx = self.current_snapshot_idx.get(),
                log_idx
            );

            self.state.storage().sql().exec(
                "INSERT INTO logs (snapshot_idx, log_idx, game_log) VALUES (?, ?, jsonb(?))",
                Some(vec![
                    (self.current_snapshot_idx.get() as i64).into(),
                    (log_idx as i64).into(),
                    serialized_log.into(),
                ]),
            )?;

            logs_with_indices.push((
                GameIndex {
                    game_idx: self.current_snapshot_idx.get(),
                    log_idx,
                },
                log.clone(),
            ));

            self.next_log_idx.set(log_idx + 1);
        }
        *self.cached_game.borrow_mut() = changed_game.game;

        // Update recent logs (keep all logs from most recent 2 snapshots)
        let mut recent_logs = self.recent_logs.borrow_mut();
        recent_logs.extend(logs_with_indices.iter().cloned());

        Ok(logs_with_indices)
    }

    fn maybe_snapshot(&self, changed_game: ChangedGame) -> anyhow::Result<()> {
        // Check if we need to create a new snapshot after storing all logs
        let total_logs_in_snapshot = self.next_log_idx.get();
        if total_logs_in_snapshot >= NUM_LOGS_PER_SNAPSHOT {
            let new_snapshot_idx = self.current_snapshot_idx.get() + 1;
            info!(
                event = "creating-new-snapshot",
                new_snapshot_idx,
                log_count = total_logs_in_snapshot
            );

            Self::store_game_snapshot(
                &self.state.storage().sql(),
                new_snapshot_idx,
                &changed_game.game,
            )?;
            self.current_snapshot_idx.set(new_snapshot_idx);

            // Reset log index for the new snapshot - future logs will start at 0 for the new snapshot
            self.next_log_idx.set(0);
        }
        Ok(())
    }

    pub fn create_invitation(&self) -> anyhow::Result<InvitationID> {
        let invitation_id = InvitationID::gen();

        self.state.storage().sql().exec(
            "INSERT INTO invitations (id) VALUES (?)",
            Some(vec![invitation_id.to_string().into()]),
        )?;

        Ok(invitation_id)
    }

    pub fn list_invitations(&self) -> anyhow::Result<Vec<InvitationID>> {
        #[derive(serde::Deserialize)]
        struct InvitationRow {
            id: String,
        }

        let rows: Vec<InvitationRow> = self
            .state
            .storage()
            .sql()
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

    pub fn delete_invitation(
        &self,
        invitation_id: InvitationID,
    ) -> anyhow::Result<Vec<InvitationID>> {
        self.state.storage().sql().exec(
            "DELETE FROM invitations WHERE id = ?",
            Some(vec![invitation_id.to_string().into()]),
        )?;

        // Return the updated list
        self.list_invitations()
    }

    pub fn register_image(&self, url: &worker::Url, image_type: ImageType) -> anyhow::Result<()> {
        self.state.storage().sql().exec(
            "INSERT INTO images (image_type, url) VALUES (?, ?)",
            Some(vec![image_type.to_string().into(), url.to_string().into()]),
        )?;

        Ok(())
    }
}

/// Test snapshot creation
#[tracing::instrument(skip(state))]
pub async fn test_snapshot_creation(state: Rc<State>) -> anyhow::Result<()> {
    test_init(&state).await?;
    // Load a real GameStorage instance
    let game_storage = GameStorage::load(state.clone())?;
    assert_eq!(game_storage.current_snapshot_idx.get(), 0);
    for i in 0..101 {
        let cmd = GMCommand::ChatFromGM {
            message: format!("Test message {i}"),
        };
        let changed_game = game_storage.game().perform_gm_command(cmd)?;
        game_storage.update_game(changed_game)?;
    }
    assert_eq!(game_storage.current_snapshot_idx.get(), 1);
    assert_eq!(game_storage.recent_logs().len(), 101);

    // Verify persisted state by loading a fresh GameStorage
    let fresh_storage = GameStorage::load(state.clone())?;
    assert_eq!(fresh_storage.current_snapshot_idx.get(), 1);
    let recent_logs = fresh_storage.recent_logs();
    assert_eq!(recent_logs.len(), 101);

    let old_snapshot_logs = recent_logs
        .iter()
        .filter(|(gi, _gl)| gi.game_idx == 0)
        .count();
    assert_eq!(old_snapshot_logs, 100);
    let new_snapshot_logs = recent_logs
        .iter()
        .filter(|(gi, _gl)| gi.game_idx == 1)
        .count();
    assert_eq!(new_snapshot_logs, 1);
    Ok(())
}

/// Test that multi-log commands don't allow log count to go above NUM_LOGS_PER_SNAPSHOT.
#[tracing::instrument(skip(state))]
pub async fn test_snapshot_creation_multilog(state: Rc<State>) -> anyhow::Result<()> {
    test_init(&state).await?;
    let game_storage = GameStorage::load(state.clone())?;
    assert_eq!(game_storage.current_snapshot_idx.get(), 0);
    assert_eq!(game_storage.recent_logs().len(), 0);
    for i in 0..98 {
        let cmd = GMCommand::ChatFromGM {
            message: format!("Test message {i}"),
        };
        let changed_game = game_storage.game().perform_gm_command(cmd)?;
        game_storage.update_game(changed_game)?;
    }
    assert_eq!(game_storage.current_snapshot_idx.get(), 0);
    assert_eq!(game_storage.recent_logs().len(), 98);

    // we could use GMCommand::CreateNote, but it may not continue generating multiple logs in the
    // future, so let's just manually build a ChangedGame with multiple logs.
    let grouped_logs: Vec<_> = (0..5)
        .into_iter()
        .map(|i| GameLog::ChatFromGM {
            message: format!("msg {i}"),
        })
        .collect();
    let game = game_storage.game();
    let changed = game.change_with_logs(grouped_logs)?;

    game_storage.update_game(changed)?;

    assert_eq!(game_storage.current_snapshot_idx.get(), 1);
    assert_eq!(game_storage.recent_logs().len(), 103);

    // Verify persisted state by loading a fresh GameStorage
    let fresh_storage = GameStorage::load(state.clone())?;
    assert_eq!(fresh_storage.current_snapshot_idx.get(), 1);
    let recent_logs = fresh_storage.recent_logs();
    assert_eq!(recent_logs.len(), 103);

    let old_snapshot_logs = recent_logs
        .iter()
        .filter(|(gi, _gl)| gi.game_idx == 0)
        .count();
    assert_eq!(old_snapshot_logs, 100);
    let new_snapshot_logs = recent_logs
        .iter()
        .filter(|(gi, _gl)| gi.game_idx == 1)
        .count();
    assert_eq!(new_snapshot_logs, 3);
    Ok(())
}
