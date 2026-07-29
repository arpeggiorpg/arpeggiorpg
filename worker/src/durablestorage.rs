use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
    rc::Rc,
};

use anyhow::anyhow;
use futures_util::lock::Mutex;
use tracing::info;
use worker::{SqlStorage, State};

use arpeggio::{
    game::GameExt,
    types::{ChangedGame, Game, GameLog},
};
use arptypes::{
    multitenant::{GameIndex, ImageType, InvitationID},
    GMCommand, ResourceRef,
};

use crate::entity_storage;
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
    update_lock: Mutex<()>,
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
        let game = entity_storage::load_game(&sql)?;
        let latest_snapshot_idx = Self::latest_snapshot_idx(&sql)?.unwrap_or(0);
        let (recent_logs, next_log_idx) = Self::load_recent_logs(&sql, latest_snapshot_idx)?;

        let game_storage = Self {
            state,
            current_snapshot_idx: Cell::new(latest_snapshot_idx),
            next_log_idx: Cell::new(next_log_idx),
            cached_game: Rc::new(RefCell::new(game)),
            recent_logs: Rc::new(RefCell::new(recent_logs)),
            update_lock: Mutex::new(()),
        };

        Ok(game_storage)
    }

    fn latest_snapshot_idx(sql: &SqlStorage) -> anyhow::Result<Option<usize>> {
        #[derive(serde::Deserialize)]
        struct SnapshotIndexRow {
            snapshot_idx: usize,
        }

        let rows: Vec<SnapshotIndexRow> = sql
            .exec(
                "SELECT snapshot_idx FROM game_snapshots ORDER BY snapshot_idx DESC LIMIT 1",
                None,
            )?
            .to_array()?;
        Ok(rows.into_iter().next().map(|row| row.snapshot_idx))
    }

    fn load_latest_game_snapshot(sql: &SqlStorage) -> anyhow::Result<Option<(Game, usize)>> {
        #[derive(serde::Deserialize)]
        struct GameRow {
            game: String, // can't figure out how to just make this `Game`
            snapshot_idx: usize,
        }

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

    fn load_snapshot_materialized_game(sql: &SqlStorage) -> anyhow::Result<Game> {
        #[derive(serde::Deserialize)]
        struct LogRow {
            game_log: String,
        }

        let (mut game, snapshot_idx) = Self::load_latest_game_snapshot(sql)?
            .ok_or_else(|| anyhow!("No game snapshot exists"))?;
        let logs: Vec<LogRow> = sql
            .exec(
                "SELECT json(game_log) AS game_log
                 FROM logs
                 WHERE snapshot_idx = ?
                 ORDER BY log_idx",
                Some(vec![(snapshot_idx as i64).into()]),
            )?
            .to_array()?;
        for row in logs {
            let log = serde_json::from_str(&row.game_log)?;
            game = game.apply_log(&log)?;
        }
        Ok(game)
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

    fn load_recent_logs(
        sql: &SqlStorage,
        latest_snapshot_idx: usize,
    ) -> anyhow::Result<(RecentGameLogs, usize)> {
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
            return Ok((recent_logs, next_log_idx));
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

            if snapshot_idx == latest_snapshot_idx {
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

        Ok((recent_logs, next_log_idx))
    }

    /// Update Game storage with changes from a changed_game. Updates the locally cached Game as well
    /// as writing new logs to storage.
    pub async fn update_game(
        &self,
        changed_game: ChangedGame,
    ) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
        let _update_guard = self.update_lock.lock().await;
        let old_game = self.game();
        let snapshot_idx = self.current_snapshot_idx.get();
        let first_log_idx = self.next_log_idx.get();
        let logs_with_indices: Vec<_> = changed_game
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
        let serialized_logs: Vec<_> = changed_game
            .logs
            .iter()
            .map(serde_json::to_string)
            .collect::<Result<_, _>>()?;
        let next_log_idx = first_log_idx + serialized_logs.len();
        let new_snapshot_idx = (next_log_idx >= NUM_LOGS_PER_SNAPSHOT).then_some(snapshot_idx + 1);

        let sql = self.state.storage().sql();
        let storage = self.state.storage();
        let old_game_for_write = old_game;
        let new_game_for_write = changed_game.game.clone();
        storage
            .transaction(move |_transaction| {
                let sql = sql.clone();
                async move {
                    entity_storage::persist_game_delta(
                        &sql,
                        &old_game_for_write,
                        &new_game_for_write,
                    )
                    .map_err(crate::rust_error)?;

                    for (offset, serialized_log) in serialized_logs.into_iter().enumerate() {
                        let log_idx = first_log_idx + offset;
                        info!(event = "storing-log", snapshot_idx, log_idx);
                        sql.exec(
                            "INSERT INTO logs (snapshot_idx, log_idx, game_log)
                             VALUES (?, ?, jsonb(?))",
                            Some(vec![
                                (snapshot_idx as i64).into(),
                                (log_idx as i64).into(),
                                serialized_log.into(),
                            ]),
                        )?;
                    }

                    if let Some(new_snapshot_idx) = new_snapshot_idx {
                        info!(
                            event = "creating-new-snapshot",
                            new_snapshot_idx,
                            log_count = next_log_idx
                        );
                        Self::store_game_snapshot(&sql, new_snapshot_idx, &new_game_for_write)
                            .map_err(crate::rust_error)?;
                    }
                    Ok(())
                }
            })
            .await
            .map_err(crate::anydbg)?;

        *self.cached_game.borrow_mut() = changed_game.game;
        if let Some(new_snapshot_idx) = new_snapshot_idx {
            self.current_snapshot_idx.set(new_snapshot_idx);
            self.next_log_idx.set(0);
        } else {
            self.next_log_idx.set(next_log_idx);
        }

        // Update recent logs (keep all logs from most recent 2 snapshots)
        let mut recent_logs = self.recent_logs.borrow_mut();
        recent_logs.extend(logs_with_indices.iter().cloned());

        Ok(logs_with_indices)
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

    pub fn check_invitation(&self, invitation_id: InvitationID) -> anyhow::Result<bool> {
        #[derive(serde::Deserialize)]
        struct CountRow {
            count: i64,
        }

        let rows: Vec<CountRow> = self
            .state
            .storage()
            .sql()
            .exec(
                "SELECT COUNT(*) as count FROM invitations WHERE id = ?",
                Some(vec![invitation_id.to_string().into()]),
            )?
            .to_array()?;

        Ok(rows.first().map(|r| r.count > 0).unwrap_or(false))
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
        game_storage.update_game(changed_game).await?;
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

/// Test that a multi-log command is kept atomic when it crosses the snapshot threshold.
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
        game_storage.update_game(changed_game).await?;
    }
    assert_eq!(game_storage.current_snapshot_idx.get(), 0);
    assert_eq!(game_storage.recent_logs().len(), 98);

    // we could use GMCommand::CreateNote, but it may not continue generating multiple logs in the
    // future, so let's just manually build a ChangedGame with multiple logs.
    let grouped_logs: Vec<_> = (0..5)
        .map(|i| GameLog::ChatFromGM {
            message: format!("msg {i}"),
        })
        .collect();
    let game = game_storage.game();
    let changed = game.change_with_logs(grouped_logs)?;

    game_storage.update_game(changed).await?;

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
    assert_eq!(old_snapshot_logs, 103);
    let new_snapshot_logs = recent_logs
        .iter()
        .filter(|(gi, _gl)| gi.game_idx == 1)
        .count();
    assert_eq!(new_snapshot_logs, 0);
    Ok(())
}

/// Test that a brand-new game DO can be initialized from scratch, exercising the full
/// migration + load path that `get_game_storage` uses.
#[tracing::instrument(skip(state))]
pub async fn test_fresh_game_initialization(state: Rc<State>) -> anyhow::Result<()> {
    // Wipe everything to simulate a brand-new Durable Object
    state.storage().delete_all().await?;

    // Run the full migration path, just like get_game_storage does
    crate::domigrations::migrate_storage_to_current(state.storage())
        .await
        .map_err(crate::anydbg)?;

    // Now load GameStorage, which should create a default game snapshot
    let game_storage = GameStorage::load(state.clone())?;
    assert_eq!(game_storage.current_snapshot_idx.get(), 0);
    assert_eq!(game_storage.recent_logs().len(), 0);

    // Verify the game is a valid default
    let game = game_storage.game();
    assert!(game.current_combat.is_none());
    assert_eq!(game.creatures.len(), 0);
    assert_eq!(game.scenes.len(), 0);

    // Run a GM command to verify the game is functional
    let cmd = GMCommand::ChatFromGM {
        message: "Hello from fresh game".to_string(),
    };
    let changed_game = game_storage.game().perform_gm_command(cmd)?;
    game_storage.update_game(changed_game).await?;
    assert_eq!(game_storage.recent_logs().len(), 1);

    // Reload from storage to verify persistence
    let fresh_storage = GameStorage::load(state.clone())?;
    assert_eq!(fresh_storage.current_snapshot_idx.get(), 0);
    assert_eq!(fresh_storage.recent_logs().len(), 1);

    let game = fresh_storage.game();
    assert!(game.current_combat.is_none());

    Ok(())
}

#[tracing::instrument(skip(state))]
pub async fn test_typed_table_persistence(state: Rc<State>) -> anyhow::Result<()> {
    test_init(&state).await?;
    let game_storage = GameStorage::load(state.clone())?;

    let changed_game = game_storage
        .game()
        .perform_gm_command(GMCommand::CreateItem {
            name: "Typed-table item".to_string(),
        })?;
    let item_id = changed_game
        .game
        .items
        .values()
        .next()
        .ok_or_else(|| anyhow!("CreateItem did not create an item"))?
        .id;
    game_storage.update_game(changed_game).await?;

    let sql = state.storage().sql();
    assert_eq!(entity_storage::load_game(&sql)?, game_storage.game());
    assert_eq!(
        GameStorage::load_snapshot_materialized_game(&sql)?,
        game_storage.game()
    );

    let changed_game = game_storage
        .game()
        .perform_gm_command(GMCommand::DeleteResource {
            resource: ResourceRef::Item(item_id),
        })?;
    game_storage.update_game(changed_game).await?;
    assert_eq!(entity_storage::load_game(&sql)?, game_storage.game());
    assert_eq!(
        GameStorage::load_snapshot_materialized_game(&sql)?,
        game_storage.game()
    );

    #[derive(serde::Deserialize)]
    struct CountRow {
        count: i64,
    }
    let item_count: CountRow = sql
        .exec("SELECT count(*) AS count FROM items", None)?
        .one()?;
    assert_eq!(item_count.count, 0);
    Ok(())
}

#[tracing::instrument(skip(state))]
pub async fn test_all_typed_tables_round_trip(state: Rc<State>) -> anyhow::Result<()> {
    use arptypes::{
        u32cm, AbilityCreation, Action, ClassCreation, CreatureCreation, CreatureEffect,
        CreatureTarget, Dice, Energy, NoteVisibility, PlayerID, SceneCreation, TileSystem, AABB,
    };

    test_init(&state).await?;
    let mut game = Game::default();
    game = game
        .perform_gm_command(GMCommand::CreateAbility {
            ability: AbilityCreation {
                name: "Round-trip ability".to_string(),
                cost: Energy(1),
                action: Action::Creature {
                    effect: CreatureEffect::Heal(Dice::Flat { value: 1 }),
                    target: CreatureTarget::Actor,
                },
                usable_ooc: true,
            },
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::CreateClass {
            class: ClassCreation {
                name: "Round-trip class".to_string(),
                abilities: game.abilities.keys().copied().collect(),
                conditions: vec![],
                color: "#ffffff".to_string(),
                emoji: Some("🧪".to_string()),
            },
        })?
        .game;
    let class_id = *game
        .classes
        .keys()
        .next()
        .ok_or_else(|| anyhow!("CreateClass did not create a class"))?;
    game = game
        .perform_gm_command(GMCommand::CreateCreature {
            creature: CreatureCreation {
                name: "Round-trip creature".to_string(),
                class: class_id,
                portrait_url: String::new(),
                icon_url: String::new(),
                note: String::new(),
                bio: String::new(),
                initiative: Dice::Flat { value: 0 },
                size: AABB {
                    x: u32cm(100),
                    y: u32cm(100),
                    z: u32cm(100),
                },
            },
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::CreateScene {
            scene: SceneCreation {
                name: "Round-trip scene".to_string(),
                background_image_url: String::new(),
                background_image_offset: None,
                background_image_scale: (1.0, 1.0),
            },
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::CreateNote {
            name: "Round-trip note".to_string(),
            content: "Stored independently".to_string(),
            visibility: NoteVisibility::GMOnly,
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::CreateItem {
            name: "Round-trip item".to_string(),
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::CreateCollection {
            name: "Round-trip collection".to_string(),
        })?
        .game;
    game = game
        .perform_gm_command(GMCommand::RegisterPlayer {
            id: PlayerID("round-trip-player".to_string()),
        })?
        .game;
    game.tile_system = TileSystem::DnD;
    game.active_scene = game.scenes.keys().next().copied();

    entity_storage::replace_game(&state.storage().sql(), &game)?;
    assert_eq!(entity_storage::load_game(&state.storage().sql())?, game);
    Ok(())
}

#[tracing::instrument(skip(state))]
pub async fn test_typed_table_cold_load(state: Rc<State>) -> anyhow::Result<()> {
    test_init(&state).await?;
    let game_storage = GameStorage::load(state.clone())?;
    let changed_game = game_storage
        .game()
        .perform_gm_command(GMCommand::CreateItem {
            name: "Typed authority".to_string(),
        })?;
    game_storage.update_game(changed_game).await?;
    let expected_game = game_storage.game();

    // The latest snapshot still predates CreateItem. Removing its log makes the snapshot
    // representation stale while leaving the typed rows authoritative.
    state.storage().sql().exec("DELETE FROM logs", None)?;
    let fresh_storage = GameStorage::load(state)?;
    assert_eq!(fresh_storage.game(), expected_game);
    Ok(())
}

#[tracing::instrument(skip(state))]
pub async fn test_failed_typed_update_is_atomic(state: Rc<State>) -> anyhow::Result<()> {
    test_init(&state).await?;
    let game_storage = GameStorage::load(state.clone())?;
    let original_game = game_storage.game();
    state.storage().sql().exec(
        "CREATE TRIGGER reject_test_item
         BEFORE INSERT ON items
         BEGIN
             SELECT RAISE(ABORT, 'intentional typed-table failure');
         END",
        None,
    )?;

    let changed_game = original_game
        .clone()
        .perform_gm_command(GMCommand::CreateItem {
            name: "Must roll back".to_string(),
        })?;
    assert!(game_storage.update_game(changed_game).await.is_err());
    assert_eq!(game_storage.game(), original_game);
    assert_eq!(
        entity_storage::load_game(&state.storage().sql())?,
        original_game
    );

    #[derive(serde::Deserialize)]
    struct CountRow {
        count: i64,
    }
    let log_count: CountRow = state
        .storage()
        .sql()
        .exec("SELECT count(*) AS count FROM logs", None)?
        .one()?;
    assert_eq!(log_count.count, 0);
    Ok(())
}
