use crate::anyhow_str;
use anyhow::Context;
use worker::{console_error, console_log, ListOptions, Storage};

const VERSION_KEY: &str = "DURABLEGAME_VERSION";

pub async fn migrate(storage: Storage) -> anyhow::Result<()> {
  let current_version = storage.get::<usize>(VERSION_KEY).await;
  match current_version {
    Ok(current_version) => Ok(migrate_from(storage, current_version).await?),
    Err(worker::Error::JsError(e)) if e == "No such value in storage." => {
      Ok(migrate_from(storage, 0).await?)
    }
    Err(e) => Err(anyhow_str(e)),
  }
}

async fn migrate_from(mut storage: Storage, current_version: usize) -> anyhow::Result<()> {
  let migrations = [(1, collapse_logs)];

  console_log!(
    "[DO migration] current_version={current_version}; latest_migration={}",
    migrations[migrations.len() - 1].0
  );

  for (migration_num, migration) in migrations.iter() {
    if current_version < *migration_num {
      migration(&mut storage)
        .await
        .map_err(anyhow_str)
        .with_context(|| format!("Running migration {migration_num}"))?;
      console_log!("[DO migration] Ran migration {migration_num} successfully!");
      storage.put(VERSION_KEY, migration_num).await.map_err(anyhow_str)?;
    }
  }
  console_log!("[DO migration] Done running migrations.");
  Ok(())
}

/// This is a dumb migration to collapse logs into the Game and save it out as the only snapshot,
/// because I'm changing the GameLog representation and don't really have production data yet.
async fn collapse_logs(storage: &mut Storage) -> anyhow::Result<()> {
  let (game, num_logs) = load(storage).await.context("Loading game")?;
  let json = serde_json::to_string(&game)?;
  for log_idx in 0..num_logs {
    storage.delete(&format!("log-{:09}-idx-{:09}", 0, log_idx)).await.map_err(anyhow_str)?;
  }
  storage.put("snapshot-0-chunk-0", json).await.map_err(anyhow_str)?;
  return Ok(());

  use arpeggio::types::{Game, GameLog};
  async fn load(storage: &mut Storage) -> anyhow::Result<(Game, usize)> {
    // TODO: support muiltple snapshots? Or maybe just wait until SQLite support exists...

    let (game, next_log_idx) = match storage.get::<String>("snapshot-0-chunk-0").await {
      Ok(game_str) => {
        let game = serde_json::from_str(&game_str).context("Loading initial snapshot")?;
        load_logs(storage, game).await.context("loading logs")?
      }
      Err(e) => {
        console_error!("ERROR: Loading game failed: {e:?}");
        match e {
          worker::Error::JsError(e) if e == "No such value in storage." => {
            console_log!("No initial snapshot. This is a new game!");
            let default_game = Default::default();
            storage
              .put("snapshot-0-chunk-0", serde_json::to_string(&default_game)?)
              .await
              .map_err(anyhow_str)?;
            (default_game, 0)
          }
          _ => Err(anyhow_str(e))?,
        }
      }
    };
    Ok((game, next_log_idx))
  }

  async fn load_logs(storage: &mut Storage, mut game: Game) -> anyhow::Result<(Game, usize)> {
    let list_options = ListOptions::new().prefix("log-");
    let items =
      storage.list_with_options(list_options).await.map_err(anyhow_str).context("listing logs")?;
    let mut log_idx = 0;
    console_log!("Loading Logs: Found {} items", items.size());
    for key in items.keys() {
      let key = key.map_err(anyhow_str).context("extracting key")?;
      let value = items.get(&key);
      let value: String = serde_wasm_bindgen::from_value(value.clone())
        .map_err(anyhow_str)
        .with_context(|| format!("parsing value from JS: {value:?}"))?;
      let key: String = serde_wasm_bindgen::from_value(key.clone())
        .map_err(anyhow_str)
        .with_context(|| format!("parsing key from JS: {key:?}"))?;
      console_log!("found a log {key:?}");
      if let ["log", _, "idx", log_idx_str] = key.split("-").collect::<Vec<_>>()[..] {
        let log: GameLog = serde_json::from_str(&value)
          .with_context(|| format!("Parsing log as JSON: {value:?}"))?;
        game = game.apply_log(&log).with_context(|| format!("Applying log: {log:?}"))?;
        log_idx =
          log_idx_str.parse().with_context(|| format!("parsing log idx: {log_idx_str:?}"))?;
      } else {
        console_log!("Don't know what this log is: {key:?}");
      }
    }

    Ok((game, log_idx + 1))
  }
}
