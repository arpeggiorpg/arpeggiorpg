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
  // Rust doesn't allow us to have an array of async function pointers, so... I guess we just have
  // to write some imperative code here instead of iterating through an array of migration
  // functions.
  let latest_migration = 1;
  console_log!(
    "[DO migration] current_version={current_version}; latest_migration={latest_migration}",
  );

  // if current_version < latest_migration {
  //   run_migration_2(&mut storage)
  //     .await
  //     .map_err(anyhow_str)
  //     .with_context(|| format!("Running migration 2"))?;
  //   console_log!("[DO migration] Ran migration 2 successfully!");
  //   storage.put(VERSION_KEY, 2).await.map_err(anyhow_str)?;
  // }

  console_log!("[DO migration] Done running migrations.");
  Ok(())
}
