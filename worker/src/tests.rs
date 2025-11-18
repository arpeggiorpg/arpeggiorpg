use worker::State;

use crate::sqlite::initialize_sqlite_tables;

/// Reinitialize the state of this Durable Object.
pub async fn test_init(state: &State) -> anyhow::Result<()> {
    state.storage().delete_all().await?;
    let sql = state.storage().sql();
    initialize_sqlite_tables(&sql).await?;
    Ok(())
}
