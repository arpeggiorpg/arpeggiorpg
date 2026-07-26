use worker::State;

/// Reinitialize the state of this Durable Object.
pub async fn test_init(state: &State) -> anyhow::Result<()> {
    state.storage().delete_all().await?;
    crate::domigrations::migrate_storage_to_current(state.storage())
        .await
        .map_err(crate::anydbg)?;
    Ok(())
}
