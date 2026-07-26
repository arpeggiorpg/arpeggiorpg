use serde::Deserialize;
use tracing::info;
use worker::{wasm_bindgen::JsValue, Error, Result, SqlStorage, State, Storage, Transaction};

use crate::sqlite::initialize_sqlite_tables;

const LEGACY_VERSION_KEY: &str = "DURABLEGAME_VERSION";
const METADATA_TABLE: &str = "arpeggio_storage_metadata";
const STORAGE_VERSION_KEY: &str = "storage_version";

pub const BASELINE_STORAGE_VERSION: StorageVersion = StorageVersion(1);
pub const CURRENT_STORAGE_VERSION: StorageVersion = BASELINE_STORAGE_VERSION;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct StorageVersion(pub u32);

#[derive(Clone, Copy)]
struct Migration {
    version: StorageVersion,
    run: fn(&SqlStorage) -> Result<()>,
}

// Add future migrations here in ascending target-version order. The production baseline is already
// the current schema, so there are intentionally no historical migrations in this registry.
const MIGRATIONS: &[Migration] = &[];

#[derive(Debug, Deserialize)]
struct CountRow {
    count: i64,
}

#[derive(Debug, Deserialize)]
struct MetadataValueRow {
    value: String,
}

enum UnversionedStorage {
    Empty,
    ProductionBaseline,
}

fn migration_error(message: impl Into<String>) -> Error {
    Error::RustError(message.into())
}

fn metadata_table_exists(sql: &SqlStorage) -> Result<bool> {
    let row: CountRow = sql
        .exec(
            &format!(
                "SELECT count(*) AS count
                 FROM sqlite_schema
                 WHERE type = 'table' AND name = '{METADATA_TABLE}'"
            ),
            None,
        )?
        .one()?;
    Ok(row.count == 1)
}

fn read_storage_version(sql: &SqlStorage) -> Result<Option<StorageVersion>> {
    if !metadata_table_exists(sql)? {
        return Ok(None);
    }

    let rows: Vec<MetadataValueRow> = sql
        .exec(
            &format!(
                "SELECT value
                 FROM {METADATA_TABLE}
                 WHERE key = '{STORAGE_VERSION_KEY}'"
            ),
            None,
        )?
        .to_array()?;
    let [row] = rows.as_slice() else {
        return Err(migration_error(
            "Storage metadata must contain exactly one storage_version",
        ));
    };
    let version = row.value.parse::<u32>().map_err(|error| {
        migration_error(format!("Invalid storage version {:?}: {error}", row.value))
    })?;
    Ok(Some(StorageVersion(version)))
}

fn write_storage_version(sql: &SqlStorage, version: StorageVersion) -> Result<()> {
    sql.exec(
        &format!(
            "INSERT INTO {METADATA_TABLE} (key, value)
             VALUES ('{STORAGE_VERSION_KEY}', '{}')
             ON CONFLICT (key) DO UPDATE SET value = excluded.value",
            version.0
        ),
        None,
    )?;
    Ok(())
}

fn initialize_metadata(sql: &SqlStorage, version: StorageVersion) -> Result<()> {
    sql.exec(
        &format!(
            "CREATE TABLE {METADATA_TABLE} (
                key TEXT PRIMARY KEY NOT NULL,
                value TEXT NOT NULL
             )"
        ),
        None,
    )?;
    write_storage_version(sql, version)
}

fn application_object_count(sql: &SqlStorage) -> Result<i64> {
    let row: CountRow = sql
        .exec(
            &format!(
                "SELECT count(*) AS count
                 FROM sqlite_schema
                 WHERE sql IS NOT NULL
                   AND name NOT LIKE 'sqlite_%'
                   AND lower(name) NOT IN ('__cf_kv', '_cf_kv')
                   AND name NOT LIKE '__miniflare_%'
                   AND name <> '{METADATA_TABLE}'"
            ),
            None,
        )?
        .one()?;
    Ok(row.count)
}

async fn classify_unversioned_storage(
    sql: &SqlStorage,
    transaction: &Transaction,
) -> Result<UnversionedStorage> {
    let entries = transaction.list().await?;
    if entries.size() == 0 {
        if application_object_count(sql)? == 0 {
            return Ok(UnversionedStorage::Empty);
        }
        return Err(migration_error(
            "Unversioned Durable Object contains SQL data without a trusted legacy version",
        ));
    }

    let legacy_version_key = JsValue::from_str(LEGACY_VERSION_KEY);
    if entries.size() != 1 || !entries.has(&legacy_version_key) {
        return Err(migration_error(
            "Unversioned Durable Object contains unknown application KV",
        ));
    }

    let version = transaction.get::<u32>(LEGACY_VERSION_KEY).await?;
    if version != BASELINE_STORAGE_VERSION.0 {
        return Err(migration_error(format!(
            "Unsupported legacy storage version {version}; expected {}",
            BASELINE_STORAGE_VERSION.0
        )));
    }

    // Every production game is known to have reached legacy version 1, so that marker is the
    // authoritative proof of the baseline. Do not duplicate the application schema here.
    Ok(UnversionedStorage::ProductionBaseline)
}

fn establish_baseline(sql: &SqlStorage, storage: UnversionedStorage) -> Result<StorageVersion> {
    match storage {
        UnversionedStorage::Empty => initialize_sqlite_tables(sql)?,
        UnversionedStorage::ProductionBaseline => {}
    }
    initialize_metadata(sql, BASELINE_STORAGE_VERSION)?;
    Ok(BASELINE_STORAGE_VERSION)
}

fn validate_migration_registry(target: StorageVersion, migrations: &[Migration]) -> Result<()> {
    let mut expected = StorageVersion(BASELINE_STORAGE_VERSION.0 + 1);
    for migration in migrations {
        if migration.version != expected {
            return Err(migration_error(format!(
                "Migration registry expected version {}, found {}",
                expected.0, migration.version.0
            )));
        }
        expected = StorageVersion(expected.0 + 1);
    }

    let registry_target = migrations
        .last()
        .map_or(BASELINE_STORAGE_VERSION, |migration| migration.version);
    if registry_target != target {
        return Err(migration_error(format!(
            "Migration registry ends at version {}, but current version is {}",
            registry_target.0, target.0
        )));
    }
    Ok(())
}

fn apply_migrations(
    sql: &SqlStorage,
    mut version: StorageVersion,
    target: StorageVersion,
    migrations: &[Migration],
) -> Result<StorageVersion> {
    if version > target {
        return Err(migration_error(format!(
            "Storage version {} is newer than supported version {}",
            version.0, target.0
        )));
    }

    for migration in migrations {
        if version == target || migration.version > target {
            break;
        }
        if migration.version <= version {
            continue;
        }
        let expected = StorageVersion(version.0 + 1);
        if migration.version != expected {
            return Err(migration_error(format!(
                "Missing migration from storage version {} to {}",
                version.0, expected.0
            )));
        }

        info!(
            event = "running-storage-migration",
            from_version = version.0,
            to_version = migration.version.0
        );
        (migration.run)(sql)?;
        write_storage_version(sql, migration.version)?;
        version = migration.version;
    }

    if version != target {
        return Err(migration_error(format!(
            "Migration chain ended at version {}, expected {}",
            version.0, target.0
        )));
    }
    Ok(version)
}

async fn migrate_storage(
    storage: Storage,
    target: StorageVersion,
    migrations: &'static [Migration],
) -> Result<StorageVersion> {
    validate_migration_registry(target, migrations)?;
    let sql = storage.sql();
    storage
        .transaction(move |transaction| {
            let sql = sql.clone();
            async move {
                let version = match read_storage_version(&sql)? {
                    Some(version) => version,
                    None => {
                        let storage = classify_unversioned_storage(&sql, &transaction).await?;
                        establish_baseline(&sql, storage)?
                    }
                };
                apply_migrations(&sql, version, target, migrations)?;
                transaction.delete(LEGACY_VERSION_KEY).await?;
                Ok(())
            }
        })
        .await?;

    read_storage_version(&storage.sql())?
        .ok_or_else(|| migration_error("Migration completed without a storage version"))
}

#[tracing::instrument(skip(storage))]
pub async fn migrate_storage_to_current(storage: Storage) -> Result<StorageVersion> {
    let version = migrate_storage(storage, CURRENT_STORAGE_VERSION, MIGRATIONS).await?;
    info!(event = "storage-current", version = version.0);
    Ok(version)
}

fn table_exists(sql: &SqlStorage, table: &str) -> Result<bool> {
    let row: CountRow = sql
        .exec(
            &format!(
                "SELECT count(*) AS count
                 FROM sqlite_schema
                 WHERE type = 'table' AND name = '{}'",
                table.replace('\'', "''")
            ),
            None,
        )?
        .one()?;
    Ok(row.count == 1)
}

pub async fn test_empty_storage_baseline(state: &State) -> Result<()> {
    let storage = state.storage();
    storage.delete_all().await?;

    let version = migrate_storage_to_current(storage).await?;
    if version != BASELINE_STORAGE_VERSION {
        return Err(migration_error("Fresh storage did not reach the baseline"));
    }
    let storage = state.storage();
    if storage.get::<u32>(LEGACY_VERSION_KEY).await?.is_some() {
        return Err(migration_error("Legacy version key was not removed"));
    }
    if read_storage_version(&storage.sql())? != Some(BASELINE_STORAGE_VERSION) {
        return Err(migration_error("SQL storage version was not recorded"));
    }
    Ok(())
}

pub async fn test_production_schema_adoption(state: &State) -> Result<()> {
    let storage = state.storage();
    storage.delete_all().await?;
    let sql = storage.sql();
    initialize_sqlite_tables(&sql)?;
    sql.exec(
        "INSERT INTO invitations (id) VALUES ('preserved-invitation')",
        None,
    )?;
    storage.put(LEGACY_VERSION_KEY, 1_u32).await?;

    migrate_storage_to_current(storage).await?;
    let storage = state.storage();
    let preserved: CountRow = storage
        .sql()
        .exec(
            "SELECT count(*) AS count
             FROM invitations
             WHERE id = 'preserved-invitation'",
            None,
        )?
        .one()?;
    if preserved.count != 1 {
        return Err(migration_error(
            "Baseline adoption did not preserve existing data",
        ));
    }
    if storage.get::<u32>(LEGACY_VERSION_KEY).await?.is_some() {
        return Err(migration_error("Legacy version key was not removed"));
    }
    Ok(())
}

pub async fn test_untrusted_unversioned_storage_rejected(state: &State) -> Result<()> {
    let storage = state.storage();
    storage.delete_all().await?;
    storage
        .sql()
        .exec("CREATE TABLE unexpected (value TEXT)", None)?;

    if migrate_storage_to_current(storage).await.is_ok() {
        return Err(migration_error("Unmarked SQL storage was accepted"));
    }

    let storage = state.storage();
    if read_storage_version(&storage.sql())?.is_some() {
        return Err(migration_error(
            "Rejected schema was assigned a storage version",
        ));
    }
    if !table_exists(&storage.sql(), "unexpected")? {
        return Err(migration_error("Rejected schema was modified"));
    }

    storage.delete_all().await?;
    storage.put(LEGACY_VERSION_KEY, 0_u32).await?;
    if migrate_storage_to_current(storage).await.is_ok() {
        return Err(migration_error(
            "Unsupported legacy storage version was accepted",
        ));
    }
    let storage = state.storage();
    if storage.get::<u32>(LEGACY_VERSION_KEY).await? != Some(0)
        || read_storage_version(&storage.sql())?.is_some()
    {
        return Err(migration_error(
            "Rejected legacy storage version was modified",
        ));
    }

    storage.delete_all().await?;
    storage.put("unexpected-kv", "preserved").await?;
    if migrate_storage_to_current(storage).await.is_ok() {
        return Err(migration_error("Unknown unversioned KV was accepted"));
    }
    let storage = state.storage();
    if storage.get::<String>("unexpected-kv").await?.as_deref() != Some("preserved")
        || read_storage_version(&storage.sql())?.is_some()
    {
        return Err(migration_error("Rejected KV storage was modified"));
    }
    Ok(())
}

fn test_migration_two(sql: &SqlStorage) -> Result<()> {
    sql.exec(
        "CREATE TABLE migration_test_two (value INTEGER NOT NULL)",
        None,
    )?;
    Ok(())
}

fn test_migration_three(sql: &SqlStorage) -> Result<()> {
    sql.exec(
        "CREATE TABLE migration_test_three (value INTEGER NOT NULL)",
        None,
    )?;
    Ok(())
}

fn test_migration_three_failure(sql: &SqlStorage) -> Result<()> {
    test_migration_three(sql)?;
    Err(migration_error("Intentional migration failure"))
}

const TEST_MIGRATIONS_TO_TWO: &[Migration] = &[Migration {
    version: StorageVersion(2),
    run: test_migration_two,
}];

const TEST_MIGRATIONS_SUCCESS: &[Migration] = &[
    Migration {
        version: StorageVersion(2),
        run: test_migration_two,
    },
    Migration {
        version: StorageVersion(3),
        run: test_migration_three,
    },
];

const TEST_MIGRATIONS_FAILURE: &[Migration] = &[
    Migration {
        version: StorageVersion(2),
        run: test_migration_two,
    },
    Migration {
        version: StorageVersion(3),
        run: test_migration_three_failure,
    },
];

pub async fn test_migration_chain_and_rollback(state: &State) -> Result<()> {
    state.storage().delete_all().await?;
    migrate_storage_to_current(state.storage()).await?;

    if migrate_storage(state.storage(), StorageVersion(3), TEST_MIGRATIONS_FAILURE)
        .await
        .is_ok()
    {
        return Err(migration_error("Failing migration chain succeeded"));
    }
    let sql = state.storage().sql();
    if read_storage_version(&sql)? != Some(BASELINE_STORAGE_VERSION)
        || table_exists(&sql, "migration_test_two")?
        || table_exists(&sql, "migration_test_three")?
    {
        return Err(migration_error("Failed migration chain was not atomic"));
    }

    let version =
        migrate_storage(state.storage(), StorageVersion(2), TEST_MIGRATIONS_TO_TWO).await?;
    if version != StorageVersion(2) || !table_exists(&state.storage().sql(), "migration_test_two")?
    {
        return Err(migration_error("Immediate migration to version 2 failed"));
    }

    let version =
        migrate_storage(state.storage(), StorageVersion(3), TEST_MIGRATIONS_SUCCESS).await?;
    if version != StorageVersion(3)
        || !table_exists(&state.storage().sql(), "migration_test_three")?
    {
        return Err(migration_error("Retry from version 2 failed"));
    }

    state.storage().delete_all().await?;
    migrate_storage_to_current(state.storage()).await?;
    let version =
        migrate_storage(state.storage(), StorageVersion(3), TEST_MIGRATIONS_SUCCESS).await?;
    if version != StorageVersion(3)
        || !table_exists(&state.storage().sql(), "migration_test_two")?
        || !table_exists(&state.storage().sql(), "migration_test_three")?
    {
        return Err(migration_error("Skipped-version migration failed"));
    }

    Ok(())
}
