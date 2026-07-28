use serde::Deserialize;
use tracing::info;
use worker::{wasm_bindgen::JsValue, Error, Result, SqlStorage, State, Storage, Transaction};

use crate::sqlite::initialize_sqlite_tables;

mod catalog_domain;

const LEGACY_VERSION_KEY: &str = "DURABLEGAME_VERSION";
const METADATA_TABLE: &str = "arpeggio_storage_metadata";
const STORAGE_VERSION_KEY: &str = "storage_version";

pub const BASELINE_STORAGE_VERSION: StorageVersion = StorageVersion(1);
pub const CURRENT_STORAGE_VERSION: StorageVersion = StorageVersion(2);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct StorageVersion(pub u32);

#[derive(Clone, Copy)]
struct Migration {
    version: StorageVersion,
    run: fn(&SqlStorage) -> Result<()>,
}

const CATALOG_DOMAIN_MIGRATION: Migration = Migration {
    version: StorageVersion(2),
    run: catalog_domain::migrate_catalog_domain,
};

const MIGRATIONS: &[Migration] = &[CATALOG_DOMAIN_MIGRATION];

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
    if version != CURRENT_STORAGE_VERSION {
        return Err(migration_error(
            "Fresh storage did not reach the current version",
        ));
    }
    let storage = state.storage();
    if storage.get::<u32>(LEGACY_VERSION_KEY).await?.is_some() {
        return Err(migration_error("Legacy version key was not removed"));
    }
    if read_storage_version(&storage.sql())? != Some(CURRENT_STORAGE_VERSION) {
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

pub async fn test_catalog_domain_migration(state: &State) -> Result<()> {
    let storage = state.storage();
    storage.delete_all().await?;
    let sql = storage.sql();
    initialize_sqlite_tables(&sql)?;
    initialize_metadata(&sql, BASELINE_STORAGE_VERSION)?;

    let mut legacy_json = serde_json::to_value(arptypes::Game::default())
        .map_err(|error| migration_error(format!("Could not serialize test game: {error}")))?;
    let game_object = legacy_json
        .as_object_mut()
        .ok_or_else(|| migration_error("Serialized test game was not an object"))?;
    game_object.remove("notes");
    game_object.remove("collections");
    let empty_folder = || {
        serde_json::json!({
            "scenes": [],
            "creatures": [],
            "notes": {},
            "items": [],
            "abilities": [],
            "classes": []
        })
    };
    game_object.insert(
        "campaign".to_string(),
        serde_json::json!({
            "data": empty_folder(),
            "children": {
                "Players": {
                    "data": empty_folder(),
                    "children": {
                        "alice": {
                            "data": empty_folder(),
                            "children": {
                                "Notes": {
                                    "data": {
                                        "scenes": [],
                                        "creatures": [],
                                        "notes": {
                                            "Scratch": {
                                                "name": "Scratch",
                                                "content": "Migrated content"
                                            }
                                        },
                                        "items": [],
                                        "abilities": [],
                                        "classes": []
                                    },
                                    "children": {}
                                }
                            }
                        }
                    }
                }
            }
        }),
    );
    let legacy_json = serde_json::to_string(&legacy_json)
        .map_err(|error| migration_error(format!("Could not encode test game: {error}")))?;
    sql.exec(
        "INSERT INTO game_snapshots (snapshot_idx, game) VALUES (0, jsonb(?))",
        Some(vec![legacy_json.into()]),
    )?;
    let legacy_log = serde_json::json!({
        "t": "EditNote",
        "path": "/Players/alice/Notes",
        "original_name": "Scratch",
        "note": {
            "name": "Journal",
            "content": "Updated after the snapshot"
        }
    });
    sql.exec(
        "INSERT INTO logs (snapshot_idx, log_idx, game_log)
         VALUES (0, 0, jsonb(?))",
        Some(vec![legacy_log.to_string().into()]),
    )?;
    let migrated_item_id = arptypes::ItemID::gen();
    let legacy_create_item = serde_json::json!({
        "t": "CreateItem",
        "path": "/Players/alice/Notes",
        "item": {
            "id": migrated_item_id,
            "name": "Migrated item"
        }
    });
    sql.exec(
        "INSERT INTO logs (snapshot_idx, log_idx, game_log)
         VALUES (0, 1, jsonb(?))",
        Some(vec![legacy_create_item.to_string().into()]),
    )?;

    let version = migrate_storage_to_current(storage).await?;
    if version != CURRENT_STORAGE_VERSION {
        return Err(migration_error(
            "Catalog migration did not advance storage version",
        ));
    }

    #[derive(Deserialize)]
    struct MigratedSnapshot {
        game: String,
    }
    let snapshot: MigratedSnapshot = state
        .storage()
        .sql()
        .exec(
            "SELECT json(game) AS game FROM game_snapshots WHERE snapshot_idx = 0",
            None,
        )?
        .one()?;
    let migrated_game: arptypes::Game = serde_json::from_str(&snapshot.game).map_err(|error| {
        migration_error(format!("Could not decode migrated test game: {error}"))
    })?;

    let note = migrated_game
        .notes
        .values()
        .next()
        .ok_or_else(|| migration_error("Catalog migration did not create a top-level note"))?;
    if note.owner != arptypes::NoteOwner::Player(arptypes::PlayerID("alice".to_string()))
        || note.visibility != arptypes::NoteVisibility::OwnerOnly
    {
        return Err(migration_error(
            "Catalog migration did not preserve player-note authorization",
        ));
    }
    let collection = migrated_game
        .collections
        .values()
        .next()
        .ok_or_else(|| migration_error("Catalog migration did not create a collection"))?;
    if collection.name != "/Players/alice/Notes" || collection.notes != vec![note.id] {
        return Err(migration_error(
            "Catalog migration created incorrect collection membership",
        ));
    }

    #[derive(Deserialize)]
    struct MigratedLog {
        game_log: String,
    }
    let migrated_logs: Vec<MigratedLog> = state
        .storage()
        .sql()
        .exec(
            "SELECT json(game_log) AS game_log
             FROM logs
             WHERE snapshot_idx = 0
             ORDER BY log_idx",
            None,
        )?
        .to_array()?;
    if migrated_logs.len() != 3 {
        return Err(migration_error(format!(
            "Expected three expanded migrated logs, found {}",
            migrated_logs.len()
        )));
    }
    let mut replayed = migrated_game;
    for migrated_log in migrated_logs {
        let raw: serde_json::Value =
            serde_json::from_str(&migrated_log.game_log).map_err(|error| {
                migration_error(format!("Could not inspect migrated test log: {error}"))
            })?;
        if raw.get("path").is_some()
            || raw
                .get("t")
                .and_then(serde_json::Value::as_str)
                .is_some_and(|tag| {
                    matches!(
                        tag,
                        "CreateFolder"
                            | "RenameFolder"
                            | "MoveFolderItem"
                            | "CopyFolderItem"
                            | "DeleteFolderItem"
                            | "RenameFolderItem"
                            | "LoadModule"
                    )
                })
        {
            return Err(migration_error(
                "Migrated log still contains a legacy folder field or variant",
            ));
        }
        let log: arptypes::GameLog = serde_json::from_value(raw).map_err(|error| {
            migration_error(format!("Could not decode migrated test log: {error}"))
        })?;
        replayed = arpeggio::game::GameExt::apply_log(&replayed, &log)
            .map_err(|error| migration_error(format!("Could not replay migrated log: {error}")))?;
    }
    let replayed_note = replayed
        .notes
        .values()
        .next()
        .ok_or_else(|| migration_error("Replayed game lost the migrated note"))?;
    if replayed_note.name != "Journal" || replayed_note.content != "Updated after the snapshot" {
        return Err(migration_error(
            "Migrated note log did not reproduce the legacy update",
        ));
    }
    if replayed
        .items
        .get(&migrated_item_id)
        .is_none_or(|item| item.name != "Migrated item")
        || replayed
            .collections
            .values()
            .all(|collection| !collection.items.contains(&migrated_item_id))
    {
        return Err(migration_error(
            "Migrated path-bearing resource log did not replay with collection membership",
        ));
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

const TEST_MIGRATIONS_TO_THREE: &[Migration] = &[
    CATALOG_DOMAIN_MIGRATION,
    Migration {
        version: StorageVersion(3),
        run: test_migration_two,
    },
];

const TEST_MIGRATIONS_SUCCESS: &[Migration] = &[
    CATALOG_DOMAIN_MIGRATION,
    Migration {
        version: StorageVersion(3),
        run: test_migration_two,
    },
    Migration {
        version: StorageVersion(4),
        run: test_migration_three,
    },
];

const TEST_MIGRATIONS_FAILURE: &[Migration] = &[
    CATALOG_DOMAIN_MIGRATION,
    Migration {
        version: StorageVersion(3),
        run: test_migration_two,
    },
    Migration {
        version: StorageVersion(4),
        run: test_migration_three_failure,
    },
];

pub async fn test_migration_chain_and_rollback(state: &State) -> Result<()> {
    state.storage().delete_all().await?;
    migrate_storage_to_current(state.storage()).await?;

    if migrate_storage(state.storage(), StorageVersion(4), TEST_MIGRATIONS_FAILURE)
        .await
        .is_ok()
    {
        return Err(migration_error("Failing migration chain succeeded"));
    }
    let sql = state.storage().sql();
    if read_storage_version(&sql)? != Some(CURRENT_STORAGE_VERSION)
        || table_exists(&sql, "migration_test_two")?
        || table_exists(&sql, "migration_test_three")?
    {
        return Err(migration_error("Failed migration chain was not atomic"));
    }

    let version =
        migrate_storage(state.storage(), StorageVersion(3), TEST_MIGRATIONS_TO_THREE).await?;
    if version != StorageVersion(3) || !table_exists(&state.storage().sql(), "migration_test_two")?
    {
        return Err(migration_error("Immediate migration to version 3 failed"));
    }

    let version =
        migrate_storage(state.storage(), StorageVersion(4), TEST_MIGRATIONS_SUCCESS).await?;
    if version != StorageVersion(4)
        || !table_exists(&state.storage().sql(), "migration_test_three")?
    {
        return Err(migration_error("Retry from version 3 failed"));
    }

    state.storage().delete_all().await?;
    migrate_storage_to_current(state.storage()).await?;
    let version =
        migrate_storage(state.storage(), StorageVersion(4), TEST_MIGRATIONS_SUCCESS).await?;
    if version != StorageVersion(4)
        || !table_exists(&state.storage().sql(), "migration_test_two")?
        || !table_exists(&state.storage().sql(), "migration_test_three")?
    {
        return Err(migration_error("Skipped-version migration failed"));
    }

    Ok(())
}
