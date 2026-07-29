use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use arpeggio::game::GameExt;
use arptypes::{
    AbilityID, ClassID, Collection, CollectionID, CreatureID, Game, GameLog, ItemID, Note, NoteID,
    NoteOwner, NoteVisibility, PlayerID, ResourceRef, SceneID,
};
use foldertree::{FolderPath, FolderTree};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use uuid::Uuid;
use worker::{Error, SqlStorage};

use crate::entity_storage;

const MIGRATION_NAMESPACE: Uuid = Uuid::from_bytes([
    0x4e, 0x7f, 0x0e, 0xf4, 0x4b, 0x64, 0x4e, 0x1d, 0x9a, 0x6f, 0x75, 0x4a, 0x11, 0x9f, 0x86, 0x7b,
]);

#[derive(Clone, Debug, Deserialize, Serialize)]
struct LegacyNote {
    name: String,
    content: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct LegacyFolder {
    scenes: HashSet<SceneID>,
    creatures: HashSet<CreatureID>,
    notes: HashMap<String, LegacyNote>,
    #[serde(default)]
    items: HashSet<ItemID>,
    #[serde(default)]
    abilities: HashSet<AbilityID>,
    #[serde(default)]
    classes: HashSet<ClassID>,
}

#[derive(Clone, Debug)]
struct TrackedNote {
    id: NoteID,
    name: String,
    content: String,
}

#[derive(Clone, Debug, Default)]
struct TrackedFolder {
    collection_id: Option<CollectionID>,
    scenes: HashSet<SceneID>,
    creatures: HashSet<CreatureID>,
    notes: HashMap<String, TrackedNote>,
    items: HashSet<ItemID>,
    abilities: HashSet<AbilityID>,
    classes: HashSet<ClassID>,
}

impl TrackedFolder {
    fn is_empty(&self) -> bool {
        self.scenes.is_empty()
            && self.creatures.is_empty()
            && self.notes.is_empty()
            && self.items.is_empty()
            && self.abilities.is_empty()
            && self.classes.is_empty()
    }
}

#[derive(Clone, Debug, Deserialize)]
enum LegacyFolderItemID {
    SceneID(SceneID),
    CreatureID(CreatureID),
    NoteID(String),
    ItemID(ItemID),
    AbilityID(AbilityID),
    ClassID(ClassID),
    SubfolderID(String),
}

#[derive(Clone)]
struct LegacyCatalog {
    tree: FolderTree<TrackedFolder>,
}

fn migration_error(message: impl Into<String>) -> String {
    message.into()
}

fn storage_migration_error(message: impl Into<String>) -> Error {
    Error::RustError(message.into())
}

#[derive(Deserialize)]
struct SnapshotRow {
    snapshot_idx: i64,
    game: String,
}

#[derive(Deserialize)]
struct LogRow {
    snapshot_idx: i64,
    log_idx: i64,
    game_log: String,
}

pub(super) fn migrate_catalog_domain(sql: &SqlStorage) -> worker::Result<()> {
    let snapshots: Vec<SnapshotRow> = sql
        .exec(
            "SELECT snapshot_idx, json(game) AS game
             FROM game_snapshots
             ORDER BY snapshot_idx",
            None,
        )?
        .to_array()?;

    let logs: Vec<LogRow> = sql
        .exec(
            "SELECT snapshot_idx, log_idx, json(game_log) AS game_log
             FROM logs
             ORDER BY snapshot_idx, log_idx",
            None,
        )?
        .to_array()?;
    let mut logs_by_snapshot: BTreeMap<i64, Vec<LogRow>> = BTreeMap::new();
    for log in logs {
        logs_by_snapshot
            .entry(log.snapshot_idx)
            .or_default()
            .push(log);
    }

    let mut migrated_snapshots = Vec::new();
    let mut migrated_logs = Vec::new();
    for snapshot in snapshots {
        let (game_json, mut catalog) = parse_snapshot_json(&snapshot.game).map_err(|error| {
            storage_migration_error(format!(
                "Could not migrate game snapshot {}: {error}",
                snapshot.snapshot_idx
            ))
        })?;
        snapshot_has_no_legacy_fields(&game_json).map_err(|error| {
            storage_migration_error(format!(
                "Game snapshot {} failed post-migration validation: {error}",
                snapshot.snapshot_idx
            ))
        })?;
        migrated_snapshots.push((snapshot.snapshot_idx, game_json));

        let mut new_log_idx = 0i64;
        for log in logs_by_snapshot
            .remove(&snapshot.snapshot_idx)
            .unwrap_or_default()
        {
            let value = parse_log_json(&log.game_log).map_err(|error| {
                storage_migration_error(format!(
                    "Could not parse log {}:{}: {error}",
                    log.snapshot_idx, log.log_idx
                ))
            })?;
            let translated = catalog.translate_log(value).map_err(|error| {
                storage_migration_error(format!(
                    "Could not migrate log {}:{}: {error}",
                    log.snapshot_idx, log.log_idx
                ))
            })?;
            for migrated in translated {
                let json = log_to_json(&migrated).map_err(storage_migration_error)?;
                migrated_logs.push((snapshot.snapshot_idx, new_log_idx, json));
                new_log_idx += 1;
            }
        }
    }

    if let Some((snapshot_idx, _)) = logs_by_snapshot.first_key_value() {
        return Err(storage_migration_error(format!(
            "Logs exist for snapshot {snapshot_idx}, but that snapshot is missing"
        )));
    }

    if migrated_snapshots.is_empty() {
        let game_json = serde_json::to_string(&Game::default()).map_err(|error| {
            storage_migration_error(format!("Could not encode default game snapshot: {error}"))
        })?;
        migrated_snapshots.push((0, game_json));
    }

    // Finish converting all legacy history before constructing any current-domain Game.
    sql.exec("DELETE FROM logs", None)?;
    for (snapshot_idx, log_idx, game_log) in &migrated_logs {
        sql.exec(
            "INSERT INTO logs (snapshot_idx, log_idx, game_log)
             VALUES (?, ?, jsonb(?))",
            Some(vec![
                (*snapshot_idx).into(),
                (*log_idx).into(),
                game_log.as_str().into(),
            ]),
        )?;
    }

    entity_storage::initialize_tables(sql).map_err(|error| {
        storage_migration_error(format!(
            "Could not create snapshot-indexed typed tables: {error}"
        ))
    })?;

    let mut current_game = None;
    for (snapshot_idx, game_json) in &migrated_snapshots {
        let snapshot_idx_usize = usize::try_from(*snapshot_idx).map_err(|_| {
            storage_migration_error(format!("Invalid negative snapshot index {snapshot_idx}"))
        })?;
        let mut game: Game = serde_json::from_str(game_json).map_err(|error| {
            storage_migration_error(format!(
                "Could not decode migrated game snapshot {snapshot_idx}: {error}"
            ))
        })?;
        entity_storage::replace_game_at_snapshot(sql, *snapshot_idx, &game).map_err(|error| {
            storage_migration_error(format!(
                "Could not store typed game snapshot {snapshot_idx}: {error}"
            ))
        })?;
        entity_storage::record_snapshot(sql, snapshot_idx_usize, "migration").map_err(|error| {
            storage_migration_error(format!(
                "Could not record typed game snapshot {snapshot_idx}: {error}"
            ))
        })?;

        let snapshot_logs: Vec<LogRow> = sql
            .exec(
                "SELECT snapshot_idx, log_idx, json(game_log) AS game_log
                 FROM logs
                 WHERE snapshot_idx = ?
                 ORDER BY log_idx",
                Some(vec![(*snapshot_idx).into()]),
            )?
            .to_array()?;
        for log in snapshot_logs {
            let game_log: GameLog = serde_json::from_str(&log.game_log).map_err(|error| {
                storage_migration_error(format!(
                    "Could not decode migrated log {}:{}: {error}",
                    log.snapshot_idx, log.log_idx
                ))
            })?;
            game = game.apply_log(&game_log).map_err(|error| {
                storage_migration_error(format!(
                    "Could not replay migrated log {}:{}: {error}",
                    log.snapshot_idx, log.log_idx
                ))
            })?;
        }
        current_game = Some(game);
    }

    let current_game = current_game
        .ok_or_else(|| storage_migration_error("Migration produced no current game"))?;
    entity_storage::replace_game(sql, &current_game).map_err(|error| {
        storage_migration_error(format!("Could not store migrated current game: {error}"))
    })?;

    sql.exec("DROP TABLE game_snapshots", None)?;

    Ok(())
}

fn stable_collection_id(path: &FolderPath) -> CollectionID {
    CollectionID(Uuid::new_v5(
        &MIGRATION_NAMESPACE,
        format!("collection:{path}").as_bytes(),
    ))
}

fn stable_note_id(path: &FolderPath, name: &str) -> NoteID {
    NoteID(Uuid::new_v5(
        &MIGRATION_NAMESPACE,
        format!("note:{path}\0{name}").as_bytes(),
    ))
}

fn note_access(path: &FolderPath) -> (NoteOwner, NoteVisibility) {
    let segments = path.clone().into_vec();
    if segments.first().is_some_and(|segment| segment == "Players") {
        if let Some(player_id) = segments.get(1) {
            return (
                NoteOwner::Player(PlayerID(player_id.clone())),
                NoteVisibility::OwnerOnly,
            );
        }
    }
    (NoteOwner::Game, NoteVisibility::GMOnly)
}

fn parse_path(value: &Value, field: &str) -> Result<FolderPath, String> {
    serde_json::from_value(
        value
            .get(field)
            .cloned()
            .ok_or_else(|| migration_error(format!("Legacy log is missing {field}")))?,
    )
    .map_err(|error| migration_error(format!("Invalid legacy {field}: {error}")))
}

fn parse_field<T: for<'de> Deserialize<'de>>(value: &Value, field: &str) -> Result<T, String> {
    serde_json::from_value(
        value
            .get(field)
            .cloned()
            .ok_or_else(|| migration_error(format!("Legacy log is missing {field}")))?,
    )
    .map_err(|error| migration_error(format!("Invalid legacy {field}: {error}")))
}

fn legacy_tree_to_tracked(
    legacy: FolderTree<LegacyFolder>,
) -> Result<FolderTree<TrackedFolder>, String> {
    fn convert(path: &FolderPath, folder: &LegacyFolder) -> TrackedFolder {
        TrackedFolder {
            collection_id: (!folder.scenes.is_empty()
                || !folder.creatures.is_empty()
                || !folder.notes.is_empty()
                || !folder.items.is_empty()
                || !folder.abilities.is_empty()
                || !folder.classes.is_empty())
            .then(|| stable_collection_id(path)),
            scenes: folder.scenes.clone(),
            creatures: folder.creatures.clone(),
            notes: folder
                .notes
                .values()
                .map(|note| {
                    (
                        note.name.clone(),
                        TrackedNote {
                            id: stable_note_id(path, &note.name),
                            name: note.name.clone(),
                            content: note.content.clone(),
                        },
                    )
                })
                .collect(),
            items: folder.items.clone(),
            abilities: folder.abilities.clone(),
            classes: folder.classes.clone(),
        }
    }

    let root = FolderPath::root();
    let mut tracked = FolderTree::new(convert(
        &root,
        legacy
            .get(&root)
            .map_err(|error| migration_error(error.to_string()))?,
    ));
    for path in legacy.walk_paths(&root) {
        if path.is_root() {
            continue;
        }
        let (parent, name) = path
            .up()
            .ok_or_else(|| migration_error("Non-root legacy path has no parent"))?;
        tracked
            .make_folder(
                &parent,
                name,
                convert(
                    path,
                    legacy
                        .get(path)
                        .map_err(|error| migration_error(error.to_string()))?,
                ),
            )
            .map_err(|error| migration_error(error.to_string()))?;
    }
    Ok(tracked)
}

fn sorted_ids<T: Copy + Ord>(values: &HashSet<T>) -> Vec<T> {
    let mut values: Vec<_> = values.iter().copied().collect();
    values.sort();
    values
}

impl LegacyCatalog {
    fn migrate_snapshot(mut game: Value) -> Result<(Value, Self), String> {
        let object = game
            .as_object_mut()
            .ok_or_else(|| migration_error("Game snapshot must be a JSON object"))?;
        let campaign = object
            .remove("campaign")
            .ok_or_else(|| migration_error("Legacy game snapshot is missing campaign"))?;
        let legacy: FolderTree<LegacyFolder> = serde_json::from_value(campaign)
            .map_err(|error| migration_error(format!("Invalid legacy campaign: {error}")))?;
        let mut catalog = Self {
            tree: legacy_tree_to_tracked(legacy)?,
        };
        catalog.ensure_collection_ids()?;
        let (notes, collections) = catalog.materialize()?;
        validate_legacy_membership(object, &collections)?;
        object.insert(
            "notes".to_string(),
            serde_json::to_value(notes)
                .map_err(|error| migration_error(format!("Could not serialize notes: {error}")))?,
        );
        object.insert(
            "collections".to_string(),
            serde_json::to_value(collections).map_err(|error| {
                migration_error(format!("Could not serialize collections: {error}"))
            })?,
        );
        Ok((game, catalog))
    }

    fn ensure_collection_ids(&mut self) -> Result<(), String> {
        let root = FolderPath::root();
        let paths = self.tree.walk_paths(&root).cloned().collect::<Vec<_>>();
        for path in paths {
            let folder = self
                .tree
                .get_mut(&path)
                .map_err(|error| migration_error(error.to_string()))?;
            if !folder.is_empty() && folder.collection_id.is_none() {
                folder.collection_id = Some(stable_collection_id(&path));
            }
        }
        Ok(())
    }

    fn materialize(
        &self,
    ) -> Result<(BTreeMap<NoteID, Note>, BTreeMap<CollectionID, Collection>), String> {
        let root = FolderPath::root();
        let mut notes = BTreeMap::new();
        let mut collections = BTreeMap::new();
        for path in self.tree.walk_paths(&root) {
            let folder = self
                .tree
                .get(path)
                .map_err(|error| migration_error(error.to_string()))?;
            if folder.is_empty() {
                continue;
            }
            let (owner, visibility) = note_access(path);
            let mut folder_notes: Vec<_> = folder.notes.values().collect();
            folder_notes.sort_by(|left, right| {
                left.name
                    .to_lowercase()
                    .cmp(&right.name.to_lowercase())
                    .then_with(|| left.id.cmp(&right.id))
            });
            let note_ids = folder_notes
                .into_iter()
                .map(|tracked| {
                    notes.insert(
                        tracked.id,
                        Note {
                            id: tracked.id,
                            name: tracked.name.clone(),
                            content: tracked.content.clone(),
                            owner: owner.clone(),
                            visibility: visibility.clone(),
                        },
                    );
                    tracked.id
                })
                .collect();
            let id = folder
                .collection_id
                .ok_or_else(|| migration_error(format!("Non-empty folder {path} has no ID")))?;
            collections.insert(
                id,
                Collection {
                    id,
                    name: if path.is_root() {
                        "/".to_string()
                    } else {
                        path.to_string()
                    },
                    scenes: sorted_ids(&folder.scenes),
                    creatures: sorted_ids(&folder.creatures),
                    notes: note_ids,
                    items: sorted_ids(&folder.items),
                    abilities: sorted_ids(&folder.abilities),
                    classes: sorted_ids(&folder.classes),
                },
            );
        }
        Ok((notes, collections))
    }

    fn reconcile(&mut self, before: &LegacyCatalog) -> Result<Vec<GameLog>, String> {
        self.ensure_collection_ids()?;
        let (before_notes, before_collections) = before.materialize()?;
        let (after_notes, after_collections) = self.materialize()?;
        let mut logs = Vec::new();

        for id in before_notes.keys() {
            if !after_notes.contains_key(id) {
                logs.push(GameLog::DeleteResource {
                    resource: ResourceRef::Note(*id),
                });
            }
        }
        for (id, note) in &after_notes {
            match before_notes.get(id) {
                None => logs.push(GameLog::CreateNote { note: note.clone() }),
                Some(previous) if previous != note => {
                    logs.push(GameLog::EditNote { note: note.clone() })
                }
                _ => {}
            }
        }
        for id in before_collections.keys() {
            if !after_collections.contains_key(id) {
                logs.push(GameLog::DeleteCollection { collection_id: *id });
            }
        }
        for (id, collection) in &after_collections {
            match before_collections.get(id) {
                None => logs.push(GameLog::CreateCollection {
                    collection: collection.clone(),
                }),
                Some(previous) if previous != collection => logs.push(GameLog::EditCollection {
                    collection: collection.clone(),
                }),
                _ => {}
            }
        }
        Ok(logs)
    }

    fn with_reconcile(
        &mut self,
        mutate: impl FnOnce(&mut FolderTree<TrackedFolder>) -> Result<(), String>,
    ) -> Result<Vec<GameLog>, String> {
        let before = self.clone();
        mutate(&mut self.tree)?;
        self.reconcile(&before)
    }

    fn remove_resource_everywhere(&mut self, resource: ResourceRef) -> Result<(), String> {
        let root = FolderPath::root();
        let paths = self.tree.walk_paths(&root).cloned().collect::<Vec<_>>();
        for path in paths {
            let folder = self
                .tree
                .get_mut(&path)
                .map_err(|error| migration_error(error.to_string()))?;
            match resource {
                ResourceRef::Scene(id) => {
                    folder.scenes.remove(&id);
                }
                ResourceRef::Creature(id) => {
                    folder.creatures.remove(&id);
                }
                ResourceRef::Note(id) => {
                    folder.notes.retain(|_, note| note.id != id);
                }
                ResourceRef::Item(id) => {
                    folder.items.remove(&id);
                }
                ResourceRef::Ability(id) => {
                    folder.abilities.remove(&id);
                }
                ResourceRef::Class(id) => {
                    folder.classes.remove(&id);
                }
            }
        }
        Ok(())
    }

    fn translate_log(&mut self, mut value: Value) -> Result<Vec<GameLog>, String> {
        let tag = value
            .get("t")
            .and_then(Value::as_str)
            .ok_or_else(|| migration_error("Legacy log is missing string tag t"))?
            .to_string();

        match tag.as_str() {
            "LoadModule" => Err(migration_error(
                "Legacy LoadModule logs are unsupported; no production module imports are known",
            )),
            // The old rollback log was informational and was never replayable by Game. Historical
            // snapshots already contain the resulting state, so it has no current-domain log.
            "Rollback" => Ok(vec![]),
            "CreateFolder" => {
                let path = parse_path(&value, "path")?;
                self.with_reconcile(|tree| {
                    tree.make_folders(&path, TrackedFolder::default());
                    Ok(())
                })
            }
            "RenameFolder" => {
                let path = parse_path(&value, "path")?;
                let new_name: String = parse_field(&value, "new_name")?;
                self.with_reconcile(|tree| {
                    tree.rename_folder(&path, new_name)
                        .map_err(|error| migration_error(error.to_string()))
                })
            }
            "MoveFolderItem" => self.translate_move(value),
            "CopyFolderItem" => self.translate_copy(value),
            "DeleteFolderItem" => self.translate_delete(value),
            "RenameFolderItem" => self.translate_rename(value),
            "CreateNote" => self.translate_create_note(value),
            "EditNote" => self.translate_edit_note(value),
            "CreateItem" | "CreateScene" | "CreateClass" | "CreateAbility" | "CreateCreature" => {
                let path = parse_path(&value, "path")?;
                value
                    .as_object_mut()
                    .ok_or_else(|| migration_error("Legacy log must be an object"))?
                    .remove("path");
                let current: GameLog = serde_json::from_value(value).map_err(|error| {
                    migration_error(format!("Could not convert legacy {tag}: {error}"))
                })?;
                let resource = created_resource(&current)
                    .ok_or_else(|| migration_error(format!("{tag} did not create a resource")))?;
                let mut logs = vec![current];
                let mut catalog_logs = self.with_reconcile(|tree| {
                    let folder = tree
                        .get_mut(&path)
                        .map_err(|error| migration_error(error.to_string()))?;
                    add_resource(folder, resource);
                    Ok(())
                })?;
                logs.append(&mut catalog_logs);
                Ok(logs)
            }
            _ => serde_json::from_value(value)
                .map(|log| vec![log])
                .map_err(|error| {
                    migration_error(format!("Could not convert legacy {tag}: {error}"))
                }),
        }
    }

    fn translate_create_note(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let path = parse_path(&value, "path")?;
        let note: LegacyNote = parse_field(&value, "note")?;
        self.with_reconcile(|tree| {
            let folder = tree
                .get_mut(&path)
                .map_err(|error| migration_error(error.to_string()))?;
            folder.notes.insert(
                note.name.clone(),
                TrackedNote {
                    id: stable_note_id(&path, &note.name),
                    name: note.name,
                    content: note.content,
                },
            );
            Ok(())
        })
    }

    fn translate_edit_note(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let path = parse_path(&value, "path")?;
        let original_name: String = parse_field(&value, "original_name")?;
        let note: LegacyNote = parse_field(&value, "note")?;
        self.with_reconcile(|tree| {
            let folder = tree
                .get_mut(&path)
                .map_err(|error| migration_error(error.to_string()))?;
            let mut tracked = folder.notes.remove(&original_name).ok_or_else(|| {
                migration_error(format!("Legacy note {path}/{original_name} was not found"))
            })?;
            tracked.name = note.name.clone();
            tracked.content = note.content;
            folder.notes.insert(note.name, tracked);
            Ok(())
        })
    }

    fn translate_move(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let source = parse_path(&value, "source")?;
        let destination = parse_path(&value, "destination")?;
        let item: LegacyFolderItemID = parse_field(&value, "item_id")?;
        self.with_reconcile(|tree| match item {
            LegacyFolderItemID::NoteID(name) => {
                let note = tree
                    .get_mut(&source)
                    .map_err(|error| migration_error(error.to_string()))?
                    .notes
                    .remove(&name)
                    .ok_or_else(|| {
                        migration_error(format!("Legacy note {source}/{name} missing"))
                    })?;
                tree.get_mut(&destination)
                    .map_err(|error| migration_error(error.to_string()))?
                    .notes
                    .insert(note.name.clone(), note);
                Ok(())
            }
            LegacyFolderItemID::SubfolderID(name) => tree
                .move_folder(&source.child(name), &destination)
                .map_err(|error| migration_error(error.to_string())),
            item => {
                let resource = legacy_resource_ref(&item)
                    .ok_or_else(|| migration_error("Unsupported legacy move"))?;
                remove_resource(
                    tree.get_mut(&source)
                        .map_err(|error| migration_error(error.to_string()))?,
                    resource,
                )?;
                add_resource(
                    tree.get_mut(&destination)
                        .map_err(|error| migration_error(error.to_string()))?,
                    resource,
                );
                Ok(())
            }
        })
    }

    fn translate_copy(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let destination_path = parse_path(&value, "dest")?;
        let source_item: LegacyFolderItemID = parse_field(&value, "item_id")?;
        let destination_item: LegacyFolderItemID = parse_field(&value, "new_item_id")?;
        let source = legacy_resource_ref(&source_item)
            .ok_or_else(|| migration_error("Legacy note/folder copies are unsupported"))?;
        let destination = legacy_resource_ref(&destination_item)
            .ok_or_else(|| migration_error("Legacy copy destination is not a resource"))?;
        let mut logs = vec![GameLog::CopyResource {
            source,
            destination,
        }];
        let mut catalog_logs = self.with_reconcile(|tree| {
            add_resource(
                tree.get_mut(&destination_path)
                    .map_err(|error| migration_error(error.to_string()))?,
                destination,
            );
            Ok(())
        })?;
        logs.append(&mut catalog_logs);
        Ok(logs)
    }

    fn translate_rename(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let path = parse_path(&value, "path")?;
        let item: LegacyFolderItemID = parse_field(&value, "item_id")?;
        let new_name: String = parse_field(&value, "new_name")?;
        match item {
            LegacyFolderItemID::SubfolderID(name) => self.with_reconcile(|tree| {
                tree.rename_folder(&path.child(name), new_name)
                    .map_err(|error| migration_error(error.to_string()))
            }),
            LegacyFolderItemID::NoteID(name) => self.with_reconcile(|tree| {
                let folder = tree
                    .get_mut(&path)
                    .map_err(|error| migration_error(error.to_string()))?;
                let mut note = folder.notes.remove(&name).ok_or_else(|| {
                    migration_error(format!("Legacy note {path}/{name} was not found"))
                })?;
                note.name = new_name.clone();
                folder.notes.insert(new_name, note);
                Ok(())
            }),
            item => Ok(vec![GameLog::RenameResource {
                resource: legacy_resource_ref(&item)
                    .ok_or_else(|| migration_error("Unsupported legacy rename"))?,
                new_name,
            }]),
        }
    }

    fn translate_delete(&mut self, value: Value) -> Result<Vec<GameLog>, String> {
        let path = parse_path(&value, "path")?;
        let item: LegacyFolderItemID = parse_field(&value, "item_id")?;
        match item {
            LegacyFolderItemID::SubfolderID(name) => {
                let target = path.child(name);
                let mut resources = Vec::new();
                for child_path in self.tree.walk_paths(&target) {
                    let folder = self
                        .tree
                        .get(child_path)
                        .map_err(|error| migration_error(error.to_string()))?;
                    resources.extend(folder.scenes.iter().copied().map(ResourceRef::Scene));
                    resources.extend(folder.creatures.iter().copied().map(ResourceRef::Creature));
                    resources.extend(folder.items.iter().copied().map(ResourceRef::Item));
                    resources.extend(folder.abilities.iter().copied().map(ResourceRef::Ability));
                    resources.extend(folder.classes.iter().copied().map(ResourceRef::Class));
                }
                resources.sort_by_key(resource_sort_key);
                resources.dedup();
                let mut logs: Vec<_> = resources
                    .iter()
                    .copied()
                    .map(|resource| GameLog::DeleteResource { resource })
                    .collect();
                let before = self.clone();
                for resource in resources {
                    self.remove_resource_everywhere(resource)?;
                }
                let mut paths = self.tree.walk_paths(&target).cloned().collect::<Vec<_>>();
                paths
                    .sort_by_key(|candidate| std::cmp::Reverse(candidate.clone().into_vec().len()));
                for child_path in paths {
                    self.tree
                        .remove(&child_path)
                        .map_err(|error| migration_error(error.to_string()))?;
                }
                let mut catalog_logs = self.reconcile(&before)?;
                logs.append(&mut catalog_logs);
                Ok(logs)
            }
            LegacyFolderItemID::NoteID(name) => self.with_reconcile(|tree| {
                tree.get_mut(&path)
                    .map_err(|error| migration_error(error.to_string()))?
                    .notes
                    .remove(&name)
                    .ok_or_else(|| migration_error(format!("Legacy note {path}/{name} missing")))?;
                Ok(())
            }),
            item => {
                let resource = legacy_resource_ref(&item)
                    .ok_or_else(|| migration_error("Unsupported legacy delete"))?;
                let before = self.clone();
                self.remove_resource_everywhere(resource)?;
                let mut logs = vec![GameLog::DeleteResource { resource }];
                let mut catalog_logs = self.reconcile(&before)?;
                logs.append(&mut catalog_logs);
                Ok(logs)
            }
        }
    }
}

fn validate_legacy_membership(
    game: &Map<String, Value>,
    collections: &BTreeMap<CollectionID, Collection>,
) -> Result<(), String> {
    fn stored_ids(game: &Map<String, Value>, field: &str) -> Result<BTreeSet<String>, String> {
        match game.get(field) {
            Some(Value::Object(values)) => Ok(values.keys().cloned().collect()),
            None if field == "items" => Ok(BTreeSet::new()),
            Some(_) => Err(migration_error(format!(
                "Legacy game field {field} is not an object"
            ))),
            None => Err(migration_error(format!(
                "Legacy game is missing resource field {field}"
            ))),
        }
    }

    fn validate_kind<ID: ToString + Copy>(
        game: &Map<String, Value>,
        collections: &BTreeMap<CollectionID, Collection>,
        field: &str,
        ids: impl Fn(&Collection) -> &[ID],
    ) -> Result<(), String> {
        let mut catalog_ids = BTreeSet::new();
        for collection in collections.values() {
            for id in ids(collection) {
                let id = id.to_string();
                if !catalog_ids.insert(id.clone()) {
                    return Err(migration_error(format!(
                        "Legacy {field} resource {id} occurs in more than one folder"
                    )));
                }
            }
        }
        let stored = stored_ids(game, field)?;
        if catalog_ids != stored {
            return Err(migration_error(format!(
                "Legacy {field} membership does not match stored resources: \
                 folders={catalog_ids:?}, stored={stored:?}"
            )));
        }
        Ok(())
    }

    validate_kind(game, collections, "scenes", |collection| &collection.scenes)?;
    validate_kind(game, collections, "creatures", |collection| {
        &collection.creatures
    })?;
    validate_kind(game, collections, "items", |collection| &collection.items)?;
    validate_kind(game, collections, "abilities", |collection| {
        &collection.abilities
    })?;
    validate_kind(game, collections, "classes", |collection| {
        &collection.classes
    })
}

fn resource_sort_key(resource: &ResourceRef) -> (u8, String) {
    match resource {
        ResourceRef::Scene(id) => (0, id.to_string()),
        ResourceRef::Creature(id) => (1, id.to_string()),
        ResourceRef::Item(id) => (2, id.to_string()),
        ResourceRef::Ability(id) => (3, id.to_string()),
        ResourceRef::Class(id) => (4, id.to_string()),
        ResourceRef::Note(id) => (5, id.to_string()),
    }
}

fn created_resource(log: &GameLog) -> Option<ResourceRef> {
    match log {
        GameLog::CreateItem { item } => Some(ResourceRef::Item(item.id)),
        GameLog::CreateScene { scene } => Some(ResourceRef::Scene(scene.id)),
        GameLog::CreateClass { class } => Some(ResourceRef::Class(class.id)),
        GameLog::CreateAbility { ability } => Some(ResourceRef::Ability(ability.id)),
        GameLog::CreateCreature { creature } => Some(ResourceRef::Creature(creature.id)),
        _ => None,
    }
}

fn legacy_resource_ref(item: &LegacyFolderItemID) -> Option<ResourceRef> {
    match item {
        LegacyFolderItemID::SceneID(id) => Some(ResourceRef::Scene(*id)),
        LegacyFolderItemID::CreatureID(id) => Some(ResourceRef::Creature(*id)),
        LegacyFolderItemID::ItemID(id) => Some(ResourceRef::Item(*id)),
        LegacyFolderItemID::AbilityID(id) => Some(ResourceRef::Ability(*id)),
        LegacyFolderItemID::ClassID(id) => Some(ResourceRef::Class(*id)),
        LegacyFolderItemID::NoteID(_) | LegacyFolderItemID::SubfolderID(_) => None,
    }
}

fn add_resource(folder: &mut TrackedFolder, resource: ResourceRef) {
    match resource {
        ResourceRef::Scene(id) => {
            folder.scenes.insert(id);
        }
        ResourceRef::Creature(id) => {
            folder.creatures.insert(id);
        }
        ResourceRef::Item(id) => {
            folder.items.insert(id);
        }
        ResourceRef::Ability(id) => {
            folder.abilities.insert(id);
        }
        ResourceRef::Class(id) => {
            folder.classes.insert(id);
        }
        ResourceRef::Note(_) => {}
    }
}

fn remove_resource(folder: &mut TrackedFolder, resource: ResourceRef) -> Result<(), String> {
    let removed = match resource {
        ResourceRef::Scene(id) => folder.scenes.remove(&id),
        ResourceRef::Creature(id) => folder.creatures.remove(&id),
        ResourceRef::Item(id) => folder.items.remove(&id),
        ResourceRef::Ability(id) => folder.abilities.remove(&id),
        ResourceRef::Class(id) => folder.classes.remove(&id),
        ResourceRef::Note(_) => false,
    };
    removed
        .then_some(())
        .ok_or_else(|| migration_error(format!("Legacy folder does not contain {resource:?}")))
}

fn parse_snapshot_json(json: &str) -> Result<(String, LegacyCatalog), String> {
    let value: Value = serde_json::from_str(json)
        .map_err(|error| migration_error(format!("Invalid game snapshot JSON: {error}")))?;
    let (migrated, catalog) = LegacyCatalog::migrate_snapshot(value)?;
    let json = serde_json::to_string(&migrated)
        .map_err(|error| migration_error(format!("Could not serialize game snapshot: {error}")))?;
    Ok((json, catalog))
}

fn parse_log_json(json: &str) -> Result<Value, String> {
    serde_json::from_str(json)
        .map_err(|error| migration_error(format!("Invalid game log JSON: {error}")))
}

fn log_to_json(log: &GameLog) -> Result<String, String> {
    serde_json::to_string(log)
        .map_err(|error| migration_error(format!("Could not serialize migrated log: {error}")))
}

fn snapshot_has_no_legacy_fields(json: &str) -> Result<(), String> {
    let value: Value = serde_json::from_str(json)
        .map_err(|error| migration_error(format!("Invalid migrated snapshot JSON: {error}")))?;
    let object: &Map<String, Value> = value
        .as_object()
        .ok_or_else(|| migration_error("Migrated snapshot is not an object"))?;
    if object.contains_key("campaign") {
        return Err(migration_error(
            "Migrated snapshot still contains legacy campaign",
        ));
    }
    if !object.contains_key("notes") || !object.contains_key("collections") {
        return Err(migration_error(
            "Migrated snapshot is missing notes or collections",
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_folder() -> Value {
        serde_json::json!({
            "scenes": [],
            "creatures": [],
            "notes": {},
            "items": [],
            "abilities": [],
            "classes": []
        })
    }

    fn legacy_snapshot(item_id: ItemID) -> Value {
        let mut game = serde_json::to_value(arptypes::Game::default()).unwrap();
        let object = game.as_object_mut().unwrap();
        object.remove("notes");
        object.remove("collections");
        object.insert(
            "items".to_string(),
            serde_json::json!({
                (item_id.to_string()): {
                    "id": item_id,
                    "name": "Relic"
                }
            }),
        );
        object.insert(
            "campaign".to_string(),
            serde_json::json!({
                "data": empty_folder(),
                "children": {
                    "Rules": {
                        "data": {
                            "scenes": [],
                            "creatures": [],
                            "notes": {},
                            "items": [item_id],
                            "abilities": [],
                            "classes": []
                        },
                        "children": {}
                    }
                }
            }),
        );
        game
    }

    fn assert_current(logs: &[GameLog]) {
        for log in logs {
            let value = serde_json::to_value(log).unwrap();
            let tag = value.get("t").and_then(Value::as_str).unwrap();
            assert!(!matches!(
                tag,
                "CreateFolder"
                    | "RenameFolder"
                    | "MoveFolderItem"
                    | "CopyFolderItem"
                    | "DeleteFolderItem"
                    | "RenameFolderItem"
                    | "LoadModule"
            ));
            assert!(value.get("path").is_none());
            serde_json::from_value::<GameLog>(value).unwrap();
        }
    }

    #[test]
    fn migrates_stateful_folder_logs_to_current_catalog_logs() {
        let item_id = ItemID::gen();
        let (_, mut catalog) = LegacyCatalog::migrate_snapshot(legacy_snapshot(item_id)).unwrap();

        assert!(catalog
            .translate_log(serde_json::json!({
                "t": "CreateFolder",
                "path": "/Archive"
            }))
            .unwrap()
            .is_empty());

        let moved = catalog
            .translate_log(serde_json::json!({
                "t": "MoveFolderItem",
                "source": "/Rules",
                "item_id": {"ItemID": item_id},
                "destination": "/Archive"
            }))
            .unwrap();
        assert_current(&moved);
        assert!(moved
            .iter()
            .any(|log| matches!(log, GameLog::CreateCollection { .. })));
        assert!(moved
            .iter()
            .any(|log| matches!(log, GameLog::DeleteCollection { .. })));

        let renamed = catalog
            .translate_log(serde_json::json!({
                "t": "RenameFolder",
                "path": "/Archive",
                "new_name": "Vault"
            }))
            .unwrap();
        assert_current(&renamed);
        assert!(renamed
            .iter()
            .any(|log| matches!(log, GameLog::EditCollection { collection } if collection.name == "/Vault")));

        let deleted = catalog
            .translate_log(serde_json::json!({
                "t": "DeleteFolderItem",
                "path": "/Vault",
                "item_id": {"ItemID": item_id}
            }))
            .unwrap();
        assert_current(&deleted);
        assert!(deleted.iter().any(
            |log| matches!(log, GameLog::DeleteResource { resource: ResourceRef::Item(id) } if *id == item_id)
        ));
    }

    #[test]
    fn rejects_legacy_module_logs_without_polluting_current_log_types() {
        let item_id = ItemID::gen();
        let (_, mut catalog) = LegacyCatalog::migrate_snapshot(legacy_snapshot(item_id)).unwrap();
        let error = catalog
            .translate_log(serde_json::json!({
                "t": "LoadModule",
                "name": "old module"
            }))
            .unwrap_err();
        assert!(error.contains("unsupported"));
    }

    #[test]
    fn drops_legacy_informational_rollback_logs() {
        let item_id = ItemID::gen();
        let (_, mut catalog) = LegacyCatalog::migrate_snapshot(legacy_snapshot(item_id)).unwrap();
        assert!(catalog
            .translate_log(serde_json::json!({
                "t": "Rollback",
                "snapshot_index": 0,
                "log_index": 1
            }))
            .unwrap()
            .is_empty());
    }
}
