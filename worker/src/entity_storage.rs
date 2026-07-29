use std::collections::HashMap;

use arptypes::{
    Ability, Class, Collection, Combat, Creature, Game, Item, Note, Player, Scene, SceneID,
    TileSystem,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use worker::SqlStorage;

const ENTITY_TABLES: &[&str] = &[
    "scenes",
    "creatures",
    "notes",
    "items",
    "abilities",
    "classes",
    "collections",
    "players",
];

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct GameState {
    current_combat: Option<Combat>,
    tile_system: TileSystem,
    active_scene: Option<SceneID>,
}

impl GameState {
    fn from_game(game: &Game) -> Self {
        Self {
            current_combat: game.current_combat.clone(),
            tile_system: game.tile_system,
            active_scene: game.active_scene,
        }
    }
}

#[derive(Deserialize)]
struct EntityRow {
    id: String,
    body: String,
}

#[derive(Deserialize)]
struct GameStateRow {
    body: String,
}

pub(crate) fn initialize_tables(sql: &SqlStorage) -> anyhow::Result<()> {
    for table in ENTITY_TABLES {
        sql.exec(
            &format!(
                "CREATE TABLE {table} (
                    id TEXT PRIMARY KEY,
                    body BLOB NOT NULL
                )"
            ),
            None,
        )?;
    }
    sql.exec(
        "CREATE TABLE game_state (
            id INTEGER PRIMARY KEY CHECK (id = 1),
            body BLOB NOT NULL
        )",
        None,
    )?;
    Ok(())
}

fn load_entities<T>(
    sql: &SqlStorage,
    table: &str,
    entity_id: impl Fn(&T) -> String,
) -> anyhow::Result<Vec<T>>
where
    T: DeserializeOwned,
{
    let rows: Vec<EntityRow> = sql
        .exec(
            &format!("SELECT id, json(body) AS body FROM {table} ORDER BY id"),
            None,
        )?
        .to_array()?;
    rows.into_iter()
        .map(|row| {
            let entity: T = serde_json::from_str(&row.body)?;
            anyhow::ensure!(
                entity_id(&entity) == row.id,
                "{table} row ID {:?} does not match its JSON body",
                row.id
            );
            Ok(entity)
        })
        .collect()
}

pub(crate) fn load_game(sql: &SqlStorage) -> anyhow::Result<Game> {
    let game_state_rows: Vec<GameStateRow> = sql
        .exec(
            "SELECT json(body) AS body FROM game_state WHERE id = 1",
            None,
        )?
        .to_array()?;
    let [game_state_row] = game_state_rows.as_slice() else {
        anyhow::bail!(
            "game_state must contain exactly one singleton row, found {}",
            game_state_rows.len()
        );
    };
    let game_state: GameState = serde_json::from_str(&game_state_row.body)?;

    Ok(Game {
        current_combat: game_state.current_combat,
        abilities: load_entities::<Ability>(sql, "abilities", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        creatures: load_entities::<Creature>(sql, "creatures", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        classes: load_entities::<Class>(sql, "classes", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        tile_system: game_state.tile_system,
        scenes: load_entities::<Scene>(sql, "scenes", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        items: load_entities::<Item>(sql, "items", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        notes: load_entities::<Note>(sql, "notes", |entity| entity.id.to_string())?
            .into_iter()
            .collect(),
        collections: load_entities::<Collection>(sql, "collections", |entity| {
            entity.id.to_string()
        })?
        .into_iter()
        .collect(),
        players: load_entities::<Player>(sql, "players", |entity| entity.player_id.to_string())?
            .into_iter()
            .collect(),
        active_scene: game_state.active_scene,
    })
}

fn upsert_entity<T: Serialize>(
    sql: &SqlStorage,
    table: &str,
    id: &str,
    entity: &T,
) -> anyhow::Result<()> {
    let body = serde_json::to_string(entity)?;
    sql.exec(
        &format!(
            "INSERT INTO {table} (id, body) VALUES (?, jsonb(?))
             ON CONFLICT (id) DO UPDATE SET body = excluded.body"
        ),
        Some(vec![id.into(), body.into()]),
    )?;
    Ok(())
}

fn delete_entity(sql: &SqlStorage, table: &str, id: &str) -> anyhow::Result<()> {
    sql.exec(
        &format!("DELETE FROM {table} WHERE id = ?"),
        Some(vec![id.into()]),
    )?;
    Ok(())
}

fn replace_entities<'a, T: Serialize + 'a>(
    sql: &SqlStorage,
    table: &str,
    entities: impl Iterator<Item = &'a T>,
    entity_id: impl Fn(&T) -> String,
) -> anyhow::Result<()> {
    sql.exec(&format!("DELETE FROM {table}"), None)?;
    for entity in entities {
        upsert_entity(sql, table, &entity_id(entity), entity)?;
    }
    Ok(())
}

fn persist_entity_delta<'a, T: PartialEq + Serialize + 'a>(
    sql: &SqlStorage,
    table: &str,
    old_entities: impl Iterator<Item = &'a T>,
    new_entities: impl Iterator<Item = &'a T>,
    entity_id: impl Fn(&T) -> String,
) -> anyhow::Result<()> {
    let old_by_id: HashMap<String, &T> = old_entities
        .map(|entity| (entity_id(entity), entity))
        .collect();
    let new_by_id: HashMap<String, &T> = new_entities
        .map(|entity| (entity_id(entity), entity))
        .collect();

    for (id, entity) in &new_by_id {
        if old_by_id.get(id).copied() != Some(*entity) {
            upsert_entity(sql, table, id, *entity)?;
        }
    }
    for id in old_by_id.keys() {
        if !new_by_id.contains_key(id) {
            delete_entity(sql, table, id)?;
        }
    }
    Ok(())
}

fn store_game_state(sql: &SqlStorage, game: &Game) -> anyhow::Result<()> {
    let body = serde_json::to_string(&GameState::from_game(game))?;
    sql.exec(
        "INSERT INTO game_state (id, body) VALUES (1, jsonb(?))
         ON CONFLICT (id) DO UPDATE SET body = excluded.body",
        Some(vec![body.into()]),
    )?;
    Ok(())
}

pub(crate) fn replace_game(sql: &SqlStorage, game: &Game) -> anyhow::Result<()> {
    replace_entities(sql, "scenes", game.scenes.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "creatures", game.creatures.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "notes", game.notes.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "items", game.items.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "abilities", game.abilities.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "classes", game.classes.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "collections", game.collections.values(), |entity| {
        entity.id.to_string()
    })?;
    replace_entities(sql, "players", game.players.values(), |entity| {
        entity.player_id.to_string()
    })?;
    store_game_state(sql, game)
}

pub(crate) fn persist_game_delta(
    sql: &SqlStorage,
    old_game: &Game,
    new_game: &Game,
) -> anyhow::Result<()> {
    let Game {
        current_combat: old_current_combat,
        abilities: old_abilities,
        creatures: old_creatures,
        classes: old_classes,
        tile_system: old_tile_system,
        scenes: old_scenes,
        items: old_items,
        notes: old_notes,
        collections: old_collections,
        players: old_players,
        active_scene: old_active_scene,
    } = old_game;
    let Game {
        current_combat: new_current_combat,
        abilities: new_abilities,
        creatures: new_creatures,
        classes: new_classes,
        tile_system: new_tile_system,
        scenes: new_scenes,
        items: new_items,
        notes: new_notes,
        collections: new_collections,
        players: new_players,
        active_scene: new_active_scene,
    } = new_game;

    persist_entity_delta(
        sql,
        "scenes",
        old_scenes.values(),
        new_scenes.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "creatures",
        old_creatures.values(),
        new_creatures.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "notes",
        old_notes.values(),
        new_notes.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "items",
        old_items.values(),
        new_items.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "abilities",
        old_abilities.values(),
        new_abilities.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "classes",
        old_classes.values(),
        new_classes.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "collections",
        old_collections.values(),
        new_collections.values(),
        |entity| entity.id.to_string(),
    )?;
    persist_entity_delta(
        sql,
        "players",
        old_players.values(),
        new_players.values(),
        |entity| entity.player_id.to_string(),
    )?;

    if old_current_combat != new_current_combat
        || old_tile_system != new_tile_system
        || old_active_scene != new_active_scene
    {
        store_game_state(sql, new_game)?;
    }
    Ok(())
}
