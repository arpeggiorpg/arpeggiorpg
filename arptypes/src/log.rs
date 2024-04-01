use std::collections::{HashMap, HashSet};

use foldertree::FolderPath;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::types::*;

/// A representation of state change in a Creature. See `GameLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "t")]
pub enum CreatureLog {
  Damage { hp: HP, rolls: Vec<i16> },
  Heal { hp: HP, rolls: Vec<i16> },
  GenerateEnergy { energy: Energy },
  ReduceEnergy { energy: Energy },
  ApplyCondition { id: ConditionID, duration: Duration, condition: Condition },
  DecrementConditionRemaining { id: ConditionID },
  RemoveCondition { id: ConditionID },
}

// TODO: get rid of CombatLog, it's dumb... unless we ever support multiple Combats?
/// Representation of state changes in a Combat. See `GameLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "t")]
pub enum CombatLog {
  // Consume some of the movement from the current combat-creatur
  ConsumeMovement {
    #[ts(type = "number")]
    distance: u32units::Length,
  },
  ChangeCreatureInitiative {
    creature_id: CreatureID,
    initiative: i16,
  },
  EndTurn {
    creature_id: CreatureID,
  }, // the end of this creature's turn
  ForceNextTurn,
  ForcePrevTurn,
  RerollInitiative {
    combatants: Vec<(CreatureID, i16)>,
  },
}

pub fn creature_logs_into_game_logs(creature_id: CreatureID, ls: Vec<CreatureLog>) -> Vec<GameLog> {
  ls.into_iter().map(|log| GameLog::CreatureLog { creature_id, log }).collect()
}

/// Representation of a change to the game state. All change to the game happens via these values.
/// Note that these represent *concrete* changes to the game, which will have deterministic results.
/// i.e., randomness happens when processing `GMCommand`s, which then result in specific
/// `GameLog`s.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "t")]
pub enum GameLog {
  LoadModule {
    name: String,
    source: ModuleSource,
    #[ts(skip)]
    module: Game,
    path: FolderPath,
  },

  SetActiveScene {
    id: Option<SceneID>,
  },

  // ** Player Manipulation **
  RegisterPlayer {
    id: PlayerID,
  },
  GiveCreaturesToPlayer {
    player_id: PlayerID,
    creature_ids: Vec<CreatureID>,
  },
  UnregisterPlayer {
    id: PlayerID,
  },
  RemoveCreaturesFromPlayer {
    player_id: PlayerID,
    creature_ids: Vec<CreatureID>,
  },
  SetPlayerScene {
    player_id: PlayerID,
    scene_id: Option<SceneID>,
  },

  ChatFromGM {
    message: String,
  },
  ChatFromPlayer {
    player_id: PlayerID,
    message: String,
  },

  AttributeCheckResult {
    creature_id: CreatureID,
    attribute_check: AttributeCheck,
    actual: u8,
    success: bool,
  },

  // ** Folder Management **
  /// Create a folder, given segments leading to it.
  CreateFolder {
    path: FolderPath,
  },
  /// Rename a folder. DEPRECATED (I think?)
  RenameFolder {
    path: FolderPath,
    new_name: String,
  },
  MoveFolderItem {
    source: FolderPath,
    item_id: FolderItemID,
    destination: FolderPath,
  },
  CopyFolderItem {
    source: FolderPath,
    item_id: FolderItemID,
    dest: FolderPath,
    new_item_id: FolderItemID,
  },
  DeleteFolderItem {
    path: FolderPath,
    item_id: FolderItemID,
  },
  RenameFolderItem {
    path: FolderPath,
    item_id: FolderItemID,
    new_name: String,
  },

  CreateItem {
    path: FolderPath,
    item: Item,
  },
  EditItem {
    item: Item,
  },

  CreateNote {
    path: FolderPath,
    note: Note,
  },
  EditNote {
    path: FolderPath,
    original_name: String,
    note: Note,
  },

  // ** Inventory management **
  TransferItem {
    from: InventoryOwner,
    to: InventoryOwner,
    item_id: ItemID,
    count: u64,
  },
  RemoveItem {
    owner: InventoryOwner,
    item_id: ItemID,
    count: u64,
  },
  SetItemCount {
    owner: InventoryOwner,
    item_id: ItemID,
    count: u64,
  },

  CreateScene {
    path: FolderPath,
    scene: Scene,
  },
  EditSceneDetails {
    scene_id: SceneID,
    details: SceneCreation,
  },
  SetSceneCreatureVisibility {
    scene_id: SceneID,
    creature_id: CreatureID,
    visibility: Visibility,
  },
  AddCreatureToScene {
    scene_id: SceneID,
    creature_id: CreatureID,
    visibility: Visibility,
  },
  RemoveCreatureFromScene {
    scene_id: SceneID,
    creature_id: CreatureID,
  },
  AddSceneChallenge {
    scene_id: SceneID,
    description: String,
    challenge: AttributeCheck,
  },
  RemoveSceneChallenge {
    scene_id: SceneID,
    description: String,
  },
  SetFocusedSceneCreatures {
    scene_id: SceneID,
    creatures: Vec<CreatureID>,
  },
  RemoveSceneVolumeCondition {
    scene_id: SceneID,
    condition_id: ConditionID,
  },

  EditSceneTerrain {
    scene_id: SceneID,
    #[ts(type = "Terrain")]
    terrain: Vec<Point3>,
  },
  EditSceneHighlights {
    scene_id: SceneID,
    #[ts(type = "Highlights")]
    highlights: HashMap<Point3, (Color, Visibility)>,
  },
  EditSceneAnnotations {
    scene_id: SceneID,
    #[ts(type = "Annotations")]
    annotations: HashMap<Point3, (String, Visibility)>,
  },
  EditSceneRelatedScenes {
    scene_id: SceneID,
    #[ts(type = "RelatedScenes")]
    related_scenes: HashSet<SceneID>,
  },
  EditSceneSceneHotspots {
    scene_id: SceneID,
    #[ts(type = "SceneHotspots")]
    scene_hotspots: HashMap<Point3, SceneID>,
  },

  CombatLog {
    log: CombatLog,
  },
  /// A creature log wrapped in a game log.
  CreatureLog {
    creature_id: CreatureID,
    log: CreatureLog,
  },
  SetCreaturePos {
    scene_id: SceneID,
    creature_id: CreatureID,
    pos: Point3,
  },
  PathCreature {
    scene_id: SceneID,
    creature_id: CreatureID,
    path: Vec<Point3>,
  },

  AddVolumeCondition {
    scene_id: SceneID,
    point: Point3,
    volume: Volume,
    condition_id: ConditionID,
    condition: Condition,
    duration: Duration,
  },

  StartCombat {
    scene_id: SceneID,
    combatants: Vec<(CreatureID, i16)>,
  },
  StopCombat,

  // ** Classes & Abilities **
  CreateClass {
    path: FolderPath,
    class: Class,
  },
  EditClass {
    class: Class,
  },
  CreateAbility {
    path: FolderPath,
    ability: Ability,
  },
  EditAbility {
    ability: Ability,
  },

  // ** Creatures **
  CreateCreature {
    path: FolderPath,
    creature: Creature,
  },
  // Deprecated: please migrate & remove
  EditCreatureDetails {
    creature_id: CreatureID,
    details: CreatureCreation,
  },
  EditCreature {
    creature: Creature,
  },
  AddCreatureToCombat {
    creature_id: CreatureID,
    initiative: i16,
  },
  RemoveCreatureFromCombat {
    creature_id: CreatureID,
  },
  Rollback {
    // This is purely informational?
    snapshot_index: usize,
    log_index: usize,
  },
}

pub fn combat_logs_into_game_logs(ls: Vec<CombatLog>) -> Vec<GameLog> {
  ls.into_iter().map(|log| GameLog::CombatLog { log }).collect()
}
