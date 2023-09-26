//! Simple types, with pure operations.

// Just disable large_enum_variant lints for now, since I'm not really that interested in fixing
// that for a while
#![cfg_attr(feature = "cargo-clippy", allow(clippy::large_enum_variant))]

use std::collections::{HashMap, HashSet, VecDeque};

use derive_more::{Add, Div, Mul, Sub};
use error_chain::bail;
use num::Saturating;
use rand::Rng;
use serde::{
  de,
  ser::{Error as SerError, SerializeStruct},
  Deserialize, Deserializer, Serialize, Serializer,
};
use serde_yaml;
use thiserror::Error;
use ts_rs::TS;
use uom::si::length::{centimeter, meter};
use uuid::{Error as UuidParseError, Uuid};

use foldertree::{FolderPath, FolderTree, FolderTreeError};
use indexed::{DeriveKey, IndexedHashMap};
use nonempty;

pub mod u32units {
  ISQ!(uom::si, u32, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

pub fn u32cm(v: u32) -> u32units::Length { u32units::Length::new::<centimeter>(v) }
pub fn u32meter<T: Into<u32>>(v: T) -> u32units::Length { u32units::Length::new::<meter>(v.into()) }

pub mod i64units {
  ISQ!(uom::si, i64, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

pub fn i64cm<T: Into<i64>>(v: T) -> i64units::Length {
  i64units::Length::new::<centimeter>(v.into())
}
pub fn i64meter<T: Into<i64>>(v: T) -> i64units::Length { i64units::Length::new::<meter>(v.into()) }

pub fn up_length(v: u32units::Length) -> i64units::Length { i64cm(v.get::<centimeter>()) }

pub type Color = String;
pub type Inventory = HashMap<ItemID, u64>;
pub type Terrain = Vec<Point3>;

/// Point3 holds a position in 3d space in meters (FOR NOW --radix)
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, TS)]
pub struct Point3 {
  #[ts(type = "number")]
  pub x: i64units::Length,
  #[ts(type = "number")]
  pub y: i64units::Length,
  #[ts(type = "number")]
  pub z: i64units::Length,
}

impl Point3 {
  pub fn new(x: i64, y: i64, z: i64) -> Point3 { Point3 { x: i64cm(x), y: i64cm(y), z: i64cm(z) } }
  pub fn from_quantities(x: i64units::Length, y: i64units::Length, z: i64units::Length) -> Self {
    Point3 { x, y, z }
  }
}

impl ::std::fmt::Display for Point3 {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(
      f,
      "{}/{}/{}",
      self.x.get::<centimeter>(),
      self.y.get::<centimeter>(),
      self.z.get::<centimeter>()
    )
  }
}

impl ::std::str::FromStr for Point3 {
  type Err = GameError;
  fn from_str(path: &str) -> Result<Point3, GameError> {
    let segments: Vec<&str> = path.split('/').collect();
    if segments.len() != 3 {
      bail!("Bad Point3 syntax")
    }
    match (segments[0].parse::<i64>(), segments[1].parse::<i64>(), segments[2].parse::<i64>()) {
      (Ok(x), Ok(y), Ok(z)) => Ok(Point3::new(x, y, z)),
      _ => bail!("Bad Point3 syntax"),
    }
  }
}

impl Serialize for Point3 {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&self.to_string())
  }
}

impl<'de> Deserialize<'de> for Point3 {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    let st: String = Deserialize::deserialize(deserializer)?;
    match st.parse() {
      Ok(x) => Ok(x),
      Err(x) => Err(de::Error::invalid_value(
        de::Unexpected::Str(&st),
        &format!("Unknown error: {:?}", x).as_ref(),
      )),
    }
  }
}

/// An axis-aligned bounding box.
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize, TS)]
pub struct AABB {
  #[ts(type = "number")]
  pub x: u32units::Length,
  #[ts(type = "number")]
  pub y: u32units::Length,
  #[ts(type = "number")]
  pub z: u32units::Length,
}

impl AABB {
  /// Get the "maximum" point of the AABB (aka the top-right point) relative to a fixed point.
  pub fn get_max(&self, pt: Point3) -> Point3 {
    Point3::from_quantities(
      pt.x + i64cm(self.x.get::<centimeter>()),
      pt.y + i64cm(self.y.get::<centimeter>()),
      pt.z + i64cm(self.z.get::<centimeter>()),
    )
  }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize, TS)]
pub enum Dice {
  Expr { num: u8, size: u8 },
  Plus(Box<Dice>, Box<Dice>),
  Flat { value: i8 },
  BestOf(u8, Box<Dice>),
}

impl Dice {
  pub fn expr(n: u8, d: u8) -> Dice { Dice::Expr { num: n, size: d } }

  pub fn flat(value: i8) -> Dice { Dice::Flat { value } }

  pub fn plus(&self, d: Dice) -> Dice { Dice::Plus(Box::new(self.clone()), Box::new(d)) }

  /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
  /// final result.
  pub fn roll(&self) -> (Vec<i16>, i32) {
    match *self {
      Dice::Expr { num, size } => {
        let mut intermediate = vec![];
        let mut result = 0i32;
        let mut rng = rand::thread_rng();

        for _ in 0..num {
          let val = rng.gen_range(1, i32::from(size) + 1);
          result += val;
          intermediate.push(val as i16);
        }
        (intermediate, result)
      }
      Dice::Flat { value } => (vec![i16::from(value)], i32::from(value)),
      Dice::Plus(ref l, ref r) => {
        let (mut intermediate, left_result) = l.roll();
        let (right_intermediate, right_result) = r.roll();
        intermediate.extend(right_intermediate);
        (intermediate, left_result + right_result)
      }
      Dice::BestOf(count, ref dice) => {
        if count == 0 {
          panic!("Sorry, can't roll best of 0.")
        }
        let (mut best_rolls, mut best_result) = dice.roll();
        for _ in 1..count {
          let (rolls, result) = dice.roll();
          if result > best_result {
            best_rolls = rolls;
            best_result = result;
          }
        }
        (best_rolls, best_result)
      }
    }
  }
}

#[derive(
  Add,
  Sub,
  Mul,
  Div,
  Clone,
  Copy,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Debug,
  Hash,
  Serialize,
  Deserialize,
  TS,
)]
pub struct HP(pub u8);
impl Saturating for HP {
  fn saturating_add(self, other: Self) -> Self { HP(self.0.saturating_add(other.0)) }
  fn saturating_sub(self, other: Self) -> Self { HP(self.0.saturating_sub(other.0)) }
}

#[derive(
  Add,
  Sub,
  Mul,
  Div,
  Clone,
  Copy,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Debug,
  Hash,
  Serialize,
  Deserialize,
  TS,
)]
pub struct Energy(pub u8);
impl Saturating for Energy {
  fn saturating_add(self, other: Self) -> Self { Energy(self.0.saturating_add(other.0)) }
  fn saturating_sub(self, other: Self) -> Self { Energy(self.0.saturating_sub(other.0)) }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize, TS)]
pub struct PlayerID(pub String);

macro_rules! uuid_id {
  ($type: ident) => {
    #[derive(
      Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize, TS,
    )]
    pub struct $type(pub Uuid);
    impl $type {
      pub fn gen() -> $type { $type(Uuid::new_v4()) }
      pub fn to_string(&self) -> String { self.0.to_hyphenated().to_string() }
    }

    impl ::std::str::FromStr for $type {
      type Err = GameError;
      fn from_str(s: &str) -> Result<$type, GameError> {
        Uuid::parse_str(s).map_err(|e| GameError::InvalidID(s.to_string(), e)).map($type)
      }
    }
  };
}

uuid_id!(ConditionID);
uuid_id!(CreatureID);
uuid_id!(ItemID);
uuid_id!(SceneID);
uuid_id!(AbilityID);
uuid_id!(ClassID);

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum FolderItemID {
  SceneID(SceneID),
  CreatureID(CreatureID),
  NoteID(String),
  ItemID(ItemID),
  AbilityID(AbilityID),
  ClassID(ClassID),
  SubfolderID(String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize, TS)]
pub enum SkillLevel {
  // The way to read these are:
  // "A {variant} person has a 75% chance of doing this."
  // If you're unskilled and you are doing a Skilled difficulty challenge: 50%?
  // Trivial to Expert: 10%?
  Inept,
  Unskilled,
  Skilled,
  Expert,
  Supernatural,
}

impl SkillLevel {
  pub fn to_ord(&self) -> i8 {
    match *self {
      SkillLevel::Inept => -1,
      SkillLevel::Unskilled => 0,
      SkillLevel::Skilled => 1,
      SkillLevel::Expert => 2,
      SkillLevel::Supernatural => 3,
    }
  }

  pub fn difficulty(&self, difficulty_level: SkillLevel) -> u8 {
    100
      - match difficulty_level.to_ord() - self.to_ord() {
        -4 => 100,
        -3 => 99,
        -2 => 95,
        -1 => 85,
        0 => 75,
        1 => 50,
        2 => 10,
        3 => 1,
        4 => 0,
        diff => panic!("[SkillLevel::difficulty] Two skill levels were too far apart: {:?}", diff),
      }
  }
}

// maybe make this a trait in the future
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum InventoryOwner {
  Scene(SceneID),
  Creature(CreatureID),
}
impl InventoryOwner {
  pub fn not_found_error(&self) -> GameError {
    match *self {
      InventoryOwner::Scene(sid) => GameError::SceneNotFound(sid),
      InventoryOwner::Creature(cid) => GameError::CreatureNotFound(cid.to_string()),
    }
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum ModuleSource {
  Module,
  SavedGame,
}

/// Top-level commands that can be sent from a client to affect the state of the app.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameCommand {
  LoadModule {
    source: ModuleSource,
    name: String,
    path: FolderPath,
  },

  ChatFromGM(String),
  ChatFromPlayer(PlayerID, String),

  AttributeCheck(CreatureID, AttributeCheck),

  /// Create a folder, given segments leading to it.
  CreateFolder(FolderPath),
  /// Rename a folder.
  RenameFolder(FolderPath, String),

  /// Move some object from one folder to another.
  MoveFolderItem(FolderPath, FolderItemID, FolderPath),
  /// Copy an object to a folder. It's okay to copy it to the same folder.
  CopyFolderItem {
    source: FolderPath,
    item_id: FolderItemID,
    dest: FolderPath,
  },
  DeleteFolderItem(FolderPath, FolderItemID),

  /// Create an Item in a folder. (this will probably take an ItemCreation in the future)
  CreateItem(FolderPath, String),
  /// Edit an Item. The ID in the given Item must match an existing Item.
  EditItem(Item),

  /// Create a Note inside of a Folder.
  CreateNote(FolderPath, Note),
  /// Rename a Note inside of a Folder.
  EditNote(FolderPath, String, Note),

  // ** Inventory management **
  // These work for creatures or scenes
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

  // ** Scene management **
  /// Create a Scene.
  CreateScene(FolderPath, SceneCreation),
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
  // AddSceneVolumeCondition {
  //   scene_id: SceneID,
  //   point: Point3,
  //   volume: Volume,
  //   condition: Condition,
  //   duration: Duration,
  // },
  RemoveSceneVolumeCondition {
    scene_id: SceneID,
    condition_id: ConditionID,
  },
  EditSceneTerrain {
    scene_id: SceneID,
    terrain: Vec<Point3>,
  },
  EditSceneHighlights {
    scene_id: SceneID,
    highlights: HashMap<Point3, (Color, Visibility)>,
  },
  EditSceneAnnotations {
    scene_id: SceneID,
    annotations: HashMap<Point3, (String, Visibility)>,
  },
  EditSceneRelatedScenes {
    scene_id: SceneID,
    related_scenes: HashSet<SceneID>,
  },
  EditSceneSceneHotspots {
    scene_id: SceneID,
    scene_hotspots: HashMap<Point3, SceneID>,
  },

  // ** Combat management **
  /// Start a combat with the specified creatures.
  StartCombat(SceneID, Vec<CreatureID>),
  /// Stop the current combat.
  StopCombat,
  /// Add a creature to combat.
  AddCreatureToCombat(CreatureID),
  /// Remove a creature from combat.
  RemoveCreatureFromCombat(CreatureID),
  /// Modify a creature's order in the combat list.
  ChangeCreatureInitiative(CreatureID, i16),
  /// Reroll initiative for all creatures in combat, and sort the combat list
  RerollCombatInitiative,
  /// Move to the next creature in the initiative list. This does *not* run any end-of-turn or
  /// start-turn events.
  ForceNextTurn,
  /// Move to the previous creature in the initiative list. This does *not* run any end-of-turn or
  /// start-turn events.
  ForcePrevTurn,

  // ** Combat **
  /// Use an Ability out of combat.
  ActCreature(SceneID, CreatureID, AbilityID, DecidedTarget),
  /// Make the current creature use an ability.
  CombatAct(AbilityID, DecidedTarget),
  /// Move the current creature in combat to a point.
  /// There must be a clear path according to the current loaded map.
  PathCurrentCombatCreature(Point3),
  /// End the current creature's turn.
  Done,

  // ** Creature Manipulation **
  /// Create a new creature.
  CreateCreature(FolderPath, CreatureCreation),
  /// Edit an existing creature.
  EditCreatureDetails {
    creature_id: CreatureID,
    details: CreatureCreation,
  },
  /// Assign a creature's position within a scene.
  SetCreaturePos(SceneID, CreatureID, Point3),
  /// Move a creature along a path within a scene.
  /// There must be a clear path according to the current loaded map. It doesn't matter whether
  /// the creature is in combat.
  PathCreature(SceneID, CreatureID, Point3),

  // ** Player Manipulation **
  /// Register a player as available for controlling a creature.
  RegisterPlayer(PlayerID),
  /// Give control of a creature to a player.
  GiveCreaturesToPlayer(PlayerID, Vec<CreatureID>),
  /// Remove a player from the game, allowing all of their creatures to be given to other players.
  UnregisterPlayer(PlayerID),
  /// Remove control of a creature from a player.
  RemoveCreaturesFromPlayer(PlayerID, Vec<CreatureID>),
  /// Move a player to a particular scene, so they only see what's happening in that scene.
  /// Note that this doesn't have any affect on a player's *characters*.
  /// The scene name can be None (null) to not show any scene to the player.
  SetPlayerScene(PlayerID, Option<SceneID>),

  SetActiveScene(Option<SceneID>),

  /// Roll back to a specific snapshot + log index
  Rollback(usize, usize),
}

/// A representation of state change in a Creature. See `GameLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum CreatureLog {
  Damage { hp: HP, rolls: Vec<i16> },
  Heal { hp: HP, rolls: Vec<i16> },
  GenerateEnergy(Energy),
  ReduceEnergy(Energy),
  ApplyCondition { id: ConditionID, duration: Duration, condition: Condition },
  DecrementConditionRemaining(ConditionID),
  RemoveCondition(ConditionID),
}

// TODO: get rid of CombatLog, it's dumb... unless we ever support multiple Combats?
/// Representation of state changes in a Combat. See `GameLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum CombatLog {
  // Consume some of the movement from the current combat-creatur
  ConsumeMovement(
    #[ts(type = "number")]
    u32units::Length
  ),
  ChangeCreatureInitiative{
    creature_id: CreatureID,
    new_initiative: i16
  },
  EndTurn(CreatureID), // the end of this creature's turn
  ForceNextTurn,
  ForcePrevTurn,
  RerollInitiative(Vec<(CreatureID, i16)>),
}

pub fn creature_logs_into_game_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<GameLog> {
  ls.into_iter().map(|l| GameLog::CreatureLog(cid, l)).collect()
}

/// Representation of a change to the game state. All change to the game happens via these values.
/// Note that these represent *concrete* changes to the game, which will have deterministic results.
/// i.e., randomness happens when processing `GameCommand`s, which then result in specific
/// `GameLog`s.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameLog {
  LoadModule {
    name: String,
    source: ModuleSource,
    module: Game,
    path: FolderPath,
  },

  SetActiveScene(Option<SceneID>),

  // ** Player Manipulation **
  RegisterPlayer(PlayerID),
  GiveCreaturesToPlayer(PlayerID, Vec<CreatureID>),
  UnregisterPlayer(PlayerID),
  RemoveCreaturesFromPlayer(PlayerID, Vec<CreatureID>),
  SetPlayerScene(PlayerID, Option<SceneID>),

  ChatFromGM(String),
  ChatFromPlayer(PlayerID, String),

  AttributeCheckResult(CreatureID, AttributeCheck, u8, bool),

  // ** Folder Management **
  /// Create a folder, given segments leading to it.
  CreateFolder(FolderPath),
  RenameFolder(FolderPath, String),
  MoveFolderItem(FolderPath, FolderItemID, FolderPath),
  CopyFolderItem {
    source: FolderPath,
    item_id: FolderItemID,
    dest: FolderPath,
    new_item_id: FolderItemID,
  },
  DeleteFolderItem(FolderPath, FolderItemID),

  CreateItem(FolderPath, Item),
  EditItem(Item),

  CreateNote(FolderPath, Note),
  EditNote(FolderPath, String, Note),

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

  CreateScene(FolderPath, Scene),
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
    terrain: Vec<Point3>,
  },
  EditSceneHighlights {
    scene_id: SceneID,
    highlights: HashMap<Point3, (Color, Visibility)>,
  },
  EditSceneAnnotations {
    scene_id: SceneID,
    annotations: HashMap<Point3, (String, Visibility)>,
  },
  EditSceneRelatedScenes {
    scene_id: SceneID,
    related_scenes: HashSet<SceneID>,
  },
  EditSceneSceneHotspots {
    scene_id: SceneID,
    scene_hotspots: HashMap<Point3, SceneID>,
  },

  CombatLog(CombatLog),
  /// A creature log wrapped in a game log.
  CreatureLog(CreatureID, CreatureLog),
  SetCreaturePos(SceneID, CreatureID, Point3),
  PathCreature(SceneID, CreatureID, Vec<Point3>),

  AddVolumeCondition {
    scene_id: SceneID,
    point: Point3,
    volume: Volume,
    condition_id: ConditionID,
    condition: Condition,
    duration: Duration,
  },

  StartCombat(SceneID, Vec<(CreatureID, i16)>),
  StopCombat,
  CreateCreature(FolderPath, Creature),
  EditCreatureDetails {
    creature_id: CreatureID,
    details: CreatureCreation,
  },
  AddCreatureToCombat(CreatureID, i16),
  RemoveCreatureFromCombat(CreatureID),
  /// Indexes into snapshots and logs.
  Rollback(usize, usize),
}

pub fn combat_logs_into_game_logs(ls: Vec<CombatLog>) -> Vec<GameLog> {
  ls.into_iter().map(GameLog::CombatLog).collect()
}

#[derive(Debug, Error)]
pub enum GameError {
  #[error("File {0} was not found")]
  FileNotFound(String),
  #[error("The Creature with ID {0:?} does not have the attribute {1:?}")]
  AttributeNotFound(CreatureID, AttrID),
  #[error("The ability with ID {0:?} already exists")]
  AbilityAlreadyExists(AbilityID),
  #[error("The creature with ID {0:?} already exists")]
  CreatureAlreadyExists(CreatureID),
  #[error("The Item {0:?} already exists")]
  ItemAlreadyExists(ItemID),
  #[error("The Item {0:?} couldn't be found")]
  ItemNotFound(ItemID),
  #[error("The scene {0:?} already exists")]
  SceneAlreadyExists(SceneID),
  #[error("The Scene '{0:?}' wasn't found")]
  SceneNotFound(SceneID),
  #[error("The scene {0:?} is in use (by combat, probably).")]
  SceneInUse(SceneID),
  #[error("The identifier {0:?} is too long.")]
  IDTooLong(String),
  #[error("The condition with ID {0:?} wasn't found.")]
  ConditionNotFound(ConditionID),
  #[error("Cannot process {0:?} in this state.")]
  InvalidCommand(GameCommand),
  #[error("The class {0:?} already exists.")]
  ClassAlreadyExists(ClassID),
  #[error("The class {0:?} was not found.")]
  ClassNotFound(ClassID),
  #[error("The ability with ID {0:?} wasn't found.")]
  NoAbility(AbilityID),
  #[error("Creatures must be supplied when starting a combat.")]
  CombatMustHaveCreatures,
  #[error("RerollInitiative can only be invoked at the beginning of a round.")]
  MustRerollAtStartOfRound,
  #[error("The creature with ID {0:?} does not have the ability {1:?}")]
  CreatureLacksAbility(CreatureID, AbilityID),
  #[error("The creature with ID {0:?} could not be found.")]
  CreatureNotFound(String),
  #[error("Creature with ID {0:?} is not a valid target.")]
  InvalidTarget(CreatureID),
  #[error("DecidedTarget {1:?} is not valid for TargetSpec {0:?}.")]
  InvalidTargetForTargetSpec(CreatureTarget, DecidedTarget),
  #[error("DecidedTarget {1:?} is not valid for Action {0:?}.")]
  InvalidTargetForAction(Action, DecidedTarget),
  #[error("Creature {0:?} is out of range.")]
  CreatureOutOfRange(CreatureID),
  #[error("Point {0:?} is out of range.")]
  PointOutOfRange(Point3),
  #[error("There's a bug in the program: {0}")]
  BuggyProgram(String),
  #[error("There is currently no combat.")]
  NotInCombat,
  #[error("Creature {0:?} is already in combat.")]
  AlreadyInCombat(CreatureID),
  #[error("Creature {0:?} cannot be moved.")]
  CannotMove(CreatureID),
  #[error("Creature {0:?} cannot act.")]
  CannotAct(CreatureID),
  #[error("A path can't be found.")]
  NoPathFound,
  #[error("Path {0} already exists")]
  FolderAlreadyExists(FolderPath),
  #[error("Can't step from {0:?} to {1:?}")]
  StepTooBig(Point3, Point3),
  #[error("Not enough energy: {0:?}")]
  NotEnoughEnergy(Energy),
  #[error("Player ID {0:?} is already registered.")]
  PlayerAlreadyExists(PlayerID),
  #[error("Player ID {0:?} was not found.")]
  PlayerNotFound(PlayerID),
  #[error("Player ID {0:?} does not control creature {1:?}.")]
  PlayerDoesntControlCreature(PlayerID, CreatureID),
  #[error("Couldn't find history item at snapshot {0} log item {1}")]
  HistoryNotFound(usize, usize),
  #[error("Initiative index {0} is out of bounds.")]
  InitiativeOutOfBounds(usize),
  #[error("The folder {0} is not empty")]
  FolderNotEmpty(FolderPath),
  #[error("The folder {0} does not contain {1:?}")]
  FolderItemNotFound(FolderPath, FolderItemID),
  #[error("Notes can't be linked or unlinked. '{0}' / '{1}'")]
  CannotLinkNotes(FolderPath, String),
  #[error("Failed to open a file containing an application: {0}")]
  CouldNotOpenAppFile(String, #[source] ::std::io::Error),
  #[error("Failed to parse a serialized application: {0}")]
  CouldNotParseApp(#[source] serde_yaml::Error),

  #[error("No module source found")]
  NoModuleSource,

  // Wrappers for other errors:
  #[error("FolderTree error: {0}")]
  FolderTreeError(#[source] FolderTreeError),
  #[error("UUID Parse Error: {0}")]
  InvalidID(String, #[source] UuidParseError),
}

impl From<FolderTreeError> for GameError {
  fn from(error: FolderTreeError) -> Self { GameError::FolderTreeError(error) }
}

impl<'a> From<&'a str> for GameError {
  fn from(error: &'a str) -> Self { GameError::BuggyProgram(error.to_string()) }
}

/// Potential targets for an ability.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum PotentialTargets {
  CreatureIDs(Vec<CreatureID>),
  Points(Vec<Point3>),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct Ability {
  pub id: AbilityID,
  pub name: String,
  pub cost: Energy,
  pub action: Action,
  pub usable_ooc: bool,
}

impl DeriveKey for Ability {
  type KeyType = AbilityID;
  fn derive_key(&self) -> AbilityID { self.id }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Action {
  Creature { effect: CreatureEffect, target: CreatureTarget },
  SceneVolume { effect: SceneEffect, target: SceneTarget },
  // Multi will require DecidedTarget::Multi
  // also PotentialTargets::Multi(Vec<(String, PotentialTarget)>)
  // Multi(Vec<(String, Action)>),
}

/// A target specifier for actions that ultimately affect creatures.
/// This doesn't mean that the target *specifier* is always a `CreatureID`, but rather that
/// ultimately the target is resolved into one or more creatures which `CreatureEffect`s will be
/// applied to. For example, `LineFromActor` is specified by the client as a Point, but will
/// affect the creatures in that line.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum CreatureTarget {
  Melee,
  Range(#[ts(type = "number")] u32units::Length),
  Actor,
  /// A *piercing* line, from an actor, which is always a fixed length.
  /// When targeted at a point, it will continue through any creatures up to *and past* that point,
  /// up to the maximum distance.
  LineFromActor {
    #[ts(type = "number")]
    distance: u32units::Length,
  },
  // LineFromActorToCreature{ distance: u32units::Length },
  SomeCreaturesInVolumeInRange {
    volume: Volume,
    /// maximum number of creatures that can be hit
    maximum: u8,
    #[ts(type = "number")]
    range: u32units::Length,
  },
  AllCreaturesInVolumeInRange {
    volume: Volume,
    #[ts(type = "number")]
    range: u32units::Length,
  },
}

/// A target specifier for actions that ultimately affect the scene by way of `SceneEffect`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum SceneTarget {
  /// RangedVolume is for applying an effect to the terrain, instead of to a creature.
  /// e.g., setting it on fire, or putting down a patch of oil, or filling a space with fog.
  RangedVolume {
    volume: Volume,
    #[ts(type = "number")]
    range: u32units::Length,
  },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum SceneEffect {
  CreateVolumeCondition { duration: Duration, condition: Condition },
  // Another example of a SceneEffect would be DestroyTerrain or BuildTerrain
}

/// The target of an ability, as chosen at play-time by a player. Generally this falls into
/// "specific creature" targeting (`Melee` and `Range`) and "aoe" targeting (the others). The
/// parameters of these variants indicate the specific target creature or point that is being
/// targeted by the player.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum DecidedTarget {
  Creature(CreatureID),
  Creatures(Vec<CreatureID>),
  Actor,
  Point(Point3),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum CreatureEffect {
  // Interrupt,
  // Resurrect,
  ApplyCondition(Duration, Condition),
  Heal(Dice),
  Damage(Dice),
  MultiEffect(Vec<CreatureEffect>),
  GenerateEnergy(Energy),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Condition {
  RecurringEffect(Box<CreatureEffect>),
  Dead,
  Incapacitated,
  AddDamageBuff(HP),
  DoubleMaxMovement,
  // Make an ability temporarily available to a creature.
  ActivateAbility(AbilityID),
}

impl Condition {
  pub fn apply(&self, duration: Duration) -> AppliedCondition {
    AppliedCondition { remaining: duration, condition: self.clone() }
  }
}

/// Serializes as either "Interminate" or {"Duration": 0}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Duration {
  Interminate,
  Rounds(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct AppliedCondition {
  pub remaining: Duration,
  pub condition: Condition,
}

/// Volume describes a volume in 3d space at an implied origin point.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Volume {
  Sphere(#[ts(type = "number")] u32units::Length),
  Line {
    vector: Point3, // this Point3 is used as a relative offset, not from 0,0,0
  },
  VerticalCylinder {
    #[ts(type = "number")]
    radius: u32units::Length,
    #[ts(type = "number")]
    height: u32units::Length,
  },
  // An Axis-Aligned Bounding Box, origin at top-left,
  // with x going east, y going south, and z going up.
  AABB(AABB),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct AbilityStatus {
  pub ability_id: AbilityID,
  pub cooldown: u8,
}

impl DeriveKey for AbilityStatus {
  type KeyType = AbilityID;
  fn derive_key(&self) -> AbilityID { self.ability_id }
}

/// A creature class, e.g. rogue, mage, warrior
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct Class {
  pub id: ClassID,
  pub name: String,
  /// A list of abilities that this class can use.
  pub abilities: Vec<AbilityID>,
  /// A list of conditions which will be *permanently* applied to any creature in this class.
  pub conditions: Vec<Condition>,
  /// An SVG-compatible color specifier
  pub color: Color,
}

impl DeriveKey for Class {
  type KeyType = ClassID;
  fn derive_key(&self) -> ClassID { self.id }
}

/// A specification for creating a new creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct CreatureCreation {
  pub name: String,
  pub class: ClassID,
  pub portrait_url: String,
  #[serde(default)]
  pub icon_url: String,
  pub note: String,
  #[serde(default)]
  pub bio: String,
  pub initiative: Dice,
  pub size: AABB,
}

/// A Creature.
///
/// A very important thing about how we deal with creatures is that whenever we change
/// a creature, we get back both a new creature *and* a log of all things that happened to that
/// creature. That log is deterministic and complete enough for us to replay it on a snapshot of a
/// creature and get an identical creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature {
  pub id: CreatureID,
  pub name: String,
  pub speed: u32units::Length,
  pub max_energy: Energy,
  pub cur_energy: Energy,
  pub abilities: IndexedHashMap<AbilityStatus>,
  pub class: ClassID,
  pub max_health: HP,
  pub cur_health: HP,
  pub conditions: HashMap<ConditionID, AppliedCondition>,
  pub note: String,
  #[serde(default)]
  pub bio: String,
  pub portrait_url: String,
  #[serde(default)]
  pub icon_url: String,
  pub attributes: HashMap<AttrID, SkillLevel>,
  pub initiative: Dice,
  pub size: AABB,
  #[serde(default)]
  pub inventory: Inventory,
}

/// A definition of an Item, which can be referenced by creatures' inventories.
#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct Item {
  pub id: ItemID,
  pub name: String,
}

impl DeriveKey for Item {
  type KeyType = ItemID;
  fn derive_key(&self) -> Self::KeyType { self.id }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct AttrID(pub String);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct Combat {
  pub scene: SceneID,
  #[ts(type = "NonEmpty")]
  pub creatures: nonempty::NonEmptyWithCursor<(CreatureID, i16)>,
  #[ts(type = "number")]
  pub movement_used: u32units::Length,
}

impl DeriveKey for Creature {
  type KeyType = CreatureID;
  fn derive_key(&self) -> CreatureID { self.id }
}

#[derive(Clone, Default, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct Game {
  pub current_combat: Option<Combat>,
  #[ts(type = "GameAbilities")]
  pub abilities: IndexedHashMap<Ability>,
  #[ts(type = "GameCreatures")]
  pub creatures: IndexedHashMap<Creature>,
  #[ts(type = "GameClasses")]
  pub classes: IndexedHashMap<Class>,
  pub tile_system: TileSystem,
  #[ts(type = "GameScenes")]
  pub scenes: IndexedHashMap<Scene>,
  #[serde(default)]
  #[ts(type = "GameItems")]
  pub items: IndexedHashMap<Item>,
  pub campaign: FolderTree<Folder>,
  #[serde(default)]
  #[ts(type = "GamePlayers")]
  pub players: IndexedHashMap<Player>,
  // The "active scene" determines which scene has mechanical effect as far as game simulation
  // goes.
  #[serde(default)]
  pub active_scene: Option<SceneID>,
}

pub struct Runtime {
  pub app: App,
  pub world: Option<CollisionWorld>,
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
  pub current_game: Game,
  pub snapshots: VecDeque<(Game, Vec<GameLog>)>,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct Player {
  pub player_id: PlayerID,
  pub scene: Option<SceneID>,
  pub creatures: HashSet<CreatureID>,
}

impl DeriveKey for Player {
  type KeyType = PlayerID;
  fn derive_key(&self) -> PlayerID { self.player_id.clone() }
}

impl Player {
  pub fn new(name: PlayerID) -> Player {
    Player { player_id: name, scene: None, creatures: HashSet::new() }
  }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct SceneCreation {
  pub name: String,
  pub background_image_url: String,
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (i32, i32),
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct Scene {
  pub id: SceneID,
  pub name: String,
  #[ts(type = "Terrain")]
  pub terrain: Vec<Point3>,
  #[ts(type = "Highlights")]
  pub highlights: HashMap<Point3, (Color, Visibility)>,
  #[ts(type = "Annotations")]
  pub annotations: HashMap<Point3, (String, Visibility)>,

  #[serde(default)]
  #[ts(type = "SceneHotspots")]
  pub scene_hotspots: HashMap<Point3, SceneID>,
  #[serde(default)]
  #[ts(type = "RelatedScenes")]
  pub related_scenes: HashSet<SceneID>,

  #[serde(default)]
  pub background_image_url: String,
  /// If this field is None, then the image will "float" fixed on the screen, instead of panning
  /// with the scene.
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (i32, i32),

  #[ts(type = "SceneCreatures")]
  pub creatures: HashMap<CreatureID, (Point3, Visibility)>,
  #[ts(type = "SceneAttributeChecks")]
  pub attribute_checks: HashMap<String, AttributeCheck>,
  #[serde(default)]
  #[ts(type = "SceneInventory")]
  pub inventory: Inventory,
  #[serde(default)]
  #[ts(type = "SceneVolumeConditions")]
  pub volume_conditions: HashMap<ConditionID, VolumeCondition>,

  /// "Focused" creatures are those which have their portraits rendered over the scene
  /// background
  #[serde(default)]
  #[ts(type = "SceneFocusedCreatures")]
  pub focused_creatures: Vec<CreatureID>,
}

pub type CollisionWorld = ::ncollide3d::world::CollisionWorld<f64, CollisionData>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum CollisionData {
  Creature(CreatureID),
  ConditionVolume(ConditionID),
  // BlockedTerrain ????
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct VolumeCondition {
  pub point: Point3,
  pub volume: Volume,
  pub remaining: Duration,
  pub condition: Condition,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct AttributeCheck {
  pub reliable: bool,
  pub attr: AttrID,
  pub target: SkillLevel,
}

impl DeriveKey for Scene {
  type KeyType = SceneID;
  fn derive_key(&self) -> SceneID { self.id }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub enum Visibility {
  GMOnly,
  AllPlayers,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynamicCombat<'game> {
  pub scene: &'game Scene,
  pub combat: &'game Combat,
  pub game: &'game Game,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynamicCreature<'creature, 'game: 'creature> {
  pub creature: &'creature Creature,
  pub game: &'game Game,
  pub class: &'game Class,
}

/// A newtype wrapper over App that has a special Serialize implementation, which includes extra
/// data dynamically as a convenience for the client.
pub struct RPIApp<'a>(pub &'a App);
/// Like `RPIApp` for Game.
pub struct RPIGame<'a>(pub &'a Game);

impl<'a> Serialize for RPIApp<'a> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("App", 2)?;
    let app = self.0;
    str.serialize_field("current_game", &RPIGame(&app.current_game))?;
    str.serialize_field("snapshots", &app.snapshots)?;
    str.end()
  }
}

impl<'a> Serialize for RPIGame<'a> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Game", 10)?;
    let game = self.0;

    str.serialize_field("current_combat", &game.current_combat)?;
    str.serialize_field("abilities", &game.abilities)?;
    str.serialize_field(
      "creatures",
      &game
        .creatures()
        .map_err(|e| S::Error::custom(format!("Oh no! Couldn't serialize creatures!? {:?}", e)))?,
    )?;
    str.serialize_field("classes", &game.classes)?;
    str.serialize_field("tile_system", &game.tile_system)?;
    str.serialize_field("scenes", &game.scenes)?;
    str.serialize_field("campaign", &game.campaign)?;
    str.serialize_field("items", &game.items)?;
    str.serialize_field("players", &game.players)?;
    str.end()
  }
}

impl<'creature, 'game: 'creature> Serialize for DynamicCreature<'creature, 'game> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Creature", 21)?;
    let creat = &self.creature;
    str.serialize_field("id", &creat.id)?;
    str.serialize_field("name", &creat.name)?;
    str.serialize_field("note", &creat.note)?;
    str.serialize_field("bio", &creat.bio)?;
    str.serialize_field("portrait_url", &creat.portrait_url)?;
    str.serialize_field("icon_url", &creat.icon_url)?;
    str.serialize_field("speed", &self.speed())?;
    str.serialize_field("max_energy", &creat.max_energy)?;
    str.serialize_field("cur_energy", &creat.cur_energy)?;
    str.serialize_field("abilities", &self.ability_statuses())?;
    str.serialize_field("class", &creat.class)?;
    str.serialize_field("max_health", &creat.max_health)?;
    str.serialize_field("cur_health", &creat.cur_health)?;
    str.serialize_field("own_conditions", self.own_conditions())?;
    str.serialize_field("volume_conditions", &self.volume_conditions())?;
    str.serialize_field("attributes", &creat.attributes)?;
    str.serialize_field("can_act", &self.can_act())?;
    str.serialize_field("can_move", &self.can_move())?;
    str.serialize_field("initiative", &creat.initiative)?;
    str.serialize_field("size", &creat.size)?;
    str.serialize_field("inventory", &creat.inventory)?;
    str.end()
  }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, TS, Default)]
pub enum TileSystem {
  /// Square grid with diagonal movement costing 1.41
  #[default]
  Realistic,
  /// Square grid with diagonal movement costing 1
  DnD,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, TS)]
pub struct Note {
  pub name: String,
  pub content: String,
}

impl DeriveKey for Note {
  type KeyType = String;
  fn derive_key(&self) -> String { self.name.clone() }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize, Eq, PartialEq, TS)]
#[ts(rename = "FolderNode")]
pub struct Folder {
  pub scenes: HashSet<SceneID>,
  pub creatures: HashSet<CreatureID>,
  #[ts(type = "Record<string, Note>")]
  pub notes: IndexedHashMap<Note>,
  #[serde(default)]
  pub items: HashSet<ItemID>,
  #[serde(default)]
  pub abilities: HashSet<AbilityID>,
  #[serde(default)]
  pub classes: HashSet<ClassID>,
}

impl Folder {
  pub fn new() -> Folder { Default::default() }
}

#[cfg(test)]
pub mod test {
  use crate::grid::test::*;
  use crate::types::*;
  use maplit::hashmap;
  use std::iter::FromIterator;

  use serde_json;
  use serde_yaml;
  pub fn uuid_0() -> Uuid { "00000000-0000-0000-0000-000000000000".parse().unwrap() }
  pub fn uuid_1() -> Uuid { "00000000-0000-0000-0000-000000000001".parse().unwrap() }
  pub fn uuid_2() -> Uuid { "00000000-0000-0000-0000-000000000002".parse().unwrap() }
  pub fn uuid_3() -> Uuid { "00000000-0000-0000-0000-000000000003".parse().unwrap() }
  pub fn uuid_4() -> Uuid { "00000000-0000-0000-0000-000000000004".parse().unwrap() }
  pub fn uuid_5() -> Uuid { "00000000-0000-0000-0000-000000000005".parse().unwrap() }
  pub fn cid_cleric() -> CreatureID { CreatureID(uuid_0()) }
  pub fn cid_ranger() -> CreatureID { CreatureID(uuid_1()) }
  pub fn cid_rogue() -> CreatureID { CreatureID(uuid_2()) }

  pub fn t_creature(name: &str, class: ClassID, init: i8) -> Creature {
    Creature::create(&CreatureCreation {
      name: name.to_string(),
      note: "".to_string(),
      bio: "".to_string(),
      class,
      portrait_url: "".to_string(),
      icon_url: "".to_string(),
      initiative: Dice::flat(init),
      size: AABB { x: u32cm(100), y: u32cm(100), z: u32cm(100) },
    })
  }

  pub fn t_rogue(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, classid_rogue(), 20) }
  }

  pub fn t_ranger(name: &str) -> Creature {
    Creature { id: cid_ranger(), ..t_creature(name, classid_ranger(), 10) }
  }

  pub fn t_cleric(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, classid_cleric(), 0) }
  }

  pub fn t_scene_id() -> SceneID { SceneID(uuid_3()) }

  pub fn t_scene() -> Scene {
    Scene {
      id: t_scene_id(),
      name: "Test Scene".to_string(),
      background_image_url: "".to_string(),
      background_image_offset: None,
      background_image_scale: (1, 1),
      terrain: huge_box(),
      highlights: HashMap::new(),
      annotations: HashMap::new(),

      scene_hotspots: HashMap::new(),
      related_scenes: HashSet::new(),

      attribute_checks: HashMap::new(),
      creatures: hashmap! {
        cid_rogue() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
        cid_cleric() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
        cid_ranger() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
      },
      inventory: HashMap::new(),
      volume_conditions: HashMap::new(),
      focused_creatures: vec![],
    }
  }

  pub fn app_cond(c: Condition, r: Duration) -> AppliedCondition {
    AppliedCondition { condition: c, remaining: r }
  }

  pub fn classid_rogue() -> ClassID { ClassID(uuid_0()) }
  pub fn classid_cleric() -> ClassID { ClassID(uuid_1()) }
  pub fn classid_ranger() -> ClassID { ClassID(uuid_2()) }

  pub fn abid_punch() -> AbilityID { AbilityID(uuid_0()) }
  pub fn abid_shoot() -> AbilityID { AbilityID(uuid_1()) }
  pub fn abid_heal() -> AbilityID { AbilityID(uuid_2()) }
  pub fn abid_fireball() -> AbilityID { AbilityID(uuid_3()) }
  pub fn abid_piercing_shot() -> AbilityID { AbilityID(uuid_4()) }
  pub fn abid_thorn_patch() -> AbilityID { AbilityID(uuid_5()) }

  pub fn t_punch() -> Ability {
    Ability {
      id: abid_punch(),
      name: "Punch".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Melee,
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_shoot() -> Ability {
    Ability {
      id: abid_shoot(),
      name: "Shoot".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Range(u32cm(500)),
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_heal() -> Ability {
    Ability {
      id: abid_heal(),
      name: "Heal".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Range(u32cm(500)),
        effect: CreatureEffect::Heal(Dice::flat(3)),
      },
    }
  }

  pub fn t_fireball() -> Ability {
    Ability {
      id: abid_fireball(),
      name: "Fireball".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::AllCreaturesInVolumeInRange {
          volume: Volume::Sphere(u32cm(1000)),
          range: u32cm(2000),
        },
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_piercing_shot() -> Ability {
    Ability {
      id: abid_piercing_shot(),
      name: "Piercing Shot".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::LineFromActor { distance: u32cm(1000) },
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_thorn_patch() -> Ability {
    Ability {
      id: abid_thorn_patch(),
      name: "Thorn Patch".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::SceneVolume {
        target: SceneTarget::RangedVolume {
          volume: Volume::Sphere(u32cm(200)),
          range: u32cm(1000),
        },
        effect: SceneEffect::CreateVolumeCondition {
          duration: Duration::Interminate,
          condition: Condition::RecurringEffect(Box::new(CreatureEffect::Damage(Dice::flat(3)))),
        },
      },
    }
  }

  pub fn t_abilities() -> IndexedHashMap<Ability> {
    IndexedHashMap::from_iter(vec![
      t_punch(),
      t_shoot(),
      t_heal(),
      t_fireball(),
      t_piercing_shot(),
      t_thorn_patch(),
    ])
  }

  #[test]
  fn serde_ids() {
    let id = abid_heal();
    let serialized = serde_yaml::to_string(&id).unwrap();
    assert_eq!(serialized, "---\n00000000-0000-0000-0000-000000000002\n");
    let deserialized = serde_yaml::from_str::<AbilityID>(&serialized).unwrap();
    assert_eq!(deserialized, id);
  }

  #[test]
  fn serde_condition_duration() {
    let cd = Duration::Interminate;
    assert_eq!(serde_json::to_string(&cd).unwrap(), "\"Interminate\"");
    let cd = Duration::Rounds(3);
    assert_eq!(serde_json::to_string(&cd).unwrap(), "{\"Rounds\":3}");
  }

  #[test]
  fn dice_plus() {
    let d = Dice::flat(1).plus(Dice::flat(1));
    assert_eq!(d.roll(), (vec![1, 1], 2));
  }

  #[test]
  fn dice_negative() {
    let d = Dice::flat(1).plus(Dice::flat(-5));
    assert_eq!(d.roll(), (vec![1, -5], -4));
  }

  #[test]
  fn serialize_hashmap_point3() {
    let p = Point3::new(0, 0, 0);
    let hm = hashmap! {p => 5};
    assert_eq!(serde_json::to_string(&hm).unwrap(), "{\"0/0/0\":5}");
  }
}
