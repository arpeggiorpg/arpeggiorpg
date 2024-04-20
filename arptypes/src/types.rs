use std::collections::{HashMap, HashSet};

use derive_more::{Add, Display, Div, Mul, Sub};
use foldertree::{FolderPath, FolderTree, FolderTreeError};
use indexed::{DeriveKey, IndexedHashMap};
use nonempty;
use num::Saturating;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use thiserror::Error;
use ts_rs::TS;
use uom::si::length::{centimeter, meter};
use uuid::{Error as UuidParseError, Uuid};

use crate::{GMCommand, GameLog};

pub type Color = String;
pub type Inventory = HashMap<ItemID, u64>;
pub type Terrain = Vec<Point3>;

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

#[derive(
  Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize, TS, Display,
)]
pub struct PlayerID(pub String);

#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct AttrID(pub String);

#[macro_export]
macro_rules! uuid_id {
  ($type: ident) => {
    #[derive(
      Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize, TS,
    )]
    pub struct $type(pub Uuid);
    impl $type {
      pub fn gen() -> $type { $type(Uuid::new_v4()) }
      pub fn to_string(&self) -> String { self.0.hyphenated().to_string() }
    }

    impl std::fmt::Display for $type {
      fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { self.0.fmt(f) }
    }

    impl ::std::str::FromStr for $type {
      type Err = $crate::types::GameError;
      fn from_str(s: &str) -> Result<$type, $crate::types::GameError> {
        Uuid::parse_str(s)
          .map_err(|e| $crate::types::GameError::InvalidID(s.to_string(), e))
          .map($type)
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

/// Serializes as either "Interminate" or {"Rounds": 0}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Duration {
  Interminate,
  Rounds(u8),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct SceneCreation {
  pub name: String,
  pub background_image_url: String,
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (f64, f64),
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub enum Visibility {
  GMOnly,
  AllPlayers,
}

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
  InvalidCommand(GMCommand),
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

/// Point3 holds a position in 3d space in decimeters
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
      return Err(GameError::BuggyProgram("Bad Point3 syntax".to_string()));
    }
    match (segments[0].parse::<i64>(), segments[1].parse::<i64>(), segments[2].parse::<i64>()) {
      (Ok(x), Ok(y), Ok(z)) => Ok(Point3::new(x, y, z)),
      _ => Err(GameError::BuggyProgram("Bad Point3 syntax".to_string())),
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct AbilityCreation {
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

/// A specification for creating or editing a class.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub struct ClassCreation {
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
/// creature and get an identical creature. See `Creature::change` and `Creature::change_with`.
///
/// Random note: Serialize and Deserialize on Creature are only for "secondary" representations of
/// Creatures like in GameLog::CreateCreature, and persistent storage of games. See DynamicCreature
/// and its Serialize impl for the good stuff.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
// I'm not calling this "Creature" in typescript just to emphasize that DynamicCreature is usually
// what you want; this is only serialized directly in GameLog or for storage.
#[ts(rename = "CreatureData")]
pub struct Creature {
  pub id: CreatureID,
  pub name: String,
  #[ts(type = "number")]
  pub speed: u32units::Length,
  // Things like "max_energy" (and maybe eventually things like "defense_score", if we have that
  // concept), should not be a a fixed number stored on the Creature. Instead, they should be
  // based on Conditions such as "ProvideMaxEnergy(Dice)" or "ProvideDefense(Dice)", and then the
  // max energy or defense of the creature would automatically select the highest value.
  pub max_energy: Energy,
  pub cur_energy: Energy,
  #[ts(type = "Record<AbilityID, AbilityStatus>")]
  pub abilities: IndexedHashMap<AbilityStatus>,
  pub class: ClassID,
  pub max_health: HP,
  pub cur_health: HP,
  #[ts(type = "CreatureConditions")]
  pub conditions: HashMap<ConditionID, AppliedCondition>,
  pub note: String,
  #[serde(default)]
  pub bio: String,
  pub portrait_url: String,
  #[serde(default)]
  pub icon_url: String,
  #[ts(type = "CreatureAttributes")]
  pub attributes: HashMap<AttrID, SkillLevel>,
  pub initiative: Dice,
  pub size: AABB,
  #[serde(default)]
  #[ts(type = "CreatureInventory")]
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

#[derive(Clone, Default, PartialEq, Debug, Serialize, Deserialize, TS)]
#[ts(rename = "GameData")]
pub struct Game {
  pub current_combat: Option<Combat>,
  #[ts(type = "GameAbilities")]
  pub abilities: IndexedHashMap<Ability>,
  #[ts(type = "GameCreaturesData")]
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

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize, TS)]
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
  pub background_image_scale: (f64, f64),

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

#[derive(Debug, Clone, PartialEq, TS, Serialize, Deserialize)]
pub struct ChangedGame {
  pub game: Game,
  pub logs: Vec<GameLog>,
}

/// Serde Serializer helpers
// These could probably store references instead of owned objects for some more efficiency, but I'm
// not sure if that would work on the client?

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize, TS)]
#[ts(rename = "Game")]
pub struct SerializedGame {
  pub current_combat: Option<Combat>,
  #[ts(type = "GameAbilities")]
  pub abilities: IndexedHashMap<Ability>,
  #[ts(type = "GameCreatures")]
  pub creatures: HashMap<CreatureID, SerializedCreature>,
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

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize, TS)]
#[ts(rename = "DynamicCreature")]
pub struct SerializedCreature {
  pub id: CreatureID,
  pub name: String,
  pub max_energy: Energy,
  pub cur_energy: Energy,
  pub class: ClassID,
  pub max_health: HP,
  pub cur_health: HP,
  pub note: String,
  #[serde(default)]
  pub bio: String,
  pub portrait_url: String,
  #[serde(default)]
  pub icon_url: String,
  #[ts(type = "CreatureAttributes")]
  pub attributes: HashMap<AttrID, SkillLevel>,
  pub initiative: Dice,
  pub size: AABB,
  #[serde(default)]
  #[ts(type = "CreatureInventory")]
  pub inventory: Inventory,
  #[ts(type = "CreatureConditions")]
  pub conditions: HashMap<ConditionID, AppliedCondition>,

  // overridden field
  #[ts(type = "Record<AbilityID, AbilityStatus>")]
  pub abilities: IndexedHashMap<AbilityStatus>,
  #[ts(type = "number")]
  pub speed: u32units::Length,

  // synthesized fields
  #[ts(type = "CreatureConditions")]
  pub own_conditions: HashMap<ConditionID, AppliedCondition>,
  #[ts(type = "CreatureConditions")]
  pub volume_conditions: HashMap<ConditionID, AppliedCondition>,
  pub can_act: bool,
  pub can_move: bool,
}
