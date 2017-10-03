//! Simple types, with pure operations.

// Just disable large_enum_variant lints for now, since I'm not really that interested in fixing
// that for a while
#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

use std::collections::{HashMap, HashSet, VecDeque};

use rand;
use rand::distributions as dist;
use rand::distributions::IndependentSample;

use uuid::{ParseError as UuidParseError, Uuid};

use serde::ser;
use serde::ser::{Error as SerError, SerializeStruct};
use serde::de;

use nonempty;
use indexed::{DeriveKey, IndexedHashMap};
use foldertree::{FolderPath, FolderTree, FolderTreeError, FolderTreeErrorKind};

/// Point3 defines a 3d position in meters.
pub type VectorCM = (i32, i32, i32);
pub type Color = String;
pub type Inventory = HashMap<ItemID, u64>;
pub type Terrain = Vec<Point3>;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct Point3 {
  pub x: i16,
  pub y: i16,
  pub z: i16,
}

impl Point3 {
  pub fn new(x: i16, y: i16, z: i16) -> Point3 {
    Point3 { x, y, z }
  }
}

impl ::std::fmt::Display for Point3 {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "{}/{}/{}", self.x, self.y, self.z)
  }
}

impl ::std::str::FromStr for Point3 {
  type Err = GameError;
  fn from_str(path: &str) -> Result<Point3, GameError> {
    let segments: Vec<&str> = path.split('/').collect();
    if segments.len() != 3 {
      bail!("Bad Point3 syntax")
    }
    match (segments[0].parse::<i16>(), segments[1].parse::<i16>(), segments[2].parse::<i16>()) {
      (Ok(x), Ok(y), Ok(z)) => Ok(Point3::new(x, y, z)),
      _ => bail!("Bad Point3 syntax"),
    }
  }
}

impl ser::Serialize for Point3 {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&self.to_string())
  }
}

impl<'de> de::Deserialize<'de> for Point3 {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: de::Deserializer<'de>,
  {
    let st: String = de::Deserialize::deserialize(deserializer)?;
    match st.parse() {
      Ok(x) => Ok(x),
      Err(x) => Err(de::Error::invalid_value(
        de::Unexpected::Str(&st),
        &format!("Unknown error: {:?}", x).as_ref(),
      )),
    }
  }
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
pub struct AABB {
  pub x: u8,
  pub y: u8,
  pub z: u8,
}

impl AABB {
  pub fn get_max(&self, pt: Point3) -> Point3 {
    Point3::new(pt.x + i16::from(self.x), pt.y + i16::from(self.y), pt.z + i16::from(self.z))
  }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
pub enum Dice {
  Expr { num: u8, size: u8 },
  Plus(Box<Dice>, Box<Dice>),
  Flat(i8),
  BestOf(u8, Box<Dice>),
}

impl Dice {
  pub fn expr(n: u8, d: u8) -> Dice {
    Dice::Expr { num: n, size: d }
  }

  pub fn flat(val: i8) -> Dice {
    Dice::Flat(val)
  }

  pub fn plus(&self, d: Dice) -> Dice {
    Dice::Plus(Box::new(self.clone()), Box::new(d))
  }

  /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
  /// final result.
  pub fn roll(&self) -> (Vec<i16>, i32) {
    match *self {
      Dice::Expr { num, size } => {
        let mut intermediate = vec![];
        let mut result = 0i32;
        let range: dist::Range<i32> = dist::Range::new(1, i32::from(size) + 1);
        let mut rng = rand::thread_rng();
        for _ in 0..num {
          let val = range.ind_sample(&mut rng);
          result += val;
          intermediate.push(val as i16);
        }
        (intermediate, result)
      }
      Dice::Flat(val) => (vec![i16::from(val)], i32::from(val)),
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

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
pub struct HP(pub u8);
impl HP {
  pub fn saturating_add(self, other: Self) -> Self {
    HP(self.0.saturating_add(other.0))
  }
  pub fn saturating_sub(self, other: Self) -> Self {
    HP(self.0.saturating_sub(other.0))
  }
}

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
pub struct Energy(pub u8);
impl Energy {
  pub fn saturating_add(self, other: Self) -> Self {
    Energy(self.0.saturating_add(other.0))
  }
  pub fn saturating_sub(self, other: Self) -> Self {
    Energy(self.0.saturating_sub(other.0))
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct PlayerID(pub String);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct ConditionID(pub Uuid);
impl ConditionID {
  pub fn gen() -> ConditionID {
    ConditionID(Uuid::new_v4())
  }
  pub fn to_string(&self) -> String {
    self.0.hyphenated().to_string()
  }
}

impl ::std::str::FromStr for ConditionID {
  type Err = GameError;
  fn from_str(s: &str) -> Result<ConditionID, GameError> {
    Ok(ConditionID(Uuid::parse_str(s)?))
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct CreatureID(Uuid);
impl CreatureID {
  pub fn gen() -> CreatureID {
    CreatureID(Uuid::new_v4())
  }
  pub fn to_string(&self) -> String {
    self.0.hyphenated().to_string()
  }
}

impl ::std::str::FromStr for CreatureID {
  type Err = GameError;
  fn from_str(s: &str) -> Result<CreatureID, GameError> {
    Ok(CreatureID(Uuid::parse_str(s)?))
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct ItemID(Uuid);
impl ItemID {
  pub fn gen() -> ItemID {
    ItemID(Uuid::new_v4())
  }
  pub fn to_string(&self) -> String {
    self.0.hyphenated().to_string()
  }
}

impl ::std::str::FromStr for ItemID {
  type Err = GameError;
  fn from_str(s: &str) -> Result<ItemID, GameError> {
    Ok(ItemID(Uuid::parse_str(s)?))
  }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct SceneID(Uuid);
impl SceneID {
  pub fn gen() -> SceneID {
    SceneID(Uuid::new_v4())
  }
  pub fn to_string(&self) -> String {
    self.0.hyphenated().to_string()
  }
}

impl ::std::str::FromStr for SceneID {
  type Err = GameError;
  fn from_str(s: &str) -> Result<SceneID, GameError> {
    Ok(SceneID(Uuid::parse_str(s)?))
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct AbilityID(Uuid);
impl AbilityID {
  pub fn gen() -> AbilityID {
    AbilityID(Uuid::new_v4())
  }
  pub fn to_string(&self) -> String {
    self.0.to_string()
  }
}

impl ::std::str::FromStr for AbilityID {
  type Err = GameError;
  fn from_str(s: &str) -> Result<AbilityID, GameError> {
    Ok(AbilityID(Uuid::parse_str(s)?))
  }
}

/// Distance in centimeters.
#[derive(Add, Sub, Mul, Div, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Distance(pub u32);
impl Distance {
  /// Convert meters as a f32 to a Distance.
  pub fn from_meters(x: f32) -> Distance {
    Distance((x * 100.0) as u32)
  }
  pub fn saturating_add(self, other: Self) -> Self {
    Distance(self.0.saturating_add(other.0))
  }
  pub fn saturating_sub(self, other: Self) -> Self {
    Distance(self.0.saturating_sub(other.0))
  }
  pub fn to_meters(&self) -> f32 {
    self.0 as f32 / 100.0
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum FolderItemID {
  SceneID(SceneID),
  CreatureID(CreatureID),
  NoteID(String),
  ItemID(ItemID),
  AbilityID(AbilityID),
  SubfolderID(String),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
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
    100 - match difficulty_level.to_ord() - self.to_ord() {
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
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum InventoryOwner {
  Scene(SceneID),
  Creature(CreatureID),
}
impl InventoryOwner {
  pub fn not_found_error(&self) -> GameError {
    match *self {
      InventoryOwner::Scene(sid) => GameErrorEnum::SceneNotFound(sid).into(),
      InventoryOwner::Creature(cid) => GameErrorEnum::CreatureNotFound(cid.to_string()).into(),
    }
  }
}


/// Top-level commands that can be sent from a client to affect the state of the app.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameCommand {
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
  CopyFolderItem { source: FolderPath, item_id: FolderItemID, dest: FolderPath },
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
  TransferItem { from: InventoryOwner, to: InventoryOwner, item_id: ItemID, count: u64 },
  RemoveItem { owner: InventoryOwner, item_id: ItemID, count: u64 },
  SetItemCount { owner: InventoryOwner, item_id: ItemID, count: u64 },

  // ** Scene management **
  /// Create a Scene.
  CreateScene(FolderPath, SceneCreation),
  EditSceneDetails { scene_id: SceneID, details: SceneCreation },
  SetSceneCreatureVisibility { scene_id: SceneID, creature_id: CreatureID, visibility: Visibility },
  AddCreatureToScene { scene_id: SceneID, creature_id: CreatureID, visibility: Visibility },
  RemoveCreatureFromScene { scene_id: SceneID, creature_id: CreatureID },
  AddSceneChallenge { scene_id: SceneID, description: String, challenge: AttributeCheck },
  RemoveSceneChallenge { scene_id: SceneID, description: String },
  SetFocusedSceneCreatures { scene_id: SceneID, creatures: Vec<CreatureID> },
  // AddSceneVolumeCondition {
  //   scene_id: SceneID,
  //   point: Point3,
  //   volume: Volume,
  //   condition: Condition,
  //   duration: Duration,
  // },
  RemoveSceneVolumeCondition { scene_id: SceneID, condition_id: ConditionID },
  EditSceneTerrain { scene_id: SceneID, terrain: Vec<Point3> },
  EditSceneHighlights { scene_id: SceneID, highlights: HashMap<Point3, (Color, Visibility)> },
  EditSceneAnnotations { scene_id: SceneID, annotations: HashMap<Point3, (String, Visibility)> },

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
  EditCreatureDetails { creature_id: CreatureID, details: CreatureCreation },
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

/// A representation of state change in a Creature. All change to a Creature happens via these
/// values. Note that these represent *concrete* changes to the Creature, which will have
/// deterministic results.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CreatureLog {
  Damage(HP, Vec<i16>),
  Heal(HP, Vec<i16>),
  GenerateEnergy(Energy),
  ReduceEnergy(Energy),
  ApplyCondition(ConditionID, Duration, Condition),
  DecrementConditionRemaining(ConditionID),
  RemoveCondition(ConditionID),
}

// TODO: get rid of CombatLog, it's dumb... unless we ever support multiple Combats
/// Representation of state changes in a Combat. See `CreatureLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CombatLog {
  ConsumeMovement(Distance),
  ChangeCreatureInitiative(CreatureID, i16),
  EndTurn(CreatureID), // the end of this creature's turn
  ForceNextTurn,
  ForcePrevTurn,
  RerollInitiative(Vec<(CreatureID, i16)>),
}

pub fn creature_logs_into_game_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<GameLog> {
  ls.into_iter().map(|l| GameLog::CreatureLog(cid, l)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameLog {
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
  TransferItem { from: InventoryOwner, to: InventoryOwner, item_id: ItemID, count: u64 },
  RemoveItem { owner: InventoryOwner, item_id: ItemID, count: u64 },
  SetItemCount { owner: InventoryOwner, item_id: ItemID, count: u64 },

  CreateScene(FolderPath, Scene),
  EditScene(Scene),
  EditSceneDetails { scene_id: SceneID, details: SceneCreation },
  SetSceneCreatureVisibility { scene_id: SceneID, creature_id: CreatureID, visibility: Visibility },
  AddCreatureToScene { scene_id: SceneID, creature_id: CreatureID, visibility: Visibility },
  RemoveCreatureFromScene { scene_id: SceneID, creature_id: CreatureID },
  AddSceneChallenge { scene_id: SceneID, description: String, challenge: AttributeCheck },
  RemoveSceneChallenge { scene_id: SceneID, description: String },
  SetFocusedSceneCreatures { scene_id: SceneID, creatures: Vec<CreatureID> },
  RemoveSceneVolumeCondition { scene_id: SceneID, condition_id: ConditionID },

  EditSceneTerrain { scene_id: SceneID, terrain: Vec<Point3> },
  EditSceneHighlights { scene_id: SceneID, highlights: HashMap<Point3, (Color, Visibility)> },
  EditSceneAnnotations { scene_id: SceneID, annotations: HashMap<Point3, (String, Visibility)> },

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
  EditCreatureDetails { creature_id: CreatureID, details: CreatureCreation },
  AddCreatureToCombat(CreatureID, i16),
  RemoveCreatureFromCombat(CreatureID),
  /// Indexes into snapshots and logs.
  Rollback(usize, usize),
}

pub fn combat_logs_into_game_logs(ls: Vec<CombatLog>) -> Vec<GameLog> {
  ls.into_iter().map(GameLog::CombatLog).collect()
}

error_chain! {
  types { GameError, GameErrorEnum, GameErrorResultExt; }

  links {
    FolderTreeError(FolderTreeError, FolderTreeErrorKind);
  }

  foreign_links {
    UUIDParseError(UuidParseError);
  }

  errors {
    AttributeNotFound(cid: CreatureID, attrid: AttrID) {
      description("A Creature does not have the supplied Attribute")
      display("The Creature with ID {} does not have the attribute {}", cid.to_string(), attrid.0)
    }
    AbilityAlreadyExists(abid: AbilityID) {
      description("An Ability with the given ID already exists")
      display("The ability with ID {} already exists", abid.to_string())
    }
    CreatureAlreadyExists(cid: CreatureID) {
      description("A Creature with the given ID already exists")
      display("The creature with ID {} already exists", cid.to_string())
    }
    ItemAlreadyExists(iid: ItemID) {
      description("An Item already exists.")
      display("The Item {} already exists", iid.0)
    }
    ItemNotFound(iid: ItemID) {
      description("An Item couldn't be found.")
      display("The Item {} couldn't be found.", iid.0)
    }
    SceneAlreadyExists(scene: SceneID) {
      description("A scene already exists.")
      display("The scene {} already exists", scene.0)
    }
    SceneNotFound(scene: SceneID) {
      description("A scene wasn't found")
      display("The scene '{}' wasn't found", scene.0)
    }
    SceneInUse(scene: SceneID) {
      description("The scene can't be deleted because it's in use (likely because it's in combat).")
      display("The scene {} is in use (by combat, probably).", scene.0)
    }
    IDTooLong(id: String) {
      description("An identifier was too long.")
      display("The identifier '{}' is too long.", id)
    }
    ConditionNotFound(id: ConditionID) {
      description("A condition wasn't found.")
      display("The condition with ID {:?} wasn't found.", id)
    }
    InvalidCommand(cmd: GameCommand) {
      description("The supplied GameCommand is not valid in the current state.")
      display("Cannot process {:?} in this state.", cmd)
    }
    ClassNotFound(cls: String) {
      description("A class wasn't found.")
      display("The class {} was not found.", cls)
    }
    NoAbility(abid: AbilityID) {
      description("An ability wasn't found.")
      display("The ability with ID {} wasn't found.", abid.to_string())
    }
    CombatMustHaveCreatures {
      description("Combat can't be started without creatures.")
      display("Creatures must be supplied when starting a combat.")
    }
    MustRerollAtStartOfRound {
      description("RerollInitiative can only be invoked at the beginning of a round.")
      display("RerollInitiative can only be invoked at the beginning of a roud.")
    }
    CreatureLacksAbility(cid: CreatureID, abid: AbilityID) {
      description("A creature cannot use the supplied ability.")
      display("The creature with ID {} does not have the ability {}",
              cid.to_string(), abid.to_string())
    }
    CreatureNotFound(id: String) {
      description("A creature with the supplied ID could not be found.")
      display("The creature with ID {} could not be found.", id)
    }
    InvalidTarget(cid: CreatureID) {
      description("The specified creature is not a valid target.")
      display("Creature with ID {} is not a valid target.", cid.to_string())
    }
    InvalidTargetForTargetSpec(tspec: CreatureTarget, dtarget: DecidedTarget) {
      description("The supplied DecidedTarget is not valid for the TargetSpec in use.")
      display("DecidedTarget {:?} is not valid for TargetSpec {:?}.", dtarget, tspec)
    }
    InvalidTargetForAction(action: Action, dtarget: DecidedTarget) {
      description("The supplied DecidedTarget is not valid for the Action in use.")
      display("DecidedTarget {:?} is not valid for Action {:?}.", dtarget, action)
    }
    CreatureOutOfRange(cid: CreatureID) {
      description("The specified creature is out of range.")
      display("Creature {} is out of range.", cid.to_string())
    }
    PointOutOfRange(pt: Point3) {
      description("The specified point is out of range.")
      display("Point {:?} is out of range.", pt)
    }
    BuggyProgram(msg: String) {
      description("There was an internal error that is caused by a broken assumption, \
                  indicating that this software is garbage.")
      display("There's a bug in the program: {}", msg)
    }
    NotInCombat {
      description("There is currently no combat when trying to do something combat-specific.")
      display("There is currently no combat.")
    }
    AlreadyInCombat(cid: CreatureID) {
      description("The specified creature is already in combat.")
      display("Creature {} is already in combat.", cid.to_string())
    }
    CannotMove(cid: CreatureID) {
      description("A creature cannot move.")
      display("Creature {} cannot be moved.", cid.to_string())
    }
    CannotAct(cid: CreatureID) {
      description("A creature cannot act.")
      display("Creature {} cannot act.", cid.to_string())
    }
    NoPathFound {
      description("A path can't be found.")
      display("A path can't be found.")
    }
    StepTooBig(from: Point3, to: Point3) {
      description("A step from one point to another is too large.")
      display("Can't step from {:?} to {:?}", from, to)
    }
    NotEnoughEnergy(nrg: Energy) {
      description("There is not enough energy to do something.")
      display("Not enough energy: {:?}", nrg)
    }
    PlayerAlreadyExists(pid: PlayerID) {
      description("The specified player ID is already registered.")
      display("Player ID {} is already registered.", pid.0)
    }
    PlayerNotFound(pid: PlayerID) {
      description("The specified player was not found.")
      display("Player ID {} was not found.", pid.0)
    }
    PlayerDoesntControlCreature(pid: PlayerID, cid: CreatureID) {
      description("The specified creature is not controlled by the current player.")
      display("Player ID {} does not control creature {}.", pid.0, cid.to_string())
    }
    HistoryNotFound(snap_idx: usize, log_idx: usize) {
      description("The requested history item was not found.")
      display("Couldn't find history item at snapshot {} log item {}", snap_idx, log_idx)
    }
    InitiativeOutOfBounds(idx: usize) {
      description("The initiative index is out of bound.")
      display("Initiative index {} is out of bounds.", idx)
    }
    FolderNotEmpty(path: FolderPath) {
      description("The user attempted to delete a folder when it wasn't empty.")
      display("The folder {} is not empty", path.to_string())
    }
    FolderItemNotFound(path: FolderPath, item: FolderItemID) {
      description("The given folder item was not found in the given folder path.")
      display("The folder {} does not contain item {:?}", path.to_string(), item)
    }
    NoteNotFound(path: FolderPath, name: String) {
      description("A note couldn't be found.")
      display("The note in '{}' named '{}' could not be found.", path.to_string(), name)
    }
    CannotLinkNotes(path: FolderPath, name: String) {
      description("Notes can't be linked or unlinked.")
      display("Notes can't be linked or unlinked. '{}' / '{}'", path.to_string(), name)
    }
  }
}

/// Potential targets for an ability.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum PotentialTargets {
  CreatureIDs(Vec<CreatureID>),
  Points(Vec<Point3>),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Ability {
  pub id: AbilityID,
  pub name: String,
  pub cost: Energy,
  pub action: Action,
  pub usable_ooc: bool,
}

impl DeriveKey for Ability {
  type KeyType = AbilityID;
  fn derive_key(&self) -> AbilityID {
    self.id.clone()
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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
/// applied to. For example, `LineFromActor` is specified as the client as a Point, but will
/// affect the creatures in that line.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum CreatureTarget {
  Melee,
  Range(Distance),
  Actor,
  /// A *piercing* line, from an actor, which is always a fixed length.
  /// When targeted at a point, it will continue through any creatures up to *and past* that point,
  /// up to the maximum distance.
  LineFromActor { distance: Distance },
  // LineFromActorToCreature{ distance: Distance },
  SomeCreaturesInVolumeInRange {
    volume: Volume,
    /// maximum number of creatures that can be hit
    maximum: u8,
    range: Distance,
  },
  AllCreaturesInVolumeInRange { volume: Volume, range: Distance },
}

/// A target specifier for actions that ultimately affect the scene by way of `SceneEffect`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum SceneTarget {
  /// RangedVolume is for applying an effect to the terrain, instead of to a creature.
  /// e.g., setting it on fire, or putting down a patch of oil, or filling a space with fog.
  RangedVolume { volume: Volume, range: Distance },
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum SceneEffect {
  CreateVolumeCondition { duration: Duration, condition: Condition },
  // Another example of a SceneEffect would be DestroyTerrain
}

/// The target of an ability, as chosen at play-time by a player. Generally this falls into
/// "specific creature" targeting (`Melee` and `Ranged`) and "aoe" targeting (the others). The
/// parameters of these variants indicate the specific target creature or point that is being
/// targeted by the player.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum DecidedTarget {
  Creature(CreatureID),
  Creatures(Vec<CreatureID>),
  Actor,
  Point(Point3),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum CreatureEffect {
  // Interrupt,
  // Resurrect,
  ApplyCondition(Duration, Condition),
  Heal(Dice),
  Damage(Dice),
  MultiEffect(Vec<CreatureEffect>),
  GenerateEnergy(Energy),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Condition {
  RecurringEffect(Box<CreatureEffect>),
  Dead,
  Incapacitated,
  AddDamageBuff(HP),
  DoubleMaxMovement,
  ActivateAbility(AbilityID),
}

impl Condition {
  pub fn apply(&self, duration: Duration) -> AppliedCondition {
    AppliedCondition { remaining: duration, condition: self.clone() }
  }
}

/// Serializes as either "Interminate" or {"Duration": 0}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Duration {
  Interminate,
  Rounds(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AppliedCondition {
  pub remaining: Duration,
  pub condition: Condition,
}

/// Volume describes a volume in 3d space at an implied origin point.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Volume {
  Sphere(Distance),
  Line { vector: VectorCM },
  VerticalCylinder { radius: Distance, height: Distance },
  // An Axis-Aligned Bounding Box, origin at top-left,
  // with x going east, y going south, and z going up.
  AABB(AABB),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
  pub ability_id: AbilityID,
  pub cooldown: u8,
}

impl DeriveKey for AbilityStatus {
  type KeyType = AbilityID;
  fn derive_key(&self) -> AbilityID {
    self.ability_id
  }
}

/// A creature class, e.g. rogue, mage, warrior
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Class {
  /// A list of abilities that this class can use.
  pub abilities: Vec<AbilityID>,
  /// A list of conditions which will be *permanently* applied to any creature in this class.
  pub conditions: Vec<Condition>,
  /// An SVG-compatible color specifier
  pub color: Color,
}

/// A specification for creating a new creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CreatureCreation {
  pub name: String,
  pub class: String,
  pub portrait_url: String,
  #[serde(default)] pub icon_url: String,
  pub note: String,
  #[serde(default)] pub bio: String,
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
  pub speed: Distance,
  pub max_energy: Energy,
  pub cur_energy: Energy,
  pub abilities: IndexedHashMap<AbilityStatus>,
  pub class: String,
  pub max_health: HP,
  pub cur_health: HP,
  pub conditions: HashMap<ConditionID, AppliedCondition>,
  pub note: String,
  #[serde(default)] pub bio: String,
  pub portrait_url: String,
  #[serde(default)] pub icon_url: String,
  pub attributes: HashMap<AttrID, SkillLevel>,
  pub initiative: Dice,
  pub size: AABB,
  #[serde(default)] pub inventory: Inventory,
}

/// A definition of an Item, which can be referenced by creatures' inventories.
#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Item {
  pub id: ItemID,
  pub name: String,
}

impl DeriveKey for Item {
  type KeyType = ItemID;
  fn derive_key(&self) -> Self::KeyType {
    self.id
  }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct AttrID(pub String);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat {
  pub scene: SceneID,
  pub creatures: nonempty::NonEmptyWithCursor<(CreatureID, i16)>,
  pub movement_used: Distance,
}

impl DeriveKey for Creature {
  type KeyType = CreatureID;
  fn derive_key(&self) -> CreatureID {
    self.id
  }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
  pub current_combat: Option<Combat>,
  pub abilities: IndexedHashMap<Ability>,
  pub creatures: IndexedHashMap<Creature>,
  pub classes: HashMap<String, Class>,
  pub tile_system: TileSystem,
  pub scenes: IndexedHashMap<Scene>,
  #[serde(default)] pub items: IndexedHashMap<Item>,
  pub campaign: FolderTree<Folder>,
  #[serde(default)] pub players: IndexedHashMap<Player>,
  // The "active scene" determines which scene has mechanical effect as far as game simulation
  // goes.
  #[serde(default)] pub active_scene: Option<SceneID>,
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Player {
  pub player_id: PlayerID,
  pub scene: Option<SceneID>,
  pub creatures: HashSet<CreatureID>,
}

impl DeriveKey for Player {
  type KeyType = PlayerID;
  fn derive_key(&self) -> PlayerID {
    self.player_id.clone()
  }
}

impl Player {
  pub fn new(name: PlayerID) -> Player {
    Player { player_id: name, scene: None, creatures: HashSet::new() }
  }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct SceneCreation {
  pub name: String,
  pub background_image_url: String,
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (i32, i32),
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Scene {
  pub id: SceneID,
  pub name: String,
  pub terrain: Vec<Point3>,
  pub highlights: HashMap<Point3, (Color, Visibility)>,
  pub annotations: HashMap<Point3, (String, Visibility)>,
  #[serde(default)] pub background_image_url: String,
  /// If this field is None, then the image will "float" fixed on the screen, instead of panning
  /// with the scene.
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (i32, i32),

  pub creatures: HashMap<CreatureID, (Point3, Visibility)>,
  pub attribute_checks: HashMap<String, AttributeCheck>,
  #[serde(default)] pub inventory: Inventory,
  #[serde(default)] pub volume_conditions: HashMap<ConditionID, VolumeCondition>,

  /// "Focused" creatures are those which have their portraits rendered over the scene
  /// background
  #[serde(default)]
  pub focused_creatures: Vec<CreatureID>,
}

pub type CollisionWorld = ::ncollide::world::CollisionWorld3<f32, CollisionData>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum CollisionData {
  Creature(CreatureID),
  ConditionVolume(ConditionID),
  // BlockedTerrain ????
}


#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct VolumeCondition {
  pub point: Point3,
  pub volume: Volume,
  pub remaining: Duration,
  pub condition: Condition,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct AttributeCheck {
  pub reliable: bool,
  pub attr: AttrID,
  pub target: SkillLevel,
}

impl DeriveKey for Scene {
  type KeyType = SceneID;
  fn derive_key(&self) -> SceneID {
    self.id
  }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
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

impl<'a> ser::Serialize for RPIApp<'a> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("App", 2)?;
    let app = self.0;
    str.serialize_field("current_game", &RPIGame(&app.current_game))?;
    str.serialize_field("snapshots", &app.snapshots)?;
    str.end()
  }
}

impl<'a> ser::Serialize for RPIGame<'a> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Game", 10)?;
    let game = self.0;

    str.serialize_field("current_combat", &game.current_combat)?;
    str.serialize_field("abilities", &game.abilities)?;
    str.serialize_field(
      "creatures",
      &game
        .creatures()
        .map_err(|e| S::Error::custom(&format!("Oh no! Couldn't serialize creatures!? {:?}", e)))?,
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

impl<'creature, 'game: 'creature> ser::Serialize for DynamicCreature<'creature, 'game> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum TileSystem {
  /// Square grid with diagonal movement costing 1.41
  Realistic,
  /// Square grid with diagonal movement costing 1
  DnD,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Note {
  pub name: String,
  pub content: String,
}

impl DeriveKey for Note {
  type KeyType = String;
  fn derive_key(&self) -> String {
    self.name.clone()
  }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Folder {
  pub scenes: HashSet<SceneID>,
  pub creatures: HashSet<CreatureID>,
  pub notes: IndexedHashMap<Note>,
  #[serde(default)] pub items: HashSet<ItemID>,
  #[serde(default)] pub abilities: HashSet<AbilityID>,
}

impl Folder {
  pub fn new() -> Folder {
    Default::default()
  }
}

#[cfg(test)]
pub mod test {
  use std::iter::FromIterator;
  use types::*;
  use grid::test::*;

  use serde_yaml;
  use serde_json;
  pub fn uuid_0() -> Uuid {
    "00000000-0000-0000-0000-000000000000".parse().unwrap()
  }
  pub fn uuid_1() -> Uuid {
    "00000000-0000-0000-0000-000000000001".parse().unwrap()
  }
  pub fn uuid_2() -> Uuid {
    "00000000-0000-0000-0000-000000000002".parse().unwrap()
  }
  pub fn uuid_3() -> Uuid {
    "00000000-0000-0000-0000-000000000003".parse().unwrap()
  }
  pub fn uuid_4() -> Uuid {
    "00000000-0000-0000-0000-000000000004".parse().unwrap()
  }
  pub fn uuid_5() -> Uuid {
    "00000000-0000-0000-0000-000000000005".parse().unwrap()
  }
  pub fn cid_cleric() -> CreatureID {
    CreatureID(uuid_0())
  }
  pub fn cid_ranger() -> CreatureID {
    CreatureID(uuid_1())
  }
  pub fn cid_rogue() -> CreatureID {
    CreatureID(uuid_2())
  }

  pub fn t_creature(name: &str, class: &str, init: i8) -> Creature {
    Creature::create(&CreatureCreation {
      name: name.to_string(),
      note: "".to_string(),
      bio: "".to_string(),
      class: class.to_string(),
      portrait_url: "".to_string(),
      icon_url: "".to_string(),
      initiative: Dice::flat(init),
      size: AABB { x: 1, y: 1, z: 1 },
    })
  }

  pub fn t_rogue(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, "rogue", 20) }
  }

  pub fn t_ranger(name: &str) -> Creature {
    Creature { id: cid_ranger(), ..t_creature(name, "ranger", 10) }
  }

  pub fn t_cleric(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, "cleric", 0) }
  }


  pub fn t_scene_id() -> SceneID {
    SceneID(uuid_3())
  }

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
      attribute_checks: HashMap::new(),
      creatures: hashmap!{
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

  pub fn abid_punch() -> AbilityID {
    AbilityID(uuid_0())
  }
  pub fn abid_shoot() -> AbilityID {
    AbilityID(uuid_1())
  }
  pub fn abid_heal() -> AbilityID {
    AbilityID(uuid_2())
  }
  pub fn abid_fireball() -> AbilityID {
    AbilityID(uuid_3())
  }
  pub fn abid_piercing_shot() -> AbilityID {
    AbilityID(uuid_4())
  }
  pub fn abid_thorn_patch() -> AbilityID {
    AbilityID(uuid_5())
  }

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
        target: CreatureTarget::Range(Distance::from_meters(5.0)),
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
        target: CreatureTarget::Range(Distance::from_meters(5.0)),
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
          volume: Volume::Sphere(Distance::from_meters(10.0)),
          range: Distance::from_meters(20.0),
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
        target: CreatureTarget::LineFromActor { distance: Distance::from_meters(10.0) },
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
          volume: Volume::Sphere(Distance(200)),
          range: Distance(1000),
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
    assert_eq!(
      serde_yaml::to_string(&id).unwrap(),
      "---\n\"00000000-0000-0000-0000-000000000002\""
    );
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
    let hm = hashmap!{p => 5};
    assert_eq!(serde_json::to_string(&hm).unwrap(), "{\"0/0/0\":5}");
  }
}
