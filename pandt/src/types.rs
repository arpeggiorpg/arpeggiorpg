//! Simple types, with pure operations.

use std::collections::{HashMap, VecDeque, HashSet};
use std::error::Error;
use std::fmt;
use string_wrapper::StringWrapper;

use rand;
use rand::distributions as dist;
use rand::distributions::IndependentSample;

use uuid;

use serde::ser;
use serde::ser::{SerializeStruct, Error as SerError};

use nonempty;
use indexed::{DeriveKey, IndexedHashMap};
use foldertree::{FolderTree, FolderPath};

/// Point3 defines a 3d position in meters.
pub type Point3 = (i16, i16, i16);

pub type MapName = String;
pub type Map = Vec<Point3>; // To be extended later. For now just a list of open voxels
pub type ConditionID = usize;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
pub struct Dice {
  pub num: u8,
  pub size: u8,
}

impl Dice {
  pub fn flat(val: u8) -> Dice {
    Dice {
      num: val,
      size: 1,
    }
  }
  pub fn roll(&self) -> (Vec<u8>, u32) {
    let mut intermediate = vec![];
    let mut result = 0u32;
    let range: dist::Range<u8> = dist::Range::new(1, self.size + 1);
    let mut rng = rand::thread_rng();
    for _ in 0..self.num {
      let val = range.ind_sample(&mut rng);
      result += val as u32;
      intermediate.push(val);
    }
    (intermediate, result)
  }
}

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize,
         Deserialize)]
pub struct HP(pub u8);
impl HP {
  pub fn saturating_add(self, other: Self) -> Self {
    HP(self.0.saturating_add(other.0))
  }
  pub fn saturating_sub(self, other: Self) -> Self {
    HP(self.0.saturating_sub(other.0))
  }
}

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Serialize,
         Deserialize)]
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

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct SceneName(pub String);


#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct CreatureID(uuid::Uuid);
impl CreatureID {
  pub fn new() -> CreatureID {
    CreatureID(uuid::Uuid::new_v4())
  }
  pub fn from_str(s: &str) -> Result<CreatureID, GameError> {
    Ok(CreatureID(uuid::Uuid::parse_str(s).map_err(|_| GameError::CreatureNotFound(s.to_string()))?))
  }
  pub fn to_string(&self) -> String {
    self.0.hyphenated().to_string()
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct AbilityID(StringWrapper<[u8; 64]>);
impl AbilityID {
  pub fn new(s: &str) -> Result<Self, GameError> {
    let sw =
      StringWrapper::from_str_safe(s).ok_or_else(|| GameError::IDTooLong(s[..64].to_string()))?;
    Ok(AbilityID(sw))
  }
  pub fn to_string(&self) -> String {
    self.0.to_string()
  }
}

#[cfg(test)]
pub fn abid(s: &str) -> AbilityID {
  AbilityID::new(s).unwrap()
}


/// Distance in centimeters.
#[derive(Add, Sub, Mul, Div, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash,
         Serialize, Deserialize)]
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
}

/// Top-level commands that can be sent from a client to affect the state of the app.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameCommand {
  // ** Folder management **
  /// Create a folder, given segments leading to it.
  CreateFolder(FolderPath),
  /// Delete a folder.
  DeleteFolder(FolderPath),
  /// Link a Creature into a Folder.
  LinkFolderCreature(FolderPath, CreatureID),
  /// Unlink a Creature from a Folder.
  UnlinkFolderCreature(FolderPath, CreatureID),
  /// Link a Scene into a Folder.
  LinkFolderScene(FolderPath, SceneName),
  /// Unlink a Scene from a Folder.
  UnlinkFolderScene(FolderPath, SceneName),
  /// Create a Note inside of a Folder.
  CreateNote(FolderPath, Note),
  /// Delete a Note from a Folder.
  DeleteNote(FolderPath, String),

  // ** Scene management **
  /// Create a scene (or, if it already exists, change the existing one).
  EditScene(Scene),
  /// Delete a scene.
  DeleteScene(SceneName),

  // ** Map management **
  /// Change the terrain data of a map.
  EditMap(MapName, Map),

  // ** Combat management **
  /// Start a combat with the specified creatures.
  StartCombat(SceneName, Vec<CreatureID>),
  /// Stop the current combat.
  StopCombat,
  /// Add a creature to combat. Combat must already be running; otherwise use `StartCombat`.
  AddCreatureToCombat(CreatureID),
  /// Remove a creature from combat. Combat must already be running.
  RemoveCreatureFromCombat(CreatureID),
  /// Use an Ability out of combat.
  ActCreature(SceneName, CreatureID, AbilityID, DecidedTarget),
  /// Make the current creature use an ability.
  CombatAct(AbilityID, DecidedTarget),
  /// Move the current creature in combat to a point.
  /// There must be a clear path according to the current loaded map.
  PathCurrentCombatCreature(Point3),
  /// End the current creature's turn.
  Done,
  /// Modify a creature's order in the combat list.
  ChangeCreatureInitiative(CreatureID, usize),

  // ** Creature Manipulation **
  /// Create a new creature.
  CreateCreature(CreatureCreation, FolderPath),
  /// Assign a creature's position within a scene.
  SetCreaturePos(SceneName, CreatureID, Point3),
  /// Move a creature along a path within a scene.
  /// There must be a clear path according to the current loaded map. It doesn't matter whether
  /// the creature is in combat.
  PathCreature(SceneName, CreatureID, Point3),
  /// Set a note on a creature.
  SetCreatureNote(CreatureID, String),
  /// Remove a creature from the game entirely. Creature must not be in combat.
  RemoveCreature(CreatureID),

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
  SetPlayerScene(PlayerID, Option<SceneName>),

  /// Roll back to a specific snapshot + log index
  Rollback(usize, usize),
}

/// A representation of state change in a Creature. All change to a Creature happens via these
/// values. Note that these represent *concrete* changes to the Creature, which will have
/// deterministic results.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CreatureLog {
  Damage(HP, Vec<u8>),
  Heal(HP, Vec<u8>),
  GenerateEnergy(Energy),
  ReduceEnergy(Energy),
  ApplyCondition(ConditionID, ConditionDuration, Condition),
  DecrementConditionRemaining(ConditionID),
  RemoveCondition(ConditionID),
  SetNote(String),
}

/// Representation of state changes in a Combat. See `CreatureLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CombatLog {
  ConsumeMovement(Distance),
  ChangeCreatureInitiative(CreatureID, usize),
  EndTurn(CreatureID), // the end of this creature's turn
}

pub fn creature_logs_into_game_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<GameLog> {
  ls.into_iter().map(|l| GameLog::CreatureLog(cid.clone(), l)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameLog {
  // ** Folder Management **
  /// Create a folder, given segments leading to it.
  CreateFolder(FolderPath),
  DeleteFolder(FolderPath),
  LinkFolderCreature(FolderPath, CreatureID),
  UnlinkFolderCreature(FolderPath, CreatureID),
  LinkFolderScene(FolderPath, SceneName),
  UnlinkFolderScene(FolderPath, SceneName),
  CreateNote(FolderPath, Note),
  DeleteNote(FolderPath, String),

  EditScene(Scene),
  DeleteScene(SceneName),
  EditMap(MapName, Map),
  CombatLog(CombatLog),
  /// A creature log wrapped in a game log.
  /// Many of these actually go via CombatLog, since most creature modification happens inside of
  /// a combat context, but things like moving out of combat needs this.
  CreatureLog(CreatureID, CreatureLog),
  SetCreaturePos(SceneName, CreatureID, Point3),
  PathCreature(SceneName, CreatureID, Vec<Point3>),
  StartCombat(SceneName, Vec<CreatureID>),
  StopCombat,
  CreateCreature(Creature),
  RemoveCreature(CreatureID),
  AddCreatureToCombat(CreatureID),
  RemoveCreatureFromCombat(CreatureID),
  /// Indexes into snapshots and logs.
  Rollback(usize, usize),
}

pub fn combat_logs_into_game_logs(ls: Vec<CombatLog>) -> Vec<GameLog> {
  ls.into_iter().map(|l| GameLog::CombatLog(l)).collect()
}

/// An error in P&T.
// TODO: look into using error-chain which hopefully gives us a nice way to hierarchicalize this.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameError {
  CreatureAlreadyExists(CreatureID),
  SceneNotFound(SceneName),
  IDTooLong(String),
  ConditionNotFound(ConditionID),
  InvalidCommand(GameCommand),
  ClassNotFound(String),
  NoAbility(AbilityID),
  CombatMustHaveCreatures,
  CreatureLacksAbility(CreatureID, AbilityID),
  CreatureNotFound(String),
  InvalidTarget(CreatureID),
  InvalidTargetForTargetSpec(TargetSpec, DecidedTarget),
  CreatureOutOfRange(CreatureID),
  InvalidCreatureState,
  BuggyProgram(String),
  NotInCombat,
  AlreadyInCombat(CreatureID),
  CannotMove(CreatureID),
  CannotAct(CreatureID),
  NoPathFound,
  /// Returned when a step in a `Move` command was more than one cube away.
  StepTooBig { from: Point3, to: Point3 },
  MapNotFound(MapName),
  NotEnoughEnergy(Energy),
  PlayerAlreadyExists(PlayerID),
  PlayerNotFound(PlayerID),
  PlayerDoesntControlCreature(PlayerID, CreatureID),
  HistoryNotFound(usize, usize),
  InitiativeOutOfBounds(usize),
}

impl fmt::Display for GameError {
  fn fmt(&self, fmter: &mut fmt::Formatter) -> fmt::Result {
    write!(fmter, "{}", format!("{:?}", self))
  }
}

impl Error for GameError {
  fn description(&self) -> &str {
    "A Game Error occurred"
  }
}

/// A specification for what kind of targeting an ability uses. i.e., this describes the rules of
/// targeting for an ability definition, not the choice of a specific target during gameplay. See
/// `DecidedTarget` for that. The parameters of these variants indicate things like how far an
/// arrow can travel or what the radius of an AoE is.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TargetSpec {
  Melee,
  Range(Distance), /* CircleWithinRange(Distance, u8), // radius
                    * Cone(Distance, u8), // radians of angle of cone (should this be steradians? is it the same?)
                    * Line(Distance),
                    * LineToFirstHit(), */
  Actor,
}

/// The target of an ability, as chosen at play-time by a player. Generally this falls into
/// "specific creature" targeting (`Melee` and `Ranged`) and "aoe" targeting (the others). The
/// paremeters of these variants indicate the specific target creature or point that is being
/// targeted by the player.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum DecidedTarget {
  Melee(CreatureID),
  // MeleeArea(Point3) // creatures can try attacking a square when they can't directly target a
  // creature -- for example if they think an invisible creature is in the
  // square. This could also be useful for things like breaking down doors.
  Range(CreatureID), /* CircleWithinRange(Point3),
                      * Cone(Angle2d),
                      * Line(Point3),
                      * LineToFirstHit(Point3), */
  Actor,
}

/// Potential targets for an ability.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum PotentialTarget {
  CreatureID(CreatureID),
  Point(Point3),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Ability {
  pub name: String,
  pub target: TargetSpec,
  pub cost: Energy,
  pub effects: Vec<Effect>,
  pub usable_ooc: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Effect {
  // Interrupt,
  // Resurrect,
  ApplyCondition(ConditionDuration, Condition),
  Heal(Dice),
  Damage(Dice),
  MultiEffect(Vec<Effect>),
  GenerateEnergy(Energy),
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Condition {
  RecurringEffect(Box<Effect>),
  Dead,
  Incapacitated,
  AddDamageBuff(HP),
  DoubleMaxMovement,
  ActivateAbility(AbilityID),
}

impl Condition {
  pub fn apply(&self, duration: ConditionDuration) -> AppliedCondition {
    AppliedCondition {
      remaining: duration,
      condition: self.clone(),
    }
  }
}

/// Serializes as either "Interminate" or {"Duration": 0}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ConditionDuration {
  Interminate,
  Duration(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AppliedCondition {
  pub remaining: ConditionDuration,
  pub condition: Condition,
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
  pub ability_id: AbilityID,
  pub cooldown: u8,
}

/// A creature class, e.g. rogue, mage, warrior
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Class {
  /// A list of abilities that this class can use.
  pub abilities: Vec<AbilityID>,
  /// A list of conditions which will be *permanently* applied to any creature in this class.
  pub conditions: Vec<Condition>,
  /// An SVG-compatible color specifier
  pub color: String,
}

/// A specification for creating a new creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CreatureCreation {
  pub name: String,
  pub class: String,
  pub portrait_url: String,
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
  pub abilities: Vec<AbilityStatus>,
  pub class: String,
  pub max_health: HP,
  pub cur_health: HP,
  pub conditions: HashMap<ConditionID, AppliedCondition>,
  pub note: String,
  pub portrait_url: String,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat {
  pub scene: SceneName,
  pub creatures: nonempty::NonEmptyWithCursor<CreatureID>,
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
  pub abilities: HashMap<AbilityID, Ability>,
  pub creatures: HashMap<CreatureID, Creature>,
  pub maps: HashMap<MapName, Map>,
  pub classes: HashMap<String, Class>,
  pub tile_system: TileSystem,
  pub scenes: IndexedHashMap<Scene>,
  pub campaign: FolderTree<Folder>,
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
  pub current_game: Game,
  pub snapshots: VecDeque<(Game, Vec<GameLog>)>,
  pub players: IndexedHashMap<Player>,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Player {
  pub player_id: PlayerID,
  pub scene: Option<SceneName>,
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
    Player {
      player_id: name,
      scene: None,
      creatures: HashSet::new(),
    }
  }
}


#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Scene {
  pub name: SceneName,
  pub map: MapName,
  pub creatures: HashMap<CreatureID, Point3>,
}

impl DeriveKey for Scene {
  type KeyType = SceneName;
  fn derive_key(&self) -> SceneName {
    self.name.clone()
  }
}

impl Scene {
  pub fn get_pos(&self, creature_id: CreatureID) -> Result<Point3, GameError> {
    self.creatures
      .get(&creature_id)
      .map(|x| *x)
      .ok_or_else(|| GameError::CreatureNotFound(creature_id.to_string()))
  }

  pub fn set_pos(&self, cid: CreatureID, pt: Point3) -> Scene {
    let mut new = self.clone();
    new.creatures.insert(cid, pt);
    new
  }
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynamicCombat<'game> {
  pub scene: &'game Scene,
  pub map: &'game Map,
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
/// Like RPIApp for Game.
pub struct RPIGame<'a>(pub &'a Game);

impl<'a> ser::Serialize for RPIApp<'a> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("App", 3)?;
    let app = self.0;
    str.serialize_field("current_game", &RPIGame(&app.current_game))?;
    str.serialize_field("snapshots", &app.snapshots)?;
    str.serialize_field("players", &app.players)?;
    str.end()
  }
}

impl<'a> ser::Serialize for RPIGame<'a> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Game", 9)?;
    let game = self.0;

    str.serialize_field("current_combat", &game.current_combat)?;
    str.serialize_field("abilities", &game.abilities)?;
    str.serialize_field("creatures", &game.creatures().map_err(|e| S::Error::custom("Oh no!"))?)?;
    str.serialize_field("maps", &game.maps)?;
    str.serialize_field("classes", &game.classes)?;
    str.serialize_field("tile_system", &game.tile_system)?;
    str.serialize_field("scenes", &game.scenes)?;
    str.serialize_field("campaign", &game.campaign)?;
    str.end()
  }
}

impl<'creature, 'game: 'creature> ser::Serialize for DynamicCreature<'creature, 'game> {
  fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Creature", 16)?;
    let creat = &self.creature;
    str.serialize_field("id", &creat.id)?;
    str.serialize_field("name", &creat.name)?;
    str.serialize_field("note", &creat.note)?;
    str.serialize_field("portrait_url", &creat.portrait_url)?;
    str.serialize_field("speed", &self.speed())?;
    str.serialize_field("max_energy", &creat.max_energy)?;
    str.serialize_field("cur_energy", &creat.cur_energy)?;
    str.serialize_field("abilities", &self.ability_statuses())?;
    str.serialize_field("class", &creat.class)?;
    str.serialize_field("max_health", &creat.max_health)?;
    str.serialize_field("cur_health", &creat.cur_health)?;
    str.serialize_field("conditions", &creat.conditions)?;
    str.serialize_field("can_act", &self.can_act())?;
    str.serialize_field("can_move", &self.can_move())?;
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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Folder {
  pub scenes: HashSet<SceneName>,
  pub creatures: HashSet<CreatureID>,
  pub notes: IndexedHashMap<Note>,
}

impl Folder {
  pub fn new() -> Folder {
    Folder {
      scenes: HashSet::new(),
      creatures: HashSet::new(),
      notes: IndexedHashMap::new(),
    }
  }
}

#[cfg(test)]
pub mod test {
  use types::*;
  use std::iter::FromIterator;
  use uuid::Uuid;

  use serde_yaml;
  use serde_json;

  pub fn t_rogue_creation(name: &str) -> CreatureCreation {
    CreatureCreation {
      name: name.to_string(),
      class: "rogue".to_string(),
      portrait_url: "".to_string(),
    }
  }

  pub fn t_cleric_creation(name: &str) -> CreatureCreation {
    CreatureCreation {
      name: name.to_string(),
      class: "cleric".to_string(),
      portrait_url: "".to_string(),
    }
  }

  pub fn t_ranger_creation(name: &str) -> CreatureCreation {
    CreatureCreation {
      name: name.to_string(),
      class: "ranger".to_string(),
      portrait_url: "".to_string(),
    }
  }

  pub fn cid_cleric() -> CreatureID {
    CreatureID(Uuid::from_fields(0, 0, 0, &[0, 0, 0, 0, 0, 0, 0, 0]).unwrap())
  }
  pub fn cid_ranger() -> CreatureID {
    CreatureID(Uuid::from_fields(0, 0, 0, &[0, 0, 0, 0, 0, 0, 0, 1]).unwrap())
  }
  pub fn cid_rogue() -> CreatureID {
    CreatureID(Uuid::from_fields(0, 0, 0, &[0, 0, 0, 0, 0, 0, 0, 2]).unwrap())
  }

  pub fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
    AppliedCondition {
      condition: c,
      remaining: r,
    }
  }

  pub fn t_punch() -> Ability {
    Ability {
      name: "Punch".to_string(),
      target: TargetSpec::Melee,
      cost: Energy(0),
      usable_ooc: true,
      effects: vec![Effect::Damage(Dice::flat(3))],
    }
  }

  pub fn t_shoot() -> Ability {
    Ability {
      name: "Shoot".to_string(),
      target: TargetSpec::Range(Distance::from_meters(5.0)),
      cost: Energy(0),
      usable_ooc: true,
      effects: vec![Effect::Damage(Dice::flat(3))],
    }
  }

  pub fn t_heal() -> Ability {
    Ability {
      name: "Heal".to_string(),
      target: TargetSpec::Range(Distance::from_meters(5.0)),
      cost: Energy(0),
      usable_ooc: true,
      effects: vec![Effect::Heal(Dice::flat(3))],
    }
  }

  pub fn t_abilities() -> HashMap<AbilityID, Ability> {
    let punch = t_punch();
    let shoot = t_shoot();
    let heal = t_heal();
    HashMap::from_iter(vec![(abid("punch"), punch), (abid("shoot"), shoot), (abid("heal"), heal)])
  }

  #[test]
  fn serde_ids() {
    let id = abid("foobar");
    assert_eq!(serde_yaml::to_string(&id).unwrap(), "---\nfoobar");
  }

  #[test]
  fn serde_condition_duration() {
    let cd = ConditionDuration::Interminate;
    assert_eq!(serde_json::to_string(&cd).unwrap(), "\"Interminate\"");
    let cd = ConditionDuration::Duration(3);
    assert_eq!(serde_json::to_string(&cd).unwrap(), "{\"Duration\":3}");
  }
}
