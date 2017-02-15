//! Simple types, with pure operations.

use std::collections::{HashMap, VecDeque, HashSet};
use std::error::Error;
use std::fmt;
use string_wrapper::StringWrapper;

use nonempty;

/// Point3 defines a 3d position in meters.
pub type Point3 = (i16, i16, i16);

pub type MapName = String;
pub type Map = Vec<Point3>; // To be extended later. For now just a list of open voxels
pub type ConditionID = usize;

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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct CreatureID(StringWrapper<[u8; 64]>);
impl CreatureID {
    pub fn new(s: &str) -> Result<Self, GameError> {
        let sw =
            StringWrapper::from_str_safe(s)
                .ok_or_else(|| GameError::IDTooLong(s[..64].to_string()))?;
        Ok(CreatureID(sw))
    }
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[cfg(test)]
pub fn cid(s: &str) -> CreatureID {
    CreatureID::new(s).unwrap()
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct AbilityID(StringWrapper<[u8; 64]>);
impl AbilityID {
    pub fn new(s: &str) -> Result<Self, GameError> {
        let sw =
            StringWrapper::from_str_safe(s)
                .ok_or_else(|| GameError::IDTooLong(s[..64].to_string()))?;
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct AbilitySetID(StringWrapper<[u8; 64]>);
impl AbilitySetID {
    pub fn new(s: &str) -> Result<Self, GameError> {
        let sw =
            StringWrapper::from_str_safe(s).ok_or_else(|| GameError::IDTooLong(s[..64].to_string()))?;
        Ok(AbilitySetID(sw))
    }
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}

/// Distance in centimeters.
#[derive(Add, Sub, Mul, Div, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash,
         Serialize, Deserialize)]
pub struct Distance(pub u32);
impl Distance {
    /// Convert meters as a f32 to a Distance.
    pub fn new(x: f32) -> Distance {
        Distance((x * 100.0) as u32)
    }
}

/// Top-level commands that can be sent from a client to affect the state of the app.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameCommand {
    /// Select the map that should be used for pathing and collision detection
    SelectMap(MapName),
    /// Change the terrain data of a map
    EditMap(MapName, Map),
    /// Start a combat with the specified creatures.
    StartCombat(Vec<CreatureID>),
    /// Stop the current combat.
    StopCombat,
    /// Make the current creature use an ability.
    Act(AbilityID, DecidedTarget),
    /// Move out of combat. There must be a clear path according to the current loaded map.
    MoveOutOfCombat(CreatureID, Point3),
    /// Move to a point. There must be a clear path according to the current loaded map.
    Move(Point3),
    /// Create a new creature.
    CreateCreature(CreatureCreation),
    /// Remove a creature from the game entirely. Creature must not be in combat.
    RemoveCreature(CreatureID),
    /// Add a creature to combat. Combat must already be running; otherwise use `StartCombat`.
    AddCreatureToCombat(CreatureID),
    /// Remove a creature from combat. Combat must already be running.
    RemoveCreatureFromCombat(CreatureID),

    /// Register a player as available for controlling a creature.
    RegisterPlayer(PlayerID),
    /// Give control of a creature to a player.
    GiveCreaturesToPlayer(PlayerID, Vec<CreatureID>),
    /// Remove a player from the game, allowing all of their creatures to be given to other players.
    UnregisterPlayer(PlayerID),
    /// Remove control of a creature from a player.
    RemoveCreaturesFromPlayer(PlayerID, Vec<CreatureID>),

    // RetrieveFromInventory(ThingID),
    // StowInInventory(ThingID),
    /// End the current creature's turn.
    Done,
}

/// A representation of state change in a Creature. All change to a Creature happens via these
/// values. Note that these represent *concrete* changes to the Creature, which will have
/// deterministic results.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CreatureLog {
    Damage(HP),
    Heal(HP),
    GenerateEnergy(Energy),
    ReduceEnergy(Energy),
    ApplyCondition(ConditionID, ConditionDuration, Condition),
    DecrementConditionRemaining(ConditionID),
    RemoveCondition(ConditionID),
    PathCreature {
        path: Vec<Point3>,
        distance: Distance,
    },
}

/// Representation of state changes in a Combat. See `CreatureLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CombatLog {
    CreatureLog(CreatureID, CreatureLog),
    EndTurn(CreatureID), // the end of this creature's turn
}

pub fn creature_logs_into_combat_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<CombatLog> {
    ls.into_iter().map(|l| CombatLog::CreatureLog(cid.clone(), l)).collect()
}

pub fn creature_logs_into_game_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<GameLog> {
    ls.into_iter().map(|l| GameLog::CreatureLog(cid.clone(), l)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameLog {
    SelectMap(MapName),
    EditMap(MapName, Map),
    CombatLog(CombatLog),
    /// A creature log wrapped in a game log.
    /// Many of these actually go via CombatLog, since most creature modification happens inside of
    /// a combat context, but things like moving out of combat needs this.
    CreatureLog(CreatureID, CreatureLog),
    StartCombat(Vec<CreatureID>),
    StopCombat,
    CreateCreature(Creature),
    RemoveCreature(CreatureID),
    AddCreatureToCombat(CreatureID),
    RemoveCreatureFromCombat(CreatureID),
}

pub fn combat_logs_into_game_logs(ls: Vec<CombatLog>) -> Vec<GameLog> {
    ls.into_iter().map(|l| GameLog::CombatLog(l)).collect()
}

/// An error in P&T.
// TODO: look into using error-chain which hopefully gives us a nice way to hierarchicalize this.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameError {
    CreatureAlreadyExists(CreatureID),
    IDTooLong(String),
    ConditionNotFound(ConditionID),
    InvalidCommand(GameCommand),
    AbilitySetRequired,
    ClassNotFound(String),
    NoAbility(AbilityID),
    CombatMustHaveCreatures,
    CreatureLacksAbility(CreatureID, AbilityID),
    CreatureNotFound(CreatureID),
    InvalidTarget(CreatureID),
    InvalidTargetNoSense(CreatureID),
    CreatureOutOfRange(CreatureID),
    InvalidCreatureState,
    BuggyProgram(String),
    NotInCombat,
    AlreadyInCombat,
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
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Effect {
    // Interrupt,
    // Resurrect,
    ApplyCondition(ConditionDuration, Condition),
    Heal(HP),
    Damage(HP),
    MultiEffect(Vec<Effect>),
    GenerateEnergy(Energy),
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Condition {
    RecurringEffect(Box<Effect>),
    Dead,
    Incapacitated,
    AddDamageBuff(HP),
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
}

/// A specification for creating a new creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CreatureCreation {
    pub id: CreatureID,
    pub name: String,
    pub class: String,
    pub pos: Point3,
}

pub struct CreatureBuilder {
    pub id: String,
    pub name: Option<String>,
    pub class: String,
    pub pos: Option<Point3>,
    pub max_energy: Option<Energy>,
    pub cur_energy: Option<Energy>,
    pub abilities: Vec<AbilityID>,
    pub max_health: Option<HP>,
    pub cur_health: Option<HP>,
    pub conditions: Vec<AppliedCondition>,
    pub speed: Option<Distance>,
}

/// A Creature.
///
/// A very important thing about how we deal with creatures is that whenever we change
/// a creature, we get back both a new creature *and* a log of all things that happened to that
/// creature. That log is deterministic and complete enough for us to reply it on a snapshot of a
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
    pub pos: Point3,
    pub conditions: HashMap<ConditionID, AppliedCondition>,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat {
    // Since we serialize a whole history of combats to JSON, using Rc<Creature> pointless, because
    // after we load data back in, because serde doesn't (currently) have any way to know that
    // multiple Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<Creature>,
    pub movement_used: Distance,
    /// Points that the current creature can move to.
    /// This is only relevant in combat, since only in combat is movement limited.
    pub movement_options: Vec<Point3>,
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
    pub current_combat: Option<Combat>,
    pub abilities: HashMap<AbilityID, Ability>,
    pub creatures: HashMap<CreatureID, Creature>,
    pub maps: HashMap<MapName, Map>,
    pub current_map: Option<MapName>,
    pub classes: HashMap<String, Class>,
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    pub current_game: Game,
    pub snapshots: VecDeque<(Game, Vec<GameLog>)>,
    pub players: HashMap<PlayerID, HashSet<CreatureID>>,
}

#[cfg(test)]
pub mod test {
    use types::*;
    use std::iter::FromIterator;

    use serde_yaml;
    use serde_json;

    pub fn t_rogue_creation(name: &str) -> CreatureCreation {
        CreatureCreation {
            id: CreatureID::new(name).unwrap(),
            name: name.to_string(),
            class: "rogue".to_string(),
            pos: (0, 0, 0),
        }
    }

    pub fn t_cleric_creation(name: &str) -> CreatureCreation {
        CreatureCreation {
            id: CreatureID::new(name).unwrap(),
            name: name.to_string(),
            class: "cleric".to_string(),
            pos: (0, 0, 0),
        }
    }

    pub fn t_ranger_creation(name: &str) -> CreatureCreation {
        CreatureCreation {
            id: CreatureID::new(name).unwrap(),
            name: name.to_string(),
            class: "ranger".to_string(),
            pos: (0, 0, 0),
        }
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
            effects: vec![Effect::Damage(HP(3))],
        }
    }

    pub fn t_shoot() -> Ability {
        Ability {
            name: "Shoot".to_string(),
            target: TargetSpec::Range(Distance::new(5.0)),
            cost: Energy(0),
            effects: vec![Effect::Damage(HP(3))],
        }
    }

    pub fn t_heal() -> Ability {
        Ability {
            name: "Heal".to_string(),
            target: TargetSpec::Range(Distance::new(5.0)),
            cost: Energy(0),
            effects: vec![Effect::Heal(HP(3))],
        }
    }

    pub fn t_abilities() -> HashMap<AbilityID, Ability> {
        let punch = t_punch();
        let shoot = t_shoot();
        let heal = t_heal();
        HashMap::from_iter(vec![(abid("punch"), punch),
                                (abid("shoot"), shoot),
                                (abid("heal"), heal)])
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
