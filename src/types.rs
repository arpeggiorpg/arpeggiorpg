//! Basic simulation types, with pure operations.
use std::error::Error;
use std::fmt;

// aliases and newtypes
pub type Point3 = (i16, i16, i16);
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(pub u8);
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct CreatureID(pub String);
#[cfg(test)]
pub fn cid(s: &str) -> CreatureID {
    CreatureID(s.to_string())
}
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(pub String);
#[cfg(test)]
pub fn abid(s: &str) -> AbilityID {
    AbilityID(s.to_string())
}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Distance(pub u32);
impl Distance {
    pub fn new(x: f32) -> Distance {
        Distance((x * 100.0) as u32)
    }
}

// A set of phantom types that are used as arguments to Creature, Combat, and App.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Incap;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Casting;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Able;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct NoCombat;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    NoAbility(AbilityID),
    CreatureLacksAbility(AbilityID),
    // CreatureNotFound(CreatureID),
    InvalidTarget(CreatureID),
    InvalidTargetNoSense(CreatureID),
    TargetOutOfRange,
    InvalidCreatureState,
    BuggyProgram(String),
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
    Range(Distance),
    CircleWithinRange(Distance, u8), // radius
    Cone(Distance, u8), // radians of angle of cone (should this be steradians? is it the same?)
    Line(Distance),
    LineToFirstHit(),
}

/// The target of an ability, as chosen at play-time by a player. Generally this falls into
/// "specific creature" targeting (`Melee` and `Ranged`) and "aoe" targeting (the others). The
/// paremeters of these variants indicate the specific target creature or point that is being
/// targeted by the player.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum DecidedTarget {
    Melee(CreatureID),
    Range(CreatureID),
    CircleWithinRange(Point3),
    Cone(u8, u8), // radians (oh shit this needs to be 3d!!!!)
    Line(Point3),
    LineToFirstHit(Point3),
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
    Heal(u8),
    Damage(u8),
    MultiEffect(Vec<Effect>),
    GenerateEnergy(Energy),
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Condition {
    RecurringEffect(Box<Effect>),
    Dead,
    Incapacitated,
    DamageBuff(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
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

#[cfg(test)]
pub fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
    AppliedCondition {
        condition: c,
        remaining: r,
    }
}

#[cfg(test)]
pub fn t_melee() -> Ability {
    Ability {
        name: "Test Ability".to_string(),
        target: TargetSpec::Melee,
        cost: Energy(0),
        effects: vec![],
    }
}

#[cfg(test)]
pub fn t_ranged() -> Ability {
    Ability {
        name: "Ranged Ability".to_string(),
        target: TargetSpec::Range(Distance::new(5.0)),
        cost: Energy(0),
        effects: vec![],
    }
}
