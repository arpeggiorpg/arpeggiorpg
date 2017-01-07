//! Core simulation types, all immutable.
use std::marker::PhantomData;
use std::error::Error;
use std::fmt;

use nonempty;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(pub u8);

#[allow(dead_code)]
#[deprecated(since="0.0.0", note="Unhandled match case")]
fn unhandled(x: &str) {
    panic!("{}", x);
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

#[cfg(test)]
pub fn t_ability() -> Ability {
    Ability {
        name: "Test Ability".to_string(),
        target: Target::Melee,
        cost: Energy(0),
        effects: vec![],
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    InvalidAbility,
    InvalidTarget,
    InvalidCreatureState,
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

#[cfg(test)]
pub fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
    AppliedCondition {
        condition: c,
        remaining: r,
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Target {
    Melee,
    Range(u8),
    CircleWithinRange(u8, u8), // radius, distance
    ConeFromCaster(u8, u8), // distance, radians of angle of cone
    LineFromCaster(u8), // distance
}

pub type Point3 = (i16, i16, i16);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum DecidedTarget {
    Melee,
    Range(Point3),
    CircleWithinRange(Point3, u8), // radius
    ConeFromCaster(u8, u8), // distance, radians
    LineFromCaster(Point3),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Ability {
    pub name: String,
    pub target: Target,
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

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    pub ability_id: AbilityID,
    pub cooldown: u8,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct CreatureID(pub String);
