//! Basic simulation types, with pure operations.
use std::error::Error;
use std::fmt;

// aliases and newtypes
pub type Point3 = (i16, i16, i16);
#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(pub u8);
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct CreatureID(pub String);
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(pub String);

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

/// A specification for what kind of targeting an ability uses. i.e., this describes the rules of
/// targeting for an ability definition, not the choice of a specific target during gameplay. See
/// `DecidedTarget` for that. The parameters of these variants indicate things like how far an
/// arrow can travel or what the radius of an AoE is.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Target {
    Melee,
    Range(u8),
    CircleWithinRange(u8, u8), // radius, distance
    Cone(u8, u8), // distance, radians of angle of cone
    Line(u8), // distance
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
    CircleWithinRange(Point3, u8), // radius
    Cone(u8, u8), // distance, radians
    Line(Point3),
    LineToFirstHit(Point3),
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
pub fn t_ability() -> Ability {
    Ability {
        name: "Test Ability".to_string(),
        target: Target::Melee,
        cost: Energy(0),
        effects: vec![],
    }
}
