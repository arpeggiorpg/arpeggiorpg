//! Simple types, with pure operations.
use std::error::Error;
use std::fmt;
use smallstring::SmallString;

// aliases and newtypes
pub type Point3 = (i16, i16, i16);
pub type ConditionID = usize;

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize,
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

#[derive(Add, Sub, Mul, Div, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct CreatureID(SmallString);
impl CreatureID {
    pub fn new(s: &str) -> Self {
        CreatureID(SmallString::new(s))
    }
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[cfg(test)]
pub fn cid(s: &str) -> CreatureID {
    CreatureID::new(s)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(SmallString);
impl AbilityID {
    pub fn new(s: &str) -> Self {
        AbilityID(SmallString::new(s))
    }
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[cfg(test)]
pub fn abid(s: &str) -> AbilityID {
    AbilityID::new(s)
}

/// A type representing distance. The wrapped value is in centimeters, but should not normally be
/// accessed. Note that distances cannot be negative.
#[derive(Add, Sub, Mul, Div, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash,
         Serialize, Deserialize)]
pub struct Distance(pub u32);
impl Distance {
    /// Convert meters as a f32 to a Distance.
    pub fn new(x: f32) -> Distance {
        Distance((x * 100.0) as u32)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AppCommand {
    StartCombat(Vec<CreatureID>),
    StopCombat,
    Act(AbilityID, DecidedTarget),
    Move(Point3),
    // RetrieveFromInventory(ThingID),
    // StowInInventory(ThingID),
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
    RemoveCondition(ConditionID),
    MoveCreature(Point3),
}

/// Rerpesentation of state changes in a Combat. See `CreatureLog`.
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum CombatLog {
    CreatureLog(CreatureID, CreatureLog),
    EndTurn(CreatureID), // the end of this creature's turn
}

pub fn creature_logs_into_combat_logs(cid: CreatureID, ls: Vec<CreatureLog>) -> Vec<CombatLog> {
    ls.into_iter().map(|l| CombatLog::CreatureLog(cid.clone(), l)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum AppLog {
    CombatLog(CombatLog),
    StartCombat(Vec<CreatureID>),
    StopCombat,
}

pub fn combat_logs_into_app_logs(ls: Vec<CombatLog>) -> Vec<AppLog> {
    ls.into_iter().map(|l| AppLog::CombatLog(l)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    ConditionNotFound(ConditionID),
    InvalidCommand(AppCommand),
    NoAbility(AbilityID),
    CombatMustHaveCreatures,
    CreatureLacksAbility(CreatureID, AbilityID),
    CreatureNotFound(CreatureID),
    InvalidTarget(CreatureID),
    InvalidTargetNoSense(CreatureID),
    TargetOutOfRange,
    InvalidCreatureState,
    BuggyProgram(String),
    NotInCombat,
    AlreadyInCombat,
    CannotMove(CreatureID),
    CannotAct(CreatureID),
    NotFastEnough {
        creature: CreatureID,
        speed: Distance,
        from: Point3,
        to: Point3,
        distance: Distance,
    },
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ConditionDuration {
    Interminate,
    Duration(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AppliedCondition {
    pub id: ConditionID,
    pub remaining: ConditionDuration,
    pub condition: Condition,
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    pub ability_id: AbilityID,
    pub cooldown: u8,
}

#[cfg(test)]
pub mod test {
    use types::*;

    #[cfg(test)]
    pub fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
        AppliedCondition {
            id: 0,
            condition: c,
            remaining: r,
        }
    }

    #[cfg(test)]
    pub fn t_punch() -> Ability {
        Ability {
            name: "Punch".to_string(),
            target: TargetSpec::Melee,
            cost: Energy(0),
            effects: vec![Effect::Damage(HP(3))],
        }
    }

    #[cfg(test)]
    pub fn t_shoot() -> Ability {
        Ability {
            name: "Shoot".to_string(),
            target: TargetSpec::Range(Distance::new(5.0)),
            cost: Energy(0),
            effects: vec![Effect::Damage(HP(3))],
        }
    }

    #[cfg(test)]
    pub fn t_heal() -> Ability {
        Ability {
            name: "Heal".to_string(),
            target: TargetSpec::Range(Distance::new(5.0)),
            cost: Energy(0),
            effects: vec![Effect::Heal(HP(3))],
        }
    }
}
