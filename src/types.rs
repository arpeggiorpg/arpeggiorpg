/// Core simulation types, all immutable.

use std::error::Error;
use std::fmt;
use std::cmp;

use odds::vec::VecExt;

use nonempty;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(u8);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
    // Since we serialize a whole history of Games to JSON, using Rc<Creature> pointless after we
    // load data back in, because serde doesn't (currently) have any way to know that multiple
    // Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<Creature>,
}

#[allow(dead_code)]
#[deprecated(since="0", note="Unhandled match case")]
fn unhandled(x: &str) {
    panic!("{}", x);
}

impl Game {
    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    /// Cause the current creature to act.
    pub fn act(&self, ability: &Ability, targets: Vec<usize>) -> Result<Game, GameError> {
        if self.current_creature().can_act() {
            // I could write this in an Actually Functional style, but I really don't care as long as
            // the function doesn't have side effects (and the type signature proves it!)
            let mut newgame = self.clone();
            // newgame.tick();
            for effect in ability.effects.iter() {
                for &tidx in targets.iter() {
                    let creature = newgame.creatures.get_mut(tidx).ok_or(GameError::InvalidTarget)?;
                    creature.apply_effect(effect);
                }
            }
            newgame.creatures.next_circle();
            newgame.tick();
            Ok(newgame)
        } else {
            Err(GameError::InvalidPlayerState)
        }
    }

    /// Private
    fn tick(&mut self) {
        for creature in self.creatures.iter_mut() {
            creature.tick();
        }
    }
}


#[cfg(test)]
fn t_ability() -> Ability {
    Ability {
        name: "Test Ability".to_string(),
        target: Target::Melee,
        cost: Energy(0),
        effects: vec![],
    }
}
#[test]
fn test_incap() {
    let mut c = t_creature();
    c.conditions = vec![app_cond(Condition::Incapacitated, ConditionDuration::Interminate)];
    let game = Game { creatures: nonempty::NonEmptyWithCursor::new(c) };
    assert_eq!(game.act(&t_ability(), vec![]),
               Err(GameError::InvalidPlayerState));

}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    InvalidAbility,
    InvalidTarget,
    InvalidPlayerState,
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature {
    // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
    name: String,
    max_energy: Energy,
    cur_energy: Energy,
    abilities: Vec<AbilityStatus>,
    max_health: u8,
    cur_health: u8,
    pos: Point3,
    conditions: Vec<AppliedCondition>,
}

impl Creature {
    /// Return true if a creature can act this turn (e.g. it's not dead or incapacitated)
    pub fn can_act(&self) -> bool {
        !self.conditions
            .iter()
            .any(|&AppliedCondition { ref condition, .. }| {
                condition == &Condition::Incapacitated || condition == &Condition::Dead
            })
    }

    /// Note that this function is private.
    fn apply_effect(&mut self, effect: &Effect) {
        match effect {
            &Effect::Damage(amt) => self.cur_health = self.cur_health.saturating_sub(amt),
            &Effect::Heal(amt) => {
                self.cur_health = cmp::min(self.max_health, self.cur_health.saturating_add(amt))
            }
            &Effect::GenerateEnergy(amt) => {
                self.cur_energy = Energy(cmp::min(self.max_energy.0,
                                                  self.cur_energy.0.saturating_add(amt.0)))
            }
            &Effect::MultiEffect(ref effects) => {
                for effect in effects {
                    self.apply_effect(&effect)
                }
            }
            &Effect::ApplyCondition(ref duration, ref condition) => {
                self.conditions.push(AppliedCondition {
                    remaining: duration.clone(),
                    condition: condition.clone(),
                });
            }
        }
    }

    /// Note that this is private.
    fn tick(&mut self) {
        let mut effs = vec![];
        self.conditions.retain_mut(|&mut AppliedCondition { ref condition, ref mut remaining }| {
            if let &mut ConditionDuration::Duration(k) = remaining {
                // this shouldn't happen normally, since we remove conditions as soon as they reach
                // remaining = 0, but handle it just in case
                if k <= 0 {
                    return false;
                }
            }
            match condition {
                &Condition::RecurringEffect(ref eff) => effs.push(eff.clone()),
                _ => {}
            }
            match remaining {
                &mut ConditionDuration::Interminate => true,
                &mut ConditionDuration::Duration(ref mut remaining) => {
                    *remaining -= 1;
                    remaining > &mut 0
                }
            }
        });

        for eff in effs {
            self.apply_effect(&eff);
        }
    }

    /// Check if a creature has the given ability.
    pub fn has_ability(&self, ability_id: &AbilityID) -> bool {
        self.abilities
            .iter()
            .any(|&AbilityStatus { ability_id: ref abid, cooldown: _ }| abid == ability_id)
    }
}

#[cfg(test)]
fn t_creature() -> Creature {
    Creature {
        name: "Bob".to_string(),
        abilities: vec![],
        max_energy: Energy(10),
        cur_energy: Energy(10),
        max_health: 10,
        cur_health: 10,
        pos: (0, 0, 0),
        conditions: vec![],
    }
}

#[cfg(test)]
fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
    AppliedCondition {
        condition: c,
        remaining: r,
    }
}

#[test]
fn test_tick_and_expire_condition_remaining() {
    let mut c = t_creature();
    c.conditions = vec![app_cond(Condition::Dead, ConditionDuration::Duration(0)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Duration(5)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Interminate)];
    c.tick();
    assert_eq!(c.conditions,
               vec![app_cond(Condition::Incapacitated, ConditionDuration::Duration(4)),
                    app_cond(Condition::Incapacitated, ConditionDuration::Interminate)]);
}

#[test]
fn test_recurring_effect() {
    let mut c = t_creature();
    c.conditions = vec![app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(1))),
                                 ConditionDuration::Duration(2))];
    c.tick();
    assert_eq!(c.cur_health, 9);
    c.tick();
    assert_eq!(c.cur_health, 8);
    c.tick();
    assert_eq!(c.cur_health, 8);
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Target {
    Melee,
    Range(u8),
    CircleWithinRange(u8, u8), // radius, distance
    ConeFromCaster(u8, u8), // distance, radians of angle of cone
    LineFromCaster(u8), // distance
}

type Point3 = (i16, i16, i16);

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
    target: Target,
    cost: Energy,
    effects: Vec<Effect>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Effect {
    // Interrupt,
    // Resurrect,
    ApplyCondition(ConditionDuration, Condition),
    Heal(u8),
    Damage(u8),
    MultiEffect(Vec<Effect>),
    GenerateEnergy(Energy),
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Condition {
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
    remaining: ConditionDuration,
    condition: Condition,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    ability_id: AbilityID,
    cooldown: u8,
}
