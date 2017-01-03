/// Core simulation types, all immutable.

use std::error::Error;
use std::fmt;
use std::cmp;

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
    }

    /// Private
    fn tick(&mut self) {
        for creature in self.creatures.iter_mut() {
            creature.tick();
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    InvalidAbility,
    InvalidTarget,
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
    pos: (u32, u32, u32),
    conditions: Vec<AppliedCondition>,
}

impl Creature {
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
            &Effect::ApplyCondition(duration, ref condition) => {
                self.conditions.push(AppliedCondition {
                    remaining: duration,
                    condition: condition.clone(),
                });
            }
        }
    }

    /// Note that this is private.
    fn tick(&mut self) {
        let mut effs = vec![];
        self.conditions.retain(|el| el.remaining > 0);

        for &mut AppliedCondition { ref condition, ref mut remaining } in
            self.conditions.iter_mut() {
            *remaining -= 1;
            match condition {
                &Condition::RecurringEffect(ref eff) => effs.push(eff.clone()),
                &Condition::Incapacitated |
                &Condition::Dead |
                &Condition::DamageBuff(_) => {}
            }
        }

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

#[test]
fn test_tick_and_expire_condition_remaining() {
    let mut c = Creature {
        name: "Bob".to_string(),
        abilities: vec![],
        max_energy: Energy(10),
        cur_energy: Energy(10),
        max_health: 10,
        cur_health: 10,
        pos: (0, 0, 0),
        conditions: vec![AppliedCondition {
                             condition: Condition::Dead,
                             remaining: 0,
                         },
                         AppliedCondition {
                             condition: Condition::Incapacitated,
                             remaining: 5,
                         }],
    };
    c.tick();
    assert_eq!(c.conditions,
               vec![AppliedCondition {
                        condition: Condition::Incapacitated,
                        remaining: 4,
                    }]);
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Ability {
    pub name: String,
    cost: Energy,
    effects: Vec<Effect>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Effect {
    // Interrupt,
    // Resurrect,
    ApplyCondition(u8, Condition),
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
pub struct AppliedCondition {
    remaining: u8,
    condition: Condition,
}


#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct AbilityID(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    ability_id: AbilityID,
    cooldown: u8,
}
