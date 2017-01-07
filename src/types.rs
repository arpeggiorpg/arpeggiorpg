//! Core simulation types, all immutable.
use std::marker::PhantomData;
use std::error::Error;
use std::fmt;
use std::cmp;

use odds::vec::VecExt;

use nonempty;

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(u8);

#[allow(dead_code)]
#[deprecated(since="0.0.0", note="Unhandled match case")]
fn unhandled(x: &str) {
    panic!("{}", x);
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Incap;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Casting;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Able;
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct NoCombat;

/// A `CombatVari` must be pattern-matched to determine which operations we can perform on behalf
/// of the current creature. Each variant contains a different type of `Combat`, and each of those
/// different types provide different methods for doing only what is possible. For example,
/// `CombatVari::Incap` wraps `Combat<Incap>`, which only has a `skip` method, since incapacitated
/// creatures cannot act, whereas `CombatVari::Able(Combat<Able>)` allows use of the `act` method.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum CombatVari {
    Incap(Combat<Incap>),
    Casting(Combat<Casting>),
    Able(Combat<Able>),
}


impl CombatVari {
    pub fn new(combatants: Vec<Creature<()>>) -> Option<CombatVari> {
        nonempty::NonEmptyWithCursor::from_vec(combatants).map(|ne| {
            Combat::<NoCombat> {
                    creatures: ne,
                    _p: PhantomData,
                }
                .into_combat_vari()
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat<CreatureState> {
    // Since we serialize a whole history of combats to JSON, using Rc<Creature> pointless after we
    // load data back in, because serde doesn't (currently) have any way to know that multiple
    // Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<Creature<()>>,
    _p: PhantomData<CreatureState>,
}

impl<CreatureState> Combat<CreatureState> {
    pub fn current_creature(&self) -> &Creature<()> {
        self.creatures.get_current()
    }

    fn tick(&mut self) {
        for creature in self.creatures.iter_mut() {
            creature.tick();
        }
    }

    /// Consume this game and wrap it in an `CombatVari`.
    pub fn into_combat_vari(self) -> CombatVari {
        if self.current_creature().can_act() {
            CombatVari::Able(Combat {
                creatures: self.creatures,
                _p: PhantomData,
            })
        } else {
            CombatVari::Incap(Combat {
                creatures: self.creatures,
                _p: PhantomData,
            })
        }
    }
}

impl Combat<Incap> {
    pub fn skip(&self) -> CombatVari {
        let mut newgame = self.clone();
        newgame.tick();
        newgame.into_combat_vari()
    }
}

impl Combat<Able> {
    /// Cause the current creature to act.
    pub fn act(&self, ability: &Ability, targets: Vec<usize>) -> Result<CombatVari, GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.clone();
        // newgame.tick();
        for effect in &ability.effects {
            for &tidx in &targets {
                let creature = newgame.creatures.get_mut(tidx).ok_or(GameError::InvalidTarget)?;
                creature.apply_effect(effect);
            }
        }
        newgame.creatures.next_circular();
        newgame.tick();
        Ok(newgame.into_combat_vari())
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature<CreatureState> {
    // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
    name: String,
    max_energy: Energy,
    cur_energy: Energy,
    abilities: Vec<AbilityStatus>,
    max_health: u8,
    cur_health: u8,
    pos: Point3,
    conditions: Vec<AppliedCondition>,
    _p: PhantomData<CreatureState>,
}

#[derive(Default)]
pub struct CreatureBuilder {
    name: String,
    max_energy: Option<Energy>,
    cur_energy: Option<Energy>,
    abilities: Vec<AbilityID>,
    max_health: Option<u8>,
    cur_health: Option<u8>,
    pos: Option<Point3>,
    conditions: Vec<AppliedCondition>,
}

impl CreatureBuilder {
    pub fn build(self) -> Option<Creature<()>> {
        Some(Creature {
            name: self.name,
            max_energy: self.max_energy.unwrap_or(Energy(10)),
            cur_energy: self.cur_energy.unwrap_or(Energy(10)),
            abilities: self.abilities
                .iter()
                .map(|ab| {
                    AbilityStatus {
                        ability_id: ab.clone(),
                        cooldown: 0,
                    }
                })
                .collect(),
            max_health: self.max_health.unwrap_or(10),
            cur_health: self.cur_health.unwrap_or(10),
            pos: self.pos.unwrap_or((0, 0, 0)),
            conditions: self.conditions,
            _p: PhantomData,
        })
    }
    pub fn max_energy(mut self, me: Energy) -> Self {
        self.max_energy = Some(me);
        self
    }
    pub fn cur_energy(mut self, ce: Energy) -> Self {
        self.cur_energy = Some(ce);
        self
    }
    pub fn abilities(mut self, abs: Vec<AbilityID>) -> Self {
        self.abilities = abs;
        self
    }
    pub fn max_health(mut self, mh: u8) -> Self {
        self.max_health = Some(mh);
        self
    }
    pub fn cur_health(mut self, ch: u8) -> Self {
        self.cur_health = Some(ch);
        self
    }
    pub fn pos(mut self, pos: Point3) -> Self {
        self.pos = Some(pos);
        self
    }
    pub fn conditions(mut self, conds: Vec<AppliedCondition>) -> Self {
        self.conditions = conds;
        self
    }
}

fn conditions_able(conditions: &Vec<AppliedCondition>) -> bool {
    !conditions.iter()
        .any(|&AppliedCondition { ref condition, .. }| {
            condition == &Condition::Incapacitated || condition == &Condition::Dead
        })
}

impl<A> Creature<A> {
    pub fn build(name: &str) -> CreatureBuilder {
        CreatureBuilder { name: name.to_string(), ..CreatureBuilder::default() }
    }
    /// Return true if a creature can act this turn (e.g. it's not dead or incapacitated)
    pub fn can_act(&self) -> bool {
        conditions_able(&self.conditions)
    }

    /// Note that this function is private.
    fn apply_effect(&mut self, effect: &Effect) {
        match *effect {
            Effect::Damage(amt) => self.cur_health = self.cur_health.saturating_sub(amt),
            Effect::Heal(amt) => {
                self.cur_health = cmp::min(self.max_health, self.cur_health.saturating_add(amt))
            }
            Effect::GenerateEnergy(amt) => {
                self.cur_energy = Energy(cmp::min(self.max_energy.0,
                                                  self.cur_energy.0.saturating_add(amt.0)))
            }
            Effect::MultiEffect(ref effects) => {
                for effect in effects {
                    self.apply_effect(effect)
                }
            }
            Effect::ApplyCondition(ref duration, ref condition) => {
                self.conditions.push(AppliedCondition {
                    remaining: duration.clone(),
                    condition: condition.clone(),
                });
            }
        }
    }

    fn tick(&mut self) {
        let mut effs = vec![];
        self.conditions.retain_mut(|&mut AppliedCondition { ref condition, ref mut remaining }| {
            if let ConditionDuration::Duration(k) = *remaining {
                // this shouldn't happen normally, since we remove conditions as soon as they reach
                // remaining = 0, but handle it just in case
                if k == 0 {
                    return false;
                }
            }
            if let Condition::RecurringEffect(ref eff) = *condition {
                effs.push(eff.clone())
            }
            match *remaining {
                ConditionDuration::Interminate => true,
                ConditionDuration::Duration(ref mut remaining) => {
                    *remaining -= 1;
                    *remaining > 0
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
            .any(|&AbilityStatus { ability_id: ref abid, .. }| abid == ability_id)
    }
}

#[cfg(test)]
pub fn t_creature() -> Creature<()> {
    Creature::<()>::build("Bob").build().unwrap()
}

#[cfg(test)]
pub fn app_cond(c: Condition, r: ConditionDuration) -> AppliedCondition {
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

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct CreatureID(pub String);
