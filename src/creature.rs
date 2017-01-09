use std::marker::PhantomData;
use std::cmp;

use odds::vec::VecExt;

use types::*;

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature<CreatureState> {
    // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
    id: CreatureID,
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

impl<A> Creature<A> {
    pub fn build(id: &str) -> CreatureBuilder {
        CreatureBuilder {
            id: CreatureID(id.to_string()),
            name: None,
            max_energy: None,
            cur_energy: None,
            abilities: vec![],
            max_health: None,
            cur_health: None,
            pos: None,
            conditions: vec![],
        }
    }

    fn into_other<B>(&self) -> Creature<B> {
        Creature::<B> {
            id: self.id.clone(),
            name: self.name.clone(),
            max_energy: self.max_energy,
            cur_energy: self.cur_energy,
            abilities: self.abilities.clone(),
            max_health: self.max_health,
            cur_health: self.cur_health,
            pos: self.pos,
            conditions: self.conditions.clone(),
            _p: PhantomData,
        }
    }

    fn apply_effect_mut(&mut self, effect: &Effect) {
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
                    self.apply_effect_mut(effect)
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

    fn tick_mut(&mut self) {
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
            self.apply_effect_mut(&eff);
        }
    }
}

pub struct CreatureBuilder {
    id: CreatureID,
    name: Option<String>,
    max_energy: Option<Energy>,
    cur_energy: Option<Energy>,
    abilities: Vec<AbilityID>,
    max_health: Option<u8>,
    cur_health: Option<u8>,
    pos: Option<Point3>,
    conditions: Vec<AppliedCondition>,
}

impl CreatureBuilder {
    pub fn build(self) -> Option<Creature<Able>> {
        if conditions_able(&self.conditions) {
            Some(Creature {
                id: self.id.clone(),
                name: self.name.unwrap_or(self.id.clone().0),
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
        } else {
            None
        }
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

fn conditions_able(conditions: &[AppliedCondition]) -> bool {
    !conditions.iter()
        .any(|&AppliedCondition { ref condition, .. }| {
            condition == &Condition::Incapacitated || condition == &Condition::Dead
        })
}

pub trait CreatureT {
    fn pos(&self) -> Point3;
    fn id(&self) -> CreatureID;
    fn set_pos(&self, Point3) -> CreatureVari;
    // These methods could feasibly return a different Creature<T> (e.g. Creature<Able> ->
    // Creature<Incap>)
    fn tick(&self) -> CreatureVari;
    fn apply_effect(&self, &Effect) -> CreatureVari;
    fn into_vari(&self) -> CreatureVari;
    /// Return true if a creature can act this turn (e.g. it's not dead or incapacitated)
    fn can_act(&self) -> bool;
    /// Check if a creature has the given ability.
    fn has_ability(&self, &AbilityID) -> bool;
}

impl<A> CreatureT for Creature<A>
    where Creature<A>: Clone
{
    fn can_act(&self) -> bool {
        conditions_able(&self.conditions)
    }

    fn has_ability(&self, ability_id: &AbilityID) -> bool {
        self.abilities
            .iter()
            .any(|&AbilityStatus { ability_id: ref abid, .. }| abid == ability_id)
    }

    fn pos(&self) -> Point3 {
        self.pos
    }
    fn id(&self) -> CreatureID {
        self.id.clone()
    }
    fn apply_effect(&self, effect: &Effect) -> CreatureVari {
        let mut newc = self.clone();
        newc.apply_effect_mut(effect);
        newc.into_vari()
    }
    fn tick(&self) -> CreatureVari {
        let mut newc = self.clone();
        newc.tick_mut();
        newc.into_vari()
    }
    fn set_pos(&self, pt: Point3) -> CreatureVari {
        let mut newc = self.clone();
        newc.pos = pt;
        newc.into_vari()
    }
    fn into_vari(&self) -> CreatureVari {
        if conditions_able(&self.conditions) {
            CreatureVari::Able(self.clone().into_other())
        } else {
            CreatureVari::Incap(self.clone().into_other())
        }
    }
}

/// An enum wrapping all the valid types of `Creature`. See `CombatVari` for a better explanation.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum CreatureVari {
    Incap(Creature<Incap>),
    Casting(Creature<Casting>),
    Able(Creature<Able>),
}

impl CreatureT for CreatureVari {
    fn can_act(&self) -> bool {
        self.tref().can_act()
    }
    fn has_ability(&self, abid: &AbilityID) -> bool {
        self.tref().has_ability(abid)
    }
    fn pos(&self) -> Point3 {
        self.tref().pos()
    }
    fn id(&self) -> CreatureID {
        self.tref().id()
    }
    fn apply_effect(&self, effect: &Effect) -> CreatureVari {
        self.tref().apply_effect(effect)
    }
    fn tick(&self) -> CreatureVari {
        self.tref().tick()
    }
    fn set_pos(&self, pt: Point3) -> CreatureVari {
        self.tref().set_pos(pt)
    }
    fn into_vari(&self) -> CreatureVari {
        self.clone()
    }
}

impl CreatureVari {
    pub fn tref(&self) -> &CreatureT {
        match *self {
            CreatureVari::Incap(ref c) => c,
            CreatureVari::Able(ref c) => c,
            CreatureVari::Casting(ref c) => c,
        }
    }
}

#[cfg(test)]
pub fn t_creature() -> Creature<Able> {
    Creature::<Able>::build("Bob").build().unwrap()
}

#[cfg(test)]
pub fn t_rogue(name: &str) -> Creature<Able> {
    Creature::<Able>::build(name)
        .abilities(vec![abid("Test Ability")])
        .build()
        .unwrap()
}


#[test]
fn test_tick_and_expire_condition_remaining() {
    let mut c = t_creature();
    c.conditions = vec![app_cond(Condition::Dead, ConditionDuration::Duration(0)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Duration(5)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Interminate)];
    c.tick_mut();
    assert_eq!(c.conditions,
               vec![app_cond(Condition::Incapacitated, ConditionDuration::Duration(4)),
                    app_cond(Condition::Incapacitated, ConditionDuration::Interminate)]);
}

#[test]
fn test_recurring_effect() {
    let mut c = t_creature();
    c.conditions = vec![app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(1))),
                                 ConditionDuration::Duration(2))];
    c.tick_mut();
    assert_eq!(c.cur_health, 9);
    c.tick_mut();
    assert_eq!(c.cur_health, 8);
    c.tick_mut();
    assert_eq!(c.cur_health, 8);
}
