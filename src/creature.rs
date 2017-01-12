use std::cmp;
use std::sync::atomic;
use std::sync::atomic::Ordering;

use odds::vec::VecExt;

use types::*;

/// A Creature.
///
/// A very important thing about how we deal with creatures is that whenever we change
/// a creature, we get back both a new creature *and* a log of all things that happened to that
/// creature. That log is deterministic and complete enough for us to reply it on a snapshot of a
/// creature and get an identical creature.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature {
    id: CreatureID,
    name: String,
    max_energy: Energy,
    cur_energy: Energy,
    abilities: Vec<AbilityStatus>,
    max_health: HP,
    cur_health: HP,
    pos: Point3,
    conditions: Vec<AppliedCondition>,
}

impl Creature {
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

    pub fn apply_log(&self, item: &CreatureLog) -> Result<Creature, GameError> {
        let mut new = self.clone();
        match *item {
            CreatureLog::Damage(ref dmg) => new.cur_health = new.cur_health.saturating_sub(*dmg),
            CreatureLog::Heal(ref dmg) => {
                new.cur_health = cmp::min(new.cur_health.saturating_add(*dmg), new.max_health)
            }
            CreatureLog::GenerateEnergy(ref nrg) => {
                new.cur_energy = cmp::min(new.cur_energy.saturating_add(*nrg), new.max_energy)
            }
            CreatureLog::ReduceEnergy(ref nrg) => {
                new.cur_energy = new.cur_energy.saturating_sub(*nrg)
            }
            CreatureLog::ApplyCondition(ref id, ref dur, ref con) => {
                new.conditions.push(AppliedCondition {
                    remaining: *dur,
                    condition: con.clone(),
                    id: *id,
                })
            }
            CreatureLog::RemoveCondition(ref id) => {
                let pos = new.conditions
                    .iter()
                    .position(|c| c.id == *id)
                    .ok_or(GameError::ConditionNotFound(*id))?;
                new.conditions.remove(pos);
            }
            CreatureLog::MoveCreature(ref pt) => new.pos = *pt,
        }
        Ok(new)
    }

    pub fn apply_effect(&self, effect: &Effect) -> Result<(Creature, Vec<CreatureLog>), GameError> {
        static CONDITION_ID: atomic::AtomicUsize = atomic::ATOMIC_USIZE_INIT;
        // it's unlikely we'll be able to rely on having a simple mapping of Effect to
        // Vec<CreatureLog> forever
        fn eff2log(effect: &Effect) -> Vec<CreatureLog> {
            match *effect {
                Effect::Damage(amt) => vec![CreatureLog::Damage(amt)],
                Effect::Heal(amt) => vec![CreatureLog::Heal(amt)],
                Effect::GenerateEnergy(amt) => vec![CreatureLog::GenerateEnergy(amt)],
                Effect::MultiEffect(ref effects) => effects.iter().flat_map(eff2log).collect(),
                Effect::ApplyCondition(ref duration, ref condition) => {
                    vec![CreatureLog::ApplyCondition(CONDITION_ID.fetch_add(1, Ordering::SeqCst),
                                                     duration.clone(),
                                                     condition.clone())]
                }
            }
        }
        let ops = eff2log(effect);
        let mut creature = self.clone();
        for op in &ops {
            creature = self.apply_log(op)?;
        }
        Ok((creature, ops))
    }

    pub fn tick(&self) -> Result<(Creature, Vec<CreatureLog>), GameError> {
        let mut new = self.clone();
        let mut effs = vec![];
        new.conditions.retain_mut(|&mut AppliedCondition { id: _, ref condition, ref mut remaining }| {
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

        let mut all_logs = vec![];
        for eff in effs {
            let res = new.apply_effect(&eff)?;
            new = res.0;
            all_logs.extend(res.1);
        }
        Ok((new, all_logs))
    }

    /// Return true if a creature can act this turn (e.g. it's not dead or incapacitated)
    pub fn can_act(&self) -> bool {
        conditions_able(&self.conditions)
    }

    /// Check if a creature has the given ability.
    pub fn has_ability(&self, ability_id: &AbilityID) -> bool {
        self.abilities
            .iter()
            .any(|&AbilityStatus { ability_id: ref abid, .. }| abid == ability_id)
    }

    pub fn pos(&self) -> Point3 {
        self.pos
    }
    pub fn id(&self) -> CreatureID {
        self.id.clone()
    }
    pub fn set_pos(&self, pt: Point3) -> Creature {
        Creature { pos: pt, ..self.clone() }
    }
}

pub struct CreatureBuilder {
    id: CreatureID,
    name: Option<String>,
    max_energy: Option<Energy>,
    cur_energy: Option<Energy>,
    abilities: Vec<AbilityID>,
    max_health: Option<HP>,
    cur_health: Option<HP>,
    pos: Option<Point3>,
    conditions: Vec<AppliedCondition>,
}

impl CreatureBuilder {
    pub fn build(self) -> Option<Creature> {
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
                max_health: self.max_health.unwrap_or(HP(10)),
                cur_health: self.cur_health.unwrap_or(HP(10)),
                pos: self.pos.unwrap_or((0, 0, 0)),
                conditions: self.conditions,
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
    pub fn max_health(mut self, mh: HP) -> Self {
        self.max_health = Some(mh);
        self
    }
    pub fn cur_health(mut self, ch: HP) -> Self {
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



#[cfg(test)]
pub mod test {
    use creature::*;
    use types::test::*;

    pub fn t_creature() -> Creature {
        Creature::build("Bob").build().unwrap()
    }

    pub fn t_rogue(name: &str) -> Creature {
        Creature::build(name)
            .abilities(vec![abid("punch")])
            .build()
            .unwrap()
    }

    pub fn t_ranger(name: &str) -> Creature {
        Creature::build(name)
            .abilities(vec![abid("shoot")])
            .build()
            .unwrap()
    }

    pub fn t_cleric(name: &str) -> Creature {
        Creature::build(name)
            .abilities(vec![abid("heal")])
            .build()
            .unwrap()
    }

    #[test]
    fn test_tick_and_expire_condition_remaining() {
        let mut c = t_creature();
        c.conditions = vec![app_cond(Condition::Dead, ConditionDuration::Duration(0)),
                            app_cond(Condition::Incapacitated, ConditionDuration::Duration(5)),
                            app_cond(Condition::Incapacitated, ConditionDuration::Interminate)];
        assert_eq!(c.tick().unwrap().0.conditions,
                   vec![app_cond(Condition::Incapacitated, ConditionDuration::Duration(4)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Interminate)]);
    }

    #[test]
    fn test_recurring_effect() {
        let mut c = t_creature();
        c.conditions = vec![app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(HP(1)))),
                                     ConditionDuration::Duration(2))];
        let c = c.tick().unwrap().0;
        assert_eq!(c.cur_health, HP(9));
        let c = c.tick().unwrap().0;
        assert_eq!(c.cur_health, HP(8));
        let c = c.tick().unwrap().0;
        assert_eq!(c.cur_health, HP(8));
    }
}
