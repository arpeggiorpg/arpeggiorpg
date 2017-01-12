use std::cmp;

use odds::vec::VecExt;

use types::*;

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

    fn apply_effect_mut(&mut self, effect: &Effect) {
        match *effect {
            Effect::Damage(amt) => self.cur_health = self.cur_health.saturating_sub(amt),
            Effect::Heal(amt) => {
                self.cur_health = cmp::min(self.max_health, self.cur_health.saturating_add(amt))
            }
            Effect::GenerateEnergy(amt) => {
                self.cur_energy = cmp::min(self.max_energy, self.cur_energy.saturating_add(amt))
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
    pub fn apply_effect(&self, effect: &Effect) -> Creature {
        let mut newc = self.clone();
        newc.apply_effect_mut(effect);
        newc
    }
    pub fn tick(&self) -> Creature {
        let mut newc = self.clone();
        newc.tick_mut();
        newc
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
        assert_eq!(c.tick().conditions,
                   vec![app_cond(Condition::Incapacitated, ConditionDuration::Duration(4)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Interminate)]);
    }

    #[test]
    fn test_recurring_effect() {
        let mut c = t_creature();
        c.conditions = vec![app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(HP(1)))),
                                     ConditionDuration::Duration(2))];
        let c = c.tick();
        assert_eq!(c.cur_health, HP(9));
        let c = c.tick();
        assert_eq!(c.cur_health, HP(8));
        let c = c.tick();
        assert_eq!(c.cur_health, HP(8));
    }
}
