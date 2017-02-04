use std::collections::HashMap;
use std::cmp;
use std::sync::atomic;
use std::sync::atomic::Ordering;

use odds::vec::VecExt;

use types::*;


/// This is carefully chosen to allow for circular-looking movement options.
/// Since we only allow movement in 8-directions, the available movement options are biased towards
/// horizontal and diagonal lines, which gives what basically looks like a star shape to movement
/// options. By increasing the speed above 10 meters but still under 11 meters, we can "fill out"
/// the shape to look more circular.
/// This only matters in wide-open spaces, of course, and I'm not sure what difficulties in may
/// solve, so I may not stick with it. One problem is that if I want to scale movement speeds (e.g.
/// dwarves move slower, monks move faster, etc) then it may be infeasible to maintain this circular
///  movement area, unless I can figure out some generalized algorithm for determining a more
/// circular movement distance.
const STANDARD_CREATURE_SPEED: u32 = 1086;


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
    speed: Distance,
    max_energy: Energy,
    cur_energy: Energy,
    abilities: Vec<AbilityStatus>,
    class: String,
    max_health: HP,
    cur_health: HP,
    pos: Point3,
    conditions: Vec<AppliedCondition>,
}

impl Creature {
    pub fn build(id: &str, class: &str) -> CreatureBuilder {
        CreatureBuilder {
            id: id.to_string(),
            name: None,
            max_energy: None,
            cur_energy: None,
            class: class.to_string(),
            abilities: vec![],
            max_health: None,
            cur_health: None,
            pos: None,
            conditions: vec![],
            speed: None,
        }
    }

    fn apply_logs(&self, logs: Vec<CreatureLog>) -> Result<Creature, GameError> {
        let mut creature = self.clone();
        for log in logs {
            creature = creature.apply_log(&log)?;
        }
        Ok(creature)
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
            CreatureLog::PathCreature { ref path, .. } => {
                match path.last() {
                    Some(pt) => new.pos = *pt,
                    None => {}
                }
            }
        }
        Ok(new)
    }

    fn apply_condition_log(&self,
                           duration: ConditionDuration,
                           condition: Condition)
                           -> CreatureLog {
        static CONDITION_ID: atomic::AtomicUsize = atomic::ATOMIC_USIZE_INIT;
        CreatureLog::ApplyCondition(CONDITION_ID.fetch_add(1, Ordering::SeqCst),
                                    duration.clone(),
                                    condition.clone())
    }
    pub fn apply_effect(&self, effect: &Effect) -> Result<(Creature, Vec<CreatureLog>), GameError> {
        // it's unlikely we'll be able to rely on having a simple mapping of Effect to
        // Vec<CreatureLog> forever
        fn eff2log(creature: &Creature, effect: &Effect) -> Vec<CreatureLog> {
            match *effect {
                Effect::Damage(amt) => vec![CreatureLog::Damage(amt)],
                Effect::Heal(amt) => vec![CreatureLog::Heal(amt)],
                Effect::GenerateEnergy(amt) => vec![CreatureLog::GenerateEnergy(amt)],
                Effect::MultiEffect(ref effects) => {
                    effects.iter().flat_map(|x| eff2log(creature, x)).collect()
                }
                Effect::ApplyCondition(ref duration, ref condition) => {
                    vec![creature.apply_condition_log(duration.clone(), condition.clone())]
                }
            }
        }
        let ops = eff2log(self, effect);
        let mut creature = self.clone();
        for op in &ops {
            creature = self.apply_log(op)?;
        }
        Ok((creature, ops))
    }

    /// Assign a position. TODO: Make this return a separate GameLog. Only used in tests for now,
    /// but it will be useful for DM-assigned positions.
    pub fn set_pos(&self, pt: Point3) -> Creature {
        let mut newc = self.clone();
        newc.pos = pt;
        newc
    }

    pub fn set_pos_path(&self,
                        pts: Vec<Point3>,
                        distance: Distance)
                        -> Result<(Creature, CreatureLog), GameError> {
        let log = CreatureLog::PathCreature {
            path: pts,
            distance: distance,
        };
        Ok((self.apply_log(&log)?, log))
    }

    pub fn tick(&self) -> Result<(Creature, Vec<CreatureLog>), GameError> {
        let mut new = self.clone();
        let mut effs = vec![];
        let mut all_logs = vec![];
        new.conditions.retain_mut(|&mut AppliedCondition { id, ref condition, ref mut remaining }| {
            if let ConditionDuration::Duration(k) = *remaining {
                // this shouldn't happen normally, since we remove conditions as soon as they reach
                // remaining = 0, but handle it just in case
                if k == 0 {
                    all_logs.push(CreatureLog::RemoveCondition(id));
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

    pub fn class(&self) -> String {
        self.class.clone()
    }

    pub fn pos(&self) -> Point3 {
        self.pos
    }
    pub fn id(&self) -> CreatureID {
        self.id
    }

    pub fn cur_health(&self) -> HP {
        self.cur_health
    }

    pub fn speed(&self) -> Distance {
        self.speed
    }

    // We probably need to move `act` to Creature, then we wouldn't need this method (?)
    pub fn reduce_energy(&self, delta: Energy) -> Self {
        let mut newcreature = self.clone();
        newcreature.cur_energy = newcreature.cur_energy - delta;
        newcreature
    }
}

pub struct CreatureBuilder {
    id: String,
    name: Option<String>,
    max_energy: Option<Energy>,
    cur_energy: Option<Energy>,
    abilities: Vec<AbilityID>,
    class: String,
    max_health: Option<HP>,
    cur_health: Option<HP>,
    pos: Option<Point3>,
    conditions: Vec<AppliedCondition>,
    speed: Option<Distance>,
}

impl CreatureBuilder {
    pub fn build(self, classes: &HashMap<String, Class>) -> Result<Creature, GameError> {
        let creature = Creature {
            id: CreatureID::new(&self.id)?,
            name: self.name.unwrap_or(self.id.to_string()),
            speed: self.speed.unwrap_or(Distance(STANDARD_CREATURE_SPEED)),
            max_energy: self.max_energy.unwrap_or(Energy(10)),
            cur_energy: self.cur_energy.unwrap_or(Energy(10)),
            abilities: vec![],
            class: self.class.clone(),
            max_health: self.max_health.unwrap_or(HP(10)),
            cur_health: self.cur_health.unwrap_or(HP(10)),
            pos: self.pos.unwrap_or((0, 0, 0)),
            conditions: self.conditions,
        };

        let ref conditions = classes.get(&self.class)
            .ok_or(GameError::ClassNotFound(self.class.clone()))?
            .conditions;
        let logs = conditions.iter()
            .map(|cond| creature.apply_condition_log(ConditionDuration::Interminate, cond.clone()))
            .collect();
        creature.apply_logs(logs)
    }
    pub fn name(mut self, name: &str) -> Self {
        self.name = Some(name.to_string());
        self
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
    pub fn speed(mut self, s: Distance) -> Self {
        self.speed = Some(s);
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
    use game::test::*;

    pub fn t_rogue(name: &str) -> Creature {
        Creature::build(name, "rogue")
            .build(&t_classes())
            .unwrap()
    }

    pub fn t_ranger(name: &str) -> Creature {
        Creature::build(name, "ranger")
            .build(&t_classes())
            .unwrap()
    }

    pub fn t_cleric(name: &str) -> Creature {
        Creature::build(name, "cleric")
            .build(&t_classes())
            .unwrap()
    }

    #[test]
    fn test_tick_and_expire_condition_remaining() {
        let mut c = t_rogue("bob");
        c.conditions = vec![app_cond(Condition::Dead, ConditionDuration::Duration(0)),
                            app_cond(Condition::Incapacitated, ConditionDuration::Duration(5)),
                            app_cond(Condition::Incapacitated, ConditionDuration::Interminate)];
        assert_eq!(c.tick().unwrap().0.conditions,
                   vec![app_cond(Condition::Incapacitated, ConditionDuration::Duration(4)),
                        app_cond(Condition::Incapacitated, ConditionDuration::Interminate)]);
    }

    #[test]
    fn test_recurring_effect() {
        let mut c = t_rogue("bob");
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
