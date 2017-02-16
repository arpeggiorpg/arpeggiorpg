use std::collections::HashMap;
use std::cmp;
use std::sync::atomic;
use std::sync::atomic::Ordering;

use types::*;


/// STANDARD_CREATURE_SPEED is carefully chosen to allow for circular-looking movement options.
/// Since we only allow 8-way movement, the available movement options are biased towards
/// horizontal and diagonal lines, which gives what basically looks like a star shape when you
/// render all potential destinations in the UI. By increasing the speed above 10 meters but still
/// under 11 meters, we can "fill out" the shape to look more circular.
///
/// This only matters in wide-open spaces, of course, and I'm not sure what difficulties it may
/// bring, so I may not stick with it. One problem is that if I want to scale movement speeds (e.g.
/// dwarves move slower, monks move faster, etc) then it may be infeasible to maintain this circular
/// movement area, unless I can figure out some generalized algorithm for determining a more
/// circular movement distance.
const STANDARD_CREATURE_SPEED: u32 = 1086;


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
                if *nrg > new.cur_energy {
                    return Err(GameError::NotEnoughEnergy(*nrg));
                } else {
                    new.cur_energy = new.cur_energy - *nrg;
                }
            }
            CreatureLog::ApplyCondition(ref id, ref dur, ref con) => {
                new.conditions.insert(*id, con.apply(*dur));
                new.can_move = conditions_able(new.conditions.values().collect());
                new.can_act = new.can_move;
            }
            CreatureLog::DecrementConditionRemaining(ref id) => {
                let mut cond =
                    new.conditions.get_mut(&id).ok_or(GameError::ConditionNotFound(*id))?;
                match cond.remaining {
                    ConditionDuration::Interminate => {
                        return Err(GameError::BuggyProgram("Tried to decrease condition duration \
                                                            of an interminate condition"
                            .to_string()))
                    }
                    ConditionDuration::Duration(ref mut dur) => *dur -= 1,
                }
            }
            CreatureLog::RemoveCondition(ref id) => {
                new.conditions.remove(id).ok_or(GameError::ConditionNotFound(*id))?;
                new.can_move = conditions_able(new.conditions.values().collect());
                new.can_act = new.can_move;
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

    fn generate_energy(&self, nrg: Energy) -> Vec<CreatureLog> {
        let delta = self.max_energy - self.cur_energy;
        if delta > Energy(0) {
            vec![CreatureLog::GenerateEnergy(cmp::min(delta, nrg))]
        } else {
            vec![]
        }
    }

    fn damage(&self, amt: HP) -> Vec<CreatureLog> {
        if amt >= self.cur_health {
            vec![CreatureLog::Damage(self.cur_health),
                 self.apply_condition_log(ConditionDuration::Interminate, Condition::Dead)]
        } else {
            vec![CreatureLog::Damage(amt)]
        }
    }

    pub fn apply_effect(&self, effect: &Effect) -> Result<ChangedCreature, GameError> {
        fn eff2log(creature: &Creature, effect: &Effect) -> Vec<CreatureLog> {
            match *effect {
                Effect::Damage(amt) => creature.damage(amt),
                Effect::Heal(amt) => vec![CreatureLog::Heal(amt)],
                Effect::GenerateEnergy(amt) => creature.generate_energy(amt),
                Effect::MultiEffect(ref effects) => {
                    effects.iter().flat_map(|x| eff2log(creature, x)).collect()
                }
                Effect::ApplyCondition(ref duration, ref condition) => {
                    vec![creature.apply_condition_log(duration.clone(), condition.clone())]
                }
            }
        }
        let ops = eff2log(self, effect);
        let mut changes = self.change();
        for op in &ops {
            changes = changes.apply(&op)?;
        }
        Ok(changes)
    }

    /// Assign a position. TODO: Make this return a CreatureLog. Only used in tests for now,
    /// but it will be useful for DM-assigned positions.
    #[cfg(test)]
    pub fn set_pos(&self, pt: Point3) -> Creature {
        let mut newc = self.clone();
        newc.pos = pt;
        newc
    }

    pub fn set_pos_path(&self,
                        pts: Vec<Point3>,
                        distance: Distance)
                        -> Result<ChangedCreature, GameError> {
        if self.can_move {
            let log = CreatureLog::PathCreature {
                path: pts,
                distance: distance,
            };
            Ok(self.change_with(log)?)
        } else {
            Err(GameError::CannotAct(self.id))
        }
    }

    pub fn tick(&self, game: &Game) -> Result<ChangedCreature, GameError> {
        let mut changes = self.change();

        for condition in self.conditions(game)? {
            if let AppliedCondition { condition: Condition::RecurringEffect(ref eff),
                                      ref remaining } = condition {
                if match remaining {
                    &ConditionDuration::Interminate => true,
                    &ConditionDuration::Duration(0) => false,
                    &ConditionDuration::Duration(_) => true,
                } {
                    changes = changes.merge(changes.creature.apply_effect(eff)?);
                }
            }
        }

        for condition_id in changes.creature
            .conditions
            .keys()
            .cloned()
            .collect::<Vec<ConditionID>>() {
            match changes.creature.conditions[&condition_id].remaining {
                ConditionDuration::Interminate => {}
                ConditionDuration::Duration(remaining) => {
                    if remaining > 0 {
                        changes =
                            changes.apply(&CreatureLog::DecrementConditionRemaining(condition_id))?;
                    } else {
                        changes = changes.apply(&CreatureLog::RemoveCondition(condition_id))?;
                    }
                }
            }
        }
        Ok(changes)
    }

    /// Get all conditions applied to a creature, including permanent conditions associated with
    /// the creature's class.
    pub fn conditions(&self, game: &Game) -> Result<Vec<AppliedCondition>, GameError> {
        let mut conditions: Vec<AppliedCondition> = self.conditions.values().cloned().collect();
        let class_conditions = &game.get_class(&self.class)?.conditions;
        let applied_class_conditions = class_conditions.iter()
            .map(|c| c.apply(ConditionDuration::Interminate));
        conditions.extend(applied_class_conditions);
        Ok(conditions)
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

    pub fn reduce_energy(&self, delta: Energy) -> Result<ChangedCreature, GameError> {
        self.change_with(CreatureLog::ReduceEnergy(delta))
    }

    pub fn change(&self) -> ChangedCreature {
        ChangedCreature {
            creature: self.clone(),
            logs: vec![],
        }
    }

    pub fn change_with(&self, log: CreatureLog) -> Result<ChangedCreature, GameError> {
        let creature = self.apply_log(&log)?;
        Ok(ChangedCreature {
            creature: creature,
            logs: vec![log],
        })
    }
}

#[derive(Clone)]
pub struct ChangedCreature {
    pub creature: Creature,
    logs: Vec<CreatureLog>,
}

impl ChangedCreature {
    pub fn apply(&self, log: &CreatureLog) -> Result<ChangedCreature, GameError> {
        let mut new = self.clone();
        new.creature = new.creature.apply_log(&log)?;
        new.logs.push(log.clone());
        Ok(new)
    }

    pub fn merge(&self, other: ChangedCreature) -> ChangedCreature {
        let mut new = self.clone();
        new.creature = other.creature;
        new.logs.extend(other.logs);
        new
    }

    pub fn done(self) -> (Creature, Vec<CreatureLog>) {
        (self.creature, self.logs)
    }
}

impl CreatureBuilder {
    pub fn build(self) -> Result<Creature, GameError> {
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
            conditions: HashMap::new(),
            can_act: true,
            can_move: true,
        };
        Ok(creature)
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
    pub fn speed(mut self, s: Distance) -> Self {
        self.speed = Some(s);
        self
    }
}

fn conditions_able(conditions: Vec<&AppliedCondition>) -> bool {
    !conditions.iter()
        .any(|&&AppliedCondition { ref condition, .. }| {
            condition == &Condition::Incapacitated || condition == &Condition::Dead
        })
}



#[cfg(test)]
pub mod test {
    use creature::*;
    use types::test::*;
    use game::test::*;

    use std::iter::FromIterator;

    pub fn t_rogue(name: &str) -> Creature {
        Creature::build(name, "rogue").build().unwrap()
    }

    pub fn t_ranger(name: &str) -> Creature {
        Creature::build(name, "ranger").build().unwrap()
    }

    pub fn t_cleric(name: &str) -> Creature {
        Creature::build(name, "cleric").build().unwrap()
    }

    #[test]
    fn test_tick_and_expire_condition_remaining() {
        let game = t_game();
        let mut c = t_rogue("bob");
        c.conditions = HashMap::from_iter(vec![(0,
                                                app_cond(Condition::Dead,
                                                         ConditionDuration::Duration(0))),
                                               (1,
                                                app_cond(Condition::Incapacitated,
                                                         ConditionDuration::Duration(5))),
                                               (2,
                                                app_cond(Condition::Incapacitated,
                                                         ConditionDuration::Interminate))]);
        assert_eq!(c.tick(&game).unwrap().creature.conditions,
                   HashMap::from_iter(vec![(1,
                                            app_cond(Condition::Incapacitated,
                                                     ConditionDuration::Duration(4))),
                                           (2,
                                            app_cond(Condition::Incapacitated,
                                                     ConditionDuration::Interminate))]));
    }

    /// A RecurringEffect with duration of "2" will tick exactly twice at the beginning of the
    /// creature's next two turns.
    #[test]
    fn test_recurring_effect_ticks_duration_times() {
        let game = t_game();
        let mut c = t_rogue("bob");
        c.conditions = HashMap::from_iter(
            vec![(0, app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(HP(1)))),
                              ConditionDuration::Duration(2)))]);
        let c = c.tick(&game).unwrap().creature;
        assert_eq!(c.cur_health, HP(9));
        let c = c.tick(&game).unwrap().creature;
        assert_eq!(c.cur_health, HP(8));
        let c = c.tick(&game).unwrap().creature;
        assert_eq!(c.cur_health, HP(8));
    }

    /// If a condition has a duration of N, it will remain on the creature until the N+1'th tick
    /// on that creature.
    #[test]
    fn test_condition_duration() {
        let game = t_game();
        let mut c = t_rogue("bob");
        c.conditions = HashMap::from_iter(vec![(0,
                                                app_cond(Condition::Incapacitated,
                                                         ConditionDuration::Duration(1)))]);
        let c = c.tick(&game).unwrap().creature;
        assert_eq!(c.conditions,
                   HashMap::from_iter(vec![(0,
                                            app_cond(Condition::Incapacitated,
                                                     ConditionDuration::Duration(0)))]));
        let c = c.tick(&game).unwrap().creature;
        assert_eq!(c.conditions, HashMap::new());
    }
}
