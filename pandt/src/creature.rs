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


impl<'creature, 'game: 'creature> DynamicCreature<'creature, 'game> {
  pub fn new(creature: &'creature Creature, game: &'game Game)
             -> Result<DynamicCreature<'creature, 'game>, GameError> {
    Ok(DynamicCreature {
      creature: creature,
      game: game,
      class: game.get_class(&creature.class)?,
    })
  }

  pub fn id(&self) -> CreatureID {
    self.creature.id
  }

  pub fn can_act(&self) -> bool {
    conditions_able(self.conditions())
  }

  pub fn can_move(&self) -> bool {
    conditions_able(self.conditions())
  }

  pub fn speed(&self) -> Distance {
    let mut speed = self.creature.speed;
    for acondition in self.conditions() {
      if acondition.condition == Condition::DoubleMaxMovement {
        speed = speed + self.creature.speed;
      }
    }
    speed
  }

  /// Get all conditions applied to a creature, including permanent conditions associated with
  /// the creature's class.
  pub fn conditions(&self) -> Vec<AppliedCondition> {
    let mut conditions: Vec<AppliedCondition> =
      self.creature.conditions.values().cloned().collect();
    let applied_class_conditions = self.class
      .conditions
      .iter()
      .map(|c| c.apply(ConditionDuration::Interminate));
    conditions.extend(applied_class_conditions);
    conditions
  }

  pub fn tick(&self) -> Result<ChangedCreature, GameError> {
    let mut changes = self.creature.change();
    for condition in self.conditions() {
      if let AppliedCondition { condition: Condition::RecurringEffect(ref eff), ref remaining } =
        condition {
        if match remaining {
          &ConditionDuration::Interminate => true,
          &ConditionDuration::Duration(0) => false,
          &ConditionDuration::Duration(_) => true,
        } {
          changes = changes.merge(changes.creature(self.game)?.apply_effect(eff)?);
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
            changes = changes.apply(&CreatureLog::DecrementConditionRemaining(condition_id))?;
          } else {
            changes = changes.apply(&CreatureLog::RemoveCondition(condition_id))?;
          }
        }
      }
    }
    Ok(changes)
  }

  fn generate_energy(&self, nrg: Energy) -> Vec<CreatureLog> {
    let delta = self.creature.max_energy - self.creature.cur_energy;
    if delta > Energy(0) { vec![CreatureLog::GenerateEnergy(cmp::min(delta, nrg))] } else { vec![] }
  }

  fn damage(&self, expr: Dice) -> Vec<CreatureLog> {
    let (dice, amt) = expr.roll();
    let amt = HP(amt as u8);
    if amt >= self.creature.cur_health {
      vec![CreatureLog::Damage(self.creature.cur_health, dice),
           Self::apply_condition_log(ConditionDuration::Interminate, Condition::Dead)]
    } else {
      vec![CreatureLog::Damage(amt, dice)]
    }
  }

  fn heal(&self, expr: Dice) -> Vec<CreatureLog> {
    let (dice, amt) = expr.roll();
    let amt = HP(amt as u8);
    let missing = self.creature.max_health - self.creature.cur_health;
    vec![CreatureLog::Heal(cmp::min(missing, amt), dice)]
  }

  fn eff2log(&self, effect: &Effect) -> Vec<CreatureLog> {
    match *effect {
      Effect::Damage(expr) => self.damage(expr),
      Effect::Heal(expr) => self.heal(expr),
      Effect::GenerateEnergy(amt) => self.generate_energy(amt),
      Effect::MultiEffect(ref effects) => effects.iter().flat_map(|x| self.eff2log(x)).collect(),
      Effect::ApplyCondition(ref duration, ref condition) => {
        vec![Self::apply_condition_log(duration.clone(), condition.clone())]
      }
    }
  }

  pub fn apply_effect(&self, effect: &Effect) -> Result<ChangedCreature, GameError> {
    let ops = Self::eff2log(self, effect);
    let mut changes = self.creature.change();
    for op in &ops {
      changes = changes.apply(&op)?;
    }
    Ok(changes)
  }

  fn apply_condition_log(duration: ConditionDuration, condition: Condition) -> CreatureLog {
    static CONDITION_ID: atomic::AtomicUsize = atomic::ATOMIC_USIZE_INIT;
    CreatureLog::ApplyCondition(CONDITION_ID.fetch_add(1, Ordering::SeqCst),
                                duration.clone(),
                                condition.clone())
  }

  pub fn ability_statuses(&self) -> Vec<AbilityStatus> {
    let mut abs = self.creature.abilities.clone();
    for acondition in self.conditions() {
      if let Condition::ActivateAbility(abid) = acondition.condition {
        abs.push(AbilityStatus {
          ability_id: abid,
          cooldown: 0,
        });
      }
    }
    for abid in self.class.abilities.iter() {
      abs.push(AbilityStatus {
        ability_id: *abid,
        cooldown: 0,
      });
    }
    abs
  }

  pub fn has_ability(&self, ability: AbilityID) -> bool {
    self.ability_statuses().iter().any(|ac| ac.ability_id == ability)
  }
}

impl Creature {
  pub fn create(spec: &CreatureCreation) -> Creature {
    Creature {
      id: CreatureID::new(),
      name: spec.name.to_string(),
      class: spec.class.clone(),
      speed: Distance(STANDARD_CREATURE_SPEED),
      max_energy: Energy(10),
      cur_energy: Energy(10),
      abilities: vec![],
      max_health: HP(10),
      cur_health: HP(10),
      conditions: HashMap::new(),
      note: "".to_string(),
      portrait_url: spec.portrait_url.clone(),
    }
  }

  pub fn apply_log(&self, item: &CreatureLog) -> Result<Creature, GameError> {
    let mut new = self.clone();
    match *item {
      CreatureLog::Damage(ref dmg, ..) => new.cur_health = new.cur_health.saturating_sub(*dmg),
      CreatureLog::Heal(ref dmg, ..) => {
        new.cur_health = cmp::min(new.cur_health.saturating_add(*dmg), new.max_health)
      }
      CreatureLog::GenerateEnergy(ref nrg) => {
        new.cur_energy = cmp::min(new.cur_energy.saturating_add(*nrg), new.max_energy)
      }
      CreatureLog::ReduceEnergy(ref nrg) => {
        if *nrg > new.cur_energy {
          return Err(GameErrorEnum::NotEnoughEnergy(*nrg).into());
        } else {
          new.cur_energy = new.cur_energy - *nrg;
        }
      }
      CreatureLog::ApplyCondition(ref id, ref dur, ref con) => {
        new.conditions.insert(*id, con.apply(*dur));
      }
      CreatureLog::DecrementConditionRemaining(ref id) => {
        let mut cond = new.conditions.get_mut(&id).ok_or(GameErrorEnum::ConditionNotFound(*id))?;
        match cond.remaining {
          ConditionDuration::Interminate => {
            bail!(GameErrorEnum::BuggyProgram("Tried to decrease condition duration of an \
                                                interminate condition"
              .to_string()))
          }
          ConditionDuration::Duration(ref mut dur) => *dur -= 1,
        }
      }
      CreatureLog::RemoveCondition(ref id) => {
        new.conditions.remove(id).ok_or(GameErrorEnum::ConditionNotFound(*id))?;
      }
      CreatureLog::SetNote(ref note) => new.note = note.clone(),
    }
    Ok(new)
  }

  pub fn set_note(&self, note: String) -> Result<ChangedCreature, GameError> {
    self.change_with(CreatureLog::SetNote(note))
  }

  pub fn class(&self) -> String {
    self.class.clone()
  }

  pub fn id(&self) -> CreatureID {
    self.id
  }

  pub fn cur_health(&self) -> HP {
    self.cur_health
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
  pub fn creature<'creature, 'game>(&'creature self, game: &'game Game)
                                    -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(&self.creature, game)
  }

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

fn conditions_able(conditions: Vec<AppliedCondition>) -> bool {
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

  use std::iter::FromIterator;

  pub fn t_creature(name: &str, class: &str) -> Creature {
    Creature::create(&CreatureCreation {
      name: name.to_string(),
      class: class.to_string(),
      portrait_url: "".to_string(),
    })
  }

  pub fn t_rogue(name: &str) -> Creature {
    let mut c = t_creature(name, "rogue");
    c.id = cid_rogue();
    c
  }

  pub fn t_ranger(name: &str) -> Creature {
    let mut c = t_creature(name, "ranger");
    c.id = cid_ranger();
    c
  }

  pub fn t_cleric(name: &str) -> Creature {
    let mut c = t_creature(name, "cleric");
    c.id = cid_rogue();
    c
  }

  #[test]
  fn test_tick_and_expire_condition_remaining() {
    let mut game = t_game();
    {
      let mut c = game.get_creature_mut(cid_rogue()).unwrap();
      c.conditions =
        HashMap::from_iter(vec![(0, app_cond(Condition::Dead, ConditionDuration::Duration(0))),
                                (1,
                                 app_cond(Condition::Incapacitated,
                                          ConditionDuration::Duration(5))),
                                (2,
                                 app_cond(Condition::Incapacitated,
                                          ConditionDuration::Interminate))]);
    }
    assert_eq!(game.get_creature(cid_rogue()).unwrap().tick().unwrap().creature.conditions,
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
    let mut game = t_game();
    {
      let mut c = game.get_creature_mut(cid_rogue()).unwrap();
      c.conditions = HashMap::from_iter(
            vec![(0, app_cond(Condition::RecurringEffect(Box::new(Effect::Damage(Dice::flat(1)))),
                              ConditionDuration::Duration(2)))]);
    }
    let c = game.get_creature(cid_rogue()).unwrap().tick().unwrap().creature;
    assert_eq!(c.cur_health, HP(9));
    let c = game.dyn_creature(&c).unwrap().tick().unwrap().creature;
    assert_eq!(c.cur_health, HP(8));
    let c = game.dyn_creature(&c).unwrap().tick().unwrap().creature;
    assert_eq!(c.cur_health, HP(8));
  }

  /// If a condition has a duration of N, it will remain on the creature until the N+1'th tick
  /// on that creature.
  #[test]
  fn test_condition_duration() {
    let mut game = t_game();
    {
      let mut c = game.get_creature_mut(cid_rogue()).unwrap();
      c.conditions = HashMap::from_iter(vec![(0,
                                              app_cond(Condition::Incapacitated,
                                                       ConditionDuration::Duration(1)))]);
    }
    let c = game.get_creature(cid_rogue()).unwrap().tick().unwrap().creature;
    assert_eq!(c.conditions,
               HashMap::from_iter(vec![(0,
                                        app_cond(Condition::Incapacitated,
                                                 ConditionDuration::Duration(0)))]));
    let c = game.dyn_creature(&c).unwrap().tick().unwrap().creature;
    assert_eq!(c.conditions, HashMap::new());
  }
}
