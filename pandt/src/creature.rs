use std::collections::HashMap;
use std::cmp;

use indexed::*;
use types::*;

/// `STANDARD_CREATURE_SPEED` is carefully chosen to allow for circular-looking movement options.
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
  pub fn new(
    creature: &'creature Creature, game: &'game Game
  ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
    Ok(DynamicCreature {
      creature: creature,
      game: game,
      class: game.get_class(creature.class)?,
    })
  }

  pub fn id(&self) -> CreatureID { self.creature.id }

  // TODO: DynamicCreature should have a `conditions` field, since calculating the conditions is
  // becoming more expensive, and we look up the conditions several times even just to serialize a
  // creature.
  // But... would we have any race conditions where we would be looking at old condition data after
  // an update?
  // We could at least reduce the number of times we calculate it by passing conditions to
  // can_act, can_move, and speed.

  pub fn can_act(&self) -> bool { conditions_able(&self.all_conditions()) }

  pub fn can_move(&self) -> bool { conditions_able(&self.all_conditions()) }

  pub fn speed(&self) -> u32units::Length {
    let mut speed = self.creature.speed;
    for acondition in self.all_conditions() {
      if acondition.condition == Condition::DoubleMaxMovement {
        speed = speed + self.creature.speed;
      }
    }
    speed
  }

  /// Get all conditions applied to a creature, including permanent conditions associated with
  /// the creature's class and any volume-conditions from the current active scene.
  pub fn all_conditions(&self) -> Vec<AppliedCondition> {
    let mut conditions: Vec<AppliedCondition> =
      self.creature.conditions.values().cloned().collect();
    let applied_class_conditions = self
      .class
      .conditions
      .iter()
      .map(|c| c.apply(Duration::Interminate));
    conditions.extend(applied_class_conditions);
    conditions.extend(self.volume_conditions().into_iter().map(|(_, v)| v));
    conditions
  }

  pub fn own_conditions(&self) -> &HashMap<ConditionID, AppliedCondition> {
    &self.creature.conditions
  }

  pub fn volume_conditions(&self) -> HashMap<ConditionID, AppliedCondition> {
    let mut conditions = HashMap::new();
    if let Some(scene_id) = self.game.active_scene {
      if let Ok(scene) = self.game.get_scene(scene_id) {
        if scene.creatures.contains_key(&self.creature.id) {
          if let Ok(conds) = scene.creature_volume_conditions(self.game, self.creature) {
            for (cond_id, volume_condition) in conds {
              conditions.insert(
                cond_id,
                volume_condition
                  .condition
                  .clone()
                  .apply(Duration::Interminate),
              );
            }
          }
        }
      }
    }
    conditions
  }

  pub fn tick(&self) -> Result<ChangedCreature, GameError> {
    let mut changes = self.creature.change();
    for condition in self.all_conditions() {
      if let AppliedCondition {
        condition: Condition::RecurringEffect(ref eff),
        ref remaining,
      } = condition
      {
        if match *remaining {
          Duration::Rounds(0) => false,
          Duration::Interminate | Duration::Rounds(_) => true,
        } {
          changes = changes.merge(changes.creature(self.game)?.apply_effect(eff)?);
        }
      }
    }

    // We clone and collect the condition IDs so that the iterator doesn't keep a borrow on
    // `changes`, which we need to mutate.
    for condition_id in changes
      .creature
      .conditions
      .keys()
      .cloned()
      .collect::<Vec<ConditionID>>()
    {
      match changes.creature.conditions[&condition_id].remaining {
        Duration::Interminate => {}
        Duration::Rounds(remaining) => if remaining > 0 {
          changes = changes.apply(&CreatureLog::DecrementConditionRemaining(condition_id))?;
        } else {
          changes = changes.apply(&CreatureLog::RemoveCondition(condition_id))?;
        },
      }
    }
    Ok(changes)
  }

  fn generate_energy(&self, nrg: Energy) -> Vec<CreatureLog> {
    let delta = self.creature.max_energy - self.creature.cur_energy;
    if delta > Energy(0) {
      vec![CreatureLog::GenerateEnergy(cmp::min(delta, nrg))]
    } else {
      vec![]
    }
  }

  fn damage(&self, expr: &Dice) -> Vec<CreatureLog> {
    let (rolls, amt) = expr.roll();
    let amt = HP(amt as u8);
    if amt >= self.creature.cur_health {
      vec![
        CreatureLog::Damage(self.creature.cur_health, rolls),
        Self::apply_condition_log(Duration::Interminate, Condition::Dead),
      ]
    } else {
      vec![CreatureLog::Damage(amt, rolls)]
    }
  }

  fn heal(&self, expr: &Dice) -> Vec<CreatureLog> {
    let (dice, amt) = expr.roll();
    let amt = HP(amt as u8);
    let missing = self.creature.max_health - self.creature.cur_health;
    vec![CreatureLog::Heal(cmp::min(missing, amt), dice)]
  }

  fn eff2log(&self, effect: &CreatureEffect) -> Vec<CreatureLog> {
    match *effect {
      CreatureEffect::Damage(ref expr) => self.damage(expr),
      CreatureEffect::Heal(ref expr) => self.heal(expr),
      CreatureEffect::GenerateEnergy(amt) => self.generate_energy(amt),
      CreatureEffect::MultiEffect(ref effects) => {
        effects.iter().flat_map(|x| self.eff2log(x)).collect()
      }
      CreatureEffect::ApplyCondition(ref duration, ref condition) => {
        vec![Self::apply_condition_log(*duration, condition.clone())]
      }
    }
  }

  pub fn apply_effect(&self, effect: &CreatureEffect) -> Result<ChangedCreature, GameError> {
    let ops = Self::eff2log(self, effect);
    let mut changes = self.creature.change();
    for op in &ops {
      changes = changes.apply(op)?;
    }
    Ok(changes)
  }

  fn apply_condition_log(duration: Duration, condition: Condition) -> CreatureLog {
    CreatureLog::ApplyCondition(ConditionID::gen(), duration, condition.clone())
  }

  pub fn ability_statuses(&self) -> IndexedHashMap<AbilityStatus> {
    let mut abs = IndexedHashMap::new();
    for acondition in self.all_conditions() {
      if let Condition::ActivateAbility(abid) = acondition.condition {
        abs.insert(AbilityStatus {
          ability_id: abid,
          cooldown: 0,
        });
      }
    }
    for abid in &self.class.abilities {
      abs.insert(AbilityStatus {
        ability_id: *abid,
        cooldown: 0,
      });
    }
    for ab in &self.creature.abilities {
      abs.insert(*ab);
    }
    abs
  }

  pub fn has_ability(&self, ability: AbilityID) -> bool {
    self
      .ability_statuses()
      .iter()
      .any(|ac| ac.ability_id == ability)
  }
}

impl Creature {
  pub fn create(spec: &CreatureCreation) -> Creature {
    Creature {
      id: CreatureID::gen(),
      name: spec.name.to_string(),
      class: spec.class,
      speed: cm(STANDARD_CREATURE_SPEED),
      max_energy: Energy(10),
      cur_energy: Energy(10),
      abilities: IndexedHashMap::new(),
      max_health: HP(10),
      cur_health: HP(10),
      conditions: HashMap::new(),
      note: spec.note.clone(),
      bio: spec.bio.clone(),
      portrait_url: spec.portrait_url.clone(),
      icon_url: spec.icon_url.clone(),
      attributes: HashMap::new(),
      initiative: spec.initiative.clone(),
      size: spec.size,
      inventory: HashMap::new(),
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
      CreatureLog::ReduceEnergy(ref nrg) => if *nrg > new.cur_energy {
        return Err(GameError::NotEnoughEnergy(*nrg).into());
      } else {
        new.cur_energy = new.cur_energy - *nrg;
      },
      CreatureLog::ApplyCondition(ref id, ref dur, ref con) => {
        new.conditions.insert(*id, con.apply(*dur));
      }
      CreatureLog::DecrementConditionRemaining(ref id) => {
        let cond = new
          .conditions
          .get_mut(id)
          .ok_or_else(|| GameError::ConditionNotFound(*id))?;
        match cond.remaining {
          Duration::Interminate => bail!(GameError::BuggyProgram(
            "Tried to decrease condition duration of an \
             interminate condition"
              .to_string()
          )),
          Duration::Rounds(ref mut dur) => *dur -= 1,
        }
      }
      CreatureLog::RemoveCondition(ref id) => {
        new
          .conditions
          .remove(id)
          .ok_or_else(|| GameError::ConditionNotFound(*id))?;
      }
    }
    Ok(new)
  }

  pub fn id(&self) -> CreatureID { self.id }

  pub fn cur_health(&self) -> HP { self.cur_health }

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

  pub fn get_attribute_score(&self, attr: &AttrID) -> Result<SkillLevel, GameError> {
    self
      .attributes
      .get(attr)
      .cloned()
      .ok_or_else(|| GameError::AttributeNotFound(self.id, attr.clone()).into())
  }

  pub fn attribute_check(&self, check: &AttributeCheck) -> Result<(u8, bool), GameError> {
    let my_skill = self.get_attribute_score(&check.attr)?;
    if check.reliable && check.target <= my_skill {
      Ok((100, true))
    } else {
      let dice = Dice::expr(1, 100);
      let roll = dice.roll().1 as u8; // panic: 1d100 better fit into a u8!
      let success = roll >= my_skill.difficulty(check.target);
      Ok((roll, success))
    }
  }
}

#[derive(Clone)]
pub struct ChangedCreature {
  pub creature: Creature,
  logs: Vec<CreatureLog>,
}

impl ChangedCreature {
  pub fn creature<'creature, 'game>(
    &'creature self, game: &'game Game
  ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(&self.creature, game)
  }

  pub fn apply(&self, log: &CreatureLog) -> Result<ChangedCreature, GameError> {
    let mut new = self.clone();
    new.creature = new.creature.apply_log(log)?;
    new.logs.push(log.clone());
    Ok(new)
  }

  pub fn merge(&self, other: ChangedCreature) -> ChangedCreature {
    let mut new = self.clone();
    new.creature = other.creature;
    new.logs.extend(other.logs);
    new
  }

  pub fn done(self) -> (Creature, Vec<CreatureLog>) { (self.creature, self.logs) }
}

fn conditions_able(conditions: &[AppliedCondition]) -> bool {
  !conditions
    .iter()
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

  #[test]
  fn test_tick_and_expire_condition_remaining() {
    let mut game = t_game();
    game.creatures.mutate(&cid_rogue(), |mut c| {
      c.conditions = HashMap::from_iter(vec![
        (
          ConditionID(uuid_0()),
          app_cond(Condition::Dead, Duration::Rounds(0)),
        ),
        (
          ConditionID(uuid_1()),
          app_cond(Condition::Incapacitated, Duration::Rounds(5)),
        ),
        (
          ConditionID(uuid_2()),
          app_cond(Condition::Incapacitated, Duration::Interminate),
        ),
      ]);
      c
    });
    assert_eq!(
      game
        .get_creature(cid_rogue())
        .unwrap()
        .tick()
        .unwrap()
        .creature
        .conditions,
      HashMap::from_iter(vec![
        (
          ConditionID(uuid_1()),
          app_cond(Condition::Incapacitated, Duration::Rounds(4)),
        ),
        (
          ConditionID(uuid_2()),
          app_cond(Condition::Incapacitated, Duration::Interminate),
        ),
      ])
    );
  }

  /// A RecurringEffect with duration of "2" will tick exactly twice at the beginning of the
  /// creature's next two turns.
  #[test]
  fn test_recurring_effect_ticks_duration_times() {
    let mut game = t_game();
    game.creatures.mutate(&cid_rogue(), |mut c| {
      c.conditions = HashMap::from_iter(vec![
        (
          ConditionID(uuid_0()),
          app_cond(
            Condition::RecurringEffect(Box::new(CreatureEffect::Damage(Dice::flat(1)))),
            Duration::Rounds(2),
          ),
        ),
      ]);
      c
    });
    let c = game
      .get_creature(cid_rogue())
      .unwrap()
      .tick()
      .unwrap()
      .creature;
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
    game.creatures.mutate(&cid_rogue(), |mut c| {
      c.conditions = HashMap::from_iter(vec![
        (
          ConditionID(uuid_0()),
          app_cond(Condition::Incapacitated, Duration::Rounds(1)),
        ),
      ]);
      c
    });
    let c = game
      .get_creature(cid_rogue())
      .unwrap()
      .tick()
      .unwrap()
      .creature;
    assert_eq!(
      c.conditions,
      HashMap::from_iter(vec![
        (
          ConditionID(uuid_0()),
          app_cond(Condition::Incapacitated, Duration::Rounds(0)),
        ),
      ])
    );
    let c = game.dyn_creature(&c).unwrap().tick().unwrap().creature;
    assert_eq!(c.conditions, HashMap::new());
  }
}
