//! Simulation of combat.

use nonempty;

use types::*;
use creature::ChangedCreature;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
pub const MELEE_RANGE: Distance = Distance(150);

impl<'game> DynamicCombat<'game> {
  pub fn remove_from_combat(&self, cid: CreatureID) -> Result<Option<Combat>, GameError> {
    self.combat.remove_from_combat(cid)
  }

  pub fn apply_log(&self, l: &CombatLog) -> Result<Combat, GameError> {
    let mut new = self.combat.clone();
    match *l {
      CombatLog::ConsumeMovement(distance) => {
        new.movement_used = new.movement_used + distance;
      }
      CombatLog::EndTurn(ref cid) => {
        assert_eq!(*cid, *new.creatures.get_current());
        new.creatures.next_circular();
        new.movement_used = Distance(0);
      }
      CombatLog::ChangeCreatureInitiative(cid, new_pos) => {
        let current_pos = new.creatures
          .iter()
          .position(|c| *c == cid)
          .ok_or(GameError::CreatureNotFound(cid.to_string()))?;
        if new_pos >= new.creatures.len() {
          return Err(GameError::InitiativeOutOfBounds(new.creatures.len() - 1));
        }
        new.creatures = new.creatures
          .mutate(|creatures| slide_vec(creatures, current_pos, new_pos))
          .0
          .expect("We do bounds-checking ahead of time, and our mutator doesn't change the \
                   number of elements");
      }
    }
    Ok(new)
  }

  pub fn next_turn(&self) -> Result<ChangedCombat<'game>, GameError> {
    let change = self.change_with(CombatLog::EndTurn(self.current_creature()?.id()))?;
    // let change = change.apply_creature(change.dyn().current_creature()?.id(), |c| c.tick())?;
    Ok(change)
  }

  pub fn current_movement_options(&self) -> Result<Vec<Point3>, GameError> {
    let current = self.current_creature()?;
    let current_speed = current.speed() - self.combat.movement_used;
    Ok(self.game
      .tile_system
      .get_all_accessible(self.current_pos()?, self.map, current_speed))
  }

  pub fn get_movement(&'game self) -> Result<CombatMove<'game>, GameError> {
    let current = self.current_creature()?;
    if current.can_move() {
      Ok(CombatMove {
        combat: self,
        movement_left: self.current_creature()?.speed() - self.combat.movement_used,
      })
    } else {
      Err(GameError::CannotAct(current.id()))
    }
  }

  pub fn change(&self) -> ChangedCombat<'game> {
    ChangedCombat {
      map: self.map,
      scene: self.scene,
      combat: self.combat.clone(),
      logs: vec![],
      game: self.game,
    }
  }

  pub fn change_with(&self, log: CombatLog) -> Result<ChangedCombat<'game>, GameError> {
    let combat = self.apply_log(&log)?;
    Ok(ChangedCombat {
      map: self.map,
      scene: self.scene,
      combat: combat,
      game: self.game,
      logs: vec![log],
    })
  }

  pub fn current_creature(&self) -> Result<DynamicCreature<'game, 'game>, GameError> {
    self.game.get_creature(self.combat.current_creature_id())
  }

  pub fn current_pos(&self) -> Result<Point3, GameError> {
    self.scene.get_pos(self.combat.current_creature_id())
  }
}

impl Combat {
  pub fn new(scene: SceneName, combatants: Vec<CreatureID>) -> Result<Combat, GameError> {
    nonempty::NonEmptyWithCursor::from_vec(combatants)
      .map(|ne| {
        let com = Combat {
          scene: scene,
          creatures: ne,
          movement_used: Distance::from_meters(0.0),
        };
        com
      })
      .ok_or(GameError::CombatMustHaveCreatures)
  }

  pub fn current_creature_id(&self) -> CreatureID {
    *self.creatures.get_current()
  }

  /// the Option<Combat> will be None if you're removing the last creature from a combat.
  pub fn remove_from_combat(&self, cid: CreatureID) -> Result<Option<Combat>, GameError> {
    let mut combat = self.clone();
    let idx = combat.creatures
      .iter()
      .position(|c| *c == cid)
      .ok_or(GameError::CreatureNotFound(cid.to_string()))?;
    match combat.creatures.remove(idx) {
      Err(nonempty::Error::OutOfBounds { .. }) => {
        Err(GameError::BuggyProgram("can't remove index THAT WE FOUND in remove_from_combat"
          .to_string()))
      }
      Err(nonempty::Error::RemoveLastElement) => Ok(None),
      Ok(creature) => Ok(Some(combat)),
    }
  }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatMove<'game> {
  movement_left: Distance,
  pub combat: &'game DynamicCombat<'game>,
}

impl<'game> CombatMove<'game> {
  pub fn movement_left(&self) -> Distance {
    self.movement_left
  }

  /// Take a series of 1-square "steps". Diagonals are allowed, but consume an accurate amount of
  /// movement.
  pub fn move_current(&self, pt: Point3) -> Result<::game::ChangedGame, GameError> {
    let (pts, distance) = self.combat
      .game
      .tile_system
      .find_path(self.combat.current_pos()?, self.movement_left, self.combat.map, pt)
      .ok_or(GameError::NoPathFound)?;
    debug_assert!(distance <= self.movement_left);

    let change = self.combat
      .game
      .change_with(GameLog::PathCreature(self.combat.combat.scene.clone(),
                                         self.combat.combat.current_creature_id(),
                                         pt))?;
    change.apply_combat(|c| c.change_with(CombatLog::ConsumeMovement(distance)))
  }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ChangedCombat<'game> {
  pub combat: Combat,
  map: &'game Map,
  scene: &'game Scene,
  game: &'game Game,
  logs: Vec<CombatLog>,
}

impl<'game> ChangedCombat<'game> {
  pub fn dyn(&self) -> DynamicCombat {
    DynamicCombat {
      map: self.map,
      scene: self.scene,
      game: self.game,
      combat: &self.combat,
    }
  }

  pub fn apply(&self, log: &CombatLog) -> Result<ChangedCombat<'game>, GameError> {
    let mut new = self.clone();
    new.combat = new.dyn().apply_log(&log)?;
    new.logs.push(log.clone());
    Ok(new)
  }

  pub fn done(self) -> (Combat, Vec<CombatLog>) {
    (self.combat, self.logs)
  }
}

fn slide_vec<T>(mut vec: &mut Vec<T>, move_me: usize, to: usize) -> bool {
  if to >= vec.len() {
    return false;
  }
  let el = vec.remove(move_me);
  vec.insert(to, el);
  true
}

#[cfg(test)]
pub mod test {
  extern crate test;

  use combat::*;
  use types::test::*;
  use game::test::t_game;

  #[test]
  fn slide_later() {
    let mut v = vec![1, 2, 3];
    slide_vec(&mut v, 0, 2);
    assert_eq!(v, [2, 3, 1]);

    let mut v = vec![1, 2, 3];
    slide_vec(&mut v, 0, 1);
    assert_eq!(v, [2, 1, 3]);

    let mut v = vec![1, 2, 3, 4, 5];
    slide_vec(&mut v, 2, 3);
    assert_eq!(v, vec![1, 2, 4, 3, 5]);
  }

  #[test]
  fn slide_earlier() {
    let mut v = vec![1, 2, 3];
    slide_vec(&mut v, 2, 0);
    assert_eq!(v, vec![3, 1, 2]);
  }

  /// Create a Test combat. Combat order is rogue, ranger, then cleric.
  pub fn t_combat() -> Game {
    let game = t_game();
    game.perform_unchecked(GameCommand::StartCombat(vec![cid("rogue"),
                                                             cid("ranger"),
                                                             cid("cleric")]))
            .unwrap()
            .game
  }

  pub fn t_act<'game>(game: &'game Game, ab: &Ability, target: DecidedTarget)
                      -> Result<ChangedCombat<'game>, GameError> {
    game.get_combat().unwrap().get_able()?.act(ab, target)
  }

  /// Try to melee-atack the ranger when the ranger is out of melee range.
  #[test]
  fn target_melee_out_of_range() {
    let game = t_combat();
    let melee = t_punch();
    let game = game.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (2, 0, 0)))
      .unwrap()
      .game;
    assert_eq!(t_act(&game, &melee, DecidedTarget::Melee(cid("ranger"))),
               Err(GameError::CreatureOutOfRange(cid("ranger"))));
  }

  /// Ranged attacks against targets (just) within range are successful.
  #[test]
  fn target_range() {
    let game = t_combat();
    let range_ab = t_shoot();
    let game = game.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (5, 0, 0)))
      .unwrap()
      .game;
    let _: Combat = t_act(&game, &range_ab, DecidedTarget::Range(cid("ranger")))
      .unwrap()
      .combat;
  }

  #[test]
  fn multiple_effects_per_target() {
    let game = t_combat();
    let ab = Ability {
      name: "MultiEffect".to_string(),
      target: TargetSpec::Melee,
      cost: Energy(0),
      usable_ooc: true,
      effects: vec![Effect::Damage(Dice::flat(3)),
                    Effect::ApplyCondition(ConditionDuration::Interminate, Condition::Dead)],
    };
    let change = t_act(&game, &ab, DecidedTarget::Melee(cid("ranger"))).unwrap();
    let next = change.dyn();
    assert_eq!(next.get_creature(cid("ranger")).unwrap().conditions(),
               vec![AppliedCondition {
                      remaining: ConditionDuration::Interminate,
                      condition: Condition::Dead,
                    }])
  }

  /// Ranged attacks against targets outside of range return `TargetOutOfRange`
  #[test]
  fn target_out_of_range() {
    let game = t_combat();
    let shoot = t_shoot();
    let game = game.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (6, 0, 0)))
      .unwrap()
      .game;
    assert_eq!(t_act(&game, &shoot, DecidedTarget::Range(cid("ranger"))),
               Err(GameError::CreatureOutOfRange(cid("ranger"))));

    let game = game.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (5, 3, 0)))
      .unwrap()
      .game;
    // d((5,3,0), (0,0,0)).round() is still 5 so it's still in range
    assert_eq!(t_act(&game, &shoot, DecidedTarget::Range(cid("ranger"))),
               Err(GameError::CreatureOutOfRange(cid("ranger"))));
  }

  #[test]
  fn move_too_far() {
    let game = t_combat();
    assert_eq!(game.get_combat()
                 .unwrap()
                 .get_movement()
                 .unwrap()
                 .move_current((11, 0, 0)),
               Err(GameError::NoPathFound))
  }

  #[test]
  fn move_some_at_a_time() {
    let game = t_combat();
    let combat = game.get_combat()
      .unwrap()
      .get_movement()
      .unwrap()
      .move_current((5, 0, 0))
      .unwrap();
    assert_eq!(combat.dyn().current_creature().unwrap().pos(), (5, 0, 0));
    let game = game.perform_unchecked(GameCommand::PathCurrentCombatCreature((10, 0, 0)))
      .unwrap()
      .game;
    assert_eq!(game.get_combat().unwrap().current_creature().unwrap().pos(), (10, 0, 0));
    assert_eq!(game.perform_unchecked(GameCommand::PathCurrentCombatCreature((11, 0, 0))),
               Err(GameError::NoPathFound));
  }

  /// The length of the past is deducted from combat.movement_used after PathCurrentCreature.
  #[test]
  fn move_honors_path() {
    let mut game = t_combat();
    game.maps.insert("circuitous".to_string(),
                     // up, right, right, down
                     vec![(0, 0, 0), (0, 1, 0), (1, 1, 0), (2, 1, 0), (2, 0, 0)]);
    game.current_map = Some("circuitous".to_string());
    let combat =
      game.get_combat().unwrap().get_movement().unwrap().move_current((2, 0, 0)).unwrap().combat;
    assert_eq!(combat.movement_used, Distance(400));
  }

  #[test]
  fn tick_on_next_turn() {
    let mut game = t_combat();
    game.current_combat
      .as_mut()
      .unwrap()
      .get_creature_mut(cid("ranger"))
      .unwrap()
      .conditions
      .insert(500,
              Condition::RecurringEffect(Box::new(Effect::Damage(Dice::flat(5))))
                .apply(ConditionDuration::Interminate));
    let combat = game.get_combat()
      .unwrap()
      .next_turn()
      .unwrap();
    let cur = combat.dyn().current_creature().unwrap().clone();
    assert_eq!(cur.id(), cid("ranger"));
    assert_eq!(cur.creature.cur_health, HP(5));
  }
}
