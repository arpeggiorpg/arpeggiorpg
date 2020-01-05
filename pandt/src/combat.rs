//! Simulation of combat.

use error_chain::bail;
use nonempty;
use num::{Saturating, Zero};

use crate::types::*;

use std::marker::PhantomData;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
pub const MELEE_RANGE: u32units::Length =
  u32units::Length { dimension: PhantomData, units: PhantomData, value: 150 };

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
        assert_eq!(*cid, new.current_creature_id());
        new.creatures.next_circular();
        new.movement_used = Zero::zero();
      }
      CombatLog::RerollInitiative(ref combatants) => {
        if new.creatures.get_cursor() != 0 {
          bail!(GameError::MustRerollAtStartOfRound);
        }
        new.creatures = sort_combatants(combatants.clone())?;
      }
      CombatLog::ChangeCreatureInitiative(cid, new_init) => {
        let cursor = new.creatures.get_cursor();
        let update_init = |&(c, i)| if c == cid { (c, new_init) } else { (c, i) };
        let creatures_with_inits =
          sort_combatants(new.creatures.iter().map(update_init).collect())?;
        new.creatures = creatures_with_inits;
        new.creatures.set_cursor(cursor);
      }
      CombatLog::ForceNextTurn => {
        new.movement_used = Zero::zero();
        new.creatures.next_circular();
      }
      CombatLog::ForcePrevTurn => {
        new.movement_used = Zero::zero();
        new.creatures.prev_circular();
      }
    }
    Ok(new)
  }

  pub fn next_turn(&self) -> Result<ChangedCombat<'game>, GameError> {
    let change = self.change_with(CombatLog::EndTurn(self.current_creature()?.id()))?;
    Ok(change)
  }

  pub fn current_movement_options(&self) -> Result<Vec<Point3>, GameError> {
    let current = self.current_creature()?;
    let current_speed = current.speed().saturating_sub(self.combat.movement_used);
    Ok(self.game.tile_system.get_all_accessible(
      self.current_pos()?,
      &self.scene.terrain,
      Volume::AABB(current.creature.size),
      current_speed,
    ))
  }

  pub fn get_movement(&'game self) -> Result<CombatMove<'game>, GameError> {
    let current = self.current_creature()?;
    if current.can_move() {
      Ok(CombatMove {
        combat: self,
        movement_left: self.current_creature()?.speed() - self.combat.movement_used,
      })
    } else {
      Err(GameError::CannotAct(current.id()).into())
    }
  }

  pub fn reroll_initiative(&self) -> Result<ChangedCombat<'game>, GameError> {
    let cids = self.combat.creature_ids();
    let combatants = Combat::roll_initiative(self.game, cids)?;
    self.change_with(CombatLog::RerollInitiative(combatants))
  }

  pub fn change(&self) -> ChangedCombat<'game> {
    ChangedCombat { scene: self.scene, combat: self.combat.clone(), logs: vec![], game: self.game }
  }

  pub fn change_with(&self, log: CombatLog) -> Result<ChangedCombat<'game>, GameError> {
    let combat = self.apply_log(&log)?;
    Ok(ChangedCombat { scene: self.scene, combat: combat, game: self.game, logs: vec![log] })
  }

  pub fn current_creature(&self) -> Result<DynamicCreature<'game, 'game>, GameError> {
    self.game.get_creature(self.combat.current_creature_id())
  }

  pub fn current_pos(&self) -> Result<Point3, GameError> {
    self.scene.get_pos(self.combat.current_creature_id())
  }
}

fn sort_combatants(
  mut combatants: Vec<(CreatureID, i16)>,
) -> Result<nonempty::NonEmptyWithCursor<(CreatureID, i16)>, GameError> {
  combatants.sort_by_key(|&(_, i)| -i);
  nonempty::NonEmptyWithCursor::from_vec(combatants)
    .ok_or_else(|| GameError::CombatMustHaveCreatures.into())
}

impl Combat {
  pub fn new(scene: SceneID, combatants: Vec<(CreatureID, i16)>) -> Result<Combat, GameError> {
    Ok(Combat {
      scene: scene,
      movement_used: Zero::zero(),
      creatures: sort_combatants(combatants)?,
    })
  }

  pub fn creature_ids(&self) -> Vec<CreatureID> { self.creatures.iter().map(|&(c, _)| c).collect() }

  pub fn roll_initiative(
    game: &Game, cids: Vec<CreatureID>,
  ) -> Result<Vec<(CreatureID, i16)>, GameError> {
    cids
      .iter()
      .map(|cid| {
        let creature = game.get_creature(*cid)?;
        Ok((*cid, creature.creature.initiative.roll().1 as i16))
      })
      .collect::<Result<Vec<(CreatureID, i16)>, GameError>>()
  }

  pub fn current_creature_id(&self) -> CreatureID { self.creatures.get_current().0 }

  pub fn contains_creature(&self, cid: CreatureID) -> bool {
    self.creatures.iter().any(|&(c, _)| c == cid)
  }

  /// the Option<Combat> will be None if you're removing the last creature from a combat.
  pub fn remove_from_combat(&self, cid: CreatureID) -> Result<Option<Combat>, GameError> {
    let mut combat = self.clone();
    let idx = combat
      .creatures
      .iter()
      .position(|&(c, _)| c == cid)
      .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
    match combat.creatures.remove(idx) {
      Err(nonempty::Error::OutOfBounds { .. }) => Err(
        GameError::BuggyProgram(
          "can't remove index THAT WE FOUND in remove_from_combat".to_string(),
        )
        .into(),
      ),
      Err(nonempty::Error::RemoveLastElement) => Ok(None),
      Ok(_) => Ok(Some(combat)),
    }
  }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatMove<'game> {
  movement_left: u32units::Length,
  pub combat: &'game DynamicCombat<'game>,
}

impl<'game> CombatMove<'game> {
  pub fn movement_left(&self) -> u32units::Length { self.movement_left }

  /// Take a series of 1-square "steps". Diagonals are allowed, but consume an accurate amount of
  /// movement.
  pub fn move_current(&self, pt: Point3) -> Result<crate::game::ChangedGame, GameError> {
    let (change, distance) = self.combat.game.path_creature_distance(
      self.combat.scene.id,
      self.combat.combat.current_creature_id(),
      pt,
      self.movement_left,
    )?;
    change.apply_combat(|c| c.change_with(CombatLog::ConsumeMovement(distance)))
  }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ChangedCombat<'game> {
  pub combat: Combat,
  scene: &'game Scene,
  game: &'game Game,
  logs: Vec<CombatLog>,
}

impl<'game> ChangedCombat<'game> {
  pub fn r#dyn(&self) -> DynamicCombat {
    DynamicCombat { scene: self.scene, game: self.game, combat: &self.combat }
  }

  pub fn apply(&self, log: &CombatLog) -> Result<ChangedCombat<'game>, GameError> {
    let mut new = self.clone();
    new.combat = new.r#dyn().apply_log(log)?;
    new.logs.push(log.clone());
    Ok(new)
  }

  pub fn done(self) -> (Combat, Vec<CombatLog>) { (self.combat, self.logs) }
}

#[cfg(test)]
pub mod test {

  use crate::combat::*;
  use crate::game::test::*;
  use crate::game::ChangedGame;
  use crate::types::test::*;

  /// Create a Test combat. Combat order is rogue, ranger, then cleric.
  pub fn t_combat() -> Game {
    let game = t_game();
    t_perform(
      &game,
      GameCommand::StartCombat(t_scene_id(), vec![cid_rogue(), cid_ranger(), cid_cleric()]),
    )
  }

  pub fn t_act(
    game: &Game, abid: AbilityID, target: DecidedTarget,
  ) -> Result<ChangedGame, GameError> {
    perf(game, GameCommand::CombatAct(abid, target))
  }

  /// Try to melee-atack the ranger when the ranger is out of melee range.
  #[test]
  fn target_melee_out_of_range() {
    let game = t_combat();
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_ranger(), Point3::new(200, 0, 0)),
    );
    match t_act(&game, abid_punch(), DecidedTarget::Creature(cid_ranger())) {
      Err(GameError::CreatureOutOfRange(cid)) => assert_eq!(cid, cid_ranger()),
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  /// Ranged attacks against targets (just) within range are successful.
  #[test]
  fn target_range() {
    let game = t_combat();
    let game = t_perform(&game, GameCommand::Done);
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_ranger(), Point3::new(500, 0, 0)),
    );
    let _: DynamicCombat = t_act(&game, abid_shoot(), DecidedTarget::Creature(cid_ranger()))
      .unwrap()
      .game
      .get_combat()
      .unwrap();
  }

  #[test]
  fn multiple_effects_per_target() {
    let mut game = t_combat();
    let abid_multi: AbilityID = "00000000-0000-0000-0000-000000000009".parse().unwrap();
    let ab = Ability {
      id: abid_multi,
      name: "MultiEffect".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Melee,

        effect: CreatureEffect::MultiEffect(vec![
          CreatureEffect::Damage(Dice::flat(3)),
          CreatureEffect::ApplyCondition(Duration::Interminate, Condition::Dead),
        ]),
      },
    };
    game.abilities.insert(ab);
    game.classes.mutate(&classid_rogue(), |r| r.abilities.push(abid_multi));
    let change = t_act(&game, abid_multi, DecidedTarget::Creature(cid_ranger())).unwrap();
    let next = change.game;
    assert_eq!(
      next.get_creature(cid_ranger()).unwrap().all_conditions(),
      vec![AppliedCondition { remaining: Duration::Interminate, condition: Condition::Dead }]
    )
  }

  /// Ranged attacks against targets outside of range return `TargetOutOfRange`
  #[test]
  fn target_out_of_range() {
    let game = t_combat();
    let game = t_perform(&game, GameCommand::Done);
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(600, 0, 0)),
    );
    match t_act(&game, abid_shoot(), DecidedTarget::Creature(cid_rogue())) {
      Err(GameError::CreatureOutOfRange(cid)) => assert_eq!(cid, cid_rogue()),
      x => panic!("Unexpected result: {:?}", x),
    }

    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(500, 300, 0)),
    );
    // d((500,300,0), (0,0,0)).round() is still 500 so it's still in range
    match t_act(&game, abid_shoot(), DecidedTarget::Creature(cid_rogue())) {
      Err(GameError::CreatureOutOfRange(cid)) => assert_eq!(cid, cid_rogue()),
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn move_too_far() {
    let game = t_combat();
    match game.get_combat().unwrap().get_movement().unwrap().move_current(Point3::new(1100, 0, 0)) {
      Err(GameError::NoPathFound) => {}
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn move_some_at_a_time() {
    let game = t_combat();
    let game = t_perform(&game, GameCommand::PathCurrentCombatCreature(Point3::new(500, 0, 0)));
    assert_eq!(
      game.get_scene(t_scene_id()).unwrap().get_pos(cid_rogue()).unwrap(),
      Point3::new(500, 0, 0)
    );
    let game = t_perform(&game, GameCommand::PathCurrentCombatCreature(Point3::new(1000, 0, 0)));
    assert_eq!(
      game.get_scene(t_scene_id()).unwrap().get_pos(cid_rogue()).unwrap(),
      Point3::new(1000, 0, 0)
    );
    match perf(&game, GameCommand::PathCurrentCombatCreature(Point3::new(1100, 0, 0))) {
      Err(GameError::NoPathFound) => {}
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  /// The length of the path is deducted from combat.movement_used after PathCurrentCreature.
  #[test]
  fn move_honors_path() {
    let mut game = t_combat();
    let terrain = vec![
      Point3::new(0, 0, 0),
      Point3::new(0, 100, 0),
      Point3::new(100, 100, 0),
      Point3::new(200, 100, 0),
      Point3::new(200, 0, 0),
    ];
    game.scenes.mutate(&t_scene_id(), move |s| s.terrain = terrain);

    let next_game = game
      .get_combat()
      .unwrap()
      .get_movement()
      .unwrap()
      .move_current(Point3::new(200, 0, 0))
      .unwrap()
      .game;
    assert_eq!(next_game.get_combat().unwrap().combat.movement_used, u32cm(400));
  }
}
