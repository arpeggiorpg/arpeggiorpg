//! Representation and simulation of combat.
//! Many simple combat-oriented types are in `types.rs`, but this module implements the
//! Combat types.

use nonempty;

use types::*;
use grid::{get_all_accessible, find_path, point3_distance};
use creature::ChangedCreature;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
pub const MELEE_RANGE: Distance = Distance(150);

impl<'combat, 'game: 'combat> DynamicCombat<'combat, 'game> {
    pub fn remove_from_combat(&self,
                              cid: CreatureID)
                              -> Result<(Option<Combat>, Creature), GameError> {
        self.combat.remove_from_combat(cid)
    }

    pub fn apply_log(&self, l: &CombatLog) -> Result<Combat, GameError> {
        let mut new = self.combat.clone();
        match *l {
            CombatLog::PathCurrentCreature(ref path) => {
                let distance = {
                    let mut c = new.current_creature_mut();
                    let c_pos = c.pos();
                    let destination = path.last().unwrap_or(&c_pos);
                    let distance = {
                        point3_distance(c.pos, *destination)
                    };
                    *c = c.apply_log(&CreatureLog::SetPos(*destination))?;
                    distance
                };
                new.movement_used = new.movement_used + distance;
            }
            CombatLog::CreatureLog(ref cid, ref cl) => {
                let mut c = new.get_creature_mut(*cid)?;
                *c = c.apply_log(cl)?;
            }
            CombatLog::EndTurn(ref cid) => {
                debug_assert!(*cid == new.current_creature().id());
                new.creatures.next_circular();
                new.movement_used = Distance(0);
            }
            CombatLog::ChangeCreatureInitiative(cid, new_pos) => {
                let current_pos = new.creatures
                    .iter()
                    .position(|c| c.id == cid)
                    .ok_or(GameError::CreatureNotFound(cid))?;
                if new_pos >= new.creatures.len() {
                    return Err(GameError::InitiativeOutOfBounds(new.creatures.len() - 1));
                }
                new.creatures = new.creatures
                    .mutate(|creatures| slide_vec(creatures, current_pos, new_pos))
                    .0
                    .unwrap();
            }
        }
        Ok(new)
    }

    pub fn next_turn(&self) -> Result<ChangedCombat<'game>, GameError> {
        let change = self.change_with(CombatLog::EndTurn(self.combat.current_creature().id()))?;
        let change =
            change.apply_creature(change.combat.current_creature().id(), |c| c.tick())?;
        Ok(change)
    }

    pub fn current_movement_options(&self) -> Result<Vec<Point3>, GameError> {
        let current_pos = self.combat.current_creature().pos();
        let current_speed = self.current_creature()?.speed() - self.combat.movement_used;
        Ok(get_all_accessible(current_pos, self.game.current_map(), current_speed))
    }

    pub fn get_movement(&'combat self) -> Result<CombatMove<'combat, 'game>, GameError> {
        if self.combat.current_creature().can_move {
            Ok(CombatMove {
                combat: self,
                movement_left: self.current_creature()?.speed() - self.combat.movement_used,
            })
        } else {
            Err(GameError::CannotAct(self.combat.current_creature().id()))
        }
    }
    pub fn get_able(&'combat self) -> Result<CombatAble<'combat, 'game>, GameError> {
        if self.combat.current_creature().can_act {
            Ok(CombatAble { combat: self })
        } else {
            Err(GameError::CannotAct(self.combat.current_creature().id()))
        }
    }


    pub fn change_creature_initiative(&self,
                                      cid: CreatureID,
                                      new_pos: usize)
                                      -> Result<ChangedCombat<'game>, GameError> {
        self.change_with(CombatLog::ChangeCreatureInitiative(cid, new_pos))
    }

    pub fn change(&self) -> ChangedCombat<'game> {
        ChangedCombat {
            combat: self.combat.clone(),
            logs: vec![],
            game: self.game,
        }
    }

    pub fn change_with(&self, log: CombatLog) -> Result<ChangedCombat<'game>, GameError> {
        let combat = self.apply_log(&log)?;
        Ok(ChangedCombat {
            combat: combat,
            game: self.game,
            logs: vec![log],
        })
    }

    pub fn current_creature(&self) -> Result<DynamicCreature, GameError> {
        self.game.dyn_creature(self.combat.creatures.get_current())
    }
}

impl Combat {
    pub fn new(combatants: Vec<Creature>) -> Result<Combat, GameError> {
        nonempty::NonEmptyWithCursor::from_vec(combatants)
            .map(|ne| {
                let com = Combat {
                    creatures: ne,
                    movement_used: Distance::new(0.0),
                };
                com
            })
            .ok_or(GameError::CombatMustHaveCreatures)
    }

    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    fn current_creature_mut(&mut self) -> &mut Creature {
        self.creatures.get_current_mut()
    }

    pub fn get_creature(&self, cid: CreatureID) -> Result<&Creature, GameError> {
        self.creatures
            .iter()
            .find(|c| c.id() == cid)
            .ok_or(GameError::CreatureNotFound(cid))
    }

    pub fn get_creature_mut(&mut self, cid: CreatureID) -> Result<&mut Creature, GameError> {
        self.creatures
            .iter_mut()
            .find(|c| c.id() == cid)
            .ok_or(GameError::CreatureNotFound(cid))
    }

    pub fn get_creatures(&self) -> Vec<&Creature> {
        self.creatures.iter().collect()
    }

    /// the Option<Combat> will be None if you're removing the last creature from a combat.
    /// Returns the Creature removed, so you can put it back into archival.
    pub fn remove_from_combat(&self,
                              cid: CreatureID)
                              -> Result<(Option<Combat>, Creature), GameError> {
        let mut combat = self.clone();
        let idx = combat.creatures
            .iter()
            .position(|c| c.id() == cid)
            .ok_or(GameError::CreatureNotFound(cid))?;
        match combat.creatures.remove(idx) {
            Err(nonempty::Error::OutOfBounds { .. }) => {
                Err(GameError::BuggyProgram("can't remove index THAT WE FOUND in \
                                             remove_from_combat"
                    .to_string()))
            }
            Err(nonempty::Error::RemoveLastElement) => {
                Ok((None, combat.current_creature().clone()))
            }
            Ok(creature) => Ok((Some(combat), creature)),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatMove<'combat, 'game: 'combat> {
    movement_left: Distance,
    pub combat: &'combat DynamicCombat<'combat, 'game>,
}

impl<'combat, 'game: 'combat> CombatMove<'combat, 'game> {
    pub fn movement_left(&self) -> Distance {
        self.movement_left
    }

    /// Take a series of 1-square "steps". Diagonals are allowed, but consume an accurate amount of
    /// movement.
    pub fn move_current(&self, pt: Point3) -> Result<ChangedCombat<'game>, GameError> {
        let (pts, distance) = find_path(self.combat.combat.current_creature().pos(),
                                        self.movement_left,
                                        self.combat.game.current_map(),
                                        pt).ok_or(GameError::NoPathFound)?;
        debug_assert!(distance <= self.movement_left);

        let change = self.combat.change_with(CombatLog::PathCurrentCreature(pts))?;
        Ok(change)
    }
}


/// The ability to act in combat.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatAble<'combat, 'game: 'combat> {
    pub combat: &'combat DynamicCombat<'combat, 'game>,
}
impl<'combat, 'game: 'combat> CombatAble<'combat, 'game> {
    /// Make the current creature use an ability.
    pub fn act(&self,
               ability: &Ability,
               target: DecidedTarget)
               -> Result<ChangedCombat<'game>, GameError> {
        self.combat.combat.current_creature().act(|cid| self.combat.combat.get_creature(cid),
                                                  ability,
                                                  target,
                                                  self.combat.change(),
                                                  true)
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ChangedCombat<'game> {
    pub combat: Combat,
    game: &'game Game,
    logs: Vec<CombatLog>,
}

impl<'game> ChangedCombat<'game> {
    pub fn dyn(&self) -> DynamicCombat {
        DynamicCombat {
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

    pub fn apply_creature<F>(&self,
                             cid: CreatureID,
                             f: F)
                             -> Result<ChangedCombat<'game>, GameError>
        where F: FnOnce(DynamicCreature) -> Result<ChangedCreature, GameError>
    {
        let creature = self.combat.get_creature(cid)?;
        let change = f(self.game.dyn_creature(creature)?)?;
        let mut new = self.clone();
        let (creature, logs) = change.done();
        *new.combat.get_creature_mut(cid)? = creature;
        new.logs.extend(creature_logs_into_combat_logs(cid, logs));
        Ok(new)
    }

    pub fn done(self) -> (Combat, Vec<CombatLog>) {
        (self.combat, self.logs)
    }
}

impl<'game> CreatureChanger for ChangedCombat<'game> {
    fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedCombat<'game>, GameError>
        where F: FnOnce(DynamicCreature) -> Result<ChangedCreature, GameError>
    {
        Ok(ChangedCombat::apply_creature(self, cid, f)?)
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

    pub fn t_act<'game>(game: &'game Game,
                        ab: &Ability,
                        target: DecidedTarget)
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
            effects: vec![Effect::Damage(Dice::flat(3)),
                          Effect::ApplyCondition(ConditionDuration::Interminate, Condition::Dead)],
        };
        let next = t_act(&game, &ab, DecidedTarget::Melee(cid("ranger"))).unwrap().combat;
        assert_eq!(game.dyn_creature(next.get_creature(cid("ranger")).unwrap()).unwrap()
                       .conditions(),
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
            .unwrap()
            .combat;
        assert_eq!(combat.current_creature().pos(), (5, 0, 0));
        let game = game.perform_unchecked(GameCommand::PathCurrentCombatCreature((10, 0, 0)))
            .unwrap()
            .game;
        assert_eq!(game.current_combat.as_ref().unwrap().current_creature().pos(),
                   (10, 0, 0));
        assert_eq!(game.perform_unchecked(GameCommand::PathCurrentCombatCreature((11, 0, 0))),
                   Err(GameError::NoPathFound));
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
            .unwrap()
            .combat;
        let cur = combat.current_creature();
        assert_eq!(cur.id, cid("ranger"));
        assert_eq!(cur.cur_health, HP(5));
    }
}
