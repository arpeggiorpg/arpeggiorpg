//! Representation and simulation of combat.
//! Many simple combat-oriented types are in `types.rs`, but this module implements the
//! Combat types. The most interesting top-level method is `Combat::act`.

use nonempty;

use types::*;
use grid::{creature_within_distance, get_all_accessible, find_path};
use creature::ChangedCreature;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
pub const MELEE_RANGE: Distance = Distance(150);

impl Combat {
    pub fn new(combatants: Vec<Creature>, terrain: &Map) -> Result<Combat, GameError> {
        nonempty::NonEmptyWithCursor::from_vec(combatants)
            .map(|ne| {
                let mut com = Combat {
                    creatures: ne,
                    movement_used: Distance::new(0.0),
                    movement_options: vec![],
                };
                com.update_movement_options_mut(terrain);
                com
            })
            .ok_or(GameError::CombatMustHaveCreatures)
    }

    fn update_movement_options_mut(&mut self, terrain: &Map) {
        let current_pos = self.current_creature().pos();
        let current_speed = self.current_creature().speed() - self.movement_used;
        self.movement_options = get_all_accessible(current_pos, terrain, current_speed);
    }

    pub fn update_movement_options(&self, terrain: &Map) -> Combat {
        let mut newcom = self.clone();
        newcom.update_movement_options_mut(terrain);
        newcom
    }

    pub fn apply_log(&self, game: &Game, l: &CombatLog) -> Result<Combat, GameError> {
        let mut new = self.clone();
        match *l {
            CombatLog::CreatureLog(ref cid,
                                   CreatureLog::PathCreature { ref path, ref distance }) => {
                // This is weird! It may be better to just implement CombatLog::ChangeMovementLeft
                // instead of special-casing CreatureLog::MoveCreature, especially since we will
                // want GM-overridden movement. OTOH that could just be
                // CreatureLog::AssignPosition...
                let c_id = {
                    let mut c = new.get_creature_mut(*cid)?;
                    *c = c.apply_log(&CreatureLog::PathCreature {
                            path: path.clone(),
                            distance: *distance,
                        })?;
                    c.id()
                };
                if c_id == *cid {
                    new.movement_used = new.movement_used + *distance;
                    new.update_movement_options_mut(game.current_map());
                }
            }
            CombatLog::CreatureLog(ref cid, ref cl) => {
                let mut c = new.get_creature_mut(*cid)?;
                *c = c.apply_log(cl)?;
            }
            CombatLog::EndTurn(ref cid) => {
                debug_assert!(*cid == new.current_creature().id());
                new.creatures.next_circular();
                new.movement_used = Distance(0);
                // new.update_movement_options_mut(game.current_map());
                // let ticked_creature = new.current_creature().tick(game)?.creature;
                // *new.current_creature_mut() = ticked_creature;
            }
        }
        Ok(new)
    }

    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    pub fn next_turn(&self, game: &Game) -> Result<ChangedCombat, GameError> {
        let change = self.change_with(game, CombatLog::EndTurn(self.current_creature().id()))?;
        let change =
            change.apply_creature(change.combat.current_creature().id(), |c| c.tick(game))?;
        Ok(change)
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

    pub fn get_able(&self) -> Result<CombatAble, GameError> {
        if self.current_creature().can_act() {
            Ok(CombatAble { combat: self })
        } else {
            Err(GameError::CannotAct(self.current_creature().id()))
        }
    }
    pub fn get_movement(&self) -> Result<CombatMove, GameError> {
        Ok(CombatMove {
            combat: self,
            movement_left: self.current_creature().speed() - self.movement_used,
        })
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

    pub fn change(&self) -> ChangedCombat {
        ChangedCombat {
            combat: self.clone(),
            logs: vec![],
        }
    }

    pub fn change_with(&self, game: &Game, log: CombatLog) -> Result<ChangedCombat, GameError> {
        let combat = self.apply_log(game, &log)?;
        Ok(ChangedCombat {
            combat: combat,
            logs: vec![log],
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatMove<'a> {
    movement_left: Distance,
    pub combat: &'a Combat,
}

impl<'a> CombatMove<'a> {
    pub fn movement_left(&self) -> Distance {
        self.movement_left
    }

    /// Take a series of 1-square "steps". Diagonals are allowed, but consume an accurate amount of
    /// movement.
    pub fn move_current(&self, game: &Game, pt: Point3) -> Result<ChangedCombat, GameError> {
        let (pts, distance) = find_path(self.combat.current_creature().pos(),
                                        self.movement_left,
                                        game.current_map(),
                                        pt).ok_or(GameError::NoPathFound)?;
        debug_assert!(distance <= self.movement_left);

        let change = self.combat.change();
        let mut change = change.apply_creature(self.combat.current_creature().id(),
                            move |c| c.set_pos_path(pts.clone(), distance))?;
        // Sadly, because of the hack we have in Combat.apply_log for handling
        // CombatLog::CreatureLog(CreatureLog::PathCreature), we have to mutate the combat here in
        // an equivalent way.
        change.combat.movement_used = change.combat.movement_used + distance;
        change.combat.update_movement_options_mut(game.current_map());
        Ok(change)
    }
}

/// The ability to act in combat.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatAble<'a> {
    pub combat: &'a Combat,
}
impl<'a> CombatAble<'a> {
    /// Make the current creature use an ability.
    // TODO: this should really be moved into `creature`, because we also need to support
    // out-of-combat ability usage.
    pub fn act(&self, ability: &Ability, target: DecidedTarget) -> Result<ChangedCombat, GameError> {
        let mut change = self.combat.change();

        {
            let mut targets = Self::resolve_targets(&change.combat, ability.target, target)?;
            for effect in &ability.effects {
                for creature_id in targets.drain(..) {
                    change = change.apply_creature(creature_id, |c| c.apply_effect(effect))?;
                }
            }
        }
        change = change.apply_creature(change.combat.current_creature().id(),
                            |c| c.reduce_energy(ability.cost))?;
        Ok(change)
    }

    fn resolve_targets(combat: &Combat,
                       target: TargetSpec,
                       decision: DecidedTarget)
                       -> Result<Vec<CreatureID>, GameError> {
        match (target, decision) {
            (TargetSpec::Melee, DecidedTarget::Melee(cid)) => {
                let target_creature = combat.get_creature(cid)?;
                if creature_within_distance(combat.creatures.get_current(),
                                            target_creature,
                                            MELEE_RANGE) {
                    Ok(vec![cid])
                } else {
                    Err(GameError::CreatureOutOfRange(cid))
                }
            }
            (TargetSpec::Range(max), DecidedTarget::Range(cid)) => {
                let target_creature = combat.get_creature(cid)?;
                if creature_within_distance(combat.creatures.get_current(), target_creature, max) {
                    Ok(vec![cid])
                } else {
                    Err(GameError::CreatureOutOfRange(cid))
                }
            }
            _ => {
                let unhandled = true;
                panic!("Unhandled decided target!")
            }
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ChangedCombat {
    pub combat: Combat,
    logs: Vec<CombatLog>,
}

impl ChangedCombat {
    pub fn apply(&self, game: &Game, log: &CombatLog) -> Result<ChangedCombat, GameError> {
        let mut new = self.clone();
        new.combat = new.combat.apply_log(game, &log)?;
        new.logs.push(log.clone());
        Ok(new)
    }

    pub fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedCombat, GameError>
        where F: Fn(&Creature) -> Result<ChangedCreature, GameError>
    {
        let creature = self.combat.get_creature(cid)?;
        let change = f(creature)?;
        let mut new = self.clone();
        let (creature, logs) = change.done();
        *new.combat.get_creature_mut(cid)? = creature;
        new.logs.extend(creature_logs_into_combat_logs(cid, logs));
        Ok(new)
    }

    pub fn merge(&self, other: ChangedCombat) -> ChangedCombat {
        let mut new = self.clone();
        new.combat = other.combat;
        new.logs.extend(other.logs);
        new
    }

    pub fn done(self) -> (Combat, Vec<CombatLog>) {
        (self.combat, self.logs)
    }
}


#[cfg(test)]
pub mod test {
    extern crate test;

    use combat::*;
    use creature::test::*;
    use types::test::*;
    use grid::test::*;
    use game::test::t_game;

    /// Create a Test combat. Combat order is rogue, ranger, then cleric.
    pub fn t_combat() -> Combat {
        let rogue = t_rogue("rogue");
        let ranger = t_ranger("ranger");
        let cleric = t_cleric("cleric");
        Combat::new(vec![rogue, ranger, cleric], &huge_box()).unwrap()
    }

    pub fn t_act(c: &Combat,
                 ab: &Ability,
                 target: DecidedTarget)
                 -> Result<ChangedCombat, GameError> {
        c.get_able()?.act(ab, target)
    }

    /// Try to melee-atack the ranger when the ranger is out of melee range.
    #[test]
    fn target_melee_out_of_range() {
        let mut combat = t_combat();
        let melee = t_punch();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((2, 0, 0));
        }
        assert_eq!(t_act(&combat, &melee, DecidedTarget::Melee(cid("ranger"))),
                   Err(GameError::CreatureOutOfRange(cid("ranger"))));
    }

    /// Ranged attacks against targets (just) within range are successful.
    #[test]
    fn target_range() {
        let mut combat = t_combat();
        let range_ab = t_shoot();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((5, 0, 0));
        }
        let _: Combat = t_act(&combat, &range_ab, DecidedTarget::Range(cid("ranger"))).unwrap().combat;
    }

    /// Ranged attacks against targets outside of range return `TargetOutOfRange`
    #[test]
    fn target_out_of_range() {
        let mut combat = t_combat();
        let shoot = t_shoot();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((6, 0, 0));
        }
        assert_eq!(t_act(&combat, &shoot, DecidedTarget::Range(cid("ranger"))),
                   Err(GameError::CreatureOutOfRange(cid("ranger"))));

        // d((5,1,0), (0,0,0)).round() is still 5 so it's still in range
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((5, 3, 0));
        }
        assert_eq!(t_act(&combat, &shoot, DecidedTarget::Range(cid("ranger"))),
                   Err(GameError::CreatureOutOfRange(cid("ranger"))));
    }

    #[test]
    fn move_too_far() {
        let combat = t_combat();
        assert_eq!(combat.get_movement().unwrap().move_current(&t_game(), (11, 0, 0)),
                   Err(GameError::NoPathFound))
    }

    #[test]
    fn move_some_at_a_time() {
        let game = t_game();
        let combat = t_combat();
        let combat = combat.get_movement().unwrap().move_current(&game, (5, 0, 0)).unwrap().combat;
        assert_eq!(combat.current_creature().pos(), (5, 0, 0));
        let combat = combat.get_movement().unwrap().move_current(&game, (10, 0, 0)).unwrap().combat;
        assert_eq!(combat.current_creature().pos(), (10, 0, 0));
        assert_eq!(combat.get_movement().unwrap().move_current(&game, (11, 0, 0)),
                   Err(GameError::NoPathFound))
    }

    #[test]
    fn tick_on_next_turn() {
        let mut combat = t_combat();
        combat.get_creature_mut(cid("ranger"))
            .unwrap()
            .conditions
            .insert(500,
                    Condition::RecurringEffect(Box::new(Effect::Damage(HP(5))))
                        .apply(ConditionDuration::Interminate));
        let combat = combat.next_turn(&t_game()).unwrap().combat;
        let cur = combat.current_creature();
        assert_eq!(cur.id, cid("ranger"));
        assert_eq!(cur.cur_health, HP(5));
    }
}
