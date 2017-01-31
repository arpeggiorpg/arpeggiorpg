//! Representation and simulation of combat.
//! Many simple combat-oriented types are in `types.rs`, but this module implements the
//! Combat types. The most interesting top-level method is `Combat::act`.

use nonempty;

use creature::*;
use types::*;
use grid::{creature_within_distance, point3_distance, get_all_accessible};

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
const MELEE_RANGE: Distance = Distance(150);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat {
    // Since we serialize a whole history of combats to JSON, using Rc<Creature> pointless, because
    // after we load data back in, because serde doesn't (currently) have any way to know that
    // multiple Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<Creature>,
    movement_used: Distance,
    /// Points that the current creature can move to.
    /// This is only relevant in combat, since only in combat is movement limited.
    movement_options: Vec<Point3>,
}

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

    pub fn apply_log(&self, l: &CombatLog, terrain: &Map) -> Result<Combat, GameError> {
        let mut new = self.clone();
        match *l {
            CombatLog::CreatureLog(ref cid, CreatureLog::MoveCreature(ref pt)) => {
                // This is weird! It may be better to just implement CombatLog::ChangeMovementLeft
                // instead of special-casing CreatureLog::MoveCreature, especially since we will
                // want GM-overridden movement. OTOH that could just be
                // CreatureLog::AssignPosition...
                let (c_pos, c_id) = {
                    let mut c = new.get_creature_mut(*cid)?;
                    let c_pos = c.pos();
                    *c = c.apply_log(&CreatureLog::MoveCreature(*pt))?;
                    (c_pos, c.id())
                };
                if c_id == *cid {
                    let distance = point3_distance(c_pos, *pt);
                    new.movement_used = new.movement_used + distance;
                    new.update_movement_options_mut(terrain);
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
                new.update_movement_options_mut(terrain);
            }
        }
        Ok(new)
    }

    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    fn current_creature_mut(&mut self) -> &mut Creature {
        self.creatures.get_current_mut()
    }

    pub fn next_turn(&self, terrain: &Map) -> Result<(Combat, Vec<CombatLog>), GameError> {
        let mut newcombat = self.clone();
        let mut all_logs = vec![];
        for creature in newcombat.creatures.iter_mut() {
            let (newcreat, logs) = creature.tick()?;
            *creature = newcreat;
            all_logs.extend(creature_logs_into_combat_logs(creature.id(), logs));
        }
        all_logs.push(CombatLog::EndTurn(newcombat.current_creature().id()));
        newcombat.creatures.next_circular();
        newcombat.movement_used = Distance(0);
        newcombat.update_movement_options_mut(terrain);
        Ok((newcombat, all_logs))
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
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatIncap<'a> {
    pub combat: &'a Combat,
}
impl<'a> CombatIncap<'a> {
    pub fn next_turn(&self, terrain: &Map) -> Result<(Combat, Vec<CombatLog>), GameError> {
        self.combat.next_turn(terrain)
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
    pub fn move_creature(&self, terrain: &Map, pts: Vec<Point3>) -> Result<(Combat, Vec<CombatLog>), GameError> {
        let mut combat = self.combat.clone();
        let mut all_logs = vec![];
        for pt in pts {
            let cpos = combat.current_creature().pos();
            // 145 ~ sqrt(200)
            if point3_distance(cpos, pt) > Distance(145) {
                return Err(GameError::StepTooBig {
                    from: cpos,
                    to: pt,
                });
            }
            combat = {
                let mvmt = combat.get_movement()?;
                let r = mvmt.teleport(terrain, pt)?;
                all_logs.extend(r.1);
                r.0
            }
        }
        Ok((combat, all_logs))
    }

    pub fn teleport(&self, terrain: &Map, pt: Point3) -> Result<(Combat, Vec<CombatLog>), GameError> {
        let c = self.combat.current_creature();
        let distance = point3_distance(c.pos(), pt);
        if distance > self.movement_left {
            Err(GameError::NotFastEnough {
                creature: c.id(),
                speed: c.speed(),
                distance: distance,
                from: c.pos(),
                to: pt,
            })
        } else {
            let mut new = self.combat.clone();
            new.movement_used = new.movement_used + distance;
            let logs = {
                let mut c = new.current_creature_mut();
                let (newc, logs) = c.set_pos(pt)?;
                *c = newc;
                logs
            };
            new.update_movement_options_mut(terrain);
            let cid = new.current_creature().id();
            Ok((new, creature_logs_into_combat_logs(cid, logs)))
        }
    }
}

/// The ability to act in combat.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatAble<'a> {
    pub combat: &'a Combat,
}
impl<'a> CombatAble<'a> {
    /// Make the current creature use an ability.
    pub fn act(&self,
               ability: &Ability,
               target: DecidedTarget)
               -> Result<(Combat, Vec<CombatLog>), GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.combat.clone();
        let mut all_logs = vec![];
        {
            let mut targets = Self::resolve_targets(&newgame, ability.target, target)?;
            for effect in &ability.effects {
                for creature_id in targets.drain(..) {
                    let mut creature = newgame.get_creature_mut(creature_id)?;
                    let (newcreat, logs) = creature.apply_effect(effect)?;
                    *creature = newcreat;
                    all_logs.extend(creature_logs_into_combat_logs(creature_id, logs));
                }
            }
        }
        Ok((newgame, all_logs))
    }

    pub fn next_turn(&self, terrain: &Map) -> Result<(Combat, Vec<CombatLog>), GameError> {
        self.combat.next_turn(terrain)
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
                    Err(GameError::TargetOutOfRange)
                }
            }
            (TargetSpec::Range(max), DecidedTarget::Range(cid)) => {
                let target_creature = combat.get_creature(cid)?;
                if creature_within_distance(combat.creatures.get_current(), target_creature, max) {
                    Ok(vec![cid])
                } else {
                    Err(GameError::TargetOutOfRange)
                }
            }
            _ => {
                let unhandled = true;
                panic!("Unhandled decided target!")
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    extern crate test;
    use self::test::Bencher;

    use combat::*;
    use creature::test::*;
    use types::test::*;

    /// Create a Test combat. Combat order is rogue, ranger, then cleric.
    pub fn t_combat() -> Combat {
        let rogue = t_rogue("rogue");
        let ranger = t_ranger("ranger");
        let cleric = t_cleric("cleric");
        Combat::new(vec![rogue, ranger, cleric], &vec![]).unwrap()
    }

    pub fn t_act(c: &Combat,
                 ab: &Ability,
                 target: DecidedTarget)
                 -> Result<(Combat, Vec<CombatLog>), GameError> {
        c.get_able()?.act(ab, target)
    }

    /// Try to melee-atack the ranger when the ranger is out of melee range.
    #[test]
    fn target_melee_out_of_range() {
        let mut combat = t_combat();
        let melee = t_punch();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((2, 0, 0)).unwrap().0;
        }
        assert_eq!(t_act(&combat, &melee, DecidedTarget::Melee(cid("ranger"))),
                   Err(GameError::TargetOutOfRange));
    }

    /// Ranged attacks against targets (just) within range are successful.
    #[test]
    fn target_range() {
        let mut combat = t_combat();
        let range_ab = t_shoot();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((5, 0, 0)).unwrap().0;
        }
        let _: Combat = t_act(&combat, &range_ab, DecidedTarget::Range(cid("ranger"))).unwrap().0;
    }

    /// Ranged attacks against targets outside of range return `TargetOutOfRange`
    #[test]
    fn target_out_of_range() {
        let mut combat = t_combat();
        let shoot = t_shoot();
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((6, 0, 0)).unwrap().0;
        }
        assert_eq!(t_act(&combat, &shoot, DecidedTarget::Range(cid("ranger"))),
                   Err(GameError::TargetOutOfRange));

        // d((5,1,0), (0,0,0)).round() is still 5 so it's still in range
        {
            let mut creature = combat.get_creature_mut(cid("ranger")).unwrap();
            *creature = creature.set_pos((5, 3, 0)).unwrap().0;
        }
        assert_eq!(t_act(&combat, &shoot, DecidedTarget::Range(cid("ranger"))),
                   Err(GameError::TargetOutOfRange));
    }

    #[test]
    fn move_too_far() {
        let combat = t_combat();
        assert_eq!(combat.get_movement().unwrap().teleport(&vec![], (11, 0, 0)),
                   Err(GameError::NotFastEnough {
                       creature: cid("rogue"),
                       speed: Distance(1086),
                       distance: Distance(1100),
                       from: (0, 0, 0),
                       to: (11, 0, 0),
                   }))
    }

    #[test]
    fn move_some_at_a_time() {
        let combat = t_combat();
        let combat = combat.get_movement().unwrap().teleport(&vec![], (5, 0, 0)).unwrap().0;
        assert_eq!(combat.current_creature().pos(), (5, 0, 0));
        let combat = combat.get_movement().unwrap().teleport(&vec![], (10, 0, 0)).unwrap().0;
        assert_eq!(combat.current_creature().pos(), (10, 0, 0));
        assert_eq!(combat.get_movement().unwrap().teleport(&vec![], (11, 0, 0)),
                   Err(GameError::NotFastEnough {
                       creature: cid("rogue"),
                       speed: Distance(1086),
                       distance: Distance::new(1.0),
                       from: (10, 0, 0),
                       to: (11, 0, 0),
                   }))
    }

    #[bench]
    fn three_char_infinite_combat(bencher: &mut Bencher) {
        let mut combat = t_combat();
        let punch = t_punch();
        let heal = t_heal();
        let iter = |combat: &Combat| -> Result<Combat, GameError> {
            let combat = t_act(&combat, &punch, DecidedTarget::Melee(cid("ranger")))?.0;
            let combat = combat.next_turn(&vec![])?.0.next_turn(&vec![])?.0;
            let combat = t_act(&combat, &heal, DecidedTarget::Range(cid("ranger")))?.0;
            let combat = combat.next_turn(&vec![])?.0;
            Ok(combat)
        };
        bencher.iter(|| {
            combat = iter(&combat).unwrap();
            combat.clone()
        });
    }
}
