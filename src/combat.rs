//! Representation and simulation of combat.
//! Many simple combat-oriented types are in `types.rs`, but this module implements the
//! Combat and CombatCapability types. The most interesting top-level method is `Combat::act`.

use nonempty;

use creature::*;
use types::*;
use take_mut::take;
use grid::creature_within_distance;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
const MELEE_RANGE: Distance = Distance(150);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat {
    // Since we serialize a whole history of combats to JSON, using Rc<Creature> pointless after we
    // load data back in, because serde doesn't (currently) have any way to know that multiple
    // Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<Creature>,
}

impl Combat {
    pub fn new(combatants: Vec<Creature>) -> Option<Combat> {
        nonempty::NonEmptyWithCursor::from_vec(combatants).map(|ne| Combat { creatures: ne })
    }

    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    fn tick(&mut self) -> Combat {
        let mut newgame = self.clone();
        for creature in newgame.creatures.iter_mut() {
            take(creature, |c: Creature| c.tick());
        }
        newgame

    }
    // This is inefficient. Two options:
    // 1. Store an additional HashMap<CreatureID, Idx> here on Combat
    // 2. Implement a NonEmptyLinkedHashMapWithCursor (https://crates.io/crates/linked-hash-map)
    //    (though that may not be efficient either)
    pub fn get_creature(&self, cid: CreatureID) -> Option<&Creature> {
        for creature in self.creatures.iter() {
            if creature.id() == cid {
                return Some(creature);
            }
        }
        None
    }

    pub fn get_creature_mut(&mut self, cid: CreatureID) -> Option<&mut Creature> {
        for creature in self.creatures.iter_mut() {
            if creature.id() == cid {
                return Some(creature);
            }
        }
        None
    }

    pub fn capability(&self) -> CombatCapability {
        if self.current_creature().can_act() {
            CombatCapability::Able(CombatAble(self))
        } else {
            CombatCapability::Incap(CombatIncap(self))
        }
    }

    fn resolve_targets(&self,
                       target: TargetSpec,
                       decision: DecidedTarget)
                       -> Result<Vec<CreatureID>, GameError> {
        match (target, decision) {
            (TargetSpec::Melee, DecidedTarget::Melee(cid)) => {
                let target_creature = self.get_creature(cid.clone())
                    .ok_or_else(|| GameError::InvalidTarget(cid.clone()))?;
                if creature_within_distance(self.creatures.get_current(),
                                            target_creature,
                                            MELEE_RANGE) {
                    Ok(vec![cid])
                } else {
                    Err(GameError::TargetOutOfRange)
                }
            }
            (TargetSpec::Range(max), DecidedTarget::Range(cid)) => {
                let target_creature = self.get_creature(cid.clone())
                    .ok_or_else(|| GameError::InvalidTarget(cid.clone()))?;
                if creature_within_distance(self.creatures.get_current(), target_creature, max) {
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatIncap<'a>(&'a Combat);
impl<'a> CombatIncap<'a> {
    pub fn skip(&self) -> Combat {
        let mut newgame = self.0.clone();
        newgame.tick()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CombatAble<'a>(&'a Combat);
impl<'a> CombatAble<'a> {
    /// Make the current creature use an ability.
    pub fn act(&self, ability: &Ability, target: DecidedTarget) -> Result<Combat, GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.0.clone();
        {
            let mut targets = newgame.resolve_targets(ability.target, target)?;
            for effect in &ability.effects {
                for creature_id in targets.drain(..) {
                    let mut creature = newgame.get_creature_mut(creature_id.clone())
                        .ok_or_else(|| GameError::InvalidTargetNoSense(creature_id.clone()))?;
                    take(creature, |c| c.apply_effect(effect));
                }
            }
        }
        newgame.creatures.next_circular();
        Ok(newgame.tick())
    }
}

/// A `CombatCapability` must be pattern-matched to determine which operations we can perform on
/// behalf of the current creature. Each variant contains a different wrapper of `Combat`, and each
/// of those different types provide different methods for doing only what is possible. For
/// example, `CombatCapability::Incap` wraps `CombatIncap`, which only has a `skip` method, since
/// incapacitated creatures cannot act, whereas `CombatCapability::Able(CombatAble)` allows use of
/// the `act` method.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum CombatCapability<'a> {
    Incap(CombatIncap<'a>),
    Able(CombatAble<'a>),
}

#[cfg(test)]
pub fn t_combat() -> Combat {
    let bob = t_rogue("bob");
    let chris = t_rogue("chris");
    Combat::new(vec![bob, chris]).unwrap()
}

#[cfg(test)]
pub fn t_act(c: &Combat, ab: &Ability, target: DecidedTarget) -> Result<Combat, GameError> {
    match c.capability() {
        CombatCapability::Able(able) => able.act(ab, target),
        _ => panic!("Not an Able combat"),
    }
}

#[test]
fn target_melee_non_adjacent() {
    let mut combat = t_combat();
    let melee = t_melee();
    take(combat.get_creature_mut(cid("chris")).unwrap(),
         |c| c.set_pos((2, 0, 0)));
    assert_eq!(t_act(&combat, &melee, DecidedTarget::Melee(cid("chris"))),
               Err(GameError::TargetOutOfRange));
}

#[test]
fn target_range() {
    let mut combat = t_combat();
    let range_ab = t_ranged();
    take(combat.get_creature_mut(cid("chris")).unwrap(),
         |c| c.set_pos((5, 0, 0)));
    let _: Combat = t_act(&combat, &range_ab, DecidedTarget::Range(cid("chris"))).unwrap();
}

#[test]
fn target_out_of_range() {
    let mut combat = t_combat();
    let range_ab = t_ranged();
    take(combat.get_creature_mut(cid("chris")).unwrap(),
         |c| c.set_pos((6, 0, 0)));
    assert_eq!(t_act(&combat, &range_ab, DecidedTarget::Range(cid("chris"))),
               Err(GameError::TargetOutOfRange));

    // d((5,1,0), (0,0,0)).round() is still 5 so it's still in range
    take(combat.get_creature_mut(cid("chris")).unwrap(),
         |c| c.set_pos((5, 3, 0)));
    assert_eq!(t_act(&combat, &range_ab, DecidedTarget::Range(cid("chris"))),
               Err(GameError::TargetOutOfRange));
}
