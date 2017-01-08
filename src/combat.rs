//! Representation and simulation of combat.
//! Many simple combat-oriented types are in `types.rs`, but this module implements the
//! CombatVari/Combat types. The most interesting top-level method is `Combat<A>::act`.

use std::marker::PhantomData;
use nonempty;

use creature::*;
use types::*;
use take_mut::take;
use grid::distance;

/// This is set to 1.5 so that it's greater than sqrt(2) -- meaning that creatures can attack
/// diagonally!
const MELEE_RANGE: f32 = 1.5;

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Combat<CreatureState> {
    // Since we serialize a whole history of combats to JSON, using Rc<Creature> pointless after we
    // load data back in, because serde doesn't (currently) have any way to know that multiple
    // Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: nonempty::NonEmptyWithCursor<CreatureVari>,
    _p: PhantomData<CreatureState>,
}

pub trait HasCreature<T> {
    fn current_creature(&self) -> &Creature<T>;
}

impl HasCreature<Able> for Combat<Able> {
    fn current_creature(&self) -> &Creature<Able> {
        match *self.creatures.get_current() {
            CreatureVari::Able(ref c) => c,
            _ => {
                panic!("Somehow the current creature of a Combat<Able> was not Able! {:?}",
                       self)
            }
        }
    }
}

impl HasCreature<Incap> for Combat<Incap> {
    fn current_creature(&self) -> &Creature<Incap> {
        match *self.creatures.get_current() {
            CreatureVari::Incap(ref c) => c,
            _ => {
                panic!("Somehow the current creature of a Combat<Able> was not Able! {:?}",
                       self)
            }
        }
    }
}

impl HasCreature<Casting> for Combat<Casting> {
    fn current_creature(&self) -> &Creature<Casting> {
        match *self.creatures.get_current() {
            CreatureVari::Casting(ref c) => c,
            _ => {
                panic!("Somehow the current creature of a Combat<Able> was not Able! {:?}",
                       self)
            }
        }
    }
}


impl<CreatureState> Combat<CreatureState>
    where Combat<CreatureState>: HasCreature<CreatureState>
{
    fn tick(&mut self) {
        for creature in self.creatures.iter_mut() {
            take(creature, |c: CreatureVari| c.tick());
        }
    }

    /// Consume this game and wrap it in an `CombatVari`.
    pub fn into_combat_vari(self) -> CombatVari {
        if self.current_creature().can_act() {
            CombatVari::Able(Combat {
                creatures: self.creatures,
                _p: PhantomData,
            })
        } else {
            CombatVari::Incap(Combat {
                creatures: self.creatures,
                _p: PhantomData,
            })
        }
    }
}

impl Combat<Incap> {
    pub fn skip(&self) -> CombatVari {
        let mut newgame = self.clone();
        newgame.tick();
        newgame.into_combat_vari()
    }
}

impl Combat<Able> {
    /// Make the current creature use an ability.
    pub fn act(&self, ability: &Ability, target: DecidedTarget) -> Result<CombatVari, GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.clone();
        {
            let mut targets = newgame.resolve_targets(target)?;
            for effect in &ability.effects {
                for creature_id in targets.drain(..) {
                    let mut creature = newgame.get_creature_mut(creature_id.clone())
                        .ok_or_else(|| GameError::InvalidTargetNoSense(creature_id.clone()))?;
                    take(creature, |c| c.apply_effect(effect));
                }
            }
        }
        newgame.creatures.next_circular();
        newgame.tick();
        Ok(newgame.into_combat_vari())
    }

    fn resolve_targets(&self, target: DecidedTarget) -> Result<Vec<CreatureID>, GameError> {
        // FIXME: This doesn't take into consideration the actual target system used by the
        // ability. IIRC, the Haskell version used a typesafe approach to ensure that actual target
        // matched up with spec target.
        match target {
            DecidedTarget::Melee(cid) => {
                let target_creature = self.get_creature(cid.clone())
                    .ok_or_else(|| GameError::InvalidTarget(cid.clone()))?;
                if distance(self.creatures.get_current(), target_creature) <= MELEE_RANGE {
                    Ok(vec![cid])
                } else {
                    Err(GameError::TargetOutOfRange)
                }
            }
            DecidedTarget::Range(cid) => Ok(vec![cid]),
            _ => {
                let unhandled = true;
                panic!("Unhandled decided target!")
            }
        }
    }
}

impl<T> Combat<T> {
    // This is inefficient. Two options:
    // 1. Store an additional HashMap<CreatureID, Idx> here on Combat
    // 2. Implement a NonEmptyLinkedHashMapWithCursor (https://crates.io/crates/linked-hash-map)
    //    (though that may not be efficient either)
    pub fn get_creature(&self, cid: CreatureID) -> Option<&CreatureVari> {
        for creature in self.creatures.iter() {
            if creature.id() == cid {
                return Some(creature);
            }
        }
        None
    }

    pub fn get_creature_mut(&mut self, cid: CreatureID) -> Option<&mut CreatureVari> {
        for creature in self.creatures.iter_mut() {
            if creature.id() == cid {
                return Some(creature);
            }
        }
        None
    }
}

/// A `CombatVari` must be pattern-matched to determine which operations we can perform on behalf
/// of the current creature. Each variant contains a different type of `Combat`, and each of those
/// different types provide different methods for doing only what is possible. For example,
/// `CombatVari::Incap` wraps `Combat<Incap>`, which only has a `skip` method, since incapacitated
/// creatures cannot act, whereas `CombatVari::Able(Combat<Able>)` allows use of the `act` method.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum CombatVari {
    Incap(Combat<Incap>),
    Casting(Combat<Casting>),
    Able(Combat<Able>),
}

impl CombatVari {
    pub fn new(combatants: Vec<CreatureVari>) -> Option<CombatVari> {
        nonempty::NonEmptyWithCursor::from_vec(combatants).map(|ne| {
            match *ne.get_current() {
                CreatureVari::Able(_) => {
                    CombatVari::Able(Combat {
                        creatures: ne,
                        _p: PhantomData,
                    })
                }
                CreatureVari::Casting(_) => {
                    CombatVari::Casting(Combat {
                        creatures: ne,
                        _p: PhantomData,
                    })
                }
                CreatureVari::Incap(_) => {
                    CombatVari::Incap(Combat {
                        creatures: ne,
                        _p: PhantomData,
                    })
                }
            }
        })
    }
}


#[cfg(test)]
pub fn t_combat() -> Combat<Able> {
    let bob = t_rogue("bob");
    let chris = t_rogue("chris");
    Combat {
        creatures: nonempty::NonEmptyWithCursor::from_vec(vec![bob.into_vari(), chris.into_vari()])
            .unwrap(),
        _p: PhantomData,
    }
}

#[test]
fn target_melee_non_adjacent() {
    let mut combat = t_combat();
    let melee = t_melee();
    take(combat.get_creature_mut(CreatureID("chris".to_string())).unwrap(),
         |c| c.set_pos((2, 0, 0)));
    assert_eq!(combat.act(&melee,
                          DecidedTarget::Melee(CreatureID("chris".to_string()))),
               Err(GameError::TargetOutOfRange));
}
