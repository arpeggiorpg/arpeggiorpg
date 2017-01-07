use std::marker::PhantomData;
use nonempty;

use creature::*;
use types::*;
use take_mut::take;

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
    /// Cause the current creature to act.
    pub fn act(&self, ability: &Ability, targets: Vec<usize>) -> Result<CombatVari, GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.clone();
        for effect in &ability.effects {
            for &tidx in &targets {
                let creature = newgame.creatures.get_mut(tidx).ok_or(GameError::InvalidTarget)?;
                take(creature, |c| c.apply_effect(effect));
            }
        }
        newgame.creatures.next_circular();
        newgame.tick();
        Ok(newgame.into_combat_vari())
    }
}
