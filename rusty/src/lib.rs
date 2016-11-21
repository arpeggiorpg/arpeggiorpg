
// Using "owned" fields means that to purely manipulate a game we must either:
// 1. destroy the old one, thus preventing keeping track of game history
// 2. clone the ENTIRE game, including all of its contents, in a memory-inefficient way,
// if we want to keep track of history.
// so, we should _probably_ use references for the fields, but then we have to keep track of
// lifetimes...
//

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Game<'a, S> {
    pub state: S,
    pub creatures: &'a Vec<&'a Creature>,
    current_creature: usize,
}

impl<'a> Game<'a, GameStarting> {
    pub fn new(creatures: &'a Vec<&'a Creature>) -> Game<'a, GameStarting> {
        Game {
            state: GameStarting,
            creatures: creatures,
            current_creature: 0,
        }
    }
    pub fn start(&self) -> Game<PlayerChoosingAbility> {
        Game {
            state: PlayerChoosingAbility,
            creatures: self.creatures,
            current_creature: 0,
        }
    }
}

impl<'a, T> Game<'a, T> {
    pub fn current_creature(self) -> &'a Creature {
        self.creatures[self.current_creature]
    }
}


#[derive(Eq, PartialEq, Debug)]
pub struct GameStarting;
#[derive(Eq, PartialEq, Debug)]
pub struct PlayerChoosingAbility;
#[derive(Eq, PartialEq, Debug)]
pub struct PlayerChoosingTargets {
    pub ability: Ability,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Creature {
    pub name: String,
    pub energy: i32,
    pub abilities: Vec<Ability>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Ability {
    pub name: String,
}
