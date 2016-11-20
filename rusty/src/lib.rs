
/*
Using "owned" fields means that to purely manipulate a game we must either:
1. destroy the old one, thus preventing keeping track of game history
2. clone the ENTIRE game, including all of its contents, in a memory-inefficient way,
   if we want to keep track of history.
so, we should _probably_ use references for the fields, but then we have to keep track of
lifetimes...

Turns out avoiding owned fields is pretty dang easy (???), see GameR.
*/

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Game<S> {
    pub state: S,
    current_creature: usize,
    pub creatures: Vec<Creature>
}

impl Game<GameStarting> {
    pub fn new(creatures: Vec<Creature>) -> Game<GameStarting> {
        Game {state: GameStarting, current_creature: 0, creatures: creatures}
    }

    pub fn start(self) -> Game<PlayerChoosingAbility> {
        Game {state: PlayerChoosingAbility, creatures: self.creatures, current_creature: 0}
    }
}

impl<T> Game<T> {
    pub fn current_creature_name(self) -> String {
       self.creatures[self.current_creature].name.clone()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct GameR<'a, S> {
    pub state: S,
    pub creatures: &'a Vec<&'a Creature>,
    current_creature: usize
}

impl<'a> GameR<'a, GameStarting> {
    pub fn new(creatures: &'a Vec<&'a Creature>) -> GameR<'a, GameStarting> {
        GameR {state: GameStarting, creatures: creatures, current_creature: 0}
    }
    pub fn start(&self) -> GameR<PlayerChoosingAbility> {
        GameR {state: PlayerChoosingAbility, creatures: self.creatures, current_creature: 0}
    }
}

impl<'a, T> GameR<'a, T> {
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
    pub ability: Ability
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Creature {
    pub name: String,
    pub energy: i32,
    pub abilities: Vec<Ability>
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Ability {
    pub name: String
}
