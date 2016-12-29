
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Game<S> {
    pub state: S,
    pub creatures: Vec<Rc<Creature>>,
    current_creature: usize,
}

impl Game<GameStarting> {
    pub fn new(creatures: Vec<Rc<Creature>>) -> Game<GameStarting> {
        Game {
            state: GameStarting,
            creatures: creatures,
            current_creature: 0,
        }
    }
    pub fn start(&self) -> Game<PlayerChoosingAbility> {
        Game {
            state: PlayerChoosingAbility,
            creatures: self.creatures.clone(),
            current_creature: 0,
        }
    }
}

impl<T> Game<T> {
    pub fn current_creature(self) -> Rc<Creature> {
        self.creatures[self.current_creature].clone()
    }
}

#[derive(Debug)]
pub enum GameWithState {
    GS(Game<GameStarting>),
    PCA(Game<PlayerChoosingAbility>),
    PCT(Game<PlayerChoosingTargets>),
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
