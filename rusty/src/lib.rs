
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Game {
    pub state: GameState,
    pub creatures: Vec<Rc<Creature>>,
    current_creature: usize,
}

impl Game {
    pub fn new(creatures: Vec<Rc<Creature>>) -> Game {
        Game {
            state: GameState::GameStarting,
            creatures: creatures,
            current_creature: 0,
        }
    }

    pub fn start(&self) -> Game {
        Game {
            state: GameState::PlayerChoosingAbility,
            creatures: self.creatures.clone(),
            current_creature: 0,
        }
    }

    pub fn current_creature(self) -> Rc<Creature> {
        self.creatures[self.current_creature].clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameState {
    GameStarting,
    PlayerChoosingAbility,
    PlayerChoosingTargets { ability: Ability },
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
