// TODO Just move this to types.rs when we switch to rust 1.15

use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
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

    pub fn current_creature(&self) -> Rc<Creature> {
        self.creatures[self.current_creature].clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameState {
    GameStarting,
    PlayerChoosingAbility,
    PlayerChoosingTargets { ability: Ability },
}


#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub struct Creature {
    name: String,
    energy: u8,
    abilities: Vec<AbilityStatus>,
    max_health: u8,
    cur_health: u8, // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
}

impl Creature {
    pub fn new(name: String, energy: u8, abilities: Vec<Ability>) -> Creature {
        Creature {
            name: name,
            energy: energy,
            max_health: 10,
            cur_health: 10,
            abilities: abilities.iter()
                .map(|ab| {
                    AbilityStatus {
                        ability: ab.clone(),
                        cooldown: 0,
                    }
                })
                .collect(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Ability {
    pub name: String,
}


#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    ability: Ability,
    cooldown: u8,
}
