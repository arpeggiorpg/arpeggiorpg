/// Core simulation types, all immutable.

use std::error::Error;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(u8);

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

    pub fn choose_ability(&self, ability_name: String) -> Result<Game, GameError> {
        match self.state {
            GameState::PlayerChoosingAbility => {
                let creature = self.current_creature();
                if creature.has_ability(ability_name.clone()) {
                    let mut newgame = self.clone();
                    newgame.state =
                        GameState::PlayerChoosingTargets { ability: ability_name.clone() };
                    Ok(newgame)
                } else {
                    Err(GameError::InvalidAbility)
                }
            }
            _ => Err(GameError::InvalidState),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    InvalidState,
    InvalidAbility,
}
impl fmt::Display for GameError {
    fn fmt(&self, fmter: &mut fmt::Formatter) -> fmt::Result {
        write!(fmter, "{}", format!("{:?}", self))
    }
}

impl Error for GameError {
    fn description(&self) -> &str {
        "A Game Error occurred"
    }
}

/// A state that the game can be in.
///
// It'd be nice to also have types representing each of these states, so we can write functions
// that are statically verified as only callable in certain states. This enum would then need to
// wrap those types, like: "GameStarting(GameStarting),
// PlayerChoosingTargets(PlayerChoosingTargets)".
//
// If Niko Matsakis's idea of enum variant subtyping[1] ever happens, we'll be able to have nice
// type safe state machine functions without having to write tons of type boilerplate.
//
// [1] http://smallcultfollowing.com/babysteps/blog/2015/08/20/virtual-structs-part-3-bringing-enums-and-structs-together/
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum GameState {
    GameStarting,
    PlayerChoosingAbility,
    PlayerChoosingTargets { ability: String },
}


#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature {
    name: String,
    energy: Energy,
    abilities: Vec<AbilityStatus>,
    max_health: u8,
    cur_health: u8, // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
}

impl Creature {
    pub fn new(name: String, energy: Energy, abilities: Vec<Ability>) -> Creature {
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
    pub fn has_ability(&self, ability_name: String) -> bool {
        for ability in self.abilities.iter() {
            if ability.ability.name == ability_name {
                return true;
            }
        }
        return false;
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Ability {
    pub name: String,
    cost: Energy,
    effects: Vec<Effect>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Effect {
    Interrupt,
    ApplyCondition(Condition),
    Heal(u8),
    Damage(u8),
    MultiEffect(Vec<Effect>),
    GenerateEnergy(Energy),
    Resurrect,
}


#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum Condition {
    RecurringEffect(Box<Effect>),
    Dead,
    Incapacitated,
    DamageBuff(u8),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AppliedCondition {
    remaning: u8,
    condition: Condition,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct AbilityStatus {
    ability: Ability,
    cooldown: u8,
}
