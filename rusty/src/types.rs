/// Core simulation types, all immutable.

use std::error::Error;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Energy(u8);

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
    // Since we serialize a whole history of Games to JSON, using Rc<Creature> pointless after we
    // load data back in, because serde doesn't (currently) have any way to know that multiple
    // Rc-wrapped values should be unified. See
    // https://github.com/erickt/serde-rfcs/blob/master/text/0001-serializing-pointers.md
    //
    // A simpler way to share these references would probably be to store a Vec<Creature> on App,
    // and then either have Vec<&Creature> here, or Vec<CreatureID>.
    pub creatures: Vec<Creature>,
    current_creature: usize,
}

impl Game {
    pub fn new(creatures: Vec<Creature>) -> Game {
        Game {
            creatures: creatures,
            current_creature: 0,
        }
    }

    pub fn current_creature(&self) -> &Creature {
        &self.creatures[self.current_creature]
    }

    /// Cause the current creature to act.
    pub fn act(&self, ability: &Ability, targets: Vec<usize>) -> Result<Game, GameError> {
        Ok(self.clone())
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

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Creature {
    name: String,
    energy: Energy,
    abilities: Vec<AbilityStatus>,
    max_health: u8,
    cur_health: u8, // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
    pos: (u32, u32, u32),
}

impl Creature {
    pub fn new(name: String, energy: Energy, abilities: Vec<Ability>) -> Creature {
        Creature {
            pos: (0, 0, 0),
            name: name,
            energy: energy,
            max_health: 10,
            cur_health: 10,
            abilities: abilities.iter()
                .map(|ab| {
                    AbilityStatus {
                        ability: ab.name.clone(),
                        cooldown: 0,
                    }
                })
                .collect(),
        }
    }

    pub fn has_ability(&self, ability_name: String) -> bool {
        self.abilities
            .iter()
            .any(|&AbilityStatus { ability: ref ab, cooldown: _ }| ab == &ability_name)
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
    ability: String,
    cooldown: u8,
}
