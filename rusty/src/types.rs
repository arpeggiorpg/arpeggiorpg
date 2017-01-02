/// Core simulation types, all immutable.

use std::error::Error;
use std::fmt;

use nonempty;

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
    pub creatures: nonempty::NonEmptyWithCursor<Creature>,
}

impl Game {
    pub fn current_creature(&self) -> &Creature {
        self.creatures.get_current()
    }

    /// Cause the current creature to act.
    pub fn act(&self, ability: &Ability, targets: Vec<usize>) -> Result<Game, GameError> {
        // I could write this in an Actually Functional style, but I really don't care as long as
        // the function doesn't have side effects (and the type signature proves it!)
        let mut newgame = self.clone();
        for effect in ability.effects.iter() {
            match effect {
                &Effect::Damage(amt) => {
                    for &tidx in targets.iter() {
                        newgame.creatures
                            .get_mut(tidx)
                            .ok_or(GameError::InvalidTarget)?
                            .cur_health -= amt as i16;
                    }
                }
                x => panic!("Unimplemented effect: {:?}", x),
            }
        }
        newgame.creatures.next_circle();
        Ok(newgame)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GameError {
    InvalidAbility,
    InvalidTarget,
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
    cur_health: i16, // casting: Option<(Ability, u8, SelectedTargetedEffect)> // yowza
    pos: (u32, u32, u32),
}

impl Creature {
    // pub fn new(name: String, energy: Energy, abilities: Vec<Ability>) -> Creature {
    //     Creature {
    //         pos: (0, 0, 0),
    //         name: name,
    //         energy: energy,
    //         max_health: 10,
    //         cur_health: 10,
    //         abilities: abilities.iter()
    //             .map(|ab| {
    //                 AbilityStatus {
    //                     ability: ab.name.clone(),
    //                     cooldown: 0,
    //                 }
    //             })
    //             .collect(),
    //     }
    // }

    pub fn has_ability(&self, ability_name: &str) -> bool {
        self.abilities
            .iter()
            .any(|&AbilityStatus { ability: ref ab, cooldown: _ }| ab == ability_name)
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
