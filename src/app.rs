use std;
use std::collections::VecDeque;
use std::collections::HashMap;

use types::*;

use serde::{Serialize, Deserialize};

/// Similar to `types::CombatVari`, but for an [App](struct.App.html) instead of a
/// [Combat](../types/struct.Combat.html)
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum AppVari {
    Incap(App<Incap>),
    Casting(App<Casting>),
    Able(App<Able>),
    NoCombat(App<NoCurrentCreature>),
}

// CombatType is a type-level function mapping our CreatureState types (Able, Incap, etc) to a
// representation for Combat.
pub trait CombatType {
    type Type;
}
impl CombatType for NoCurrentCreature {
    type Type = ();
}
impl CombatType for Incap {
    type Type = Combat<Incap>;
}
impl CombatType for Able {
    type Type = Combat<Able>;
}
impl CombatType for Casting {
    type Type = Combat<Casting>;
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
///
/// The CreatureState type parameter is one of the types we use for representing what state the
/// "current creature" is in (Incap, Able, etc), with the addition of NoCurrentCreature, which is
/// used when there's no current combat. This CreatureState parameter effects the type of the
/// `current_combat` field, which is usually some Game<T> (Game<Incap>, Game<Able>...) but in the
/// case of NoCurrentCreature, it's () -- so that we don't have any current_combat at all inside of
/// the App.
///
/// This is basically all an alternative to having current_combat be an Option<Game<CreatureState>>,
/// which would require me to pattern match at runtime when looking at current_combat.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App<CreatureState: CombatType>
    where <CreatureState as CombatType>::Type: Serialize + Deserialize + Clone + Eq + PartialEq + std::fmt::Debug
{
    // more state might need to go into the history... not sure
    game_history: VecDeque<CombatVari>,
    current_combat: <CreatureState as CombatType>::Type,
    abilities: HashMap<AbilityID, Ability>,
}

impl<CreatureState> App<CreatureState>
    where CreatureState: CombatType,
          <CreatureState as CombatType>::Type: Serialize + Deserialize + Clone + Eq + PartialEq + std::fmt::Debug
{
    fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(&ability_id).ok_or(GameError::InvalidAbility)?.clone())
    }

/// Consume this App, and consume an CombatVari, to return a new AppVari.
    fn to_app_vari(self, combat: CombatVari) -> AppVari {
        match combat {
            CombatVari::Incap(incap_game) => {
                AppVari::Incap(App {
                    current_combat: incap_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Able(able_game) => {
                AppVari::Able(App {
                    current_combat: able_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Casting(casting_game) => {
                AppVari::Casting(App {
                    current_combat: casting_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
        }
    }
}

impl App<Able> {
    fn perform_able_op<F>(mut self, op: F) -> Result<AppVari, GameError>
        where F: FnOnce(&Combat<Able>) -> Result<CombatVari, GameError>
    {
        let g = op(&self.current_combat)?;
        if self.game_history.len() >= 1000 {
            let _ = self.game_history.pop_front();
        }
        self.game_history.push_back((&self.current_combat).clone().to_combat_vari());
        Ok(self.to_app_vari(g))
    }

    pub fn act(self, ability_id: AbilityID, targets: Vec<usize>) -> Result<AppVari, GameError> {
        let ability = self.get_ability(&ability_id)?;
        self.perform_able_op(move |g| {
            if g.current_creature().has_ability(&ability_id) {
                g.act(&ability, targets)
            } else {
                Err(GameError::InvalidAbility)
            }
        })
    }
}
