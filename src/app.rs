use std::collections::VecDeque;
use std::collections::HashMap;
use std::fmt::Debug;

use types::*;

use serde::{Serialize, Deserialize};

/// Similar to `types::CombatVari`, but for an [App](struct.App.html) instead of a
/// [Combat](../types/struct.Combat.html)
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum AppVari {
    Incap(App<Incap>),
    Casting(App<Casting>),
    Able(App<Able>),
    NoCombat(App<NoCombat>),
}

// CombatType is a type-level function mapping our CreatureState types (Able, Incap, etc) to a
// representation for Combat.
pub trait CombatTypeFn {
    type Type;
}
/// A nicer syntax for calling the type-level function: `CombatType<T>` instead of `<T as
/// CombatType>::Type`
type CombatType<CS> = <CS as CombatTypeFn>::Type;

impl CombatTypeFn for NoCombat {
    type Type = ();
}
impl CombatTypeFn for Incap {
    type Type = Combat<Incap>;
}
impl CombatTypeFn for Able {
    type Type = Combat<Able>;
}
impl CombatTypeFn for Casting {
    type Type = Combat<Casting>;
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
///
/// The CreatureState type parameter is one of the types we use for representing what state the
/// "current creature" is in (Incap, Able, etc), with the addition of NoCombat, which is
/// used when there's no current combat. This CreatureState parameter effects the type of the
/// `current_combat` field, which is usually some Combat<T> (Combat<Incap>, Combat<Able>...) but in
/// the case of NoCombat, it's () -- so that we don't have any current_combat at all
/// inside of the App.
///
/// This is basically all an alternative to having current_combat be an
/// Option<Combat<CreatureState>>, which would require me to pattern match at runtime when looking
/// at current_combat.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App<CreatureState: CombatTypeFn>
    where CombatType<CreatureState>: Serialize + Deserialize + Clone + Eq + PartialEq + Debug
{
    // more state might need to go into the history... not sure
    game_history: VecDeque<CombatVari>,
    current_combat: CombatType<CreatureState>,
    abilities: HashMap<AbilityID, Ability>,
    creatures: HashMap<CreatureID, Creature>,
}

impl<CreatureState> App<CreatureState>
    where CreatureState: CombatTypeFn,
          CombatType<CreatureState>: Serialize + Deserialize + Clone + Eq + PartialEq + Debug
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
                    creatures: self.creatures,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Able(able_game) => {
                AppVari::Able(App {
                    current_combat: able_game,
                    creatures: self.creatures,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Casting(casting_game) => {
                AppVari::Casting(App {
                    current_combat: casting_game,
                    creatures: self.creatures,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
        }
    }
}

impl App<NoCombat> {
    /// Create a Combat and return a new App with it.
    fn start_combat(self, combatants: Vec<CreatureID>) -> Option<AppVari> {
        let combatant_objs: Vec<Creature> =
            combatants.iter().flat_map(|cid| self.creatures.get(cid)).cloned().collect();
        if combatant_objs.len() != combatants.len() {
            None
        } else {
            CombatVari::new(combatant_objs).map(|cv| self.to_app_vari(cv))
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
        /// FIXME XXX ugh having to_combat_vari as public is gross
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


#[test]
fn workflow() {
    let mut creatures = HashMap::new();
    let creature = t_creature();
    creatures.insert(CreatureID("bob".to_string()), creature);
    let app: App<NoCombat> = App {
        game_history: VecDeque::new(),
        abilities: HashMap::new(),
        current_combat: (),
        creatures: creatures,
    };
    let app = app.start_combat(vec![CreatureID("bob".to_string())])
        .expect("start_combat didn't return Some");
}
