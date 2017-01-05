use std::collections::VecDeque;
use std::collections::HashMap;

use types::*;

/// Similar to `types::CombatVari`, but for an [App](struct.App.html) instead of a
/// [Combat](../types/struct.Combat.html)
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum AppVari {
    Incap(App<Incap>),
    Casting(App<Casting>),
    Able(App<Able>),
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App<PlayerState> {
    game_history: VecDeque<CombatVari>,
    current_game: Combat<PlayerState>,
    abilities: HashMap<AbilityID, Ability>,
}

impl<PlayerState> App<PlayerState> {
    fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(&ability_id).ok_or(GameError::InvalidAbility)?.clone())
    }

    /// Consume this App, and consume an CombatVari, to return a new AppVari.
    fn to_app_vari(self, combat: CombatVari) -> AppVari {
        match combat {
            CombatVari::Incap(incap_game) => {
                AppVari::Incap(App {
                    current_game: incap_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Able(able_game) => {
                AppVari::Able(App {
                    current_game: able_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            CombatVari::Casting(casting_game) => {
                AppVari::Casting(App {
                    current_game: casting_game,
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
        let g = op(&self.current_game)?;
        if self.game_history.len() >= 1000 {
            let _ = self.game_history.pop_front();
        }
        self.game_history.push_back(self.current_game.clone().to_combat_vari());
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
