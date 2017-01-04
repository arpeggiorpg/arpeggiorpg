use std::collections::VecDeque;
use std::collections::HashMap;

use types::*;

/// See `types::ActorGame`.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum ActorApp {
    Incap(App<Incap>),
    Casting(App<Casting>),
    Able(App<Able>),
}

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App<PlayerState> {
    game_history: VecDeque<ActorGame>,
    current_game: Game<PlayerState>,
    abilities: HashMap<AbilityID, Ability>,
}

impl<PlayerState> App<PlayerState> {
    fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(&ability_id).ok_or(GameError::InvalidAbility)?.clone())
    }

    /// Consume this App, and consume an ActorGame, to return a new ActorApp.
    fn to_actor_app(self, game: ActorGame) -> ActorApp {
        match game {
            ActorGame::Incap(incap_game) => {
                ActorApp::Incap(App {
                    current_game: incap_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            ActorGame::Able(able_game) => {
                ActorApp::Able(App {
                    current_game: able_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
            ActorGame::Casting(casting_game) => {
                ActorApp::Casting(App {
                    current_game: casting_game,
                    abilities: self.abilities,
                    game_history: self.game_history,
                })
            }
        }
    }
}

impl App<Able> {
    fn perform_able_op<F>(mut self, op: F) -> Result<ActorApp, GameError>
        where F: FnOnce(&Game<Able>) -> Result<ActorGame, GameError>
    {
        let g = op(&self.current_game)?;
        if self.game_history.len() >= 1000 {
            let _ = self.game_history.pop_front();
        }
        self.game_history.push_back(self.current_game.clone().to_actor_game());
        Ok(self.to_actor_app(g))
    }

    pub fn act(self, ability_id: AbilityID, targets: Vec<usize>) -> Result<ActorApp, GameError> {
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
