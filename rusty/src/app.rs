use std::collections::VecDeque;
use std::collections::HashMap;

use types::*;

/// A data structure maintaining state for the whole app. While the types in types.rs are all
/// operated on immutably, this is the mutable top-level type. It keeps track of the history of the
/// whole game, and exposes the top-level methods that will traverse the state machine of the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    game_history: VecDeque<Game>,
    current_game: Game,
    abilities: HashMap<AbilityID, Ability>,
}

impl App {
    // pub fn new(creatures: Vec<Creature>) -> App {
    //     App {
    //         abilities: HashMap::new(),
    //         game_history: VecDeque::with_capacity(1000),
    //         current_game: Game::new(creatures),
    //     }
    // }

    fn perform_op<F>(&mut self, op: F) -> Result<(), GameError>
        where F: FnOnce(&Game) -> Result<Game, GameError>
    {
        let g = op(&self.current_game)?;
        if self.game_history.len() >= 1000 {
            let _ = self.game_history.pop_front();
        }
        self.game_history.push_back(self.current_game.clone());
        self.current_game = g;
        Ok(())
    }

    fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(&ability_id).ok_or(GameError::InvalidAbility)?.clone())
    }

    pub fn act(&mut self, ability_id: AbilityID, targets: Vec<usize>) -> Result<(), GameError> {
        let ability = self.get_ability(&ability_id)?;
        self.perform_op(move |g| {
            if g.current_creature().has_ability(&ability_id) {
                g.act(&ability, targets)
            } else {
                Err(GameError::InvalidAbility)
            }
        })
    }
}
