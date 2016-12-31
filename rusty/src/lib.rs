#![feature(proc_macro)]
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;

use std::collections::VecDeque;
use std::collections::HashMap;
use std::error::Error;
mod types;
use types::*;

/// A data structure maintaining state for the whole app. While the types in types.rs are all
/// operated on immutably, this is the mutable top-level type. It keeps track of the history of the
/// whole game, and exposes the top-level methods that will traverse the state machine of the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    game_history: VecDeque<Game>,
    current_game: Game,
    abilities: HashMap<String, Ability>,
}

impl App {
    pub fn new(creatures: Vec<Creature>) -> App {
        App {
            abilities: HashMap::new(),
            game_history: VecDeque::with_capacity(1000),
            current_game: Game::new(creatures),
        }
    }

    fn perform_op<F>(&mut self, op: F) -> Result<(), GameError>
        where F: FnOnce(&Game) -> Result<Game, GameError>
    {
        match op(&self.current_game) {
            Ok(g) => {
                if self.game_history.len() >= 1000 {
                    let _ = self.game_history.pop_front();
                }
                self.game_history.push_back(self.current_game.clone());
                self.current_game = g;
                Ok(())
            }
            Err(x) => Err(x),
        }
    }

    pub fn act(&mut self, ability_name: String, targets: Vec<usize>) -> Result<(), GameError> {
        let ability = self.abilities.get(&ability_name).ok_or(GameError::InvalidAbility)?.clone();
        self.perform_op(move |g| {
            if g.current_creature().has_ability(ability_name) {
                g.act(&ability, targets)
            } else {
                Err(GameError::InvalidAbility)
            }
        })
    }
}


fn opt2res<T, E>(opt: Option<T>, error: E) -> Result<T, E>
    where E: Error
{
    match opt {
        Some(x) => Ok(x),
        None => return Err(error),
    }
}

#[test]
fn test_history() {
    ap
}
