#![feature(proc_macro)]
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;
use std::rc::Rc;

mod types;
use types::*;
/// A data structure maintaining state for the whole app. While the types in types.rs are all
/// operated immutably, this is the mutable top-level type. It keeps track of the history of the
/// whole game, and exposes the top-level methods that will traverse the state machine of the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    game_history: Vec<Game>,
}

impl App {
    pub fn new(creatures: Vec<Rc<Creature>>) -> App {
        App { game_history: vec![Game::new(creatures)] }
    }

    fn perform_op<F>(&mut self, op: F) -> Result<(), GameError>
        where F: FnOnce(&Game) -> Result<Game, GameError>
    {
        match op(self.game_history.last().unwrap()) {
            Ok(g) => {
                self.game_history.push(g);
                Ok(())
            }
            Err(x) => Err(x),
        }
    }

    pub fn choose_ability(&mut self, ability_name: String) -> Result<(), GameError> {
        // last.unwwrap: App is always initialized with a game
        self.perform_op(move |game| game.choose_ability(ability_name))
    }
}
