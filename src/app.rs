use std::collections::VecDeque;

use game::*;
use types::*;

// random misplaced notes
//
// A workflow:
//
// - player moves or whatever
// - player says they want to attack
// - they send Act with an ability
// - as soon as they target an... NPC? anyone?
//   - the resulting state goes back to the GM for vetting
//     - GM makes arbitrary changes to the game state, confirms
//     - OR GM outright denies the action, returning pre-Act state.
// - when to vet could be an option.
//   - ALL player commands
//   - only Actions on NPCs
//   - only Actions on anyone (including other PCs)
//   - never vet, give jesus the wheel
//   - ... well, it needs to be the GM's turn *some* time... top of initiative or something

// pending game state until vetted
// editable logs
// vetting creates a snapshot?


/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    current_game: Game,
    snapshots: VecDeque<(Game, Vec<GameLog>)>,
}

impl App {
    pub fn new(g: Game) -> Self {
        let mut snapshots = VecDeque::with_capacity(1000);
        snapshots.push_back((g.clone(), Vec::with_capacity(100)));
        App {
            current_game: g,
            snapshots: snapshots,
        }
    }
    pub fn perform_unchecked(&mut self,
                             cmd: GameCommand)
                             -> Result<(&Game, Vec<GameLog>), GameError> {
        let (game, logs) = self.current_game.perform_unchecked(cmd)?;
        if self.snapshots.len() >= 1 {
            if self.snapshots.back().unwrap().1.len() + logs.len() > 100 {
                self.snapshot();
            }
            self.snapshots.back_mut().unwrap().1.extend(logs.clone());
        }
        self.current_game = game;
        Ok((&self.current_game, logs))
    }

    pub fn snapshot(&mut self) {
        self.snapshots.push_back((self.current_game.clone(), Vec::with_capacity(100)));
    }

    pub fn game(&self) -> &Game {
        &self.current_game
    }
}


mod test {
    use app::*;
    use test::Bencher;
    use game::test::*;

    pub fn t_app() -> App {
        App::new(t_game())
    }

    pub fn t_app_act(app: &mut App, ab: &str, dtarget: DecidedTarget) -> Result<(), GameError> {
        app.perform_unchecked(GameCommand::Act(abid(ab), dtarget))?;
        Ok(())
    }

    #[bench]
    fn three_char_infinite_combat(bencher: &mut Bencher) {
        let mut app = t_app();
        app.perform_unchecked(GameCommand::StartCombat(vec![cid("rogue"),
                                                             cid("ranger"),
                                                             cid("cleric")]))
            .unwrap();
        let iter = |app: &mut App| -> Result<(), GameError> {
            t_app_act(app, "punch", DecidedTarget::Melee(cid("ranger")))?;
            app.perform_unchecked(GameCommand::Done)?;
            app.perform_unchecked(GameCommand::Done)?;
            t_app_act(app, "heal", DecidedTarget::Range(cid("ranger")))?;
            app.perform_unchecked(GameCommand::Done)?;
            Ok(())
        };
        bencher.iter(|| {
            for _ in 0..1000 {
                iter(&mut app).unwrap();
            }
            app.clone()
        });
    }

}
