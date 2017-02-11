use std::collections::{VecDeque, HashMap, HashSet};

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

// When editing a log it may invalidate later logs. you can edit it, but a big red "X" will appear
// where an error occurred later on. This will require some model for ephemeral, uncommitted game
// modifications...
// I *think* that will need to be stored in the model (though perhaps not on disk), since we
// probably don't want to just accept a modify Game back from the client...
// But maybe that's okay actually, we would only be sending it to the GM.


/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    current_game: Game,
    snapshots: VecDeque<(Game, Vec<GameLog>)>,
    players: HashMap<PlayerID, HashSet<CreatureID>>,
}

impl App {
    pub fn new(g: Game) -> Self {
        let mut snapshots = VecDeque::with_capacity(1000);
        snapshots.push_back((g.clone(), Vec::with_capacity(100)));
        App {
            current_game: g,
            snapshots: snapshots,
            players: HashMap::new(),
        }
    }
    pub fn perform_unchecked(&mut self,
                             cmd: GameCommand)
                             -> Result<(&Game, Vec<GameLog>), GameError> {
        match &cmd {
            &GameCommand::RegisterPlayer(ref pid) => self.register_player(pid),
            &GameCommand::UnregisterPlayer(ref pid) => self.unregister_player(pid),
            &GameCommand::GiveCreatureToPlayer(ref pid, ref cid) => {
                self.give_creature_to_player(pid, cid)
            }
            &GameCommand::RemoveCreatureFromPlayer(ref pid, ref cid) => {
                self.remove_creature_from_player(pid, cid)
            }
            _ => {
                let (game, logs) = self.current_game.perform_unchecked(cmd.clone())?;
                if self.snapshots.len() >= 1 {
                    if self.snapshots.back().unwrap().1.len() + logs.len() > 100 {
                        self.snapshot();
                    }
                    self.snapshots.back_mut().unwrap().1.extend(logs.clone());
                }
                self.current_game = game;
                Ok((&self.current_game, logs))
            }
        }
    }

    fn register_player(&mut self, pid: &PlayerID) -> Result<(&Game, Vec<GameLog>), GameError> {
        if self.players.contains_key(&pid) {
            Err(GameError::PlayerAlreadyExists(pid.clone()))
        } else {
            self.players.insert(pid.clone(), HashSet::new());
            Ok((&self.current_game, vec![]))
        }
    }

    fn unregister_player(&mut self, pid: &PlayerID) -> Result<(&Game, Vec<GameLog>), GameError> {
        self.players.remove(pid).ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
        Ok((&self.current_game, vec![]))
    }

    fn give_creature_to_player(&mut self,
                               pid: &PlayerID,
                               cid: &CreatureID)
                               -> Result<(&Game, Vec<GameLog>), GameError> {
        self.current_game.find_creature(*cid)?;
        let mut creatures =
            self.players.get_mut(pid).ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
        creatures.insert(*cid);
        Ok((&self.current_game, vec![]))
    }

    fn remove_creature_from_player(&mut self,
                                 pid: &PlayerID,
                                 cid: &CreatureID)
                                 -> Result<(&Game, Vec<GameLog>), GameError> {
        self.current_game.find_creature(*cid)?;
        let mut creatures =
            self.players.get_mut(pid).ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
        if creatures.remove(cid) {
            Ok((&self.current_game, vec![]))
        } else {
            Err(GameError::PlayerDoesntControlCreature(pid.clone(), *cid))
        }
        
    }

    pub fn snapshot(&mut self) {
        self.snapshots.push_back((self.current_game.clone(), Vec::with_capacity(100)));
    }

    pub fn game(&self) -> &Game {
        &self.current_game
    }

    pub fn get_movement_options(&self, creature_id: CreatureID) -> Result<Vec<Point3>, GameError> {
        self.current_game.get_movement_options(creature_id)
    }

    pub fn get_target_options(&self,
                              cid: CreatureID,
                              abid: AbilityID)
                              -> Result<Vec<PotentialTarget>, GameError> {
        self.current_game.get_target_options(cid, abid)
    }
}

#[cfg(test)]
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
            iter(&mut app).unwrap();
            app.clone()
        });
    }
}
