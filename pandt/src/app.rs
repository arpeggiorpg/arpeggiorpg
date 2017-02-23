use std::collections::{VecDeque, HashMap, HashSet};

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
  pub fn perform_unchecked(&mut self, cmd: GameCommand)
                           -> Result<(&Game, Vec<GameLog>), GameError> {
    match &cmd {
      &GameCommand::RegisterPlayer(ref pid) => self.register_player(pid),
      &GameCommand::UnregisterPlayer(ref pid) => self.unregister_player(pid),
      &GameCommand::GiveCreaturesToPlayer(ref pid, ref cids) => {
        self.give_creatures_to_player(pid, cids)
      }
      &GameCommand::RemoveCreaturesFromPlayer(ref pid, ref cids) => {
        self.remove_creatures_from_player(pid, cids)
      }
      &GameCommand::Rollback(ref snapshot_idx, ref log_idx) => {
        let newgame = self.rollback_to(*snapshot_idx, *log_idx)?;
        self.current_game = newgame;
        self.snapshots
          .back_mut()
          .unwrap()
          .1
          .push(GameLog::Rollback(*snapshot_idx, *log_idx));
        Ok((&self.current_game, vec![]))
      }
      _ => {
        let (game, logs) = self.current_game.perform_unchecked(cmd.clone())?.done();

        if self.snapshots.len() == 0 || self.snapshots.back().unwrap().1.len() + logs.len() > 100 {
          self.snapshots
            .push_back((self.current_game.clone(), Vec::with_capacity(100)));
        }

        self.snapshots.back_mut().unwrap().1.extend(logs.clone());
        self.current_game = game;
        Ok((&self.current_game, logs))
      }
    }
  }

  /// Rollback to a particular point by replaying logs after a snapshot
  fn rollback_to(&self, snapshot_idx: usize, log_idx: usize) -> Result<Game, GameError> {
    let &(ref baseline, ref logs_to_apply) =
      self.snapshots.get(snapshot_idx).ok_or(GameError::HistoryNotFound(snapshot_idx, log_idx))?;
    if logs_to_apply.len() - 1 < log_idx {
      return Err(GameError::HistoryNotFound(snapshot_idx, log_idx));
    }
    let mut newgame = baseline.clone();
    for (i, log) in logs_to_apply.iter().enumerate() {
      println!("Applying log {:?}", log);
      println!("To... {:?}", newgame.current_combat);
      if i == log_idx {
        break;
      }
      newgame = newgame.apply_log(log)?;
      println!("Applied log {:?}", newgame.current_combat);
    }
    Ok(newgame)
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

  fn give_creatures_to_player(&mut self, pid: &PlayerID, cids: &[CreatureID])
                              -> Result<(&Game, Vec<GameLog>), GameError> {
    let mut creatures =
      self.players.get_mut(pid).ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
    let mut cids_to_insert = vec![];
    for cid in cids {
      self.current_game.find_creature(*cid)?;
      cids_to_insert.push(cid);
    }
    creatures.extend(cids_to_insert);
    Ok((&self.current_game, vec![]))
  }

  fn remove_creatures_from_player(&mut self, pid: &PlayerID, cids: &[CreatureID])
                                  -> Result<(&Game, Vec<GameLog>), GameError> {

    let mut creatures =
      self.players.get_mut(pid).ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
    let mut cids_to_remove = vec![];
    for cid in cids {
      self.current_game.find_creature(*cid)?;
      if creatures.contains(cid) {
        cids_to_remove.push(cid);
      } else {
        return Err(GameError::PlayerDoesntControlCreature(pid.clone(), *cid));
      }
    }
    for cid in cids_to_remove {
      creatures.remove(cid);
    }
    Ok((&self.current_game, vec![]))
  }

  pub fn game(&self) -> &Game {
    &self.current_game
  }

  pub fn get_movement_options(&self, creature_id: CreatureID) -> Result<Vec<Point3>, GameError> {
    self.current_game.get_movement_options(creature_id)
  }

  pub fn get_combat_movement_options(&self) -> Result<Vec<Point3>, GameError> {
    Ok(self.current_game.get_combat()?.current_movement_options()?)
  }

  pub fn get_target_options(&self, cid: CreatureID, abid: AbilityID)
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
    app.perform_unchecked(GameCommand::CombatAct(abid(ab), dtarget))?;
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

  #[test]
  fn rollback() {
    let mut app = t_app();
    app.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (1, 1, 1))).unwrap();
    app.perform_unchecked(GameCommand::Rollback(0, 0)).unwrap();
    let ranger = app.current_game.get_creature(cid("ranger")).unwrap();
    assert_eq!(ranger.pos(), (0, 0, 0));
    let logs = &app.snapshots[0].1;
    println!("{:?}", logs);
    assert_eq!(logs.len(), 2);
  }

  // bug test: for some reason combat-related things aren't working.
  #[test]
  fn rollback_through_combat() {
    // 0
    let mut app = t_app();
    // 1
    app.perform_unchecked(GameCommand::StartCombat(vec![cid("ranger"),
                                                       cid("rogue"),
                                                       cid("cleric")]))
      .unwrap();
    // 2
    app.perform_unchecked(GameCommand::StopCombat).unwrap();
    // 3
    app.perform_unchecked(GameCommand::SetCreaturePos(cid("ranger"), (1, 1, 1))).unwrap();
    app.perform_unchecked(GameCommand::Rollback(0, 2)).unwrap();
  }
}
