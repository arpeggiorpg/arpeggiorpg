use std::collections::VecDeque;
use std::path::Path;

use error_chain::bail;

use crate::types::*;

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

const LOGS_PER_SNAP: usize = 300;
const SNAPSHOTS: usize = 2;

impl App {
  pub fn new(g: Game) -> Self {
    let snapshots = VecDeque::with_capacity(SNAPSHOTS);
    App { current_game: g, snapshots: snapshots }
  }
  pub fn perform_command(
    &mut self, cmd: GameCommand, saved_game_path: &Path, module_path: Option<&Path>
  ) -> Result<(&Game, Vec<GameLog>), GameError> {
    match cmd {
      GameCommand::Rollback(ref snapshot_idx, ref log_idx) => {
        let newgame = self.rollback_to(*snapshot_idx, *log_idx)?;
        self.current_game = newgame;
        let log = GameLog::Rollback(*snapshot_idx, *log_idx);
        self.snapshots.back_mut().unwrap().1.push(log.clone());
        Ok((&self.current_game, vec![log]))
      }
      _ => {
        let (game, logs) =
          self.current_game.perform_command(cmd.clone(), saved_game_path, module_path)?.done();

        if self.snapshots.is_empty()
          || self.snapshots.back().unwrap().1.len() + logs.len() > LOGS_PER_SNAP
        {
          self.snapshots.push_back((self.current_game.clone(), Vec::with_capacity(LOGS_PER_SNAP)));
        }

        for _ in 0..(self.snapshots.len().saturating_sub(SNAPSHOTS)) {
          println!("There are {} too many snapshots", self.snapshots.len() - SNAPSHOTS);
          self.snapshots.pop_front();
        }

        self.snapshots.back_mut().unwrap().1.extend(logs.clone());
        self.current_game = game;
        Ok((&self.current_game, logs))
      }
    }
  }

  /// Rollback to a particular point by replaying logs after a snapshot
  fn rollback_to(&self, snapshot_idx: usize, log_idx: usize) -> Result<Game, GameError> {
    println!("Calling rollback_to {:?}[{:?}]", snapshot_idx, log_idx);
    let &(ref baseline, ref logs_to_apply) = self
      .snapshots
      .get(snapshot_idx)
      .ok_or_else(|| GameError::HistoryNotFound(snapshot_idx, log_idx))?;
    if logs_to_apply.len() - 1 < log_idx {
      bail!(GameError::HistoryNotFound(snapshot_idx, log_idx));
    }
    println!("All logs: {:?}", logs_to_apply);
    let logs_to_apply = &logs_to_apply[..log_idx];
    Self::apply_game_logs(baseline.clone(), baseline.clone(), logs_to_apply)
  }

  fn apply_game_logs(baseline: Game, mut game: Game, logs: &[GameLog]) -> Result<Game, GameError> {
    for log in logs {
      if let GameLog::Rollback(_sni, li) = *log {
        // TODO: unimplemented! Honor `sni`, the snapshot index
        // 1. assert li is within bounds?
        // 2. need to handle SnapshotIndex -- this assumes it's always based on the same snapshot
        // 3. this is super inefficient
        // 4. if each Rollback also created a Snapshot, things could be easier... we would never
        //    need to apply a Rollback as a log in that case
        game = Self::apply_game_logs(baseline.clone(), baseline.clone(), &logs[..li])?;
      } else {
        game = game.apply_log(log)?;
      }
    }
    Ok(game)
  }

  pub fn game(&self) -> &Game { &self.current_game }

  pub fn get_movement_options(
    &self, scene: SceneID, creature_id: CreatureID
  ) -> Result<Vec<Point3>, GameError> {
    self.current_game.get_movement_options(scene, creature_id)
  }

  pub fn get_combat_movement_options(&self) -> Result<Vec<Point3>, GameError> {
    Ok(self.current_game.get_combat()?.current_movement_options()?)
  }

  pub fn get_target_options(
    &self, scene: SceneID, cid: CreatureID, abid: AbilityID
  ) -> Result<PotentialTargets, GameError> {
    self.current_game.get_target_options(scene, cid, abid)
  }

  pub fn preview_volume_targets(
    &self, sid: SceneID, actor_id: CreatureID, ability_id: AbilityID, pt: Point3
  ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
    let scene = self.current_game.get_scene(sid)?;
    self.current_game.preview_volume_targets(scene, actor_id, ability_id, pt)
  }
}

#[cfg(test)]
mod test {
  use crate::app::*;
  use crate::game::test::*;
  use crate::types::test::*;
  use std::path::PathBuf;

  pub fn t_app() -> App { App::new(t_game()) }

  pub fn perf(app: &mut App, cmd: GameCommand) -> Result<(&Game, Vec<GameLog>), GameError> {
    app.perform_command(cmd, &PathBuf::from(""), None)
  }

  // pub fn t_app_act(app: &mut App, ab: AbilityID, dtarget: DecidedTarget) -> Result<(), GameError> {
  //   perf(app, GameCommand::CombatAct(ab, dtarget))?;
  //   Ok(())
  // }

  //  #[bench]
  //  fn three_char_infinite_combat(bencher: &mut Bencher) {
  //    let mut app = t_app();
  //    perf(
  //      &mut app,
  //      GameCommand::StartCombat(t_scene_id(), vec![cid_rogue(), cid_ranger(), cid_cleric()]),
  //    ).unwrap();
  //    let iter = |app: &mut App| -> Result<(), GameError> {
  //      t_app_act(app, abid_punch(), DecidedTarget::Creature(cid_ranger()))?;
  //      perf(app, GameCommand::Done)?;
  //      perf(app, GameCommand::Done)?;
  //      t_app_act(app, abid_heal(), DecidedTarget::Creature(cid_ranger()))?;
  //      perf(app, GameCommand::Done)?;
  //      Ok(())
  //    };
  //    bencher.iter(|| {
  //      iter(&mut app).unwrap();
  //      app.clone()
  //    });
  //  }

  #[test]
  fn rollback() {
    // 0
    let mut app = t_app();
    // 1
    perf(&mut app, GameCommand::SetCreaturePos(t_scene_id(), cid_ranger(), Point3::new(1, 1, 1)))
      .unwrap();
    perf(&mut app, GameCommand::Rollback(0, 0)).unwrap();
    let ranger = app.current_game.get_creature(cid_ranger()).unwrap();
    let scene = app.current_game.get_scene(t_scene_id()).unwrap();
    assert_eq!(scene.get_pos(ranger.id()).unwrap(), Point3::new(0, 0, 0));
    let logs = &app.snapshots[0].1;
    println!("{:?}", logs);
    assert_eq!(logs.len(), 2);
  }

  /// bug test: ensure precedent logs are also applied, not just the one being rolled back to.
  #[test]
  fn rollback_reapplies_precedents() {
    // 0
    let mut app = t_app();
    // 1
    perf(
      &mut app,
      GameCommand::StartCombat(t_scene_id(), vec![cid_ranger(), cid_rogue(), cid_cleric()]),
    ).unwrap();
    // 2
    perf(&mut app, GameCommand::StopCombat).unwrap();
    // 3
    perf(&mut app, GameCommand::SetCreaturePos(t_scene_id(), cid_ranger(), Point3::new(1, 1, 1)))
      .unwrap();
    perf(&mut app, GameCommand::Rollback(0, 2)).unwrap();
    assert_eq!(app.current_game.current_combat, None);
    let scene = app.current_game.get_scene(t_scene_id()).unwrap();
    assert_eq!(scene.get_pos(cid_ranger()).unwrap(), Point3::new(0, 0, 0));
  }

  ///
  #[test]
  fn rollback_through_rollback() {
    // 0
    let mut app = t_app();
    // 1
    perf(&mut app, GameCommand::SetCreaturePos(t_scene_id(), cid_ranger(), Point3::new(1, 1, 1)))
      .unwrap();
    // 2
    perf(&mut app, GameCommand::Rollback(0, 0)).unwrap(); // oops didn't mean to move ranger
                                                          // 3
    perf(&mut app, GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), Point3::new(1, 1, 1)))
      .unwrap();
    // 4
    perf(&mut app, GameCommand::Rollback(0, 2)).unwrap(); // oops didn't mean to move cleric
                                                          // 5
    perf(&mut app, GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(1, 1, 1)))
      .unwrap();
    let scene = app.current_game.get_scene(t_scene_id()).unwrap();
    assert_eq!(scene.get_pos(cid_cleric()).unwrap(), Point3::new(0, 0, 0));
    assert_eq!(scene.get_pos(cid_rogue()).unwrap(), Point3::new(1, 1, 1));
    assert_eq!(scene.get_pos(cid_ranger()).unwrap(), Point3::new(0, 0, 0));
  }
}
