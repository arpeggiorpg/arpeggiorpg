use std::collections::HashMap;

use indexed::IndexedHashMap;
use types::*;
use combat::*;
use creature::ChangedCreature;
use foldertree::FolderTree;

// Generic methods for any kind of Game regardless of the CreatureState.
impl Game {
  pub fn new(classes: HashMap<String, Class>, abilities: HashMap<AbilityID, Ability>) -> Self {
    Game {
      // campaign: FolderTree::new(Folder::new()),
      abilities: abilities,
      current_combat: None,
      creatures: HashMap::new(),
      maps: HashMap::new(),
      classes: classes,
      tile_system: TileSystem::Realistic,
      scenes: IndexedHashMap::new(),
    }
  }

  pub fn creatures(&self) -> Result<HashMap<CreatureID, DynamicCreature>, GameError> {
    let mut map = HashMap::new();
    for creature in self.creatures.values() {
      map.insert(creature.id, self.dyn_creature(creature)?);
    }
    Ok(map)
  }

  pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
    // maybe this should just return &Ability?
    Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
  }

  /// Perform a GameCommand on the current Game.
  pub fn perform_unchecked(&self, cmd: GameCommand) -> Result<ChangedGame, GameError> {
    use self::GameCommand::*;
    let change = match cmd {
      // ** Folder Management **
      CreateFolder(path) => self.change_with(GameLog::CreateFolder(path)),
      DeleteFolder(path) => self.change_with(GameLog::DeleteFolder(path)),
      LinkFolderCreature(path, cid) => self.change_with(GameLog::LinkFolderCreature(path, cid)),
      UnlinkFolderCreature(path, cid) => self.change_with(GameLog::UnlinkFolderCreature(path, cid)),
      LinkFolderScene(path, sceneName) => {
        self.change_with(GameLog::LinkFolderScene(path, sceneName))
      }
      UnlinkFolderScene(path, sceneName) => {
        self.change_with(GameLog::UnlinkFolderScene(path, sceneName))
      }
      CreateNote(path, note) => self.change_with(GameLog::CreateNote(path, note)),
      DeleteNote(path, noteName) => self.change_with(GameLog::DeleteNote(path, noteName)),

      EditScene(scene) => self.change_with(GameLog::EditScene(scene)),
      DeleteScene(name) => self.change_with(GameLog::DeleteScene(name)),
      CreateCreature(c, path) => self.create_creature(c),
      PathCreature(scene, cid, pt) => Ok(self.path_creature(scene, cid, pt)?.0),
      SetCreaturePos(scene, cid, pt) => self.change_with(GameLog::SetCreaturePos(scene, cid, pt)),
      SetCreatureNote(cid, note) => {
        self.change().apply_creature(cid, |c| c.creature.set_note(note.clone()))
      }
      PathCurrentCombatCreature(pt) => self.get_combat()?.get_movement()?.move_current(pt),
      CombatAct(abid, dtarget) => self.combat_act(abid, dtarget),
      ActCreature(scene, cid, abid, dtarget) => self.ooc_act(scene, cid, abid, dtarget),
      EditMap(ref name, ref terrain) => {
        self.change_with(GameLog::EditMap(name.clone(), terrain.clone()))
      }
      RemoveCreature(cid) => self.change_with(GameLog::RemoveCreature(cid)),
      StartCombat(scene, cids) => self.change_with(GameLog::StartCombat(scene, cids)),
      StopCombat => self.change_with(GameLog::StopCombat),
      AddCreatureToCombat(cid) => self.change_with(GameLog::AddCreatureToCombat(cid)),
      RemoveCreatureFromCombat(cid) => self.change_with(GameLog::RemoveCreatureFromCombat(cid)),
      ChangeCreatureInitiative(cid, new_pos) => {
        self.change_with(GameLog::CombatLog(CombatLog::ChangeCreatureInitiative(cid, new_pos)))
      }
      Done => self.next_turn(),

      // These are handled by the app before being passed to the Game:
      RegisterPlayer(..) => bug("Game RegisterPlayer"),
      UnregisterPlayer(..) => bug("Game UnregisterPlayer"),
      GiveCreaturesToPlayer(..) => bug("Game GiveCreaturesToPlayer"),
      RemoveCreaturesFromPlayer(..) => bug("Game RemoveCreaturesFromPlayer"),
      Rollback(..) => bug("Game Rollback"),
      SetPlayerScene(..) => bug("Game SetPlayerScene"),
    }?;
    Ok(change)
  }

  pub fn path_creature(&self, scene: SceneName, cid: CreatureID, pt: Point3)
                       -> Result<(ChangedGame, Distance), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(&self, scene_name: SceneName, cid: CreatureID, pt: Point3,
                                max_distance: Distance)
                                -> Result<(ChangedGame, Distance), GameError> {
    let scene = self.get_scene(scene_name.clone())?;
    let terrain = self.get_map(&scene.map)?;
    let (pts, distance) = self.tile_system
      .find_path(scene.get_pos(cid)?, max_distance, terrain, pt)
      .ok_or(GameError::NoPathFound)?;
    debug_assert!(distance <= max_distance);

    let change = self.change_with(GameLog::PathCreature(scene_name, cid, pts))?;
    Ok((change, distance))
  }

  fn next_turn(&self) -> Result<ChangedGame, GameError> {
    let change = self.change().apply_combat(|c| c.next_turn())?;
    change.apply_creature(self.current_combat.as_ref().unwrap().current_creature_id(), |c| c.tick())
  }

  fn create_creature(&self, spec: CreatureCreation) -> Result<ChangedGame, GameError> {
    let creature = Creature::create(&spec);
    self.change_with(GameLog::CreateCreature(creature))
  }

  pub fn get_map(&self, map_name: &str) -> Result<&Map, GameError> {
    self.maps.get(map_name).ok_or_else(|| GameError::MapNotFound(map_name.to_string()))
  }

  pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
    use self::GameLog::*;
    let mut newgame = self.clone();
    match *log {
      CreateFolder(ref path) => {}
      DeleteFolder(ref path) => {}
      LinkFolderCreature(ref path, ref cid) => {}
      UnlinkFolderCreature(ref path, ref cid) => {}
      LinkFolderScene(ref path, ref sceneName) => {}
      UnlinkFolderScene(ref path, ref sceneName) => {}
      CreateNote(ref path, ref note) => {}
      DeleteNote(ref path, ref noteName) => {}
      EditScene(ref scene) => {
        newgame.check_map(&scene.map)?;
        newgame.scenes.insert(scene.clone());
      }
      DeleteScene(ref name) => {
        newgame.scenes.remove(name);
      }
      EditMap(ref name, ref terrain) => {
        newgame.maps.insert(name.clone(), terrain.clone());
      }
      CreateCreature(ref c) => {
        if newgame.creatures.contains_key(&c.id()) {
          return Err(GameError::CreatureAlreadyExists(c.id()));
        } else {
          newgame.creatures.insert(c.id(), c.clone());
        }
      }
      RemoveCreature(cid) => {
        newgame.creatures.remove(&cid).ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
      }
      AddCreatureToCombat(cid) => {
        let mut combat = newgame.current_combat.clone().ok_or(GameError::NotInCombat)?;
        self.check_creature_id(cid)?;
        if combat.creatures.contains(&cid) {
          return Err(GameError::AlreadyInCombat(cid));
        }
        combat.creatures.push(cid);
        newgame.current_combat = Some(combat);
      }
      RemoveCreatureFromCombat(cid) => {
        let combat = {
          let combat = newgame.get_combat()?;
          combat.remove_from_combat(cid)?
        };
        newgame.current_combat = combat;
      }
      CombatLog(ref cl) => {
        return Ok(Game { current_combat: Some(self.get_combat()?.apply_log(cl)?), ..self.clone() });
      }
      CreatureLog(cid, ref cl) => {
        let mut newgame = self.clone();
        let creature = self.get_creature(cid)?.creature.apply_log(cl)?;
        *newgame.get_creature_mut(cid)? = creature;
        return Ok(newgame);
      }
      StartCombat(ref scene, ref cids) => {
        for cid in cids {
          self.check_creature_id(*cid)?;
        }
        self.check_scene(scene.clone())?;
        newgame.current_combat = Some(Combat::new(scene.clone(), cids.clone())?);
      }
      StopCombat => {
        newgame.current_combat.take().ok_or(GameError::NotInCombat)?;
      }
      SetCreaturePos(ref scene_name, ref cid, ref pt) => {
        let scene = self.get_scene(scene_name.clone())?.set_pos(*cid, *pt);
        newgame.scenes.insert(scene);
      }
      PathCreature(ref scene_name, ref cid, ref pts) => {
        let current_pos = self.get_scene(scene_name.clone())?.get_pos(*cid)?;
        let dest = pts.last().map(|x| *x).unwrap_or(current_pos);
        let scene = self.get_scene(scene_name.clone())?.set_pos(*cid, dest);
        newgame.scenes.insert(scene);
      }
      // Things that are handled at the App level
      Rollback(..) => {
        return bug("GameLog Rollback");
      }
    }
    Ok(newgame)
  }

  fn check_map(&self, map_name: &str) -> Result<(), GameError> {
    if self.maps.contains_key(map_name) {
      Ok(())
    } else {
      Err(GameError::MapNotFound(map_name.to_string()))
    }
  }

  pub fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError> {
    if self.creatures.contains_key(&cid) {
      Ok(())
    } else {
      Err(GameError::CreatureNotFound(cid.to_string()))
    }
  }

  fn check_scene(&self, scene: SceneName) -> Result<(), GameError> {
    if self.scenes.contains_key(&scene) { Ok(()) } else { Err(GameError::SceneNotFound(scene)) }
  }

  pub fn is_in_combat(&self, cid: CreatureID) -> bool {
    match self.get_combat() {
      Ok(combat) => combat.combat.creatures.contains(&cid),
      Err(_) => false,
    }
  }

  pub fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature, GameError> {
    self.dyn_creature(self.creatures.get(&cid).ok_or(GameError::CreatureNotFound(cid.to_string()))?)
  }

  // this is only public for tests :(
  pub fn get_creature_mut(&mut self, cid: CreatureID) -> Result<&mut Creature, GameError> {
    self.creatures.get_mut(&cid).ok_or(GameError::CreatureNotFound(cid.to_string()))
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>
    (&'game self, creature: &'creature Creature)
     -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, name: SceneName) -> Result<&Scene, GameError> {
    self.scenes.get(&name).ok_or(GameError::SceneNotFound(name.clone()))
  }

  pub fn get_combat<'game>(&'game self) -> Result<DynamicCombat<'game>, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameError::NotInCombat)?;
    let scene = self.get_scene(combat.scene.clone())?;
    let map = self.get_map(&scene.map)?;
    Ok(DynamicCombat {
      scene: &scene,
      map: &map,
      combat: &combat,
      game: self,
    })
  }

  // ** CONSIDER ** moving this chunk of code to... Scene.rs?

  fn combat_act(&self, abid: AbilityID, target: DecidedTarget) -> Result<ChangedGame, GameError> {
    let combat = self.get_combat()?;
    let scene = combat.scene;
    let actor = combat.combat.current_creature_id();
    self._act(scene, actor, abid, target, true)
  }

  fn ooc_act(&self, scene: SceneName, cid: CreatureID, abid: AbilityID, target: DecidedTarget)
             -> Result<ChangedGame, GameError> {
    let scene = self.get_scene(scene)?;
    self._act(scene, cid, abid, target, false)
  }

  fn _act(&self, scene: &Scene, cid: CreatureID, abid: AbilityID, target: DecidedTarget,
          in_combat: bool)
          -> Result<ChangedGame, GameError> {
    if !scene.creatures.contains_key(&cid) {
      return Err(GameError::CreatureNotFound(cid.to_string()));
    }
    let creature = self.get_creature(cid)?;
    if creature.can_act() {
      if creature.has_ability(abid) {
        self.creature_act(&creature,
                          scene,
                          &self.get_ability(&abid)?,
                          target,
                          self.change(),
                          in_combat)
      } else {
        Err(GameError::CreatureLacksAbility(creature.id(), abid))
      }
    } else {
      Err(GameError::CannotAct(creature.id()))
    }
  }

  pub fn creature_act(&self, creature: &DynamicCreature, scene: &Scene, ability: &Ability,
                      target: DecidedTarget, mut change: ChangedGame, in_combat: bool)
                      -> Result<ChangedGame, GameError> {
    let targets = self.resolve_targets(creature, scene, ability.target, target)?;
    for creature_id in targets.iter() {
      for effect in &ability.effects {
        change = change.apply_creature(*creature_id, |c| c.apply_effect(effect))?;
      }
    }
    if in_combat {
      change = change.apply_creature(creature.id(), |c| c.creature.reduce_energy(ability.cost))?;
    }
    Ok(change)
  }

  pub fn resolve_targets(&self, creature: &DynamicCreature, scene: &Scene, target: TargetSpec,
                         decision: DecidedTarget)
                         -> Result<Vec<CreatureID>, GameError> {
    match (target, decision) {
      (TargetSpec::Melee, DecidedTarget::Melee(cid)) => {
        if self.tile_system
          .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, MELEE_RANGE) {
          Ok(vec![cid])
        } else {
          Err(GameError::CreatureOutOfRange(cid))
        }
      }
      (TargetSpec::Range(max), DecidedTarget::Range(cid)) => {
        if self.tile_system
          .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, max) {
          Ok(vec![cid])
        } else {
          Err(GameError::CreatureOutOfRange(cid))
        }
      }
      (TargetSpec::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (spec, decided) => Err(GameError::InvalidTargetForTargetSpec(spec, decided)),
    }
  }


  pub fn get_movement_options(&self, scene: SceneName, creature_id: CreatureID)
                              -> Result<Vec<Point3>, GameError> {
    let scene = self.get_scene(scene)?;
    let creature = self.get_creature(creature_id)?;
    if creature.can_move() {
      Ok(self.tile_system.get_all_accessible(scene.get_pos(creature_id)?,
                                             self.get_map(&scene.map)?,
                                             creature.speed()))
    } else {
      Err(GameError::CannotAct(creature.id()))
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(&self, scene: SceneName, creature_id: CreatureID,
                            ability_id: AbilityID)
                            -> Result<Vec<PotentialTarget>, GameError> {
    let ability = self.get_ability(&ability_id)?;

    Ok(match ability.target {
      TargetSpec::Melee => self.creatures_in_range(scene, creature_id, MELEE_RANGE)?,
      TargetSpec::Range(distance) => self.creatures_in_range(scene, creature_id, distance)?,
      TargetSpec::Actor => vec![PotentialTarget::CreatureID(creature_id)],
    })
  }

  fn creatures_in_range(&self, scene: SceneName, creature_id: CreatureID, distance: Distance)
                        -> Result<Vec<PotentialTarget>, GameError> {
    let scene = self.get_scene(scene)?;
    let my_pos = scene.get_pos(creature_id)?;
    let mut results = vec![];
    for (creature_id, creature_pos) in scene.creatures.iter() {
      if self.tile_system.points_within_distance(my_pos, *creature_pos, distance) {
        results.push(PotentialTarget::CreatureID(*creature_id));
      }
    }
    Ok(results)
  }

  // ** END CONSIDERATION **

  pub fn get_class(&self, class: &str) -> Result<&Class, GameError> {
    self.classes.get(class).ok_or_else(|| GameError::ClassNotFound(class.to_string()))
  }

  pub fn change(&self) -> ChangedGame {
    ChangedGame {
      game: self.clone(),
      logs: vec![],
    }
  }

  pub fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError> {
    let game = self.apply_log(&log)?;
    Ok(ChangedGame {
      game: game,
      logs: vec![log],
    })
  }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChangedGame {
  pub game: Game,
  pub logs: Vec<GameLog>,
}

impl ChangedGame {
  pub fn apply(&self, log: &GameLog) -> Result<ChangedGame, GameError> {
    let mut new = self.clone();
    new.game = self.game.apply_log(log)?;
    Ok(new)
  }

  pub fn apply_combat<'game, F>(&'game self, f: F) -> Result<ChangedGame, GameError>
    where F: FnOnce(DynamicCombat<'game>) -> Result<ChangedCombat<'game>, GameError>
  {
    let dyn_combat = self.game.get_combat()?;
    let change = f(dyn_combat)?;
    let (combat, logs) = change.done();
    let mut new = self.clone();
    new.game.current_combat = Some(combat);
    new.logs.extend(combat_logs_into_game_logs(logs));
    Ok(new)
  }

  pub fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedGame, GameError>
    where F: FnOnce(DynamicCreature) -> Result<ChangedCreature, GameError>
  {
    let creature = self.game.get_creature(cid)?;
    let change = f(creature)?;
    let mut new = self.clone();
    let (creature, logs) = change.done();
    *new.game.get_creature_mut(cid)? = creature;
    new.logs.extend(creature_logs_into_game_logs(cid, logs));
    Ok(new)
  }

  pub fn done(self) -> (Game, Vec<GameLog>) {
    (self.game, self.logs)
  }
}

fn bug<T>(msg: &str) -> Result<T, GameError> {
  Err(GameError::BuggyProgram(msg.to_string()))
}


#[cfg(test)]
pub mod test {
  use std::collections::HashSet;
  use std::iter::FromIterator;

  use game::*;
  use creature::test::*;
  use combat::test::*;
  use types::test::*;
  use grid::test::*;

  pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    game.perform_unchecked(GameCommand::StartCombat(SceneName("Test Scene".to_string()), combatants)).unwrap().game
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    game.perform_unchecked(GameCommand::CombatAct(ability_id, target)).unwrap().game
  }

  pub fn t_game() -> Game {
    let mut game = Game::new(t_classes(), t_abilities());
    game.maps.insert("huge".to_string(), huge_box());
    let rogue_creation = t_rogue_creation("rogue");
    let ranger_creation = t_ranger_creation("ranger");
    let cleric_creation = t_cleric_creation("cleric");
    game.creatures.insert(cid_rogue(), Creature::create(&rogue_creation));
    game.creatures.insert(cid_cleric(), Creature::create(&cleric_creation));
    game.creatures.insert(cid_ranger(), Creature::create(&ranger_creation));
    game.scenes.insert(Scene {
      name: SceneName("Test Scene".to_string()),
      map: "huge".to_string(),
      creatures: HashMap::from_iter(vec![(cid_rogue(), (0, 0, 0)),
                                         (cid_cleric(), (0, 0, 0)),
                                         (cid_ranger(), (0, 0, 0))]),
    });
    game
  }

  pub fn t_classes() -> HashMap<String, Class> {
    let rogue_abs = vec![abid("punch")];
    let ranger_abs = vec![abid("shoot")];
    let cleric_abs = vec![abid("heal")];
    HashMap::from_iter(vec![("rogue".to_string(),
                             Class {
                               abilities: rogue_abs,
                               conditions: vec![],
                               color: "purple".to_string(),
                             }),
                            ("ranger".to_string(),
                             Class {
                               abilities: ranger_abs,
                               conditions: vec![],
                               color: "darkgreen".to_string(),
                             }),
                            ("cleric".to_string(),
                             Class {
                               abilities: cleric_abs,
                               conditions: vec![],
                               color: "lightgreen".to_string(),
                             })])
  }

  #[test]
  fn workflow() {
    let mut creatures = HashMap::new();
    let punch = t_punch();
    let punch_id = abid("punch");
    let bob_id = CreatureID::new();
    let creature = t_rogue("bob");
    creatures.insert(bob_id, creature);
    let mut abilities = HashMap::new();
    abilities.insert(punch_id.clone(), punch);
    let game = Game {
      abilities: abilities,
      classes: t_classes(),
      current_combat: None,
      creatures: creatures,
      maps: HashMap::from_iter(vec![("huge".to_string(), huge_box())]),
      scenes: IndexedHashMap::new(),
      tile_system: TileSystem::Realistic,
    };
    let game = t_start_combat(&game, vec![bob_id]);
    let next =
      game.perform_unchecked(GameCommand::CombatAct(punch_id, DecidedTarget::Melee(bob_id)));
    let next: Game = next.expect("punch did not succeed").game;
    let _: Game = next.perform_unchecked(GameCommand::StopCombat).unwrap().game;
  }

  #[test]
  fn start_combat_not_found() {
    let game = t_game();
    let non = CreatureID::new();
    assert_eq!(game.perform_unchecked(GameCommand::StartCombat(SceneName("Test Scene".to_string()),
                                                             vec![non])),
               Err(GameError::CreatureNotFound(non.to_string())));
  }

  #[test]
  fn combat_must_have_creatures() {
    let game = t_game();
    assert_eq!(game.perform_unchecked(GameCommand::StartCombat(SceneName("Test Scene".to_string()),
                                                             vec![])),
               Err(GameError::CombatMustHaveCreatures));
  }

  #[test]
  fn stop_combat() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    let game =
      game.perform_unchecked(GameCommand::CombatAct(abid("punch"),
                                                  DecidedTarget::Melee(cid_ranger())))
        .unwrap()
        .game;
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
    let game = game.perform_unchecked(GameCommand::StopCombat).unwrap().game;
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
  }

  #[test]
  fn movement() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    game.perform_unchecked(GameCommand::PathCurrentCombatCreature((1, 0, 0))).unwrap();
  }

  #[test]
  fn change_creature_initiative() {
    let game = t_combat();
    fn combat_cids(game: &Game) -> Vec<CreatureID> {
      game.get_combat().unwrap().combat.creatures.iter().map(|c| *c).collect()
    }
    assert_eq!(combat_cids(&game), vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    // move ranger to position 0
    let game =
      game.perform_unchecked(GameCommand::ChangeCreatureInitiative(cid_ranger(), 0)).unwrap().game;
    assert_eq!(combat_cids(&game), vec![cid_ranger(), cid_rogue(), cid_cleric()]);
  }

  #[test]
  fn three_char_infinite_combat() {
    let game = t_game();
    let game = game.perform_unchecked(GameCommand::StartCombat(SceneName("Test Scene".to_string()),
                                                  vec![cid_rogue(), cid_ranger(), cid_cleric()]))
      .unwrap()
      .game;
    let iter = |game: &Game| -> Result<Game, GameError> {
      let game = t_game_act(game, abid("punch"), DecidedTarget::Melee(cid_ranger()));
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      let game = t_game_act(&game, abid("heal"), DecidedTarget::Range(cid_ranger()));
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      Ok(game)
    };
    iter(&game).unwrap();
  }
}
