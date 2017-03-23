use std::collections::HashMap;

use indexed::IndexedHashMap;
use types::*;
use combat::*;
use creature::ChangedCreature;
use foldertree::{FolderTree, FolderPath};

// Generic methods for any kind of Game regardless of the CreatureState.
impl Game {
  pub fn new(classes: HashMap<String, Class>, abilities: HashMap<AbilityID, Ability>) -> Self {
    Game {
      campaign: FolderTree::new(Folder::new()),
      abilities: abilities,
      current_combat: None,
      creatures: HashMap::new(),
      maps: IndexedHashMap::new(),
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
    Ok(self.abilities.get(ability_id).ok_or(GameErrorEnum::NoAbility(ability_id.clone()))?.clone())
  }

  /// Perform a GameCommand on the current Game.
  pub fn perform_unchecked(&self, cmd: GameCommand) -> Result<ChangedGame, GameError> {
    use self::GameCommand::*;
    let change = match cmd {
      // ** Folder Management **
      CreateFolder(path) => self.change_with(GameLog::CreateFolder(path)),
      DeleteFolder(path) => self.change_with(GameLog::DeleteFolder(path)),
      CreateNote(path, note) => self.change_with(GameLog::CreateNote(path, note)),
      EditNote(path, orig, new) => self.change_with(GameLog::EditNote(path, orig, new)),
      DeleteNote(path, note_name) => self.change_with(GameLog::DeleteNote(path, note_name)),
      CreateScene(path, sc) => {
        let scene = Scene::create(sc);
        let log = GameLog::LinkFolderScene(path, scene.id);
        let change = self.change_with(GameLog::CreateScene(scene))?;
        change.apply(&log)
      }
      EditScene(scene) => self.change_with(GameLog::EditScene(scene)),
      DeleteScene(name) => self.change_with(GameLog::DeleteScene(name)),
      CreateCreature(c, path) => self.create_creature(c, path),
      PathCreature(scene, cid, pt) => Ok(self.path_creature(scene, cid, pt)?.0),
      SetCreaturePos(scene, cid, pt) => self.change_with(GameLog::SetCreaturePos(scene, cid, pt)),
      SetCreatureNote(cid, note) => {
        self.change().apply_creature(cid, |c| c.creature.set_note(note.clone()))
      }
      PathCurrentCombatCreature(pt) => self.get_combat()?.get_movement()?.move_current(pt),
      CombatAct(abid, dtarget) => self.combat_act(abid, dtarget),
      ActCreature(scene, cid, abid, dtarget) => self.ooc_act(scene, cid, abid, dtarget),
      CreateMap(ref path, ref creation) => {
        let map = Map::create(creation.clone());
        let map_id = map.id;
        let ch = self.change_with(GameLog::CreateMap(map))?;
        ch.apply(&GameLog::LinkFolderMap(path.clone(), map_id))
      }
      EditMap(ref map) => self.change_with(GameLog::EditMap(map.clone())),
      DeleteMap(mid) => self.change_with(GameLog::DeleteMap(mid)),
      DeleteCreature(cid) => self.change_with(GameLog::DeleteCreature(cid)),
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

  pub fn path_creature(&self, scene: SceneID, cid: CreatureID, pt: Point3)
                       -> Result<(ChangedGame, Distance), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(&self, scene_name: SceneID, cid: CreatureID, pt: Point3,
                                max_distance: Distance)
                                -> Result<(ChangedGame, Distance), GameError> {
    let scene = self.get_scene(scene_name.clone())?;
    let terrain = self.get_map(scene.map)?;
    let (pts, distance) = self.tile_system
      .find_path(scene.get_pos(cid)?, max_distance, terrain, pt)
      .ok_or(GameErrorEnum::NoPathFound)?;
    debug_assert!(distance <= max_distance);

    let change = self.change_with(GameLog::PathCreature(scene_name, cid, pts))?;
    Ok((change, distance))
  }

  fn next_turn(&self) -> Result<ChangedGame, GameError> {
    let change = self.change().apply_combat(|c| c.next_turn())?;
    change.apply_creature(self.current_combat.as_ref().unwrap().current_creature_id(), |c| c.tick())
  }

  fn create_creature(&self, spec: CreatureCreation, path: FolderPath)
                     -> Result<ChangedGame, GameError> {
    let creature = Creature::create(&spec);
    let cid = creature.id;
    let ch = self.change_with(GameLog::CreateCreature(creature))?;
    ch.apply(&GameLog::LinkFolderCreature(path, cid))
  }

  pub fn get_map(&self, map_id: MapID) -> Result<&Map, GameError> {
    self.maps.get(&map_id).ok_or_else(|| GameErrorEnum::MapNotFound(map_id).into())
  }

  pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
    use self::GameLog::*;
    let mut newgame = self.clone();
    match *log {
      CreateFolder(ref path) => newgame.campaign.make_folders(path, Folder::new()),
      DeleteFolder(ref path) => {
        newgame.campaign.remove(path)?;
      }
      LinkFolderCreature(ref path, ref cid) => {
        newgame.campaign.get_mut(path)?.creatures.insert(*cid);
      }
      UnlinkFolderCreature(ref path, ref cid) => {
        newgame.campaign.get_mut(path)?.creatures.remove(cid);
      }
      LinkFolderScene(ref path, ref sid) => {
        newgame.campaign.get_mut(path)?.scenes.insert(*sid);
      }
      UnlinkFolderScene(ref path, ref scene_name) => {
        newgame.campaign.get_mut(path)?.scenes.remove(scene_name);
      }
      CreateNote(ref path, ref note) => {
        newgame.campaign.get_mut(path)?.notes.insert(note.clone());
      }
      EditNote(ref path, ref name, ref new_note) => {
        let node = newgame.campaign.get_mut(path)?;
        node.notes.mutate(name, move |_| new_note.clone());
      }
      DeleteNote(ref path, ref note_name) => {
        newgame.campaign.get_mut(path)?.notes.remove(note_name);
      }
      LinkFolderMap(ref path, ref name) => {
        newgame.campaign.get_mut(path)?.maps.insert(name.clone());
      }
      UnlinkFolderMap(ref path, ref name) => {
        newgame.campaign.get_mut(path)?.maps.remove(name);
      }
      CreateScene(ref scene) => {
        if newgame.scenes.contains_key(&scene.id) {
          bail!(GameErrorEnum::SceneAlreadyExists(scene.id));
        } else {
          newgame.scenes.insert(scene.clone());
        }
      }
      EditScene(ref scene) => {
        newgame.check_map(scene.map)?;
        newgame.scenes
          .mutate(&scene.id, move |s| scene.clone())
          .ok_or(GameErrorEnum::SceneNotFound(scene.id))?;
      }
      DeleteScene(ref sid) => {
        // TODO: Figure out how to deal with players referencing this scene.
        // - disallow deleting if in combat
        if let Ok(combat) = self.get_combat() {
          if combat.scene.id == *sid {
            bail!(GameErrorEnum::SceneInUse(*sid));
          }
        }
        let all_folders: Vec<FolderPath> =
          newgame.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = newgame.campaign.get_mut(&path)?;
          node.scenes.remove(&sid);
        }
        newgame.scenes.remove(sid);
      }
      CreateMap(ref map) => {
        if newgame.maps.contains_key(&map.id) {
          bail!(GameErrorEnum::MapAlreadyExists(map.id));
        } else {
          newgame.maps.insert(map.clone());
        }
      }
      EditMap(ref map) => {
        newgame.maps
          .mutate(&map.id, move |_| map.clone())
          .ok_or(GameErrorEnum::MapNotFound(map.id))?;
      }
      DeleteMap(ref mid) => {
        let scenes_using_this_map: Vec<SceneID> = newgame.scenes
          .values()
          .filter_map(|s| if s.map == *mid { Some(s.id) } else { None })
          .collect();
        if scenes_using_this_map.len() > 0 {
          bail!(GameErrorEnum::MapInUse(*mid, scenes_using_this_map));
        }
        let all_folders: Vec<FolderPath> =
          newgame.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = newgame.campaign.get_mut(&path)?;
          node.maps.remove(&mid);
        }
        newgame.maps.remove(mid).ok_or(GameErrorEnum::MapNotFound(*mid))?;
      }
      CreateCreature(ref c) => {
        if newgame.creatures.contains_key(&c.id()) {
          bail!(GameErrorEnum::CreatureAlreadyExists(c.id()));
        } else {
          newgame.creatures.insert(c.id(), c.clone());
        }
      }
      DeleteCreature(cid) => {
        let scenes_with_this_creature: Vec<SceneID> = newgame.scenes
          .values()
          .filter_map(|s| if s.creatures.contains_key(&cid) { Some(s.id) } else { None })
          .collect();
        for sid in scenes_with_this_creature {
          newgame.scenes.mutate(&sid, |mut sc| {
            sc.creatures.remove(&cid);
            sc
          });
        }
        let all_folders: Vec<FolderPath> =
          newgame.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = newgame.campaign.get_mut(&path)?;
          node.creatures.remove(&cid);
        }
        newgame.current_combat = {
          if let Ok(combat) = newgame.get_combat() { combat.remove_from_combat(cid)? } else { None }
        };

        newgame.creatures
          .remove(&cid)
          .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?;
      }
      AddCreatureToCombat(cid) => {
        let mut combat = newgame.current_combat.clone().ok_or(GameErrorEnum::NotInCombat)?;
        newgame.check_creature_id(cid)?;
        if combat.creatures.contains(&cid) {
          bail!(GameErrorEnum::AlreadyInCombat(cid));
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
        newgame.current_combat = Some(newgame.get_combat()?.apply_log(cl)?);
      }
      CreatureLog(cid, ref cl) => {
        let creature = newgame.get_creature(cid)?.creature.apply_log(cl)?;
        *newgame.get_creature_mut(cid)? = creature;
      }
      StartCombat(ref scene, ref cids) => {
        for cid in cids {
          newgame.check_creature_id(*cid)?;
        }
        newgame.check_scene(scene.clone())?;
        newgame.current_combat = Some(Combat::new(scene.clone(), cids.clone())?);
      }
      StopCombat => {
        newgame.current_combat.take().ok_or(GameErrorEnum::NotInCombat)?;
      }
      SetCreaturePos(ref scene_name, ref cid, ref pt) => {
        let scene = newgame.get_scene(scene_name.clone())?.set_pos(*cid, *pt);
        newgame.scenes.insert(scene);
      }
      PathCreature(ref scene_name, ref cid, ref pts) => {
        let current_pos = newgame.get_scene(scene_name.clone())?.get_pos(*cid)?;
        let dest = pts.last().map(|x| *x).unwrap_or(current_pos);
        let scene = newgame.get_scene(scene_name.clone())?.set_pos(*cid, dest);
        newgame.scenes.insert(scene);
      }
      // Things that are handled at the App level
      Rollback(..) => {
        return bug("GameLog Rollback");
      }
    }
    Ok(newgame)
  }

  fn check_map(&self, map_id: MapID) -> Result<(), GameError> {
    if self.maps.contains_key(&map_id) {
      Ok(())
    } else {
      Err(GameErrorEnum::MapNotFound(map_id).into())
    }
  }

  pub fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError> {
    if self.creatures.contains_key(&cid) {
      Ok(())
    } else {
      Err(GameErrorEnum::CreatureNotFound(cid.to_string()).into())
    }
  }

  fn check_scene(&self, scene: SceneID) -> Result<(), GameError> {
    if self.scenes.contains_key(&scene) {
      Ok(())
    } else {
      Err(GameErrorEnum::SceneNotFound(scene).into())
    }
  }

  pub fn is_in_combat(&self, cid: CreatureID) -> bool {
    match self.get_combat() {
      Ok(combat) => combat.combat.creatures.contains(&cid),
      Err(_) => false,
    }
  }

  pub fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature, GameError> {
    self.dyn_creature(self.creatures
      .get(&cid)
      .ok_or(GameErrorEnum::CreatureNotFound(cid.to_string()))?)
  }

  // this is only public for tests :(
  pub fn get_creature_mut(&mut self, cid: CreatureID) -> Result<&mut Creature, GameError> {
    self.creatures.get_mut(&cid).ok_or(GameErrorEnum::CreatureNotFound(cid.to_string()).into())
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>
    (&'game self, creature: &'creature Creature)
     -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, id: SceneID) -> Result<&Scene, GameError> {
    self.scenes.get(&id).ok_or(GameErrorEnum::SceneNotFound(id).into())
  }

  pub fn get_combat<'game>(&'game self) -> Result<DynamicCombat<'game>, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameErrorEnum::NotInCombat)?;
    let scene = self.get_scene(combat.scene.clone())?;
    let map = self.get_map(scene.map)?;
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

  fn ooc_act(&self, scene: SceneID, cid: CreatureID, abid: AbilityID, target: DecidedTarget)
             -> Result<ChangedGame, GameError> {
    let scene = self.get_scene(scene)?;
    self._act(scene, cid, abid, target, false)
  }

  fn _act(&self, scene: &Scene, cid: CreatureID, abid: AbilityID, target: DecidedTarget,
          in_combat: bool)
          -> Result<ChangedGame, GameError> {
    if !scene.creatures.contains_key(&cid) {
      bail!(GameErrorEnum::CreatureNotFound(cid.to_string()));
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
        Err(GameErrorEnum::CreatureLacksAbility(creature.id(), abid).into())
      }
    } else {
      Err(GameErrorEnum::CannotAct(creature.id()).into())
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
          Err(GameErrorEnum::CreatureOutOfRange(cid).into())
        }
      }
      (TargetSpec::Range(max), DecidedTarget::Range(cid)) => {
        if self.tile_system
          .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, max) {
          Ok(vec![cid])
        } else {
          Err(GameErrorEnum::CreatureOutOfRange(cid).into())
        }
      }
      (TargetSpec::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (spec, decided) => Err(GameErrorEnum::InvalidTargetForTargetSpec(spec, decided).into()),
    }
  }


  pub fn get_movement_options(&self, scene: SceneID, creature_id: CreatureID)
                              -> Result<Vec<Point3>, GameError> {
    let scene = self.get_scene(scene)?;
    let creature = self.get_creature(creature_id)?;
    if creature.can_move() {
      Ok(self.tile_system.get_all_accessible(scene.get_pos(creature_id)?,
                                             self.get_map(scene.map)?,
                                             creature.speed()))
    } else {
      Err(GameErrorEnum::CannotAct(creature.id()).into())
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(&self, scene: SceneID, creature_id: CreatureID, ability_id: AbilityID)
                            -> Result<Vec<PotentialTarget>, GameError> {
    let ability = self.get_ability(&ability_id)?;

    Ok(match ability.target {
      TargetSpec::Melee => self.creatures_in_range(scene, creature_id, MELEE_RANGE)?,
      TargetSpec::Range(distance) => self.creatures_in_range(scene, creature_id, distance)?,
      TargetSpec::Actor => vec![PotentialTarget::CreatureID(creature_id)],
    })
  }

  fn creatures_in_range(&self, scene: SceneID, creature_id: CreatureID, distance: Distance)
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
    self.classes.get(class).ok_or_else(|| GameErrorEnum::ClassNotFound(class.to_string()).into())
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
  Err(GameErrorEnum::BuggyProgram(msg.to_string()).into())
}


#[cfg(test)]
pub mod test {
  use std::iter::FromIterator;

  use game::*;
  use combat::test::*;
  use types::test::*;
  use grid::test::*;

  pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    game.perform_unchecked(GameCommand::StartCombat(t_scene_id(), combatants)).unwrap().game
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    game.perform_unchecked(GameCommand::CombatAct(ability_id, target)).unwrap().game
  }

  pub fn t_game() -> Game {
    let mut game = Game::new(t_classes(), t_abilities());
    game.maps.insert(huge_box());
    let rogue_creation = t_rogue_creation("rogue");
    let ranger_creation = t_ranger_creation("ranger");
    let cleric_creation = t_cleric_creation("cleric");
    let mut rogue = Creature::create(&rogue_creation);
    rogue.id = cid_rogue();
    let mut cleric = Creature::create(&cleric_creation);
    cleric.id = cid_cleric();
    let mut ranger = Creature::create(&ranger_creation);
    ranger.id = cid_ranger();
    game.creatures.insert(cid_rogue(), rogue);
    game.creatures.insert(cid_cleric(), cleric);
    game.creatures.insert(cid_ranger(), ranger);
    game.scenes.insert(Scene {
      id: t_scene_id(),
      name: "Test Scene".to_string(),
      map: t_map_id(),
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
  fn start_combat_not_found() {
    let game = t_game();
    let non = CreatureID::new();
    let result = game.perform_unchecked(GameCommand::StartCombat(t_scene_id(), vec![non]));
    match result {
      Err(GameError(GameErrorEnum::CreatureNotFound(id), _)) => assert_eq!(id, non.to_string()),
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn combat_must_have_creatures() {
    let game = t_game();
    let result = game.perform_unchecked(GameCommand::StartCombat(t_scene_id(), vec![]));
    match result {
      Err(GameError(GameErrorEnum::CombatMustHaveCreatures, _)) => {}
      x => panic!("Unexpected result: {:?}", x),
    }
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
    let game = game.perform_unchecked(GameCommand::StartCombat(t_scene_id(),
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
