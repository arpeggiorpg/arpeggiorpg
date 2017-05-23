use std::collections::HashMap;

use indexed::IndexedHashMap;
use types::*;
use combat::*;
use creature::ChangedCreature;
use foldertree::{FolderTree, FolderPath};

impl Game {
  pub fn new(classes: HashMap<String, Class>, abilities: HashMap<AbilityID, Ability>) -> Self {
    Game {
      campaign: FolderTree::new(Folder::new()),
      abilities: abilities,
      current_combat: None,
      creatures: IndexedHashMap::new(),
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

  pub fn get_ability(&self, abid: &AbilityID) -> Result<&Ability, GameError> {
    // maybe this should just return &Ability?
    self.abilities.get(abid).ok_or_else(|| GameErrorEnum::NoAbility(*abid).into())
  }

  /// Perform a GameCommand on the current Game.
  pub fn perform_unchecked(&self, cmd: GameCommand) -> Result<ChangedGame, GameError> {
    use self::GameCommand::*;
    let change = match cmd {
      // ** Attribute checks **
      AttributeCheck(cid, check) => self.attribute_check(cid, &check),
      // ** Folder Management **
      CreateFolder(path) => self.change_with(GameLog::CreateFolder(path)),
      RenameFolder(path, name) => self.change_with(GameLog::RenameFolder(path, name)),
      DeleteFolder(path) => self.change_with(GameLog::DeleteFolder(path)),
      MoveFolderItem(src, item, dst) => self.change_with(GameLog::MoveFolderItem(src, item, dst)),
      CreateNote(path, note) => self.change_with(GameLog::CreateNote(path, note)),
      EditNote(path, orig, new) => self.change_with(GameLog::EditNote(path, orig, new)),
      DeleteNote(path, note_name) => self.change_with(GameLog::DeleteNote(path, note_name)),
      CreateScene(path, sc) => {
        let scene = Scene::create(sc);
        self.change_with(GameLog::CreateScene(path, scene))
      }
      EditScene(scene) => self.change_with(GameLog::EditScene(scene)),
      DeleteScene(name) => self.change_with(GameLog::DeleteScene(name)),
      CreateCreature(path, spec) => {
        let creature = Creature::create(&spec);
        self.change_with(GameLog::CreateCreature(path, creature))
      }
      EditCreature(creature) => self.change_with(GameLog::EditCreature(creature)),
      PathCreature(scene, cid, pt) => Ok(self.path_creature(scene, cid, pt)?.0),
      SetCreaturePos(scene, cid, pt) => self.change_with(GameLog::SetCreaturePos(scene, cid, pt)),
      PathCurrentCombatCreature(pt) => self.get_combat()?.get_movement()?.move_current(pt),
      CombatAct(abid, dtarget) => self.combat_act(abid, dtarget),
      ActCreature(scene, cid, abid, dtarget) => self.ooc_act(scene, cid, abid, dtarget),
      CreateMap(ref path, ref creation) => {
        let map = Map::create(creation.clone());
        self.change_with(GameLog::CreateMap(path.clone(), map))
      }
      EditMap(ref map) => self.change_with(GameLog::EditMap(map.clone())),
      DeleteMap(mid) => self.change_with(GameLog::DeleteMap(mid)),
      DeleteCreature(cid) => self.change_with(GameLog::DeleteCreature(cid)),
      StartCombat(scene, cids) => self.start_combat(scene, cids),
      StopCombat => self.change_with(GameLog::StopCombat),
      AddCreatureToCombat(cid) => self.add_creature_to_combat(cid),
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

  fn start_combat(&self, scene_id: SceneID, cids: Vec<CreatureID>)
                  -> Result<ChangedGame, GameError> {
    let cids_with_inits = cids
      .iter()
      .map(|cid| {
             let creature = self.get_creature(*cid)?;
             Ok((*cid, creature.creature.initiative.roll().1 as i16))
           })
      .collect::<Result<Vec<(CreatureID, i16)>, GameError>>()?;
    self.change_with(GameLog::StartCombat(scene_id, cids_with_inits))
  }

  fn add_creature_to_combat(&self, cid: CreatureID) -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(cid)?;
    let init = creature.creature.initiative.roll().1 as i16;
    self.change_with(GameLog::AddCreatureToCombat(cid, init))
  }

  fn attribute_check(&self, cid: CreatureID, check: &AttributeCheck)
                     -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(cid)?;
    let (rolled, success) = creature.creature.attribute_check(check)?;
    self.change_with(GameLog::AttributeCheckResult(cid, check.clone(), rolled, success))
  }

  pub fn path_creature(&self, scene: SceneID, cid: CreatureID, pt: Point3)
                       -> Result<(ChangedGame, Distance), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(&self, scene_id: SceneID, cid: CreatureID, pt: Point3,
                                max_distance: Distance)
                                -> Result<(ChangedGame, Distance), GameError> {
    let scene = self.get_scene(scene_id)?;
    let terrain = self.get_map(scene.map)?;
    let (pts, distance) = self
      .tile_system
      .find_path(scene.get_pos(cid)?, max_distance, terrain, pt)
      .ok_or(GameErrorEnum::NoPathFound)?;
    debug_assert!(distance <= max_distance);

    let change = self.change_with(GameLog::PathCreature(scene_id, cid, pts))?;
    Ok((change, distance))
  }

  fn next_turn(&self) -> Result<ChangedGame, GameError> {
    let change = self.change().apply_combat(|c| c.next_turn())?;
    change.apply_creature(self.current_combat.as_ref().unwrap().current_creature_id(), |c| c.tick())
  }

  pub fn get_map(&self, map_id: MapID) -> Result<&Map, GameError> {
    self.maps.get(&map_id).ok_or_else(|| GameErrorEnum::MapNotFound(map_id).into())
  }

  fn link_folder_item(&mut self, path: &FolderPath, item_id: &FolderItemID)
                      -> Result<(), GameError> {
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => node.creatures.insert(cid),
      FolderItemID::SceneID(sid) => node.scenes.insert(sid),
      FolderItemID::MapID(mid) => node.maps.insert(mid),
      FolderItemID::SubfolderID(_) => bail!("Cannot link folders."),
      FolderItemID::NoteID(ref nid) => {
        bail!(GameErrorEnum::CannotLinkNotes(path.clone(), nid.clone()))
      }
    };
    Ok(())
  }

  fn unlink_folder_item(&mut self, path: &FolderPath, item_id: &FolderItemID)
                        -> Result<(), GameError> {
    fn remove_set<T: ::std::hash::Hash + Eq>(path: &FolderPath, item: &FolderItemID,
                                             s: &mut ::std::collections::HashSet<T>, key: &T)
                                             -> Result<(), GameError> {
      if !s.remove(key) {
        bail!(GameErrorEnum::FolderItemNotFound(path.clone(), item.clone()))
      }
      Ok(())
    }
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => remove_set(path, item_id, &mut node.creatures, &cid)?,
      FolderItemID::SceneID(sid) => remove_set(path, item_id, &mut node.scenes, &sid)?,
      FolderItemID::MapID(mid) => remove_set(path, item_id, &mut node.maps, &mid)?,
      FolderItemID::SubfolderID(_) => bail!("Cannot unlink folders."),
      FolderItemID::NoteID(ref nid) => {
        bail!(GameErrorEnum::CannotLinkNotes(path.clone(), nid.clone()))
      }
    };
    Ok(())
  }

  pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
    let mut newgame = self.clone();
    newgame.apply_log_mut(log)?;
    Ok(newgame)
  }

  fn apply_log_mut(&mut self, log: &GameLog) -> Result<(), GameError> {
    use self::GameLog::*;
    match *log {
      AttributeCheckResult(..) => {} // purely informational
      CreateFolder(ref path) => self.campaign.make_folders(path, Folder::new()),
      RenameFolder(ref path, ref name) => self.campaign.rename_folder(path, name.clone())?,
      DeleteFolder(ref path) => {
        {
          let node = self.campaign.get(path)?;
          if node.scenes.is_empty() || node.maps.is_empty() || node.creatures.is_empty() ||
             node.notes.is_empty() {
            bail!(GameErrorEnum::FolderNotEmpty(path.clone()));
          }
        }
        self.campaign.remove(path)?;
      }
      MoveFolderItem(ref src, ref item_id, ref dst) => {
        match *item_id {
          FolderItemID::NoteID(ref name) => {
            let note = self
              .campaign
              .get_mut(src)?
              .notes
              .remove(name)
              .ok_or_else(|| GameErrorEnum::NoteNotFound(src.clone(), name.clone()))?;
            self.campaign.get_mut(dst)?.notes.insert(note.clone());
          }
          FolderItemID::SubfolderID(ref name) => {
            self.campaign.move_folder(&src.child(name.clone()), dst)?;
          }
          _ => {
            self.unlink_folder_item(src, item_id)?;
            self.link_folder_item(dst, item_id)?;
          }
        }
      }
      CreateNote(ref path, ref note) => {
        self.campaign.get_mut(path)?.notes.insert(note.clone());
      }
      EditNote(ref path, ref name, ref new_note) => {
        let node = self.campaign.get_mut(path)?;
        node.notes.mutate(name, move |_| new_note.clone());
      }
      DeleteNote(ref path, ref note_name) => {
        self.campaign.get_mut(path)?.notes.remove(note_name);
      }
      CreateScene(ref path, ref rscene) => {
        let scene = rscene.clone();
        self.scenes.try_insert(scene).ok_or_else(|| GameErrorEnum::SceneAlreadyExists(rscene.id))?;
        self.link_folder_item(path, &FolderItemID::SceneID(rscene.id))?;
      }
      EditScene(ref scene) => {
        self.check_map(scene.map)?;
        self
          .scenes
          .mutate(&scene.id, move |_| scene.clone())
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene.id))?;
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
          self.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = self.campaign.get_mut(&path)?;
          node.scenes.remove(sid);
        }
        self.scenes.remove(sid);
      }
      CreateMap(ref path, ref map) => {
        self.maps.try_insert(map.clone()).ok_or_else(|| GameErrorEnum::MapAlreadyExists(map.id))?;
        self.link_folder_item(path, &FolderItemID::MapID(map.id))?;
      }
      EditMap(ref map) => {
        self
          .maps
          .mutate(&map.id, move |_| map.clone())
          .ok_or_else(|| GameErrorEnum::MapNotFound(map.id))?;
      }
      DeleteMap(ref mid) => {
        let scenes_using_this_map: Vec<SceneID> = self
          .scenes
          .values()
          .filter_map(|s| if s.map == *mid { Some(s.id) } else { None })
          .collect();
        if scenes_using_this_map.is_empty() {
          bail!(GameErrorEnum::MapInUse(*mid, scenes_using_this_map));
        }
        let all_folders: Vec<FolderPath> =
          self.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = self.campaign.get_mut(&path)?;
          node.maps.remove(mid);
        }
        self.maps.remove(mid).ok_or_else(|| GameErrorEnum::MapNotFound(*mid))?;
      }
      CreateCreature(ref path, ref rc) => {
        let c = rc.clone();
        self.creatures.try_insert(c).ok_or_else(|| GameErrorEnum::CreatureAlreadyExists(rc.id()))?;
        self.link_folder_item(path, &FolderItemID::CreatureID(rc.id()))?;
      }
      EditCreature(ref creature) => {
        let mutated = self.creatures.mutate(&creature.id, |_| creature.clone());
        mutated.ok_or_else(|| GameErrorEnum::CreatureNotFound(creature.id.to_string()))?;
      }
      DeleteCreature(cid) => {
        let scenes_with_this_creature: Vec<SceneID> = self
          .scenes
          .values()
          .filter_map(|s| if s.creatures.contains_key(&cid) { Some(s.id) } else { None })
          .collect();
        for sid in scenes_with_this_creature {
          self
            .scenes
            .mutate(&sid, |mut sc| {
              sc.creatures.remove(&cid);
              sc
            });
        }
        let all_folders: Vec<FolderPath> =
          self.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
        for path in all_folders {
          let node = self.campaign.get_mut(&path)?;
          node.creatures.remove(&cid);
        }
        self.current_combat = {
          if let Ok(combat) = self.get_combat() { combat.remove_from_combat(cid)? } else { None }
        };

        self.creatures
          .remove(&cid)
          .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?;
      }
      AddCreatureToCombat(cid, init) => {
        let mut combat = self.current_combat.clone().ok_or(GameErrorEnum::NotInCombat)?;
        self.check_creature_id(cid)?;
        if combat.creatures.iter().position(|&(c, _)| c == cid).is_some() {
          bail!(GameErrorEnum::AlreadyInCombat(cid));
        }
        combat.creatures.push((cid, init));
        self.current_combat = Some(combat);
      }
      RemoveCreatureFromCombat(cid) => {
        let combat = {
          let combat = self.get_combat()?;
          combat.remove_from_combat(cid)?
        };
        self.current_combat = combat;
      }
      CombatLog(ref cl) => {
        self.current_combat = Some(self.get_combat()?.apply_log(cl)?);
      }
      CreatureLog(cid, ref cl) => {
        let creature = self.get_creature(cid)?.creature.apply_log(cl)?;
        self.creatures.mutate(&cid, |_| creature);
      }
      StartCombat(ref scene, ref cids_with_init) => {
        for &(cid, _) in cids_with_init {
          self.check_creature_id(cid)?;
        }
        self.check_scene(*scene)?;
        self.current_combat = Some(Combat::new(*scene, cids_with_init.clone())?);
      }
      StopCombat => {
        self.current_combat.take().ok_or(GameErrorEnum::NotInCombat)?;
      }
      SetCreaturePos(ref scene_id, ref cid, ref pt) => {
        let scene = self.get_scene(*scene_id)?.set_pos(*cid, *pt)?;
        self.scenes.insert(scene);
      }
      PathCreature(ref scene_id, ref cid, ref pts) => {
        let scene = {
          let scene = self.get_scene(*scene_id)?;
          let current_pos = scene.get_pos(*cid)?;
          let dest = pts.last().cloned().unwrap_or(current_pos);
          scene.set_pos(*cid, dest)?
        };
        self.scenes.insert(scene);
      }
      // Things that are handled at the App level
      Rollback(..) => {
        return bug("GameLog Rollback");
      }
    }
    Ok(())
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
      Ok(combat) => combat.combat.contains_creature(cid),
      Err(_) => false,
    }
  }

  pub fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature, GameError> {
    self.dyn_creature(self
                        .creatures
                        .get(&cid)
                        .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?)
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>
    (&'game self, creature: &'creature Creature)
     -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, id: SceneID) -> Result<&Scene, GameError> {
    self.scenes.get(&id).ok_or_else(|| GameErrorEnum::SceneNotFound(id).into())
  }

  pub fn get_combat(&self) -> Result<DynamicCombat, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameErrorEnum::NotInCombat)?;
    let scene = self.get_scene(combat.scene)?;
    let map = self.get_map(scene.map)?;
    Ok(DynamicCombat {
         scene: scene,
         map: map,
         combat: combat,
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
                          self.get_ability(&abid)?,
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
    for creature_id in &targets {
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
      (TargetSpec::Melee, DecidedTarget::Creature(cid)) => {
        if self
             .tile_system
             .points_within_distance(scene.get_pos(creature.id())?,
                                     scene.get_pos(cid)?,
                                     MELEE_RANGE) {
          Ok(vec![cid])
        } else {
          Err(GameErrorEnum::CreatureOutOfRange(cid).into())
        }
      }
      (TargetSpec::Range(max), DecidedTarget::Creature(cid)) => {
        if self.tile_system.points_within_distance(scene.get_pos(creature.id())?,
                                                   scene.get_pos(cid)?,
                                                   max) {
          Ok(vec![cid])
        } else {
          Err(GameErrorEnum::CreatureOutOfRange(cid).into())
        }
      }
      (TargetSpec::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (TargetSpec::AllCreaturesInVolumeInRange { volume, range }, DecidedTarget::Point(pt)) => {
        if !self.tile_system.points_within_distance(scene.get_pos(creature.id())?, pt, range) {
          bail!(GameErrorEnum::PointOutOfRange(pt));
        }
        Ok(self.creatures_in_volume(scene, pt, volume))
      }
      (spec, decided) => Err(GameErrorEnum::InvalidTargetForTargetSpec(spec, decided).into()),
    }
  }

  pub fn creatures_in_volume(&self, scene: &Scene, pt: Point3, volume: Volume) -> Vec<CreatureID> {
    let creature_locations = scene.creatures.iter().map(|(cid, &(pt, _))| (*cid, pt)).collect();
    self.tile_system.items_within_volume(volume, pt, &creature_locations)
  }

  pub fn creatures_and_terrain_in_volume(&self, scene: &Scene, pt: Point3, volume: Volume)
                                         -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
    let cids = self.creatures_in_volume(scene, pt, volume);
    let terrain = self.get_map(scene.map)?.terrain.iter();
    let all_tiles = terrain.map(|pt| (*pt, *pt)).collect();
    let result_tiles = self.tile_system.items_within_volume(volume, pt, &all_tiles);
    Ok((cids, result_tiles))
  }

  pub fn get_movement_options(&self, scene: SceneID, creature_id: CreatureID)
                              -> Result<Vec<Point3>, GameError> {
    let scene = self.get_scene(scene)?;
    let creature = self.get_creature(creature_id)?;
    if creature.can_move() {
      Ok(self
           .tile_system
           .get_all_accessible(scene.get_pos(creature_id)?,
                               self.get_map(scene.map)?,
                               creature.speed()))
    } else {
      Err(GameErrorEnum::CannotAct(creature.id()).into())
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(&self, scene: SceneID, creature_id: CreatureID, ability_id: AbilityID)
                            -> Result<PotentialTargets, GameError> {
    let ability = self.get_ability(&ability_id)?;

    Ok(match ability.target {
         TargetSpec::Melee => self.creatures_in_range(scene, creature_id, MELEE_RANGE)?,
         TargetSpec::Range(distance) => self.creatures_in_range(scene, creature_id, distance)?,
         TargetSpec::Actor => PotentialTargets::CreatureIDs(vec![creature_id]),
         TargetSpec::SomeCreaturesInVolumeInRange {
           volume,
           maximum,
           range,
         } => panic!(),
         TargetSpec::AllCreaturesInVolumeInRange { range, .. } => {
           self.open_terrain_in_range(scene, creature_id, range)?
         }
         TargetSpec::Volume { volume, range } => panic!(),
       })
  }

  fn open_terrain_in_range(&self, scene: SceneID, creature_id: CreatureID, range: Distance)
                           -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let creature_pos = scene.get_pos(creature_id)?;
    let map = self.get_map(scene.map)?;
    let pts = self.tile_system.open_points_in_range(creature_pos, map, range);
    Ok(PotentialTargets::Points(pts))
  }

  fn creatures_in_range(&self, scene: SceneID, creature_id: CreatureID, distance: Distance)
                        -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let my_pos = scene.get_pos(creature_id)?;
    let mut results = vec![];
    for (creature_id, &(creature_pos, _)) in &scene.creatures {
      if self.tile_system.points_within_distance(my_pos, creature_pos, distance) {
        results.push(*creature_id);
      }
    }
    Ok(PotentialTargets::CreatureIDs(results))
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
    new.game.creatures.mutate(&cid, |_| creature);
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
    t_perform(game, GameCommand::StartCombat(t_scene_id(), combatants))
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    t_perform(game, GameCommand::CombatAct(ability_id, target))
  }

  pub fn t_game() -> Game {
    let mut game = Game::new(t_classes(), t_abilities());
    game.maps.insert(huge_box());
    let rogue_creation = t_rogue_creation("rogue");
    let ranger_creation = t_ranger_creation("ranger");
    let cleric_creation = t_cleric_creation("cleric");
    let mut rogue = Creature::create(&rogue_creation);
    rogue.id = cid_rogue();
    rogue.initiative = Dice::flat(20);
    let mut ranger = Creature::create(&ranger_creation);
    ranger.id = cid_ranger();
    ranger.initiative = Dice::flat(10);
    let mut cleric = Creature::create(&cleric_creation);
    cleric.id = cid_cleric();
    cleric.initiative = Dice::flat(0);
    game.creatures.insert(rogue);
    game.creatures.insert(ranger);
    game.creatures.insert(cleric);
    game
      .scenes
      .insert(Scene {
                id: t_scene_id(),
                name: "Test Scene".to_string(),
                map: t_map_id(),
                attribute_checks: HashMap::new(),
                creatures: HashMap::from_iter(vec![(cid_rogue(),
                                                    ((0, 0, 0), Visibility::AllPlayers)),
                                                   (cid_cleric(),
                                                    ((0, 0, 0), Visibility::AllPlayers)),
                                                   (cid_ranger(),
                                                    ((0, 0, 0), Visibility::AllPlayers))]),
              });
    game
  }

  pub fn t_classes() -> HashMap<String, Class> {
    let rogue_abs = vec![abid("punch")];
    let ranger_abs = vec![abid("shoot")];
    let cleric_abs = vec![abid("heal"), abid("fireball")];
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

  pub fn t_perform(game: &Game, cmd: GameCommand) -> Game {
    game.perform_unchecked(cmd).unwrap().game
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
    let game = t_perform(&game,
                         GameCommand::CombatAct(abid("punch"),
                                                DecidedTarget::Creature(cid_ranger())));
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
    let game = t_perform(&game, GameCommand::StopCombat);
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
  }

  #[test]
  fn movement() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    t_perform(&game, GameCommand::PathCurrentCombatCreature((1, 0, 0)));
  }

  #[test]
  fn change_creature_initiative() {
    let game = t_combat();
    fn combat_cids(game: &Game) -> Vec<CreatureID> {
      game.get_combat().unwrap().combat.creatures.iter().map(|&(c, _)| c).collect()
    }
    assert_eq!(combat_cids(&game), vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    // move ranger to position 0
    let game = t_perform(&game, GameCommand::ChangeCreatureInitiative(cid_ranger(), 0));
    assert_eq!(combat_cids(&game), vec![cid_ranger(), cid_rogue(), cid_cleric()]);
  }

  #[test]
  fn three_char_infinite_combat() {
    let game = t_game();
    let game = t_perform(&game,
                         GameCommand::StartCombat(t_scene_id(),
                                                  vec![cid_rogue(), cid_ranger(), cid_cleric()]));
    let iter = |game: &Game| -> Result<Game, GameError> {
      let game = t_game_act(game, abid("punch"), DecidedTarget::Creature(cid_ranger()));
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      let game = t_game_act(&game, abid("heal"), DecidedTarget::Creature(cid_ranger()));
      let game = game.perform_unchecked(GameCommand::Done)?.game;
      Ok(game)
    };
    iter(&game).unwrap();
  }

  #[test]
  fn ability_creatures_within_area() {
    // the cleric moves away, then casts a fireball at the ranger and rogue.
    let game = t_game();
    let game = t_perform(&game,
                         GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), (11, 0, 0)));
    let game = t_perform(&game,
                         GameCommand::ActCreature(t_scene_id(),
                                                  cid_cleric(),
                                                  abid("fireball"),
                                                  DecidedTarget::Point((0, 0, 0))));
    assert_eq!(game.get_creature(cid_rogue()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_cleric()).unwrap().creature.cur_health, HP(10));
  }
}
