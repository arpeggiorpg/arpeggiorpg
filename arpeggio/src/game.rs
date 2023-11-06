use std::{
  cmp,
  collections::{HashMap, HashSet},
  iter::FromIterator,
};

use crate::{combat::*, creature::ChangedCreature, grid::line_through_point, types::*};
use foldertree::{FolderPath, FolderTreeError};

impl Game {
  pub fn export_module(&self, export_path: &FolderPath) -> Result<Game, GameError> {
    let mut new_game: Game = Default::default();
    new_game.tile_system = self.tile_system;

    // First the easy part: create a subtree of the campaign to use as the new campaign folder tree.
    new_game.campaign = self.campaign.subtree(export_path)?;
    // Now, walk that campaign and copy over the actual data to the new game.
    for path in new_game.campaign.walk_paths(&FolderPath::root()) {
      let folder = new_game.campaign.get(path).expect("folder we're walking must exist");
      for abid in &folder.abilities {
        new_game.abilities.insert(self.get_ability(*abid)?.clone());
      }
      for classid in &folder.classes {
        new_game.classes.insert(self.get_class(*classid)?.clone());
      }
      for cid in &folder.creatures {
        new_game.creatures.insert(self.get_creature(*cid)?.creature.clone());
      }
      for iid in &folder.items {
        new_game.items.insert(self.get_item(*iid)?.clone());
      }
      for sid in &folder.scenes {
        new_game.scenes.insert(self.get_scene(*sid)?.clone());
      }
    }

    // TODO FIXME RADIX: There are *other* references to game objects that we need to consider. This
    // may be something we should actually surface to the user as a warning when exporting a folder
    // as a module. At the very least, it would be better to fail during export rather than
    // exporting a module that can't be loaded (or which can be loaded and then breaks
    // serialization).
    // - Creatures inside the module may have classes outside of the module.
    // - Scenes inside the module may have hotspots to other scenes outside of the module.

    new_game.validate_campaign()?;
    Ok(new_game)
  }

  fn import_module(&mut self, import_path: &FolderPath, module: &Game) -> Result<(), GameError> {
    // go through all the basic owned contents of the `module`, copy them to self
    for ability in &module.abilities {
      self.abilities.insert(ability.clone());
    }
    for class in &module.classes {
      self.classes.insert(class.clone());
    }
    for creature in &module.creatures {
      self.creatures.insert(creature.clone());
    }
    for item in &module.items {
      self.items.insert(item.clone());
    }
    for scene in &module.scenes {
      self.scenes.insert(scene.clone());
    }
    self.campaign.copy_from_tree(import_path, &module.campaign)?;
    self.validate_campaign()?;
    Ok(())
  }

  pub fn validate_campaign(&self) -> Result<(), GameError> {
    let mut all_abilities = HashSet::new();
    let mut all_creatures = HashSet::new();
    let mut all_scenes = HashSet::new();
    let mut all_items = HashSet::new();
    let mut all_classes = HashSet::new();
    for folder_path in self.campaign.walk_paths(&FolderPath::root()) {
      let folder = self.campaign.get(folder_path).expect("walk_paths must return valid path");
      for sid in &folder.scenes {
        if all_scenes.contains(sid) {
          return Err(GameError::SceneAlreadyExists(*sid));
        }
        if !self.scenes.contains_key(sid) {
          return Err(GameError::SceneNotFound(*sid));
        }
        all_scenes.insert(*sid);
      }
      for cid in &folder.creatures {
        if all_creatures.contains(cid) {
          return Err(GameError::CreatureAlreadyExists(*cid));
        }
        if !self.creatures.contains_key(cid) {
          return Err(GameError::CreatureNotFound(cid.to_string()));
        }
        all_creatures.insert(*cid);
      }
      for iid in &folder.items {
        if all_items.contains(iid) {
          return Err(GameError::ItemAlreadyExists(*iid));
        }
        if !self.items.contains_key(iid) {
          return Err(GameError::ItemNotFound(*iid));
        }
        all_items.insert(*iid);
      }
      for abid in &folder.abilities {
        if all_abilities.contains(abid) {
          return Err(GameError::AbilityAlreadyExists(*abid));
        }
        if !self.abilities.contains_key(abid) {
          return Err(GameError::NoAbility(*abid));
        }
        all_abilities.insert(*abid);
      }
      for classid in &folder.classes {
        if all_classes.contains(classid) {
          return Err(GameError::ClassAlreadyExists(*classid));
        }
        if !self.classes.contains_key(classid) {
          return Err(GameError::ClassNotFound(*classid));
        }
        all_classes.insert(*classid);
      }
    }
    if all_scenes != HashSet::from_iter(self.scenes.keys().cloned()) {
      return Err(GameError::BuggyProgram("Not all scenes were in the campaign!".to_string()));
    }
    if all_creatures != HashSet::from_iter(self.creatures.keys().cloned()) {
      return Err(GameError::BuggyProgram("Not all creatures were in the campaign!".to_string()));
    }
    if all_items != HashSet::from_iter(self.items.keys().cloned()) {
      return Err(GameError::BuggyProgram("Not all items were in the campaign!".to_string()));
    }
    let game_abilities = HashSet::from_iter(self.abilities.keys().cloned());
    if all_abilities != game_abilities {
      return Err(GameError::BuggyProgram(format!(
        "Not all abilities were in the campaign! {all_abilities:?} VS {game_abilities:?}"
      )));
    }
    if all_classes != HashSet::from_iter(self.classes.keys().cloned()) {
      return Err(GameError::BuggyProgram("Not all classes were in the campaign!".to_string()));
    }
    Ok(())
  }

  pub fn creatures(&self) -> Result<HashMap<CreatureID, DynamicCreature>, GameError> {
    let mut map = HashMap::new();
    for creature in self.creatures.values() {
      map.insert(creature.id, self.dyn_creature(creature)?);
    }
    Ok(map)
  }

  pub fn get_item(&self, iid: ItemID) -> Result<&Item, GameError> {
    self.items.get(&iid).ok_or_else(|| GameError::ItemNotFound(iid))
  }

  pub fn get_ability(&self, abid: AbilityID) -> Result<&Ability, GameError> {
    self.abilities.get(&abid).ok_or_else(|| GameError::NoAbility(abid))
  }

  fn player_path(&self, suffix: FolderPath, player_id: &PlayerID) -> (FolderPath, Option<GameLog>) {
    let mut path = vec!["Players".to_string(), player_id.0.clone()];
    path.extend(suffix.into_vec().into_iter());
    let path: FolderPath = path.into();

    if let Err(FolderTreeError::FolderNotFound(_)) = self.campaign.get(&path) {
      (path.clone(), Some(GameLog::CreateFolder { path }))
    } else {
      (path, None)
    }
  }

  /// Perform a PlayerCommand on the current Game.
  pub fn perform_player_command(
    &self, player_id: PlayerID, cmd: PlayerCommand,
  ) -> Result<ChangedGame, GameError> {
    use self::PlayerCommand::*;
    let player =
      self.players.get(&player_id).ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
    let change = match cmd {
      ChatFromPlayer { message } => {
        self.change_with(GameLog::ChatFromPlayer { player_id, message: message.to_owned() })
      }
      CreateNote { path, note } => {
        let (path, log) = self.player_path(path, &player_id);
        let mut logs = log.into_iter().collect::<Vec<_>>();
        logs.push(GameLog::CreateNote { path: path.clone(), note });
        self.change_with_logs(logs)
      }
      EditNote { path, original_name, note } => {
        let (path, log) = self.player_path(path, &player_id);
        // we *could* add a new log called PlayerEditNote that includes the player_id, but that's
        // not strictly necessary; it would only be for informational purposes.
        let mut logs = vec![GameLog::EditNote { path, original_name, note }];
        logs.extend(log);
        self.change_with_logs(logs)
      }
      PathCreature { creature_id, destination } => {
        let scene_id =
          player.scene.ok_or(GameError::BuggyProgram(format!("Player isn't in a scene")))?;
        let scene = self.get_scene(scene_id)?;
        Ok(self.path_creature(scene.id, creature_id, destination)?.0)
      }
      CombatAct { ability_id, target } => {
        self.auth_combat(&player)?;
        self.combat_act(ability_id, target)
      }
      PathCurrentCombatCreature { destination } => {
        self.auth_combat(&player)?;
        self.get_combat()?.get_movement()?.move_current(destination)
      }
      EndTurn => {
        self.auth_combat(&player)?;
        self.next_turn()
      }
    };
    Ok(change?)
  }

  /// Check that the player controls the current combat creature.
  fn auth_combat(&self, player: &Player) -> Result<(), GameError> {
    if !player.creatures.contains(&self.get_combat()?.combat.current_creature_id()) {
      return Err(GameError::BuggyProgram(
        "You don't control the current combat creature".to_string(),
      ));
    }
    Ok(())
  }

  /// Perform a GMCommand on the current Game.
  ///
  /// The result includes a new Game instance and a Vec of GameLogs. These GameLogs should be a
  /// deterministic representation of the changes made to the Game, so they can be used to replay
  /// history and get the same exact result. An Undo operation can be implemented by rolling back to
  /// a previous game Snapshot and replaying until the desired GameLog.
  pub fn perform_gm_command(&self, cmd: GMCommand) -> Result<ChangedGame, GameError> {
    use self::GMCommand::*;
    let change = match cmd {
      LoadModule { ref name, ref path, source, game } => self.change_with(GameLog::LoadModule {
        name: name.clone(),
        module: game,
        path: path.clone(),
        source,
      }),
      SetActiveScene { id } => self.change_with(GameLog::SetActiveScene { id }),
      // ** Player Management **
      RegisterPlayer { ref id } => self.change_with(GameLog::RegisterPlayer { id: id.clone() }),
      GiveCreaturesToPlayer { ref player_id, ref creature_ids } => {
        self.change_with(GameLog::GiveCreaturesToPlayer {
          player_id: player_id.clone(),
          creature_ids: creature_ids.clone(),
        })
      }
      UnregisterPlayer { ref id } => self.change_with(GameLog::UnregisterPlayer { id: id.clone() }),
      RemoveCreaturesFromPlayer { ref player_id, ref creature_ids } => {
        self.change_with(GameLog::RemoveCreaturesFromPlayer {
          player_id: player_id.clone(),
          creature_ids: creature_ids.clone(),
        })
      }
      SetPlayerScene { ref player_id, scene_id } => {
        self.change_with(GameLog::SetPlayerScene { player_id: player_id.clone(), scene_id })
      }

      // ** Chat **
      ChatFromGM { ref message } => {
        self.change_with(GameLog::ChatFromGM { message: message.to_owned() })
      }

      // ** Attribute checks **
      AttributeCheck { creature_id, attribute_check } => {
        self.attribute_check(creature_id, &attribute_check)
      }
      // ** Folder Management **
      CreateFolder { path } => self.change_with(GameLog::CreateFolder { path }),
      RenameFolder { path, new_name } => self.change_with(GameLog::RenameFolder { path, new_name }),
      MoveFolderItem { source, item_id, destination } => {
        self.change_with(GameLog::MoveFolderItem { source, item_id, destination })
      }
      CopyFolderItem { source, item_id, dest } => {
        let new_item_id = match item_id {
          FolderItemID::CreatureID(_) => FolderItemID::CreatureID(CreatureID::gen()),
          FolderItemID::SceneID(_) => FolderItemID::SceneID(SceneID::gen()),
          FolderItemID::ItemID(_) => FolderItemID::ItemID(ItemID::gen()),
          FolderItemID::AbilityID(_) => FolderItemID::AbilityID(AbilityID::gen()),
          FolderItemID::ClassID(_) => FolderItemID::ClassID(ClassID::gen()),
          FolderItemID::NoteID(_) | FolderItemID::SubfolderID(_) => item_id.clone(),
        };
        self.change_with(GameLog::CopyFolderItem { source, item_id, dest, new_item_id })
      }
      DeleteFolderItem { path, item_id } => {
        self.change_with(GameLog::DeleteFolderItem { path, item_id })
      }

      CreateItem { path, name } => {
        let item = Item { id: ItemID::gen(), name };
        self.change_with(GameLog::CreateItem { path, item })
      }
      EditItem { item } => self.change_with(GameLog::EditItem { item }),

      CreateNote { path, note } => self.change_with(GameLog::CreateNote { path, note }),
      EditNote { path, original_name, note } => {
        self.change_with(GameLog::EditNote { path, original_name, note })
      }

      // ** Inventory Management **
      TransferItem { from, to, item_id, count } => {
        self.change_with(GameLog::TransferItem { from, to, item_id, count })
      }
      RemoveItem { owner, item_id, count } => {
        self.change_with(GameLog::RemoveItem { owner, item_id, count })
      }

      SetItemCount { owner, item_id, count } => {
        self.change_with(GameLog::SetItemCount { owner, item_id, count })
      }

      CreateScene { path, scene } => {
        let scene = Scene::create(scene);
        self.change_with(GameLog::CreateScene { path, scene })
      }
      EditSceneDetails { scene_id, details } => {
        self.change_with(GameLog::EditSceneDetails { scene_id, details })
      }
      SetSceneCreatureVisibility { scene_id, creature_id, visibility } => {
        self.change_with(GameLog::SetSceneCreatureVisibility { scene_id, creature_id, visibility })
      }
      AddCreatureToScene { scene_id, creature_id, ref visibility } => {
        self.change_with(GameLog::AddCreatureToScene {
          scene_id,
          creature_id,
          visibility: visibility.clone(),
        })
      }
      RemoveCreatureFromScene { scene_id, creature_id } => {
        self.change_with(GameLog::RemoveCreatureFromScene { scene_id, creature_id })
      }
      AddSceneChallenge { scene_id, ref description, ref challenge } => {
        self.change_with(GameLog::AddSceneChallenge {
          scene_id,
          description: description.clone(),
          challenge: challenge.clone(),
        })
      }
      RemoveSceneChallenge { scene_id, ref description } => self
        .change_with(GameLog::RemoveSceneChallenge { scene_id, description: description.clone() }),
      SetFocusedSceneCreatures { scene_id, ref creatures } => self
        .change_with(GameLog::SetFocusedSceneCreatures { scene_id, creatures: creatures.clone() }),
      RemoveSceneVolumeCondition { scene_id, condition_id } => {
        self.change_with(GameLog::RemoveSceneVolumeCondition { scene_id, condition_id })
      }

      // ** Classes & Abilities **
      CreateClass { path, class } => {
        let id = ClassID::gen();
        let class = Class {
          id,
          name: class.name.clone(),
          abilities: class.abilities.clone(),
          conditions: class.conditions.clone(),
          color: class.color.clone(),
        };
        self.change_with(GameLog::CreateClass { path, class })
      }
      EditClass { class } => self.change_with(GameLog::EditClass { class }),
      CreateAbility { path, ability } => {
        let id = AbilityID::gen();
        let ability = Ability {
          id,
          name: ability.name.clone(),
          cost: ability.cost,
          action: ability.action.clone(),
          usable_ooc: ability.usable_ooc,
        };
        self.change_with(GameLog::CreateAbility { path, ability })
      }
      EditAbility { ability } => self.change_with(GameLog::EditAbility { ability }),

      CreateCreature { path, creature } => {
        let creature = Creature::create(&creature);
        self.change_with(GameLog::CreateCreature { path, creature })
      }
      EditCreatureDetails { creature_id, details } => {
        self.change_with(GameLog::EditCreatureDetails { creature_id, details })
      }
      PathCreature { scene_id, creature_id, destination } => {
        Ok(self.path_creature(scene_id, creature_id, destination)?.0)
      }
      SetCreaturePos { scene_id, creature_id, pos } => {
        self.change_with(GameLog::SetCreaturePos { scene_id, creature_id, pos })
      }
      PathCurrentCombatCreature { destination } => {
        self.get_combat()?.get_movement()?.move_current(destination)
      }
      CombatAct { ability_id, target } => self.combat_act(ability_id, target),
      ActCreature { scene_id, creature_id, ability_id, target } => {
        self.ooc_act(scene_id, creature_id, ability_id, target)
      }
      EditSceneTerrain { scene_id, ref terrain } => {
        self.change_with(GameLog::EditSceneTerrain { scene_id, terrain: terrain.clone() })
      }
      EditSceneHighlights { scene_id, ref highlights } => {
        self.change_with(GameLog::EditSceneHighlights { scene_id, highlights: highlights.clone() })
      }
      EditSceneAnnotations { scene_id, ref annotations } => self
        .change_with(GameLog::EditSceneAnnotations { scene_id, annotations: annotations.clone() }),
      EditSceneRelatedScenes { scene_id, ref related_scenes } => {
        self.change_with(GameLog::EditSceneRelatedScenes {
          scene_id,
          related_scenes: related_scenes.clone(),
        })
      }
      EditSceneSceneHotspots { scene_id, ref scene_hotspots } => {
        self.change_with(GameLog::EditSceneSceneHotspots {
          scene_id,
          scene_hotspots: scene_hotspots.clone(),
        })
      }
      StartCombat { scene_id, combatants } => self.start_combat(scene_id, combatants),
      StopCombat => self.change_with(GameLog::StopCombat),
      AddCreatureToCombat { creature_id } => self.add_creature_to_combat(creature_id),
      RemoveCreatureFromCombat { creature_id } => {
        self.change_with(GameLog::RemoveCreatureFromCombat { creature_id })
      }
      RerollCombatInitiative => self.change().apply_combat(|c| c.reroll_initiative()),
      ChangeCreatureInitiative { creature_id, initiative } => {
        self.change_with(GameLog::CombatLog {
          log: CombatLog::ChangeCreatureInitiative { creature_id, initiative },
        })
      }
      ForceNextTurn => self.change_with(GameLog::CombatLog { log: CombatLog::ForceNextTurn }),
      ForcePrevTurn => self.change_with(GameLog::CombatLog { log: CombatLog::ForcePrevTurn }),
      EndTurn => self.next_turn(),

      // These are handled by the app before being passed to the Game:
      Rollback { .. } => bug("Game Rollback"),
    }?;
    Ok(change)
  }

  fn start_combat(
    &self, scene_id: SceneID, cids: Vec<CreatureID>,
  ) -> Result<ChangedGame, GameError> {
    let combatants = Combat::roll_initiative(self, cids)?;
    self.change_with(GameLog::StartCombat { scene_id, combatants })
  }

  fn add_creature_to_combat(&self, creature_id: CreatureID) -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(creature_id)?;
    let initiative = creature.creature.initiative.roll().1 as i16;
    self.change_with(GameLog::AddCreatureToCombat { creature_id, initiative })
  }

  fn attribute_check(
    &self, creature_id: CreatureID, check: &AttributeCheck,
  ) -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(creature_id)?;
    let (actual, success) = creature.creature.attribute_check(check)?;
    self.change_with(GameLog::AttributeCheckResult {
      creature_id,
      attribute_check: check.clone(),
      actual,
      success,
    })
  }

  pub fn path_creature(
    &self, scene: SceneID, cid: CreatureID, pt: Point3,
  ) -> Result<(ChangedGame, u32units::Length), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(
    &self, scene_id: SceneID, creature_id: CreatureID, pt: Point3, max_distance: u32units::Length,
  ) -> Result<(ChangedGame, u32units::Length), GameError> {
    let scene = self.get_scene(scene_id)?;
    let creature = self.get_creature(creature_id)?;
    let (path, distance) = self
      .tile_system
      .find_path(
        scene.get_pos(creature_id)?,
        max_distance,
        &scene.terrain,
        Volume::AABB(creature.creature.size),
        pt,
      )
      .ok_or(GameError::NoPathFound)?;
    debug_assert!(distance <= max_distance);

    let change = self.change_with(GameLog::PathCreature { scene_id, creature_id, path })?;
    Ok((change, distance))
  }

  fn next_turn(&self) -> Result<ChangedGame, GameError> {
    let change = self.change().apply_combat(|c| c.next_turn())?;
    change.apply_creature(self.current_combat.as_ref().unwrap().current_creature_id(), |c| c.tick())
  }

  fn link_folder_item(
    &mut self, path: &FolderPath, item_id: &FolderItemID,
  ) -> Result<(), GameError> {
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => node.creatures.insert(cid),
      FolderItemID::SceneID(sid) => node.scenes.insert(sid),
      FolderItemID::ItemID(iid) => node.items.insert(iid),
      FolderItemID::AbilityID(abid) => node.abilities.insert(abid),
      FolderItemID::ClassID(classid) => node.classes.insert(classid),
      FolderItemID::SubfolderID(_) => {
        return Err(GameError::BuggyProgram("Cannot link folders.".to_string()))
      }
      FolderItemID::NoteID(ref nid) => {
        return Err(GameError::CannotLinkNotes(path.clone(), nid.clone()))
      }
    };
    Ok(())
  }

  fn unlink_folder_item(
    &mut self, path: &FolderPath, item_id: &FolderItemID,
  ) -> Result<(), GameError> {
    fn remove_set<T: ::std::hash::Hash + Eq>(
      path: &FolderPath, item: &FolderItemID, s: &mut ::std::collections::HashSet<T>, key: &T,
    ) -> Result<(), GameError> {
      if !s.remove(key) {
        return Err(GameError::FolderItemNotFound(path.clone(), item.clone()));
      }
      Ok(())
    }
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => remove_set(path, item_id, &mut node.creatures, &cid)?,
      FolderItemID::SceneID(sid) => remove_set(path, item_id, &mut node.scenes, &sid)?,
      FolderItemID::ItemID(iid) => remove_set(path, item_id, &mut node.items, &iid)?,
      FolderItemID::AbilityID(abid) => remove_set(path, item_id, &mut node.abilities, &abid)?,
      FolderItemID::ClassID(classid) => remove_set(path, item_id, &mut node.classes, &classid)?,
      FolderItemID::SubfolderID(_) => {
        return Err(GameError::BuggyProgram("Cannot unlink folders.".to_string()))
      }
      FolderItemID::NoteID(ref nid) => {
        return Err(GameError::CannotLinkNotes(path.clone(), nid.clone()))
      }
    };
    Ok(())
  }

  pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
    let mut newgame = self.clone();
    newgame.apply_log_mut(log)?;
    Ok(newgame)
  }

  fn mutate_owner_inventory<F>(&mut self, owner_id: InventoryOwner, f: F) -> Result<(), GameError>
  where
    F: FnOnce(&mut Inventory),
  {
    let opt = match owner_id {
      InventoryOwner::Scene(sid) => self.scenes.mutate(&sid, |s| f(&mut s.inventory)),
      InventoryOwner::Creature(cid) => self.creatures.mutate(&cid, |c| f(&mut c.inventory)),
    };
    opt.ok_or_else(|| owner_id.not_found_error())
  }

  fn get_owner_inventory(&self, owner_id: InventoryOwner) -> Result<&Inventory, GameError> {
    match owner_id {
      InventoryOwner::Scene(sid) => self.get_scene(sid).map(|s| &s.inventory),
      InventoryOwner::Creature(cid) => self.get_creature(cid).map(|c| &c.creature.inventory),
    }
  }

  /// Remove some number of items from an inventory, returning the actual number removed.
  fn remove_inventory(
    &mut self, owner: InventoryOwner, item_id: ItemID, count: u64,
  ) -> Result<u64, GameError> {
    let actually_has = *self.get_owner_inventory(owner)?.get(&item_id).unwrap_or(&0);
    self.set_item_count(owner, item_id, actually_has - count)?;
    Ok(cmp::min(actually_has, count))
  }

  fn set_item_count(
    &mut self, owner: InventoryOwner, item_id: ItemID, count: u64,
  ) -> Result<(), GameError> {
    self.mutate_owner_inventory(owner, move |inventory: &mut Inventory| {
      if count == 0 {
        inventory.remove(&item_id).unwrap_or(0);
      } else {
        inventory.insert(item_id, count);
      }
    })
  }

  /// Apply a log to a *mutable* Game.
  // This is done so that we don't have to worry about `self` vs `newgame` -- all
  // manipulations here work on &mut self.
  fn apply_log_mut(&mut self, log: &GameLog) -> Result<(), GameError> {
    // HEY! Maintainer note! Don't use a call to *ID::gen(), or any other random or side-effecting
    // functions! All of that stuff should be resolved in perform_command. This function MUST be
    // purely deterministic.
    use self::GameLog::*;
    match *log {
      LoadModule { ref module, ref path, .. } => {
        if self.campaign.get(path).is_ok() {
          return Err(GameError::FolderAlreadyExists(path.clone()));
        } else {
          self.import_module(path, module)?;
        }
      }

      SetActiveScene { id } => self.active_scene = id,

      // Player stuff
      RegisterPlayer { ref id } => {
        if self.players.contains_key(id) {
          return Err(GameError::PlayerAlreadyExists(id.clone()));
        } else {
          self.players.insert(Player::new(id.clone()));
        }
      }

      UnregisterPlayer { ref id } => {
        self.players.remove(id).ok_or_else(|| GameError::PlayerNotFound(id.clone()))?;
      }

      GiveCreaturesToPlayer { ref player_id, ref creature_ids } => {
        for cid in creature_ids {
          self.check_creature_id(*cid)?;
        }
        self
          .players
          .mutate(player_id, |p| p.creatures.extend(creature_ids))
          .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
      }

      RemoveCreaturesFromPlayer { ref player_id, ref creature_ids } => {
        self
          .players
          .mutate(player_id, |p| {
            for cid in creature_ids {
              p.creatures.remove(cid);
            }
          })
          .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
      }

      SetPlayerScene { ref player_id, scene_id } => {
        self
          .players
          .mutate(player_id, move |p| p.scene = scene_id)
          .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
      }

      // purely informational
      ChatFromGM { .. } | ChatFromPlayer { .. } | AttributeCheckResult { .. } => {}

      // purely informational
      CreateFolder { ref path } => self.campaign.make_folders(path, Folder::new()),
      RenameFolder { ref path, ref new_name } => {
        self.campaign.rename_folder(path, new_name.clone())?
      }
      MoveFolderItem { ref source, ref item_id, ref destination } => match *item_id {
        FolderItemID::NoteID(ref name) => {
          let note = self.campaign.get_mut(source)?.notes.remove(name).ok_or_else(|| {
            GameError::FolderItemNotFound(source.clone(), FolderItemID::NoteID(name.clone()))
          })?;
          self.campaign.get_mut(destination)?.notes.insert(note);
        }
        FolderItemID::SubfolderID(ref name) => {
          self.campaign.move_folder(&source.child(name.clone()), destination)?;
        }
        _ => {
          self.unlink_folder_item(source, item_id)?;
          self.link_folder_item(destination, item_id)?;
        }
      },
      CopyFolderItem { ref item_id, ref dest, ref new_item_id, .. } => match (item_id, new_item_id)
      {
        (&FolderItemID::CreatureID(id), &FolderItemID::CreatureID(new_id)) => {
          let mut creature = self.get_creature(id)?.creature.clone();
          creature.id = new_id;
          self.apply_log_mut(&CreateCreature { path: dest.clone(), creature })?;
        }
        (&FolderItemID::CreatureID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::SceneID(id), &FolderItemID::SceneID(new_id)) => {
          let mut scene = self.get_scene(id)?.clone();
          scene.id = new_id;
          self.apply_log_mut(&CreateScene { path: dest.clone(), scene })?;
        }
        (&FolderItemID::SceneID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::ItemID(id), &FolderItemID::ItemID(new_id)) => {
          let mut item = self.get_item(id)?.clone();
          item.id = new_id;
          self.apply_log_mut(&CreateItem { path: dest.clone(), item })?;
        }
        (&FolderItemID::ItemID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::AbilityID(id), &FolderItemID::AbilityID(new_id)) => {
          let mut ability =
            self.abilities.get(&id).ok_or_else(|| GameError::NoAbility(id))?.clone();
          ability.id = new_id;
          self
            .abilities
            .try_insert(ability)
            .ok_or_else(|| GameError::AbilityAlreadyExists(new_id))?;
          self.link_folder_item(dest, &FolderItemID::AbilityID(new_id))?;
        }
        (&FolderItemID::AbilityID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::ClassID(id), &FolderItemID::ClassID(new_id)) => {
          let mut new_class =
            self.classes.get(&id).ok_or_else(|| GameError::ClassNotFound(id))?.clone();
          new_class.id = new_id;
          self
            .classes
            .try_insert(new_class)
            .ok_or_else(|| GameError::ClassAlreadyExists(new_id))?;
          self.link_folder_item(dest, &FolderItemID::ClassID(new_id))?;
        }
        (&FolderItemID::ClassID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::SubfolderID(_), _) => unimplemented!("Can't Copy subfolders"),
        (&FolderItemID::NoteID(_), _) => unimplemented!("Can't clone notes... yet?"),
      },
      DeleteFolderItem { ref path, ref item_id } => {
        // because we're being paranoid, we're walking ALL folder paths and checking if the given
        // item ID is found in ANY of them and cleaning it up.
        let all_folders: Vec<FolderPath> =
          self.campaign.walk_paths(&FolderPath::root()).cloned().collect();
        match *item_id {
          FolderItemID::NoteID(ref name) => {
            self.campaign.get_mut(path)?.notes.remove(name);
          }
          FolderItemID::ItemID(iid) => {
            for folder in all_folders {
              self.campaign.get_mut(&folder)?.items.remove(&iid);
            }
            // Also delete the item from all creature inventory slots
            let cids: Vec<CreatureID> = self.creatures.keys().cloned().collect();
            for cid in cids {
              self
                .creatures
                .mutate(&cid, |c| {
                  c.inventory.remove(&iid);
                })
                .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
            }
            // Also delete the item from all scene inventory slots
            let sids: Vec<SceneID> = self.scenes.keys().cloned().collect();
            for sid in sids {
              self
                .scenes
                .mutate(&sid, |s| {
                  s.inventory.remove(&iid);
                })
                .ok_or_else(|| GameError::SceneNotFound(sid))?;
            }
            // Also delete the item from the core item DB!
            self.items.remove(&iid);
          }
          FolderItemID::CreatureID(cid) => {
            for path in all_folders {
              let node = self.campaign.get_mut(&path)?;
              node.creatures.remove(&cid);
            }
            let scenes_with_this_creature: Vec<SceneID> = self
              .scenes
              .values()
              .filter_map(|s| if s.creatures.contains_key(&cid) { Some(s.id) } else { None })
              .collect();
            for sid in scenes_with_this_creature {
              self.scenes.mutate(&sid, |sc| {
                sc.creatures.remove(&cid);
              });
            }
            self.current_combat = {
              if let Ok(combat) = self.get_combat() {
                combat.remove_from_combat(cid)?
              } else {
                None
              }
            };

            self
              .creatures
              .remove(&cid)
              .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
          }
          FolderItemID::SceneID(sid) => {
            // TODO: Figure out how to deal with players referencing this scene.
            // - disallow deleting if in combat
            if let Ok(combat) = self.get_combat() {
              if combat.scene.id == sid {
                return Err(GameError::SceneInUse(sid));
              }
            }
            for path in all_folders {
              let node = self.campaign.get_mut(&path)?;
              node.scenes.remove(&sid);
            }
            self.scenes.remove(&sid);
          }
          FolderItemID::AbilityID(abid) => {
            for path in all_folders {
              let node = self.campaign.get_mut(&path)?;
              node.abilities.remove(&abid);
            }
            for class_id in self.classes.keys().cloned().collect::<Vec<_>>() {
              self
                .classes
                .mutate(&class_id, |c| {
                  c.abilities.retain(|el| *el == abid);
                })
                .expect("iterating classes...");
            }
            for cid in self.creatures.keys().cloned().collect::<Vec<CreatureID>>() {
              self
                .creatures
                .mutate(&cid, |c| {
                  c.abilities.remove(&abid);
                })
                .expect("Must exist");
            }
            self.abilities.remove(&abid);
          }
          FolderItemID::ClassID(classid) => {
            for cid in self.creatures.keys().cloned().collect::<Vec<CreatureID>>() {
              if self.get_creature(cid)?.creature.class == classid {
                return Err(GameError::BuggyProgram("Class in use!".to_string()));
              }
            }
            for path in all_folders {
              let node = self.campaign.get_mut(&path)?;
              node.classes.remove(&classid);
            }
            self.classes.remove(&classid);
          }
          FolderItemID::SubfolderID(ref name) => {
            // basically we delete everything by simulating GameLog::DeleteFolderItem for each
            // child. Order may matter here in case some objects can't be deleted before their
            // referents are cleaned up.
            let path = path.child(name.to_string());
            for child_folder in self.campaign.get_children(&path)?.clone() {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::SubfolderID(child_folder.clone()),
              })?;
            }
            let node = self.campaign.get(&path)?.clone();
            for scene_id in node.scenes {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::SceneID(scene_id),
              })?;
            }
            for cid in node.creatures {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::CreatureID(cid),
              })?;
            }
            for iid in node.items {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::ItemID(iid),
              })?;
            }
            for abid in node.abilities {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::AbilityID(abid),
              })?;
            }
            for classid in node.classes {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::ClassID(classid),
              })?;
            }
            for nname in node.notes.keys() {
              self.apply_log_mut(&DeleteFolderItem {
                path: path.clone(),
                item_id: FolderItemID::NoteID(nname.clone()),
              })?;
            }
            self.campaign.remove(&path)?;
          }
        }
      }

      CreateItem { ref path, item: ref ritem } => {
        let item = ritem.clone();
        self.items.try_insert(item).ok_or_else(|| GameError::ItemAlreadyExists(ritem.id))?;
        self.link_folder_item(path, &FolderItemID::ItemID(ritem.id))?;
      }
      EditItem { ref item } => {
        self
          .items
          .mutate(&item.id, move |i| *i = item.clone())
          .ok_or_else(|| GameError::ItemNotFound(item.id))?;
      }

      CreateNote { ref path, ref note } => {
        self.campaign.get_mut(path)?.notes.insert(note.clone());
      }
      EditNote { ref path, ref original_name, note: ref new_note } => {
        let node = self.campaign.get_mut(path)?;
        node.notes.mutate(original_name, move |note| *note = new_note.clone()).ok_or_else(
          || {
            GameError::FolderItemNotFound(
              path.clone(),
              FolderItemID::NoteID(original_name.to_string()),
            )
          },
        )?;
      }

      // ** Inventory Management **
      TransferItem { from, to, item_id, count } => {
        // I love rust! This code is guaranteed to run atomically because we have a &mut,
        // aka "exclusive borrow". Also we can return errors even if we've already mutated,
        // because apply_log creates a copy of the Game before mutating it.
        let to_give = self.remove_inventory(from, item_id, count)?;
        self.mutate_owner_inventory(to, |to_inv| {
          let recip_has = *to_inv.get(&item_id).unwrap_or(&0);
          to_inv.insert(item_id, to_give + recip_has);
        })?;
      }
      RemoveItem { owner, item_id, count } => {
        self.remove_inventory(owner, item_id, count)?;
      }
      SetItemCount { owner, item_id, count } => {
        self.set_item_count(owner, item_id, count)?;
      }

      // ** Scenes **
      CreateScene { ref path, scene: ref rscene } => {
        let scene = rscene.clone();
        self.scenes.try_insert(scene).ok_or_else(|| GameError::SceneAlreadyExists(rscene.id))?;
        self.link_folder_item(path, &FolderItemID::SceneID(rscene.id))?;
      }
      EditSceneDetails { scene_id, ref details } => {
        self
          .scenes
          .mutate(&scene_id, move |scene| {
            scene.name = details.name.clone();
            scene.background_image_url = details.background_image_url.clone();
            scene.background_image_offset = details.background_image_offset;
            scene.background_image_scale = details.background_image_scale;
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      SetSceneCreatureVisibility { scene_id, creature_id, ref visibility } => {
        if !self.get_scene(scene_id)?.creatures.contains_key(&creature_id) {
          return Err(GameError::CreatureNotFound(creature_id.to_string()));
        }
        self
          .scenes
          .mutate(&scene_id, move |scene| {
            let entry = scene.creatures.get_mut(&creature_id);
            let entry = entry.expect("Already checked that creature exists?!");
            entry.1 = visibility.clone();
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      AddCreatureToScene { scene_id, creature_id, ref visibility } => {
        self
          .scenes
          .mutate(&scene_id, move |s| {
            s.creatures.insert(creature_id, (Point3::new(0, 0, 0), visibility.clone()));
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      RemoveCreatureFromScene { scene_id, creature_id } => {
        self
          .scenes
          .mutate(&scene_id, move |s| {
            s.creatures.remove(&creature_id);
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      AddSceneChallenge { scene_id, ref description, ref challenge } => {
        self
          .scenes
          .mutate(&scene_id, move |s| {
            s.attribute_checks.insert(description.clone(), challenge.clone());
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      RemoveSceneChallenge { scene_id, ref description } => {
        self
          .scenes
          .mutate(&scene_id, move |s| {
            s.attribute_checks.remove(description);
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      SetFocusedSceneCreatures { scene_id, ref creatures } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.focused_creatures = creatures.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      RemoveSceneVolumeCondition { scene_id, condition_id } => {
        self
          .scenes
          .mutate(&scene_id, move |s| {
            s.volume_conditions.remove(&condition_id);
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      EditSceneTerrain { scene_id, ref terrain } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.terrain = terrain.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneHighlights { scene_id, ref highlights } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.highlights = highlights.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneAnnotations { scene_id, ref annotations } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.annotations = annotations.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneRelatedScenes { scene_id, ref related_scenes } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.related_scenes = related_scenes.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneSceneHotspots { scene_id, ref scene_hotspots } => {
        self
          .scenes
          .mutate(&scene_id, move |s| s.scene_hotspots = scene_hotspots.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      // ** Classes & Abilities **
      CreateClass { ref path, ref class } => {
        self.classes.insert(class.clone());
        self.link_folder_item(&path, &FolderItemID::ClassID(class.id))?;
      }
      EditClass { ref class } => {
        self.classes.mutate(&class.id, move |c| {
          c.name = class.name.clone();
          c.abilities = class.abilities.clone();
          c.conditions = class.conditions.clone();
          c.color = class.color.clone();
        });
      }
      CreateAbility { ref path, ref ability } => {
        self.abilities.insert(ability.clone());
        self.link_folder_item(&path, &FolderItemID::AbilityID(ability.id))?;
      }
      EditAbility { ref ability } => {
        self.abilities.mutate(&ability.id, move |a| {
          a.name = ability.name.clone();
          a.cost = ability.cost;
          a.action = ability.action.clone();
          a.usable_ooc = ability.usable_ooc;
        });
      }

      // ** Creatures **
      CreateCreature { ref path, creature: ref rcreature } => {
        let creature = rcreature.clone();
        self
          .creatures
          .try_insert(creature)
          .ok_or_else(|| GameError::CreatureAlreadyExists(rcreature.id()))?;
        self.link_folder_item(path, &FolderItemID::CreatureID(rcreature.id()))?;
      }
      EditCreatureDetails { creature_id, ref details } => {
        let mutated = self.creatures.mutate(&creature_id, move |c| {
          c.name = details.name.clone();
          c.class = details.class;
          c.portrait_url = details.portrait_url.clone();
          c.icon_url = details.icon_url.clone();
          c.note = details.note.clone();
          c.bio = details.bio.clone();
          c.initiative = details.initiative.clone();
          c.size = details.size;
        });
        mutated.ok_or_else(|| GameError::CreatureNotFound(creature_id.to_string()))?;
      }
      AddCreatureToCombat { creature_id, initiative } => {
        let mut combat = self.current_combat.clone().ok_or(GameError::NotInCombat)?;
        self.check_creature_id(creature_id)?;
        if combat.creatures.iter().any(|&(c, _)| c == creature_id) {
          return Err(GameError::AlreadyInCombat(creature_id));
        }
        combat.creatures.push((creature_id, initiative));
        self.current_combat = Some(combat);
      }
      RemoveCreatureFromCombat { creature_id } => {
        let combat = {
          let combat = self.get_combat()?;
          combat.remove_from_combat(creature_id)?
        };
        self.current_combat = combat;
      }
      CombatLog { ref log } => {
        self.current_combat = Some(self.get_combat()?.apply_log(log)?);
      }
      CreatureLog { creature_id, ref log } => {
        let creature = self.get_creature(creature_id)?.creature.apply_log(log)?;
        self.creatures.mutate(&creature_id, |c| *c = creature);
      }
      StartCombat { ref scene_id, ref combatants } => {
        for &(cid, _) in combatants {
          self.check_creature_id(cid)?;
        }
        self.check_scene(*scene_id)?;
        self.current_combat = Some(Combat::new(*scene_id, combatants.clone())?);
      }
      StopCombat => {
        self.current_combat.take().ok_or(GameError::NotInCombat)?;
      }
      SetCreaturePos { ref scene_id, ref creature_id, ref pos } => {
        let scene = self.get_scene(*scene_id)?.set_pos(*creature_id, *pos)?;
        self.scenes.insert(scene);
      }
      PathCreature { ref scene_id, ref creature_id, ref path } => {
        let scene = {
          let scene = self.get_scene(*scene_id)?;
          let current_pos = scene.get_pos(*creature_id)?;
          let dest = path.last().cloned().unwrap_or(current_pos);
          scene.set_pos(*creature_id, dest)?
        };
        self.scenes.insert(scene);
      }

      AddVolumeCondition { ref scene_id, point, volume, condition_id, ref condition, duration } => {
        let scene = self.get_scene(*scene_id)?.add_volume_condition(
          condition_id,
          point,
          volume,
          condition.clone(),
          duration,
        );
        self.scenes.insert(scene);
      }

      // Things that are handled at the App level
      Rollback { .. } => {
        return bug("GameLog Rollback");
      }
    }
    Ok(())
  }

  pub fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError> {
    if self.creatures.contains_key(&cid) {
      Ok(())
    } else {
      Err(GameError::CreatureNotFound(cid.to_string()))
    }
  }

  fn check_scene(&self, scene: SceneID) -> Result<(), GameError> {
    if self.scenes.contains_key(&scene) {
      Ok(())
    } else {
      Err(GameError::SceneNotFound(scene))
    }
  }

  pub fn is_in_combat(&self, cid: CreatureID) -> bool {
    match self.get_combat() {
      Ok(combat) => combat.combat.contains_creature(cid),
      Err(_) => false,
    }
  }

  pub fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature, GameError> {
    self.dyn_creature(
      self.creatures.get(&cid).ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?,
    )
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>(
    &'game self, creature: &'creature Creature,
  ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, id: SceneID) -> Result<&Scene, GameError> {
    self.scenes.get(&id).ok_or_else(|| GameError::SceneNotFound(id))
  }

  pub fn get_combat(&self) -> Result<DynamicCombat, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameError::NotInCombat)?;
    let scene = self.get_scene(combat.scene)?;
    Ok(DynamicCombat { scene, combat, game: self })
  }

  // ** CONSIDER ** moving this chunk of code to... Scene.rs?

  fn combat_act(&self, abid: AbilityID, target: DecidedTarget) -> Result<ChangedGame, GameError> {
    let combat = self.get_combat()?;
    let scene = combat.scene;
    let actor = combat.combat.current_creature_id();
    self._act(scene, actor, abid, target, true)
  }

  fn ooc_act(
    &self, scene: SceneID, cid: CreatureID, abid: AbilityID, target: DecidedTarget,
  ) -> Result<ChangedGame, GameError> {
    let scene = self.get_scene(scene)?;
    self._act(scene, cid, abid, target, false)
  }

  fn _act(
    &self, scene: &Scene, cid: CreatureID, abid: AbilityID, target: DecidedTarget, in_combat: bool,
  ) -> Result<ChangedGame, GameError> {
    if !scene.creatures.contains_key(&cid) {
      return Err(GameError::CreatureNotFound(cid.to_string()));
    }
    let creature = self.get_creature(cid)?;
    if creature.can_act() {
      if creature.has_ability(abid) {
        self.creature_act(
          &creature,
          scene,
          self.get_ability(abid)?,
          target,
          self.change(),
          in_combat,
        )
      } else {
        Err(GameError::CreatureLacksAbility(creature.id(), abid))
      }
    } else {
      Err(GameError::CannotAct(creature.id()))
    }
  }

  pub fn creature_act(
    &self, creature: &DynamicCreature, scene: &Scene, ability: &Ability, target: DecidedTarget,
    mut change: ChangedGame, in_combat: bool,
  ) -> Result<ChangedGame, GameError> {
    let mut change = match ability.action {
      Action::Creature { ref effect, target: tspec } => {
        let targets = self.resolve_creature_targets(creature, scene, tspec, target)?;
        for creature_id in &targets {
          change = change.apply_creature(*creature_id, |c| c.apply_effect(effect))?;
        }
        change
      }
      Action::SceneVolume { ref effect, target: tspec } => {
        match (effect, tspec, target) {
          (
            &SceneEffect::CreateVolumeCondition { duration, ref condition },
            SceneTarget::RangedVolume { volume, .. },
            // TODO: unimplemented!: honor and check `range`
            DecidedTarget::Point(point),
          ) => {
            let log = GameLog::AddVolumeCondition {
              condition_id: ConditionID::gen(),
              scene_id: scene.id,
              point,
              volume,
              condition: condition.clone(),
              duration,
            };
            change = change.apply(&log)?;
          }
          _ => return Err(GameError::BuggyProgram("Ugh".to_string())),
        }
        change
      }
    };

    if in_combat {
      change = change.apply_creature(creature.id(), |c| c.creature.reduce_energy(ability.cost))?;
    }
    Ok(change)
  }

  pub fn resolve_creature_targets(
    &self, creature: &DynamicCreature, scene: &Scene, target: CreatureTarget,
    decision: DecidedTarget,
  ) -> Result<Vec<CreatureID>, GameError> {
    match (target, decision) {
      (CreatureTarget::Melee, DecidedTarget::Creature(cid)) => {
        if self.tile_system.points_within_distance(
          scene.get_pos(creature.id())?,
          scene.get_pos(cid)?,
          MELEE_RANGE,
        ) {
          Ok(vec![cid])
        } else {
          Err(GameError::CreatureOutOfRange(cid))
        }
      }
      (CreatureTarget::Range(max), DecidedTarget::Creature(cid)) => {
        if self.tile_system.points_within_distance(
          scene.get_pos(creature.id())?,
          scene.get_pos(cid)?,
          max,
        ) {
          Ok(vec![cid])
        } else {
          Err(GameError::CreatureOutOfRange(cid))
        }
      }
      (CreatureTarget::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (_, DecidedTarget::Point(pt)) => {
        self.volume_creature_targets(scene, creature.creature.id, target, pt)
      }
      (spec, decided) => Err(GameError::InvalidTargetForTargetSpec(spec, decided)),
    }
  }

  // TODO: unimplemented! Honor terrain!
  // 1. `pt` must be visible to the caster
  // 2. volumes must not go through blocked terrain
  // 3. volumes must (generally) not go around corners
  fn volume_creature_targets(
    &self, scene: &Scene, actor_id: CreatureID, target: CreatureTarget, pt: Point3,
  ) -> Result<Vec<CreatureID>, GameError> {
    match target {
      CreatureTarget::AllCreaturesInVolumeInRange { volume, .. } => {
        // TODO: unimplemented! honor and check `range`
        Ok(scene.creatures_in_volume(self.tile_system, pt, volume))
      }
      CreatureTarget::LineFromActor { distance } => {
        let actor_pos = scene.get_pos(actor_id)?;
        let volume = line_through_point(actor_pos, pt, distance);
        let cids = scene.creatures_in_volume(self.tile_system, actor_pos, volume);
        // TODO: *ideally* we should start the line adjacent to the caster, but filtering out
        // also works.
        let cids = cids.into_iter().filter(|cid| *cid != actor_id).collect();
        Ok(cids)
      }
      _ => Err(GameError::InvalidTargetForTargetSpec(target, DecidedTarget::Point(pt))),
    }
  }

  /// Calculate which *points* and which *creatures* will be affected by an ability targeted at a
  /// point.
  pub fn preview_volume_targets(
    &self, scene: &Scene, actor_id: CreatureID, ability_id: AbilityID, pt: Point3,
  ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
    let ability = self.get_ability(ability_id)?;

    let cids = match ability.action {
      Action::Creature { target, .. } => {
        self.volume_creature_targets(scene, actor_id, target, pt)?
      }
      Action::SceneVolume { target: SceneTarget::RangedVolume { volume, .. }, .. } => {
        scene.creatures_in_volume(self.tile_system, pt, volume)
      }
    };
    let tiles = match ability.action {
      Action::Creature {
        target: CreatureTarget::AllCreaturesInVolumeInRange { volume, .. },
        ..
      }
      | Action::SceneVolume { target: SceneTarget::RangedVolume { volume, .. }, .. } => {
        // TODO: unimplemented! honor and check `range`
        scene.open_terrain_in_volume(self, pt, volume)?
      }
      Action::Creature { target: CreatureTarget::LineFromActor { distance }, .. } => {
        let actor_pos = scene.get_pos(actor_id)?;
        let volume = line_through_point(actor_pos, pt, distance);
        scene.open_terrain_in_volume(self, actor_pos, volume)?
      }
      _ => vec![],
    };
    Ok((cids, tiles))
  }

  pub fn get_movement_options(
    &self, scene: SceneID, creature_id: CreatureID,
  ) -> Result<Vec<Point3>, GameError> {
    let scene = self.get_scene(scene)?;
    let creature = self.get_creature(creature_id)?;
    if creature.can_move() {
      Ok(self.tile_system.get_all_accessible(
        scene.get_pos(creature_id)?,
        &scene.terrain,
        Volume::AABB(creature.creature.size),
        creature.speed(),
      ))
    } else {
      Err(GameError::CannotAct(creature.id()))
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(
    &self, scene: SceneID, creature_id: CreatureID, ability_id: AbilityID,
  ) -> Result<PotentialTargets, GameError> {
    let ability = self.get_ability(ability_id)?;

    use crate::types::{Action as A, CreatureTarget as CT};
    Ok(match ability.action {
      A::Creature { target: CT::Melee, .. } => {
        self.creatures_in_range(scene, creature_id, MELEE_RANGE)?
      }
      A::Creature { target: CT::Range(distance), .. } => {
        self.creatures_in_range(scene, creature_id, distance)?
      }
      A::Creature { target: CT::Actor, .. } => PotentialTargets::CreatureIDs(vec![creature_id]),
      A::Creature { target: CT::AllCreaturesInVolumeInRange { range, .. }, .. }
      | A::SceneVolume { target: SceneTarget::RangedVolume { range, .. }, .. } => {
        self.open_terrain_in_range(scene, creature_id, range)?
      }
      A::Creature { target: CT::LineFromActor { distance }, .. } => {
        self.open_terrain_in_range(scene, creature_id, distance)?
      }
      A::Creature { target: CT::SomeCreaturesInVolumeInRange { .. }, .. } => {
        unimplemented!("SomeCreaturesInVolumeInRange not implemented")
      }
    })
  }

  fn open_terrain_in_range(
    &self, scene: SceneID, creature_id: CreatureID, range: u32units::Length,
  ) -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let creature_pos = scene.get_pos(creature_id)?;
    let pts = self.tile_system.open_points_in_range(creature_pos, &scene.terrain, range);
    Ok(PotentialTargets::Points(pts))
  }

  fn creatures_in_range(
    &self, scene: SceneID, creature_id: CreatureID, distance: u32units::Length,
  ) -> Result<PotentialTargets, GameError> {
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

  pub fn get_class(&self, class: ClassID) -> Result<&Class, GameError> {
    self.classes.get(&class).ok_or_else(|| GameError::ClassNotFound(class))
  }

  pub fn change(&self) -> ChangedGame { ChangedGame { game: self.clone(), logs: vec![] } }

  pub fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError> {
    let game = self.apply_log(&log)?;
    Ok(ChangedGame { game, logs: vec![log] })
  }

  pub fn change_with_logs(&self, logs: Vec<GameLog>) -> Result<ChangedGame, GameError> {
    let mut game = self.clone();
    for log in logs.iter() {
      game = game.apply_log(log)?;
    }
    Ok(ChangedGame { game, logs })
  }
}

impl ChangedGame {
  pub fn apply(&self, log: &GameLog) -> Result<ChangedGame, GameError> {
    let mut new = self.clone();
    new.game = self.game.apply_log(log)?;
    Ok(new)
  }

  pub fn apply_combat<'game, F>(&'game self, f: F) -> Result<ChangedGame, GameError>
  where
    F: FnOnce(DynamicCombat<'game>) -> Result<ChangedCombat<'game>, GameError>,
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
  where
    F: FnOnce(DynamicCreature) -> Result<ChangedCreature, GameError>,
  {
    let creature = self.game.get_creature(cid)?;
    let change = f(creature)?;
    let mut new = self.clone();
    let (creature, logs) = change.done();
    new.game.creatures.mutate(&cid, move |c| *c = creature);
    new.logs.extend(creature_logs_into_game_logs(cid, logs));
    Ok(new)
  }

  pub fn done(self) -> (Game, Vec<GameLog>) { (self.game, self.logs) }
}

fn bug<T>(msg: &str) -> Result<T, GameError> { Err(GameError::BuggyProgram(msg.to_string())) }

#[cfg(test)]
pub mod test {
  use std::{collections::HashSet, iter::FromIterator};

  use maplit::hashset;

  use crate::{combat::test::*, game::*, types::test::*};
  use indexed::IndexedHashMap;

  pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    t_perform(game, GMCommand::StartCombat { scene_id: t_scene_id(), combatants })
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    t_perform(game, GMCommand::CombatAct { ability_id, target })
  }

  pub fn t_game() -> Game {
    let mut game: Game = Default::default();
    game.abilities = t_abilities();
    game.classes = t_classes();
    let mut rogue = t_rogue("rogue");
    rogue.id = cid_rogue();
    let mut ranger = t_ranger("ranger");
    ranger.id = cid_ranger();
    let mut cleric = t_cleric("cleric");
    cleric.id = cid_cleric();
    game.creatures.insert(rogue);
    game.creatures.insert(ranger);
    game.creatures.insert(cleric);
    game.scenes.insert(t_scene());
    let mut folder = Folder::new();
    for creature_id in game.creatures.keys() {
      folder.creatures.insert(*creature_id);
    }
    for scene_id in game.scenes.keys() {
      folder.scenes.insert(*scene_id);
    }
    for class_id in game.classes.keys() {
      folder.classes.insert(*class_id);
    }
    for ab_id in game.abilities.keys() {
      folder.abilities.insert(*ab_id);
    }
    game.campaign.make_folder(&FolderPath::root(), "testdata".to_string(), folder).unwrap();
    game
  }

  #[test]
  fn validate_test_game() { t_game().validate_campaign().expect("Test game must validate"); }

  pub fn t_classes() -> IndexedHashMap<Class> {
    let rogue_abs = vec![abid_punch()];
    let ranger_abs = vec![abid_shoot(), abid_piercing_shot()];
    let cleric_abs = vec![abid_heal(), abid_fireball()];
    IndexedHashMap::from_iter(vec![
      Class {
        id: classid_rogue(),
        name: "Rogue".to_string(),
        abilities: rogue_abs,
        conditions: vec![],
        color: "purple".to_string(),
      },
      Class {
        id: classid_ranger(),
        name: "Ranger".to_string(),
        abilities: ranger_abs,
        conditions: vec![],
        color: "darkgreen".to_string(),
      },
      Class {
        id: classid_cleric(),
        name: "Cleric".to_string(),
        abilities: cleric_abs,
        conditions: vec![],
        color: "lightgreen".to_string(),
      },
    ])
  }

  pub fn perf(game: &Game, cmd: GMCommand) -> Result<ChangedGame, GameError> {
    game.perform_gm_command(cmd)
  }

  pub fn t_perform(game: &Game, cmd: GMCommand) -> Game { perf(game, cmd).unwrap().game }

  #[test]
  fn start_combat_not_found() {
    let game = t_game();
    let non = CreatureID::gen();
    let result = game
      .perform_gm_command(GMCommand::StartCombat { scene_id: t_scene_id(), combatants: vec![non] });
    match result {
      Err(GameError::CreatureNotFound(id)) => assert_eq!(id, non.to_string()),
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn combat_must_have_creatures() {
    let game = t_game();
    let result = game
      .perform_gm_command(GMCommand::StartCombat { scene_id: t_scene_id(), combatants: vec![] });
    match result {
      Err(GameError::CombatMustHaveCreatures) => {}
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn stop_combat() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    let game = t_perform(
      &game,
      GMCommand::CombatAct {
        ability_id: abid_punch(),
        target: DecidedTarget::Creature(cid_ranger()),
      },
    );
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
    let game = t_perform(&game, GMCommand::StopCombat);
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health(), HP(7));
  }

  #[test]
  fn movement() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    t_perform(&game, GMCommand::PathCurrentCombatCreature { destination: Point3::new(100, 0, 0) });
  }

  #[test]
  fn change_creature_initiative() {
    let game = t_combat();
    assert_eq!(
      game.get_combat().unwrap().combat.creature_ids(),
      vec![cid_rogue(), cid_ranger(), cid_cleric()]
    );
    // move ranger to have an initiative higher than the rogue
    let game = t_perform(
      &game,
      GMCommand::ChangeCreatureInitiative { creature_id: cid_ranger(), initiative: 30 },
    );
    assert_eq!(
      game.get_combat().unwrap().combat.creature_ids(),
      vec![cid_ranger(), cid_rogue(), cid_cleric()]
    );
  }

  #[test]
  fn three_char_infinite_combat() {
    let game = t_game();
    let game = t_perform(
      &game,
      GMCommand::StartCombat {
        scene_id: t_scene_id(),
        combatants: vec![cid_rogue(), cid_ranger(), cid_cleric()],
      },
    );
    let iter = |game: &Game| -> Result<Game, GameError> {
      let game = t_game_act(game, abid_punch(), DecidedTarget::Creature(cid_ranger()));
      let game = t_perform(&game, GMCommand::EndTurn);
      let game = t_perform(&game, GMCommand::EndTurn);
      let game = t_game_act(&game, abid_heal(), DecidedTarget::Creature(cid_ranger()));
      let game = t_perform(&game, GMCommand::EndTurn);
      Ok(game)
    };
    iter(&game).unwrap();
  }

  #[test]
  fn ability_creatures_within_area() {
    // the cleric moves away, then casts a fireball at the ranger and rogue.
    let game = t_game();
    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_cleric(),
        pos: Point3::new(1100, 0, 0),
      },
    );
    let game = t_perform(
      &game,
      GMCommand::ActCreature {
        scene_id: t_scene_id(),
        creature_id: cid_cleric(),
        ability_id: abid_fireball(),
        target: DecidedTarget::Point(Point3::new(0, 0, 0)),
      },
    );
    assert_eq!(game.get_creature(cid_rogue()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_cleric()).unwrap().creature.cur_health, HP(10));
  }

  #[test]
  fn test_creatures_in_sphere() {
    let game = t_game();
    let volume = Volume::Sphere(u32cm(200));
    let pt = Point3::new(500, 0, 0);

    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_rogue(),
        pos: Point3::new(500, 0, 0),
      },
    );
    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_cleric(),
        pos: Point3::new(600, 0, 0),
      },
    );
    let scene = game.get_scene(t_scene_id()).unwrap();

    let cids = scene.creatures_in_volume(game.tile_system, pt, volume);
    let cids = HashSet::<CreatureID>::from_iter(cids);
    assert_eq!(cids, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn test_sphere_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::AllCreaturesInVolumeInRange {
      range: u32cm(1000),
      volume: Volume::Sphere(u32cm(200)),
    };
    let pt = Point3::new(500, 0, 0);

    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_rogue(),
        pos: Point3::new(500, 0, 0),
      },
    );
    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_cleric(),
        pos: Point3::new(600, 0, 0),
      },
    );

    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game.volume_creature_targets(scene, cid_ranger(), target_spec, pt).unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn test_line_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::LineFromActor { distance: u32cm(1000) };
    let pt = Point3::new(100, 0, 0);

    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_rogue(),
        pos: Point3::new(100, 0, 0),
      },
    );
    let game = t_perform(
      &game,
      GMCommand::SetCreaturePos {
        scene_id: t_scene_id(),
        creature_id: cid_cleric(),
        pos: Point3::new(200, 0, 0),
      },
    );
    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game.volume_creature_targets(scene, cid_ranger(), target_spec, pt).unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn preview_volume_targets_shows_creatures_for_scene_volume_actions() {
    // When previewing volume targets for an action that affects a Scene Volume, the creatures
    // within the volume will be returned.

    let game = t_game();
    let scene = game.get_scene(t_scene_id()).unwrap();
    let cleric = cid_cleric();
    let ability_id = abid_thorn_patch();
    let preview =
      game.preview_volume_targets(scene, cleric, ability_id, Point3::new(0, 0, 0)).unwrap();
    let expected = hashset! {cid_cleric(), cid_ranger(), cid_rogue()};
    assert_eq!(HashSet::from_iter(preview.0), expected);
  }

  #[test]
  fn test_export_module() {
    let root_path = FolderPath::root();
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let note = Note { name: "My Note".to_string(), content: "My Content".to_string() };
    let mut folder = Folder::new();
    folder.notes.insert(note.clone());

    let mut game = t_game();
    game.campaign.make_folder(&root_path, "Rules".to_string(), folder).expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    let new_note = new_game.campaign.get(&root_path).unwrap().notes.get("My Note");
    assert_eq!(new_note.expect("My Note wasn't at root"), &note);
  }

  #[test]
  fn test_export_module_references() {
    let root_path = FolderPath::root();
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let mut folder = Folder::new();
    folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game.campaign.make_folder(&root_path, "Rules".to_string(), folder).expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
      new_game.get_class(classid_ranger()).expect("new game didn't have ranger class"),
      game.get_class(classid_ranger()).expect("Old game didn't have ranger class")
    );
  }

  #[test]
  fn test_export_subfolders() {
    let root_path = FolderPath::root();
    let rules_path = "/Rules".parse().unwrap();
    let root = Folder::new();
    let mut classes_folder = Folder::new();
    classes_folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game.campaign.make_folder(&root_path, "Rules".to_string(), root).unwrap();
    game.campaign.make_folder(&rules_path, "Classes".to_string(), classes_folder).unwrap();
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
      new_game.get_class(classid_ranger()).expect("new game didn't have ranger class"),
      game.get_class(classid_ranger()).expect("Old game didn't have ranger class")
    );

    let new_classes = &new_game.campaign.get(&"/Classes".parse().unwrap()).unwrap().classes;
    let old_classes = &game.campaign.get(&"/Rules/Classes".parse().unwrap()).unwrap().classes;
    assert_eq!(new_classes, old_classes);
  }

  #[test]
  fn test_import_module() {
    let mut module: Game = Default::default();
    let classid = ClassID::gen();
    let class = Class {
      id: classid,
      name: "Blood Hunter".to_string(),
      abilities: vec![],
      conditions: vec![],
      color: "blue".to_string(),
    };
    module.classes.insert(class);
    module.link_folder_item(&FolderPath::root(), &FolderItemID::ClassID(classid)).unwrap();

    let sys_path = "/System".parse().unwrap();

    let mut game = t_game();
    game.import_module(&sys_path, &module).expect("import must succeed");

    assert_eq!(
      game.get_class(classid).expect("New game missing class"),
      module.get_class(classid).expect("Old game missing class")
    );
    assert!(game.campaign.get(&sys_path).unwrap().classes.contains(&classid));
  }
}
