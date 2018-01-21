use std::collections::{HashMap, HashSet};
use std::cmp;
use std::fs::File;
use std::iter::FromIterator;
use std::io::Read;
use std::path::{Path, PathBuf};

use serde_yaml;

use types::*;
use combat::*;
use creature::ChangedCreature;
use foldertree::FolderPath;
use grid::line_through_point;

impl Game {
  pub fn export_module(&self, export_path: &FolderPath) -> Result<Game, GameError> {
    let mut new_game: Game = Default::default();
    new_game.tile_system = self.tile_system;

    // First the easy part: create a subtree of the campaign to use as the new campaign folder tree.
    new_game.campaign = self.campaign.subtree(export_path)?;
    // Now, walk that campaign and copy over the actual data to the new game.
    for path in new_game
      .campaign
      .walk_paths(&FolderPath::from_vec(vec![]))
      .cloned()
      .collect::<Vec<_>>()
    {
      let folder = new_game
        .campaign
        .get(&path)
        .expect("folder we're walking must exist");
      for abid in &folder.abilities {
        new_game.abilities.insert(self.get_ability(*abid)?.clone());
      }
      for classid in &folder.classes {
        new_game.classes.insert(self.get_class(*classid)?.clone());
      }
      for cid in &folder.creatures {
        new_game
          .creatures
          .insert(self.get_creature(*cid)?.creature.clone());
      }
      for iid in &folder.items {
        new_game.items.insert(self.get_item(*iid)?.clone());
      }
      for sid in &folder.scenes {
        new_game.scenes.insert(self.get_scene(*sid)?.clone());
      }
    }
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
    // then merge the `module`'s folder structure into self
    Ok(())
  }

  pub fn validate_campaign(&self) -> Result<(), GameError> {
    let mut all_abilities = HashSet::new();
    let mut all_creatures = HashSet::new();
    let mut all_scenes = HashSet::new();
    let mut all_items = HashSet::new();
    let mut all_classes = HashSet::new();
    let all_folders: Vec<FolderPath> = self
      .campaign
      .walk_paths(&FolderPath::from_vec(vec![]))
      .cloned()
      .collect();
    for folder_path in all_folders {
      let folder = self
        .campaign
        .get(&folder_path)
        .expect("walk_paths must return valid path");
      for sid in &folder.scenes {
        if all_scenes.contains(sid) {
          bail!(GameError::SceneAlreadyExists(*sid));
        }
        if !self.scenes.contains_key(sid) {
          bail!(GameError::SceneNotFound(*sid));
        }
        all_scenes.insert(*sid);
      }
      for cid in &folder.creatures {
        if all_creatures.contains(cid) {
          bail!(GameError::CreatureAlreadyExists(*cid));
        }
        if !self.creatures.contains_key(cid) {
          bail!(GameError::CreatureNotFound(cid.to_string()));
        }
        all_creatures.insert(*cid);
      }
      for iid in &folder.items {
        if all_items.contains(iid) {
          bail!(GameError::ItemAlreadyExists(*iid));
        }
        if !self.items.contains_key(iid) {
          bail!(GameError::ItemNotFound(*iid));
        }
        all_items.insert(*iid);
      }
      for abid in &folder.abilities {
        if all_abilities.contains(abid) {
          bail!(GameError::AbilityAlreadyExists(*abid));
        }
        if !self.abilities.contains_key(abid) {
          bail!(GameError::NoAbility(*abid));
        }
        all_abilities.insert(*abid);
      }
      for classid in &folder.classes {
        if all_classes.contains(classid) {
          bail!(GameError::ClassAlreadyExists(*classid));
        }
        if !self.classes.contains_key(classid) {
          bail!(GameError::ClassNotFound(*classid));
        }
        all_classes.insert(*classid);
      }
    }
    if all_scenes != HashSet::from_iter(self.scenes.keys().cloned()) {
      bail!("Not all scenes were in the campaign!");
    }
    if all_creatures != HashSet::from_iter(self.creatures.keys().cloned()) {
      bail!("Not all creatures were in the campaign!");
    }
    if all_items != HashSet::from_iter(self.items.keys().cloned()) {
      bail!("Not all items were in the campaign!");
    }
    if all_abilities != HashSet::from_iter(self.abilities.keys().cloned()) {
      bail!("Not all abilities were in the campaign!");
    }
    if all_classes != HashSet::from_iter(self.classes.keys().cloned()) {
      bail!("Not all classes were in the campaign!");
    }
    Ok(())
  }

  pub fn get_world(&self) -> Result<Option<CollisionWorld>, GameError> {
    match self.active_scene {
      Some(scene_id) => {
        let scene = self.get_scene(scene_id)?;
        scene.get_world(self).map(Some)
      }
      None => Ok(None),
    }
  }

  pub fn creatures(&self) -> Result<HashMap<CreatureID, DynamicCreature>, GameError> {
    let mut map = HashMap::new();
    for creature in self.creatures.values() {
      map.insert(creature.id, self.dyn_creature(creature)?);
    }
    Ok(map)
  }

  pub fn get_item(&self, iid: ItemID) -> Result<&Item, GameError> {
    self
      .items
      .get(&iid)
      .ok_or_else(|| GameError::ItemNotFound(iid).into())
  }

  pub fn get_ability(&self, abid: AbilityID) -> Result<&Ability, GameError> {
    self
      .abilities
      .get(&abid)
      .ok_or_else(|| GameError::NoAbility(abid).into())
  }

  /// Perform a GameCommand on the current Game.
  pub fn perform_command(
    &self, cmd: GameCommand, saved_game_path: PathBuf
  ) -> Result<ChangedGame, GameError> {
    use self::GameCommand::*;
    let change = match cmd {
      LoadModule { ref name, ref path } => {
        let app = load_app_from_path(&saved_game_path, name)?;
        let module = app.current_game;
        self.change_with(GameLog::LoadModule {
          module,
          path: path.clone(),
        })
      }
      SetActiveScene(m_sid) => self.change_with(GameLog::SetActiveScene(m_sid)),
      // ** Player Management **
      RegisterPlayer(ref pid) => self.change_with(GameLog::RegisterPlayer(pid.clone())),
      GiveCreaturesToPlayer(ref pid, ref cids) => {
        self.change_with(GameLog::GiveCreaturesToPlayer(pid.clone(), cids.clone()))
      }
      UnregisterPlayer(ref pid) => self.change_with(GameLog::UnregisterPlayer(pid.clone())),
      RemoveCreaturesFromPlayer(ref pid, ref cids) => self.change_with(
        GameLog::RemoveCreaturesFromPlayer(pid.clone(), cids.clone()),
      ),
      SetPlayerScene(ref pid, opt_sid) => {
        self.change_with(GameLog::SetPlayerScene(pid.clone(), opt_sid))
      }

      // ** Chat **
      ChatFromGM(ref msg) => self.change_with(GameLog::ChatFromGM(msg.to_owned())),
      ChatFromPlayer(ref pid, ref msg) => {
        self.change_with(GameLog::ChatFromPlayer(pid.to_owned(), msg.to_owned()))
      }

      // ** Attribute checks **
      AttributeCheck(cid, check) => self.attribute_check(cid, &check),
      // ** Folder Management **
      CreateFolder(path) => self.change_with(GameLog::CreateFolder(path)),
      RenameFolder(path, name) => self.change_with(GameLog::RenameFolder(path, name)),
      MoveFolderItem(src, item, dst) => self.change_with(GameLog::MoveFolderItem(src, item, dst)),
      CopyFolderItem {
        source,
        item_id,
        dest,
      } => {
        let new_item_id = match item_id {
          FolderItemID::CreatureID(_) => FolderItemID::CreatureID(CreatureID::gen()),
          FolderItemID::SceneID(_) => FolderItemID::SceneID(SceneID::gen()),
          FolderItemID::ItemID(_) => FolderItemID::ItemID(ItemID::gen()),
          FolderItemID::AbilityID(_) => FolderItemID::AbilityID(AbilityID::gen()),
          FolderItemID::ClassID(_) => FolderItemID::ClassID(ClassID::gen()),
          FolderItemID::NoteID(_) | FolderItemID::SubfolderID(_) => item_id.clone(),
        };
        self.change_with(GameLog::CopyFolderItem {
          source,
          item_id,
          dest,
          new_item_id,
        })
      }
      DeleteFolderItem(path, item_id) => self.change_with(GameLog::DeleteFolderItem(path, item_id)),

      CreateItem(path, name) => {
        let item = Item {
          id: ItemID::gen(),
          name,
        };
        self.change_with(GameLog::CreateItem(path, item))
      }
      EditItem(item) => self.change_with(GameLog::EditItem(item)),

      CreateNote(path, note) => self.change_with(GameLog::CreateNote(path, note)),
      EditNote(path, orig, new) => self.change_with(GameLog::EditNote(path, orig, new)),

      // ** Inventory Management **
      TransferItem {
        from,
        to,
        item_id,
        count,
      } => self.change_with(GameLog::TransferItem {
        from,
        to,
        item_id,
        count,
      }),
      RemoveItem {
        owner,
        item_id,
        count,
      } => self.change_with(GameLog::RemoveItem {
        owner,
        item_id,
        count,
      }),

      SetItemCount {
        owner,
        item_id,
        count,
      } => self.change_with(GameLog::SetItemCount {
        owner,
        item_id,
        count,
      }),

      CreateScene(path, sc) => {
        let scene = Scene::create(sc);
        self.change_with(GameLog::CreateScene(path, scene))
      }
      EditSceneDetails { scene_id, details } => {
        self.change_with(GameLog::EditSceneDetails { scene_id, details })
      }
      SetSceneCreatureVisibility {
        scene_id,
        creature_id,
        visibility,
      } => self.change_with(GameLog::SetSceneCreatureVisibility {
        scene_id,
        creature_id,
        visibility: visibility.clone(),
      }),
      AddCreatureToScene {
        scene_id,
        creature_id,
        ref visibility,
      } => self.change_with(GameLog::AddCreatureToScene {
        scene_id,
        creature_id,
        visibility: visibility.clone(),
      }),
      RemoveCreatureFromScene {
        scene_id,
        creature_id,
      } => self.change_with(GameLog::RemoveCreatureFromScene {
        scene_id,
        creature_id,
      }),
      AddSceneChallenge {
        scene_id,
        ref description,
        ref challenge,
      } => self.change_with(GameLog::AddSceneChallenge {
        scene_id,
        description: description.clone(),
        challenge: challenge.clone(),
      }),
      RemoveSceneChallenge {
        scene_id,
        ref description,
      } => self.change_with(GameLog::RemoveSceneChallenge {
        scene_id,
        description: description.clone(),
      }),
      SetFocusedSceneCreatures {
        scene_id,
        ref creatures,
      } => self.change_with(GameLog::SetFocusedSceneCreatures {
        scene_id,
        creatures: creatures.clone(),
      }),
      RemoveSceneVolumeCondition {
        scene_id,
        condition_id,
      } => self.change_with(GameLog::RemoveSceneVolumeCondition {
        scene_id,
        condition_id,
      }),

      CreateCreature(path, spec) => {
        let creature = Creature::create(&spec);
        self.change_with(GameLog::CreateCreature(path, creature))
      }
      EditCreatureDetails {
        creature_id,
        details,
      } => self.change_with(GameLog::EditCreatureDetails {
        creature_id,
        details,
      }),
      PathCreature(scene, cid, pt) => Ok(self.path_creature(scene, cid, pt)?.0),
      SetCreaturePos(scene, cid, pt) => self.change_with(GameLog::SetCreaturePos(scene, cid, pt)),
      PathCurrentCombatCreature(pt) => self.get_combat()?.get_movement()?.move_current(pt),
      CombatAct(abid, dtarget) => self.combat_act(abid, dtarget),
      ActCreature(scene, cid, abid, dtarget) => self.ooc_act(scene, cid, abid, dtarget),
      EditSceneTerrain {
        scene_id,
        ref terrain,
      } => self.change_with(GameLog::EditSceneTerrain {
        scene_id,
        terrain: terrain.clone(),
      }),
      EditSceneHighlights {
        scene_id,
        ref highlights,
      } => self.change_with(GameLog::EditSceneHighlights {
        scene_id,
        highlights: highlights.clone(),
      }),
      EditSceneAnnotations {
        scene_id,
        ref annotations,
      } => self.change_with(GameLog::EditSceneAnnotations {
        scene_id,
        annotations: annotations.clone(),
      }),
      StartCombat(scene, cids) => self.start_combat(scene, cids),
      StopCombat => self.change_with(GameLog::StopCombat),
      AddCreatureToCombat(cid) => self.add_creature_to_combat(cid),
      RemoveCreatureFromCombat(cid) => self.change_with(GameLog::RemoveCreatureFromCombat(cid)),
      RerollCombatInitiative => self.change().apply_combat(|c| c.reroll_initiative()),
      ChangeCreatureInitiative(cid, new_pos) => self.change_with(GameLog::CombatLog(
        CombatLog::ChangeCreatureInitiative(cid, new_pos),
      )),
      ForceNextTurn => self.change_with(GameLog::CombatLog(CombatLog::ForceNextTurn)),
      ForcePrevTurn => self.change_with(GameLog::CombatLog(CombatLog::ForcePrevTurn)),
      Done => self.next_turn(),

      // These are handled by the app before being passed to the Game:
      Rollback(..) => bug("Game Rollback"),
    }?;
    Ok(change)
  }

  fn start_combat(
    &self, scene_id: SceneID, cids: Vec<CreatureID>
  ) -> Result<ChangedGame, GameError> {
    let cids_with_inits = Combat::roll_initiative(self, cids)?;
    self.change_with(GameLog::StartCombat(scene_id, cids_with_inits))
  }

  fn add_creature_to_combat(&self, cid: CreatureID) -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(cid)?;
    let init = creature.creature.initiative.roll().1 as i16;
    self.change_with(GameLog::AddCreatureToCombat(cid, init))
  }

  fn attribute_check(
    &self, cid: CreatureID, check: &AttributeCheck
  ) -> Result<ChangedGame, GameError> {
    let creature = self.get_creature(cid)?;
    let (rolled, success) = creature.creature.attribute_check(check)?;
    self.change_with(GameLog::AttributeCheckResult(
      cid,
      check.clone(),
      rolled,
      success,
    ))
  }

  pub fn path_creature(
    &self, scene: SceneID, cid: CreatureID, pt: Point3
  ) -> Result<(ChangedGame, u32units::Length), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(
    &self, scene_id: SceneID, cid: CreatureID, pt: Point3, max_distance: u32units::Length
  ) -> Result<(ChangedGame, u32units::Length), GameError> {
    let scene = self.get_scene(scene_id)?;
    let creature = self.get_creature(cid)?;
    let (pts, distance) = self
      .tile_system
      .find_path(
        scene.get_pos(cid)?,
        max_distance,
        &scene.terrain,
        Volume::AABB(creature.creature.size),
        pt,
      )
      .ok_or(GameError::NoPathFound)?;
    debug_assert!(distance <= max_distance);

    let change = self.change_with(GameLog::PathCreature(scene_id, cid, pts))?;
    Ok((change, distance))
  }

  fn next_turn(&self) -> Result<ChangedGame, GameError> {
    let change = self.change().apply_combat(|c| c.next_turn())?;
    change.apply_creature(
      self.current_combat.as_ref().unwrap().current_creature_id(),
      |c| c.tick(),
    )
  }

  fn link_folder_item(
    &mut self, path: &FolderPath, item_id: &FolderItemID
  ) -> Result<(), GameError> {
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => node.creatures.insert(cid),
      FolderItemID::SceneID(sid) => node.scenes.insert(sid),
      FolderItemID::ItemID(iid) => node.items.insert(iid),
      FolderItemID::AbilityID(abid) => node.abilities.insert(abid),
      FolderItemID::ClassID(classid) => node.classes.insert(classid),
      FolderItemID::SubfolderID(_) => bail!("Cannot link folders."),
      FolderItemID::NoteID(ref nid) => bail!(GameError::CannotLinkNotes(path.clone(), nid.clone())),
    };
    Ok(())
  }

  fn unlink_folder_item(
    &mut self, path: &FolderPath, item_id: &FolderItemID
  ) -> Result<(), GameError> {
    fn remove_set<T: ::std::hash::Hash + Eq>(
      path: &FolderPath, item: &FolderItemID, s: &mut ::std::collections::HashSet<T>, key: &T
    ) -> Result<(), GameError> {
      if !s.remove(key) {
        bail!(GameError::FolderItemNotFound(path.clone(), item.clone()))
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
      FolderItemID::SubfolderID(_) => bail!("Cannot unlink folders."),
      FolderItemID::NoteID(ref nid) => bail!(GameError::CannotLinkNotes(path.clone(), nid.clone())),
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
    F: FnOnce(&mut Inventory) -> (),
  {
    let opt = match owner_id {
      InventoryOwner::Scene(sid) => self.scenes.mutate(&sid, |mut s| {
        f(&mut s.inventory);
        s
      }),
      InventoryOwner::Creature(cid) => self.creatures.mutate(&cid, |mut c| {
        f(&mut c.inventory);
        c
      }),
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
    &mut self, owner: InventoryOwner, item_id: ItemID, count: u64
  ) -> Result<u64, GameError> {
    let actually_has = *self.get_owner_inventory(owner)?.get(&item_id).unwrap_or(&0);
    self.set_item_count(owner, item_id, actually_has - count)?;
    Ok(cmp::min(actually_has, count))
  }

  fn set_item_count(
    &mut self, owner: InventoryOwner, item_id: ItemID, count: u64
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
  #[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
  fn apply_log_mut(&mut self, log: &GameLog) -> Result<(), GameError> {
    use self::GameLog::*;
    match *log {
      LoadModule {
        ref module,
        ref path,
      } => {
        if self.campaign.get(path).is_ok() {
          bail!(GameError::FolderAlreadyExists(path.clone()))
        } else {
          self.import_module(path, module)?;
        }
      }

      SetActiveScene(m_sid) => self.active_scene = m_sid,

      // Player stuff
      RegisterPlayer(ref pid) => if self.players.contains_key(pid) {
        bail!(GameError::PlayerAlreadyExists(pid.clone()))
      } else {
        self.players.insert(Player::new(pid.clone()));
      },

      UnregisterPlayer(ref pid) => {
        self
          .players
          .remove(pid)
          .ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
      }

      GiveCreaturesToPlayer(ref pid, ref cids) => {
        for cid in cids {
          self.check_creature_id(*cid)?;
        }
        self
          .players
          .mutate(pid, |mut p| {
            p.creatures.extend(cids);
            p
          })
          .ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
      }

      RemoveCreaturesFromPlayer(ref pid, ref cids) => {
        self
          .players
          .mutate(pid, |mut p| {
            for cid in cids {
              p.creatures.remove(cid);
            }
            p
          })
          .ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
      }

      SetPlayerScene(ref pid, scene) => {
        self
          .players
          .mutate(pid, move |mut p| {
            p.scene = scene;
            p
          })
          .ok_or_else(|| GameError::PlayerNotFound(pid.clone()))?;
      }

      // purely informational
      ChatFromGM(..) | ChatFromPlayer(..) | AttributeCheckResult(..) => {}

      // purely informational
      CreateFolder(ref path) => self.campaign.make_folders(path, Folder::new()),
      RenameFolder(ref path, ref name) => self.campaign.rename_folder(path, name.clone())?,
      MoveFolderItem(ref src, ref item_id, ref dst) => match *item_id {
        FolderItemID::NoteID(ref name) => {
          let note = self
            .campaign
            .get_mut(src)?
            .notes
            .remove(name)
            .ok_or_else(|| GameError::NoteNotFound(src.clone(), name.clone()))?;
          self.campaign.get_mut(dst)?.notes.insert(note.clone());
        }
        FolderItemID::SubfolderID(ref name) => {
          self.campaign.move_folder(&src.child(name.clone()), dst)?;
        }
        _ => {
          self.unlink_folder_item(src, item_id)?;
          self.link_folder_item(dst, item_id)?;
        }
      },
      CopyFolderItem {
        ref item_id,
        ref dest,
        ref new_item_id,
        ..
      } => match (item_id, new_item_id) {
        (&FolderItemID::CreatureID(id), &FolderItemID::CreatureID(new_id)) => {
          let mut new_creature = self.get_creature(id)?.creature.clone();
          new_creature.id = new_id;
          self.apply_log_mut(&CreateCreature(dest.clone(), new_creature))?;
        }
        (&FolderItemID::CreatureID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::SceneID(id), &FolderItemID::SceneID(new_id)) => {
          let mut new_scene = self.get_scene(id)?.clone();
          new_scene.id = new_id;
          self.apply_log_mut(&CreateScene(dest.clone(), new_scene))?;
        }
        (&FolderItemID::SceneID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::ItemID(id), &FolderItemID::ItemID(new_id)) => {
          let mut new_item = self.get_item(id)?.clone();
          new_item.id = new_id;
          self.apply_log_mut(&CreateItem(dest.clone(), new_item))?;
        }
        (&FolderItemID::ItemID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::AbilityID(id), &FolderItemID::AbilityID(new_id)) => {
          let mut new_ability = self
            .abilities
            .get(&id)
            .ok_or_else(|| GameError::NoAbility(id))?
            .clone();
          new_ability.id = new_id;
          self
            .abilities
            .try_insert(new_ability)
            .ok_or_else(|| GameError::AbilityAlreadyExists(new_id))?;
          self.link_folder_item(dest, &FolderItemID::AbilityID(new_id))?;
        }
        (&FolderItemID::AbilityID(_), _) => panic!("Mismatched folder item ID!"),
        (&FolderItemID::ClassID(id), &FolderItemID::ClassID(new_id)) => {
          let mut new_class = self
            .classes
            .get(&id)
            .ok_or_else(|| GameError::ClassNotFound(id))?
            .clone();
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
      DeleteFolderItem(ref path, ref item_id) => {
        // because we're being paranoid, we're walking ALL folder paths and checking if the given
        // item ID is found in ANY of them and cleaning it up.
        let all_folders: Vec<FolderPath> = self
          .campaign
          .walk_paths(&FolderPath::from_vec(vec![]))
          .cloned()
          .collect();
        match *item_id {
          FolderItemID::NoteID(ref name) => {
            self.campaign.get_mut(path)?.notes.remove(name);
          }
          FolderItemID::ItemID(iid) => {
            for folder in all_folders {
              let node = self.campaign.get_mut(&folder)?;
              node.items.remove(&iid);
            }
            // Also delete the item from all creature inventory slots
            let cids: Vec<CreatureID> = self.creatures.keys().cloned().collect();
            for cid in cids {
              self
                .creatures
                .mutate(&cid, |mut c| {
                  c.inventory.remove(&iid);
                  c
                })
                .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
            }
            // Also delete the item from all scene inventory slots
            let sids: Vec<SceneID> = self.scenes.keys().cloned().collect();
            for sid in sids {
              self
                .scenes
                .mutate(&sid, |mut s| {
                  s.inventory.remove(&iid);
                  s
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
              .filter_map(|s| {
                if s.creatures.contains_key(&cid) {
                  Some(s.id)
                } else {
                  None
                }
              })
              .collect();
            for sid in scenes_with_this_creature {
              self.scenes.mutate(&sid, |mut sc| {
                sc.creatures.remove(&cid);
                sc
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
                bail!(GameError::SceneInUse(sid));
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
                .mutate(&class_id, |mut class| {
                  class.abilities.retain(|el| *el == abid);
                  class
                })
                .expect("iterating classes...");
            }
            for cid in self.creatures.keys().cloned().collect::<Vec<CreatureID>>() {
              self
                .creatures
                .mutate(&cid, |mut creature| {
                  creature.abilities.remove(&abid);
                  creature
                })
                .expect("Must exist");
            }
            self.abilities.remove(&abid);
          }
          FolderItemID::ClassID(classid) => {
            for cid in self.creatures.keys().cloned().collect::<Vec<CreatureID>>() {
              if self.get_creature(cid)?.creature.class == classid {
                bail!("Class in use!");
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
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::SubfolderID(child_folder.clone()),
              ))?;
            }
            let node = self.campaign.get(&path)?.clone();
            for scene_id in node.scenes {
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::SceneID(scene_id),
              ))?;
            }
            for cid in node.creatures {
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::CreatureID(cid),
              ))?;
            }
            for iid in node.items {
              self.apply_log_mut(&DeleteFolderItem(path.clone(), FolderItemID::ItemID(iid)))?;
            }
            for abid in node.abilities {
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::AbilityID(abid),
              ))?;
            }
            for classid in node.classes {
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::ClassID(classid),
              ))?;
            }
            for nname in node.notes.keys() {
              self.apply_log_mut(&DeleteFolderItem(
                path.clone(),
                FolderItemID::NoteID(nname.clone()),
              ))?;
            }
            self.campaign.remove(&path)?;
          }
        }
      }

      CreateItem(ref path, ref ritem) => {
        let item = ritem.clone();
        self
          .items
          .try_insert(item)
          .ok_or_else(|| GameError::ItemAlreadyExists(ritem.id))?;
        self.link_folder_item(path, &FolderItemID::ItemID(ritem.id))?;
      }
      EditItem(ref item) => {
        self
          .items
          .mutate(&item.id, move |_| item.clone())
          .ok_or_else(|| GameError::ItemNotFound(item.id))?;
      }

      CreateNote(ref path, ref note) => {
        self.campaign.get_mut(path)?.notes.insert(note.clone());
      }
      EditNote(ref path, ref name, ref new_note) => {
        let node = self.campaign.get_mut(path)?;
        node
          .notes
          .mutate(name, move |_| new_note.clone())
          .ok_or_else(|| GameError::NoteNotFound(path.clone(), name.to_string()))?;
      }

      // ** Inventory Management **
      TransferItem {
        from,
        to,
        item_id,
        count,
      } => {
        // I love rust! This code is guaranteed to run atomically because we have a &mut,
        // aka "exclusive borrow". Also we can return errors even if we've already mutated,
        // because apply_log creates a copy of the Game before mutating it.
        let to_give = self.remove_inventory(from, item_id, count)?;
        self.mutate_owner_inventory(to, |to_inv| {
          let recip_has = *to_inv.get(&item_id).unwrap_or(&0);
          to_inv.insert(item_id, to_give + recip_has);
        })?;
      }
      RemoveItem {
        owner,
        item_id,
        count,
      } => {
        self.remove_inventory(owner, item_id, count)?;
      }
      SetItemCount {
        owner,
        item_id,
        count,
      } => {
        self.set_item_count(owner, item_id, count)?;
      }

      CreateScene(ref path, ref rscene) => {
        let scene = rscene.clone();
        self
          .scenes
          .try_insert(scene)
          .ok_or_else(|| GameError::SceneAlreadyExists(rscene.id))?;
        self.link_folder_item(path, &FolderItemID::SceneID(rscene.id))?;
      }
      EditScene(ref scene) => {
        self
          .scenes
          .mutate(&scene.id, move |_| scene.clone())
          .ok_or_else(|| GameError::SceneNotFound(scene.id))?;
      }
      EditSceneDetails {
        scene_id,
        ref details,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.name = details.name.clone();
            scene.background_image_url = details.background_image_url.clone();
            scene.background_image_offset = details.background_image_offset;
            scene.background_image_scale = details.background_image_scale;
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      SetSceneCreatureVisibility {
        scene_id,
        creature_id,
        ref visibility,
      } => {
        if !self
          .get_scene(scene_id)?
          .creatures
          .contains_key(&creature_id)
        {
          bail!(GameError::CreatureNotFound(creature_id.to_string()));
        }
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            {
              let entry = scene.creatures.get_mut(&creature_id);
              let entry = entry.expect("Already checked that creature exists?!");
              entry.1 = visibility.clone();
            }
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      AddCreatureToScene {
        scene_id,
        creature_id,
        ref visibility,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene
              .creatures
              .insert(creature_id, (Point3::new(0, 0, 0), visibility.clone()));
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      RemoveCreatureFromScene {
        scene_id,
        creature_id,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.creatures.remove(&creature_id);
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      AddSceneChallenge {
        scene_id,
        ref description,
        ref challenge,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene
              .attribute_checks
              .insert(description.clone(), challenge.clone());
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      RemoveSceneChallenge {
        scene_id,
        ref description,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.attribute_checks.remove(description);
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      SetFocusedSceneCreatures {
        scene_id,
        ref creatures,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.focused_creatures = creatures.clone();
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      RemoveSceneVolumeCondition {
        scene_id,
        condition_id,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.volume_conditions.remove(&condition_id);
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }

      EditSceneTerrain {
        scene_id,
        ref terrain,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.terrain = terrain.clone();
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneHighlights {
        scene_id,
        ref highlights,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.highlights = highlights.clone();
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      EditSceneAnnotations {
        scene_id,
        ref annotations,
      } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.annotations = annotations.clone();
            scene
          })
          .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
      }
      CreateCreature(ref path, ref rc) => {
        let c = rc.clone();
        self
          .creatures
          .try_insert(c)
          .ok_or_else(|| GameError::CreatureAlreadyExists(rc.id()))?;
        self.link_folder_item(path, &FolderItemID::CreatureID(rc.id()))?;
      }
      EditCreatureDetails {
        creature_id,
        ref details,
      } => {
        let mutated = self.creatures.mutate(&creature_id, move |mut c| {
          c.name = details.name.clone();
          c.class = details.class;
          c.portrait_url = details.portrait_url.clone();
          c.icon_url = details.icon_url.clone();
          c.note = details.note.clone();
          c.bio = details.bio.clone();
          c.initiative = details.initiative.clone();
          c.size = details.size;
          c
        });
        mutated.ok_or_else(|| GameError::CreatureNotFound(creature_id.to_string()))?;
      }
      AddCreatureToCombat(cid, init) => {
        let mut combat = self.current_combat.clone().ok_or(GameError::NotInCombat)?;
        self.check_creature_id(cid)?;
        if combat.creatures.iter().any(|&(c, _)| c == cid) {
          bail!(GameError::AlreadyInCombat(cid));
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
        self.current_combat.take().ok_or(GameError::NotInCombat)?;
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

      AddVolumeCondition {
        ref scene_id,
        point,
        volume,
        condition_id,
        ref condition,
        duration,
      } => {
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
      Rollback(..) => {
        return bug("GameLog Rollback");
      }
    }
    Ok(())
  }

  pub fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError> {
    if self.creatures.contains_key(&cid) {
      Ok(())
    } else {
      Err(GameError::CreatureNotFound(cid.to_string()).into())
    }
  }

  fn check_scene(&self, scene: SceneID) -> Result<(), GameError> {
    if self.scenes.contains_key(&scene) {
      Ok(())
    } else {
      Err(GameError::SceneNotFound(scene).into())
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
      .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?)
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>(
    &'game self, creature: &'creature Creature
  ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, id: SceneID) -> Result<&Scene, GameError> {
    self
      .scenes
      .get(&id)
      .ok_or_else(|| GameError::SceneNotFound(id).into())
  }

  pub fn get_combat(&self) -> Result<DynamicCombat, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameError::NotInCombat)?;
    let scene = self.get_scene(combat.scene)?;
    Ok(DynamicCombat {
      scene: scene,
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

  fn ooc_act(
    &self, scene: SceneID, cid: CreatureID, abid: AbilityID, target: DecidedTarget
  ) -> Result<ChangedGame, GameError> {
    let scene = self.get_scene(scene)?;
    self._act(scene, cid, abid, target, false)
  }

  fn _act(
    &self, scene: &Scene, cid: CreatureID, abid: AbilityID, target: DecidedTarget, in_combat: bool
  ) -> Result<ChangedGame, GameError> {
    if !scene.creatures.contains_key(&cid) {
      bail!(GameError::CreatureNotFound(cid.to_string()));
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
        Err(GameError::CreatureLacksAbility(creature.id(), abid).into())
      }
    } else {
      Err(GameError::CannotAct(creature.id()).into())
    }
  }

  pub fn creature_act(
    &self, creature: &DynamicCreature, scene: &Scene, ability: &Ability, target: DecidedTarget,
    mut change: ChangedGame, in_combat: bool,
  ) -> Result<ChangedGame, GameError> {
    let mut change = match ability.action {
      Action::Creature {
        ref effect,
        target: tspec,
      } => {
        let targets = self.resolve_creature_targets(creature, scene, tspec, target)?;
        for creature_id in &targets {
          change = change.apply_creature(*creature_id, |c| c.apply_effect(effect))?;
        }
        change
      }
      Action::SceneVolume {
        ref effect,
        target: tspec,
      } => {
        match (effect, tspec, target) {
          (
            &SceneEffect::CreateVolumeCondition {
              duration,
              ref condition,
            },
            SceneTarget::RangedVolume { volume, .. },
            // TODO: unimplemented!: honor and check `range`
            DecidedTarget::Point(point),
          ) => {
            let log = GameLog::AddVolumeCondition {
              condition_id: ConditionID::gen(),
              scene_id: scene.id,
              point,
              volume: volume,
              condition: condition.clone(),
              duration,
            };
            change = change.apply(&log)?;
          }
          _ => bail!(GameError::BuggyProgram("Ugh".to_string())),
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
          Err(GameError::CreatureOutOfRange(cid).into())
        }
      }
      (CreatureTarget::Range(max), DecidedTarget::Creature(cid)) => if self
        .tile_system
        .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, max)
      {
        Ok(vec![cid])
      } else {
        Err(GameError::CreatureOutOfRange(cid).into())
      },
      (CreatureTarget::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (_, DecidedTarget::Point(pt)) => {
        self.volume_creature_targets(scene, creature.creature.id, target, pt)
      }
      (spec, decided) => Err(GameError::InvalidTargetForTargetSpec(spec, decided).into()),
    }
  }

  // TODO: unimplemented! Honor terrain!
  // 1. `pt` must be visible to the caster
  // 2. volumes must not go through blocked terrain
  // 3. volumes must (generally) not go around corners
  fn volume_creature_targets(
    &self, scene: &Scene, actor_id: CreatureID, target: CreatureTarget, pt: Point3
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
      _ => bail!(GameError::InvalidTargetForTargetSpec(
        target,
        DecidedTarget::Point(pt)
      )),
    }
  }

  /// Calculate which *points* and which *creatures* will be affected by an ability targeted at a
  /// point.
  pub fn preview_volume_targets(
    &self, scene: &Scene, actor_id: CreatureID, ability_id: AbilityID, pt: Point3
  ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
    let ability = self.get_ability(ability_id)?;

    let cids = match ability.action {
      Action::Creature { target, .. } => self.volume_creature_targets(scene, actor_id, target, pt)?,
      Action::SceneVolume {
        target: SceneTarget::RangedVolume { volume, .. },
        ..
      } => scene.creatures_in_volume(self.tile_system, pt, volume),
    };
    let tiles = match ability.action {
      Action::Creature {
        target: CreatureTarget::AllCreaturesInVolumeInRange { volume, .. },
        ..
      }
      | Action::SceneVolume {
        target: SceneTarget::RangedVolume { volume, .. },
        ..
      } => {
        // TODO: unimplemented! honor and check `range`
        scene.open_terrain_in_volume(self, pt, volume)?
      }
      Action::Creature {
        target: CreatureTarget::LineFromActor { distance },
        ..
      } => {
        let actor_pos = scene.get_pos(actor_id)?;
        let volume = line_through_point(actor_pos, pt, distance);
        scene.open_terrain_in_volume(self, actor_pos, volume)?
      }
      _ => vec![],
    };
    Ok((cids, tiles))
  }

  pub fn get_movement_options(
    &self, scene: SceneID, creature_id: CreatureID
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
      Err(GameError::CannotAct(creature.id()).into())
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(
    &self, scene: SceneID, creature_id: CreatureID, ability_id: AbilityID
  ) -> Result<PotentialTargets, GameError> {
    let ability = self.get_ability(ability_id)?;

    use types::Action as A;
    use types::CreatureTarget as CT;
    Ok(match ability.action {
      A::Creature {
        target: CT::Melee, ..
      } => self.creatures_in_range(scene, creature_id, MELEE_RANGE)?,
      A::Creature {
        target: CT::Range(distance),
        ..
      } => self.creatures_in_range(scene, creature_id, distance)?,
      A::Creature {
        target: CT::Actor, ..
      } => PotentialTargets::CreatureIDs(vec![creature_id]),
      A::Creature {
        target: CT::AllCreaturesInVolumeInRange { range, .. },
        ..
      }
      | A::SceneVolume {
        target: SceneTarget::RangedVolume { range, .. },
        ..
      } => self.open_terrain_in_range(scene, creature_id, range)?,
      A::Creature {
        target: CT::LineFromActor { distance },
        ..
      } => self.open_terrain_in_range(scene, creature_id, distance)?,
      A::Creature {
        target: CT::SomeCreaturesInVolumeInRange { .. },
        ..
      } => unimplemented!("SomeCreaturesInVolumeInRange not implemented"),
    })
  }

  fn open_terrain_in_range(
    &self, scene: SceneID, creature_id: CreatureID, range: u32units::Length
  ) -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let creature_pos = scene.get_pos(creature_id)?;
    let pts = self
      .tile_system
      .open_points_in_range(creature_pos, &scene.terrain, range);
    Ok(PotentialTargets::Points(pts))
  }

  fn creatures_in_range(
    &self, scene: SceneID, creature_id: CreatureID, distance: u32units::Length
  ) -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let my_pos = scene.get_pos(creature_id)?;
    let mut results = vec![];
    for (creature_id, &(creature_pos, _)) in &scene.creatures {
      if self
        .tile_system
        .points_within_distance(my_pos, creature_pos, distance)
      {
        results.push(*creature_id);
      }
    }
    Ok(PotentialTargets::CreatureIDs(results))
  }

  // ** END CONSIDERATION **

  pub fn get_class(&self, class: ClassID) -> Result<&Class, GameError> {
    self
      .classes
      .get(&class)
      .ok_or_else(|| GameError::ClassNotFound(class).into())
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
    new.game.creatures.mutate(&cid, |_| creature);
    new.logs.extend(creature_logs_into_game_logs(cid, logs));
    Ok(new)
  }

  pub fn done(self) -> (Game, Vec<GameLog>) { (self.game, self.logs) }
}

fn bug<T>(msg: &str) -> Result<T, GameError> {
  Err(GameError::BuggyProgram(msg.to_string()).into())
}

pub fn load_app_from_path(parent: &Path, filename: &str) -> Result<App, GameError> {
  let filename = parent.join(filename);
  let mut appf = File::open(filename)?;
  let mut apps = String::new();
  appf.read_to_string(&mut apps).unwrap();
  let app: App = serde_yaml::from_str(&apps)?;
  app.current_game.validate_campaign()?;
  Ok(app)
}

#[cfg(test)]
pub mod test {
  use std::collections::HashSet;
  use std::iter::FromIterator;

  use combat::test::*;
  use game::*;
  use indexed::IndexedHashMap;
  use types::test::*;

  pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    t_perform(game, GameCommand::StartCombat(t_scene_id(), combatants))
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    t_perform(game, GameCommand::CombatAct(ability_id, target))
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
    game
  }

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

  pub fn perf(game: &Game, cmd: GameCommand) -> Result<ChangedGame, GameError> {
    game.perform_command(cmd, PathBuf::from(""))
  }

  pub fn t_perform(game: &Game, cmd: GameCommand) -> Game { perf(game, cmd).unwrap().game }

  #[test]
  fn start_combat_not_found() {
    let game = t_game();
    let non = CreatureID::gen();
    let result = game.perform_command(
      GameCommand::StartCombat(t_scene_id(), vec![non]),
      PathBuf::from(""),
    );
    match result {
      Err(GameError::CreatureNotFound(id)) => assert_eq!(id, non.to_string()),
      x => panic!("Unexpected result: {:?}", x),
    }
  }

  #[test]
  fn combat_must_have_creatures() {
    let game = t_game();
    let result = game.perform_command(
      GameCommand::StartCombat(t_scene_id(), vec![]),
      PathBuf::from(""),
    );
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
      GameCommand::CombatAct(abid_punch(), DecidedTarget::Creature(cid_ranger())),
    );
    assert_eq!(
      game
        .get_creature(cid_ranger())
        .unwrap()
        .creature
        .cur_health(),
      HP(7)
    );
    let game = t_perform(&game, GameCommand::StopCombat);
    assert_eq!(
      game
        .get_creature(cid_ranger())
        .unwrap()
        .creature
        .cur_health(),
      HP(7)
    );
  }

  #[test]
  fn movement() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    t_perform(
      &game,
      GameCommand::PathCurrentCombatCreature(Point3::new(1, 0, 0)),
    );
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
      GameCommand::ChangeCreatureInitiative(cid_ranger(), 30),
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
      GameCommand::StartCombat(t_scene_id(), vec![cid_rogue(), cid_ranger(), cid_cleric()]),
    );
    let iter = |game: &Game| -> Result<Game, GameError> {
      let game = t_game_act(game, abid_punch(), DecidedTarget::Creature(cid_ranger()));
      let game = t_perform(&game, GameCommand::Done);
      let game = t_perform(&game, GameCommand::Done);
      let game = t_game_act(&game, abid_heal(), DecidedTarget::Creature(cid_ranger()));
      let game = t_perform(&game, GameCommand::Done);
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
      GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), Point3::new(11, 0, 0)),
    );
    let game = t_perform(
      &game,
      GameCommand::ActCreature(
        t_scene_id(),
        cid_cleric(),
        abid_fireball(),
        DecidedTarget::Point(Point3::new(0, 0, 0)),
      ),
    );
    assert_eq!(
      game.get_creature(cid_rogue()).unwrap().creature.cur_health,
      HP(7)
    );
    assert_eq!(
      game.get_creature(cid_ranger()).unwrap().creature.cur_health,
      HP(7)
    );
    assert_eq!(
      game.get_creature(cid_cleric()).unwrap().creature.cur_health,
      HP(10)
    );
  }

  #[test]
  fn test_creatures_in_sphere() {
    let game = t_game();
    let volume = Volume::Sphere(cm(200));
    let pt = Point3::new(5, 0, 0);

    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(5, 0, 0)),
    );
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), Point3::new(6, 0, 0)),
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
      range: cm(1000),
      volume: Volume::Sphere(cm(200)),
    };
    let pt = Point3::new(5, 0, 0);

    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(5, 0, 0)),
    );
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), Point3::new(6, 0, 0)),
    );

    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game
      .volume_creature_targets(scene, cid_ranger(), target_spec, pt)
      .unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn test_line_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::LineFromActor {
      distance: cm(1000),
    };
    let pt = Point3::new(1, 0, 0);

    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), Point3::new(1, 0, 0)),
    );
    let game = t_perform(
      &game,
      GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), Point3::new(2, 0, 0)),
    );
    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game
      .volume_creature_targets(scene, cid_ranger(), target_spec, pt)
      .unwrap();
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
    let preview = game
      .preview_volume_targets(scene, cleric, ability_id, Point3::new(0, 0, 0))
      .unwrap();
    let expected = hashset!{cid_cleric(), cid_ranger(), cid_rogue()};
    assert_eq!(HashSet::from_iter(preview.0), expected);
  }

  #[test]
  fn test_export_module() {
    let root_path = FolderPath::from_vec(vec![]);
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let note = Note {
      name: "My Note".to_string(),
      content: "My Content".to_string(),
    };
    let mut folder = Folder::new();
    folder.notes.insert(note.clone());

    let mut game = t_game();
    game
      .campaign
      .make_folder(&root_path, "Rules".to_string(), folder)
      .expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    let new_note = new_game
      .campaign
      .get(&root_path)
      .unwrap()
      .notes
      .get("My Note");
    assert_eq!(new_note.expect("My Note wasn't at root"), &note);
  }

  #[test]
  fn test_export_module_references() {
    let root_path = FolderPath::from_vec(vec![]);
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let mut folder = Folder::new();
    folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game
      .campaign
      .make_folder(&root_path, "Rules".to_string(), folder)
      .expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
      new_game
        .get_class(classid_ranger())
        .expect("new game didn't have ranger class"),
      game
        .get_class(classid_ranger())
        .expect("Old game didn't have ranger class")
    );
  }

  #[test]
  fn test_export_subfolders() {
    let root_path = "".parse().unwrap();
    let rules_path = "/Rules".parse().unwrap();
    let root = Folder::new();
    let mut classes_folder = Folder::new();
    classes_folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game
      .campaign
      .make_folder(&root_path, "Rules".to_string(), root)
      .unwrap();
    game
      .campaign
      .make_folder(&rules_path, "Classes".to_string(), classes_folder)
      .unwrap();
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
      new_game
        .get_class(classid_ranger())
        .expect("new game didn't have ranger class"),
      game
        .get_class(classid_ranger())
        .expect("Old game didn't have ranger class")
    );

    let new_classes = &new_game
      .campaign
      .get(&"/Classes".parse().unwrap())
      .unwrap()
      .classes;
    let old_classes = &game
      .campaign
      .get(&"/Rules/Classes".parse().unwrap())
      .unwrap()
      .classes;
    assert_eq!(new_classes, old_classes);
  }
}
