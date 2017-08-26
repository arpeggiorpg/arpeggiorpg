use std::collections::HashMap;
use std::cmp;

use indexed::IndexedHashMap;
use types::*;
use combat::*;
use creature::ChangedCreature;
use foldertree::{FolderPath, FolderTree};
use grid::line_through_point;

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
      items: IndexedHashMap::new(),
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
      CopyFolderItem { source, item_id, dest } => {
        let new_item_id = match &item_id {
          &FolderItemID::CreatureID(_) => FolderItemID::CreatureID(CreatureID::new()),
          &FolderItemID::NoteID(_) => item_id.clone(),
          &FolderItemID::SceneID(_) => FolderItemID::SceneID(SceneID::new()),
          &FolderItemID::MapID(_) => FolderItemID::MapID(MapID::new()),
          &FolderItemID::ItemID(_) => FolderItemID::ItemID(ItemID::new()),
          &FolderItemID::SubfolderID(_) => item_id.clone(),
        };
        self.change_with(GameLog::CopyFolderItem { source, item_id, dest, new_item_id })
      }
      DeleteFolderItem(path, item_id) => self.change_with(GameLog::DeleteFolderItem(path, item_id)),

      CreateItem(path, name) => {
        let item = Item { id: ItemID::new(), name };
        self.change_with(GameLog::CreateItem(path, item))
      }
      EditItem(item) => self.change_with(GameLog::EditItem(item)),

      CreateNote(path, note) => self.change_with(GameLog::CreateNote(path, note)),
      EditNote(path, orig, new) => self.change_with(GameLog::EditNote(path, orig, new)),

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

      CreateScene(path, sc) => {
        let scene = Scene::create(sc);
        self.change_with(GameLog::CreateScene(path, scene))
      }
      EditSceneDetails { scene_id, details } => {
        self.change_with(GameLog::EditSceneDetails { scene_id, details })
      }
      SetSceneCreatureVisibility { scene_id, creature_id, visibility } => {
        self.change_with(GameLog::SetSceneCreatureVisibility {
          scene_id,
          creature_id,
          visibility: visibility.clone(),
        })
      }
      AddCreatureToScene { scene_id, creature_id, ref visibility } => self.change_with(
        GameLog::AddCreatureToScene { scene_id, creature_id, visibility: visibility.clone() },
      ),
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

      CreateCreature(path, spec) => {
        let creature = Creature::create(&spec);
        self.change_with(GameLog::CreateCreature(path, creature))
      }
      EditCreatureDetails { creature_id, details } => {
        self.change_with(GameLog::EditCreatureDetails { creature_id, details })
      }
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
      EditMapDetails { id, ref details } => {
        self.change_with(GameLog::EditMapDetails { id, details: details.clone() })
      }
      EditMapTerrain { id, ref terrain, ref specials } => self.change_with(
        GameLog::EditMapTerrain { id, terrain: terrain.clone(), specials: specials.clone() },
      ),
      StartCombat(scene, cids) => self.start_combat(scene, cids),
      StopCombat => self.change_with(GameLog::StopCombat),
      AddCreatureToCombat(cid) => self.add_creature_to_combat(cid),
      RemoveCreatureFromCombat(cid) => self.change_with(GameLog::RemoveCreatureFromCombat(cid)),
      RerollCombatInitiative => self.change().apply_combat(|c| c.reroll_initiative()),
      ChangeCreatureInitiative(cid, new_pos) => {
        self.change_with(GameLog::CombatLog(CombatLog::ChangeCreatureInitiative(cid, new_pos)))
      }
      ForceNextTurn => self.change_with(GameLog::CombatLog(CombatLog::ForceNextTurn)),
      ForcePrevTurn => self.change_with(GameLog::CombatLog(CombatLog::ForcePrevTurn)),
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
    self.change_with(GameLog::AttributeCheckResult(cid, check.clone(), rolled, success))
  }

  pub fn path_creature(
    &self, scene: SceneID, cid: CreatureID, pt: Point3
  ) -> Result<(ChangedGame, Distance), GameError> {
    let creature = self.get_creature(cid)?;
    self.path_creature_distance(scene, cid, pt, creature.speed())
  }

  pub fn path_creature_distance(
    &self, scene_id: SceneID, cid: CreatureID, pt: Point3, max_distance: Distance
  ) -> Result<(ChangedGame, Distance), GameError> {
    let scene = self.get_scene(scene_id)?;
    let terrain = self.get_map(scene.map)?;
    let creature = self.get_creature(cid)?;
    let (pts, distance) = self
      .tile_system
      .find_path(
        scene.get_pos(cid)?,
        max_distance,
        terrain,
        Volume::AABB(creature.creature.size),
        pt,
      )
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

  fn link_folder_item(
    &mut self, path: &FolderPath, item_id: &FolderItemID
  ) -> Result<(), GameError> {
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => node.creatures.insert(cid),
      FolderItemID::SceneID(sid) => node.scenes.insert(sid),
      FolderItemID::MapID(mid) => node.maps.insert(mid),
      FolderItemID::ItemID(iid) => node.items.insert(iid),
      FolderItemID::SubfolderID(_) => bail!("Cannot link folders."),
      FolderItemID::NoteID(ref nid) => {
        bail!(GameErrorEnum::CannotLinkNotes(path.clone(), nid.clone()))
      }
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
        bail!(GameErrorEnum::FolderItemNotFound(path.clone(), item.clone()))
      }
      Ok(())
    }
    let node = self.campaign.get_mut(path)?;
    match *item_id {
      FolderItemID::CreatureID(cid) => remove_set(path, item_id, &mut node.creatures, &cid)?,
      FolderItemID::SceneID(sid) => remove_set(path, item_id, &mut node.scenes, &sid)?,
      FolderItemID::MapID(mid) => remove_set(path, item_id, &mut node.maps, &mid)?,
      FolderItemID::ItemID(iid) => remove_set(path, item_id, &mut node.items, &iid)?,
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

  fn mutate_owner_inventory<F>(&mut self, owner_id: InventoryOwner, f: F) -> Result<(), GameError>
  where
    F: FnOnce(&mut Inventory) -> (),
  {
    let opt = match owner_id {
      InventoryOwner::Scene(sid) => self.scenes.mutate(&sid, |mut s| {
        f(&mut s.inventory);
        return s;
      }),
      InventoryOwner::Creature(cid) => self.creatures.mutate(&cid, |mut c| {
        f(&mut c.inventory);
        return c;
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
    return Ok(cmp::min(actually_has, count));
  }

  fn set_item_count(
    &mut self, owner: InventoryOwner, item_id: ItemID, count: u64
  ) -> Result<(), GameError> {
    self.mutate_owner_inventory(owner, move |mut inventory: &mut Inventory| if count <= 0 {
      inventory.remove(&item_id).unwrap_or(0);
    } else {
      inventory.insert(item_id, count);
    })
  }

  /// Apply a log to a *mutable* Game.
  // This is done so that we don't have to worry about `self` vs `newgame` -- all
  // manipulations here work on &mut self.
  fn apply_log_mut(&mut self, log: &GameLog) -> Result<(), GameError> {
    use self::GameLog::*;
    match *log {
      ChatFromGM(..) => {}
      ChatFromPlayer(..) => {}
      AttributeCheckResult(..) => {} // purely informational
      CreateFolder(ref path) => self.campaign.make_folders(path, Folder::new()),
      RenameFolder(ref path, ref name) => self.campaign.rename_folder(path, name.clone())?,
      MoveFolderItem(ref src, ref item_id, ref dst) => match *item_id {
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
      },
      CopyFolderItem { ref source, ref item_id, ref dest, ref new_item_id } => {
        match (item_id, new_item_id) {
          (&FolderItemID::CreatureID(id), &FolderItemID::CreatureID(new_id)) => {
            let mut new_creature = self.get_creature(id)?.creature.clone();
            new_creature.id = new_id;
            self.apply_log_mut(&CreateCreature(dest.clone(), new_creature))?;
          }
          (&FolderItemID::CreatureID(id), _) => panic!("Mismatched folder item ID!"),
          (&FolderItemID::SceneID(id), &FolderItemID::SceneID(new_id)) => {
            let mut new_scene = self.get_scene(id)?.clone();
            new_scene.id = new_id;
            self.apply_log_mut(&CreateScene(dest.clone(), new_scene))?;
          }
          (&FolderItemID::SceneID(id), _) => panic!("Mismatched folder item ID!"),
          (&FolderItemID::ItemID(id), &FolderItemID::ItemID(new_id)) => {
            let mut new_item =
              self.items.get(&id).ok_or_else(|| GameErrorEnum::ItemNotFound(id.clone()))?.clone();
            new_item.id = new_id;
            self.apply_log_mut(&CreateItem(dest.clone(), new_item))?;
          }
          (&FolderItemID::ItemID(id), _) => panic!("Mismatched folder item ID!"),
          (&FolderItemID::MapID(id), &FolderItemID::MapID(new_id)) => {
            let mut new_map = self.get_map(id)?.clone();
            new_map.id = new_id;
            self.apply_log_mut(&CreateMap(dest.clone(), new_map))?;
          }
          (&FolderItemID::MapID(id), _) => panic!("Mismatched folder item ID!"),
          (&FolderItemID::SubfolderID(_), _) => unimplemented!(),
          (&FolderItemID::NoteID(_), _) => panic!("Can't clone notes... yet?"),
        }
      }
      DeleteFolderItem(ref path, ref item_id) => {
        // because we're being paranoid, we're walking ALL folder paths and checking if the given
        // item ID is found in ANY of them and cleaning it up.
        let all_folders: Vec<FolderPath> =
          self.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
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
                .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?;
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
                .ok_or_else(|| GameErrorEnum::SceneNotFound(sid))?;
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
              .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?;
          }
          FolderItemID::SceneID(sid) => {
            // TODO: Figure out how to deal with players referencing this scene.
            // - disallow deleting if in combat
            if let Ok(combat) = self.get_combat() {
              if combat.scene.id == sid {
                bail!(GameErrorEnum::SceneInUse(sid));
              }
            }
            let all_folders: Vec<FolderPath> =
              self.campaign.walk_paths(FolderPath::from_vec(vec![])).cloned().collect();
            for path in all_folders {
              let node = self.campaign.get_mut(&path)?;
              node.scenes.remove(&sid);
            }
            self.scenes.remove(&sid);
          }
          FolderItemID::MapID(id) => {
            for path in all_folders {
              self.campaign.get_mut(&path)?.maps.remove(&id);
            }
            let scenes_using_this_map: Vec<SceneID> =
              self.scenes.values().filter(|s| s.map == id).map(|s| s.id).collect();
            if !scenes_using_this_map.is_empty() {
              bail!(GameErrorEnum::MapInUse(id, scenes_using_this_map));
            }
            self.maps.remove(&id).ok_or_else(|| GameErrorEnum::MapNotFound(id))?;
          }
          FolderItemID::SubfolderID(ref name) => {
            // basically we delete everything by simulating GameLog::DeleteFolderItem for each
            // child.
            // This must be done in a particular order to ensure scenes are deleted before maps
            // (since maps can't be deleted if there are scenes referring to them).
            let path = path.child(name.to_string());
            for child_folder in self.campaign.get_children(&path)?.clone() {
              self
                .apply_log_mut(
                  &DeleteFolderItem(path.clone(), FolderItemID::SubfolderID(child_folder.clone())),
                )?;
            }
            let node = self.campaign.get(&path)?.clone();
            for scene_id in node.scenes {
              self.apply_log_mut(&DeleteFolderItem(path.clone(), FolderItemID::SceneID(scene_id)))?;
            }
            for map_id in node.maps {
              self.apply_log_mut(&DeleteFolderItem(path.clone(), FolderItemID::MapID(map_id)))?;
            }
            for cid in node.creatures {
              self.apply_log_mut(&DeleteFolderItem(path.clone(), FolderItemID::CreatureID(cid)))?;
            }
            for iid in node.items {
              self.apply_log_mut(&DeleteFolderItem(path.clone(), FolderItemID::ItemID(iid)))?;
            }
            for nname in node.notes.keys() {
              self
                .apply_log_mut(&DeleteFolderItem(path.clone(),
                                                 FolderItemID::NoteID(nname.clone())))?;
            }
            self.campaign.remove(&path)?;
          }
        }
      }

      CreateItem(ref path, ref ritem) => {
        let item = ritem.clone();
        self.items.try_insert(item).ok_or_else(|| GameErrorEnum::ItemAlreadyExists(ritem.id))?;
        self.link_folder_item(path, &FolderItemID::ItemID(ritem.id))?;
      }
      EditItem(ref item) => {
        self
          .items
          .mutate(&item.id, move |_| item.clone())
          .ok_or_else(|| GameErrorEnum::ItemNotFound(item.id))?;
      }

      CreateNote(ref path, ref note) => {
        self.campaign.get_mut(path)?.notes.insert(note.clone());
      }
      EditNote(ref path, ref name, ref new_note) => {
        let node = self.campaign.get_mut(path)?;
        node
          .notes
          .mutate(name, move |_| new_note.clone())
          .ok_or_else(|| GameErrorEnum::NoteNotFound(path.clone(), name.to_string()))?;
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
      EditSceneDetails { scene_id, ref details } => {
        self.check_map(details.map)?;
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.name = details.name.clone();
            scene.map = details.map;
            scene.background_image_url = details.background_image_url.clone();
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }
      SetSceneCreatureVisibility { scene_id, creature_id, ref visibility } => {
        if !self.get_scene(scene_id)?.creatures.contains_key(&creature_id) {
          bail!(GameErrorEnum::CreatureNotFound(creature_id.to_string()));
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
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }
      AddCreatureToScene { scene_id, creature_id, ref visibility } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.creatures.insert(creature_id, ((0, 0, 0), visibility.clone()));
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }
      RemoveCreatureFromScene { scene_id, creature_id } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.creatures.remove(&creature_id);
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }
      AddSceneChallenge { scene_id, ref description, ref challenge } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.attribute_checks.insert(description.clone(), challenge.clone());
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }
      RemoveSceneChallenge { scene_id, ref description } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.attribute_checks.remove(description);
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
      }

      SetFocusedSceneCreatures { scene_id, ref creatures } => {
        self
          .scenes
          .mutate(&scene_id, move |mut scene| {
            scene.focused_creatures = creatures.clone();
            scene
          })
          .ok_or_else(|| GameErrorEnum::SceneNotFound(scene_id))?;
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
      EditMapDetails { id, ref details } => {
        self
          .maps
          .mutate(&id, move |mut m| {
            m.name = details.name.clone();
            m.background_image_url = details.background_image_url.clone();
            m.background_image_scale = details.background_image_scale;
            m.background_image_offset = details.background_image_offset;
            m
          })
          .ok_or_else(|| GameErrorEnum::MapNotFound(id))?;
      }
      EditMapTerrain { id, ref terrain, ref specials } => {
        self
          .maps
          .mutate(&id, move |mut m| {
            m.terrain = terrain.clone();
            m.specials = specials.clone();
            m
          })
          .ok_or_else(|| GameErrorEnum::MapNotFound(id))?;
      }
      CreateCreature(ref path, ref rc) => {
        let c = rc.clone();
        self.creatures.try_insert(c).ok_or_else(|| GameErrorEnum::CreatureAlreadyExists(rc.id()))?;
        self.link_folder_item(path, &FolderItemID::CreatureID(rc.id()))?;
      }
      EditCreatureDetails { creature_id, ref details } => {
        let mutated = self.creatures.mutate(&creature_id, move |mut c| {
          c.name = details.name.clone();
          c.class = details.class.clone();
          c.portrait_url = details.portrait_url.clone();
          c.icon_url = details.icon_url.clone();
          c.note = details.note.clone();
          c.bio = details.bio.clone();
          c.initiative = details.initiative.clone();
          c.size = details.size;
          c
        });
        mutated.ok_or_else(|| GameErrorEnum::CreatureNotFound(creature_id.to_string()))?;
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

      AddVolumeCondition { ref scene_id, point, volume, condition_id, ref condition, duration } => {
        let scene = self
          .get_scene(*scene_id)?
          .add_volume_condition(condition_id, point, volume, condition.clone(), duration);
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
    self.dyn_creature(
      self.creatures.get(&cid).ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?,
    )
  }

  /// Only pub for tests.
  pub fn dyn_creature<'creature, 'game: 'creature>(
    &'game self, creature: &'creature Creature
  ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
    DynamicCreature::new(creature, self)
  }

  pub fn get_scene(&self, id: SceneID) -> Result<&Scene, GameError> {
    self.scenes.get(&id).ok_or_else(|| GameErrorEnum::SceneNotFound(id).into())
  }

  pub fn get_combat(&self) -> Result<DynamicCombat, GameError> {
    let combat = self.current_combat.as_ref().ok_or(GameErrorEnum::NotInCombat)?;
    let scene = self.get_scene(combat.scene)?;
    let map = self.get_map(scene.map)?;
    Ok(DynamicCombat { scene: scene, map: map, combat: combat, game: self })
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
      bail!(GameErrorEnum::CreatureNotFound(cid.to_string()));
    }
    let creature = self.get_creature(cid)?;
    if creature.can_act() {
      if creature.has_ability(abid) {
        self.creature_act(
          &creature,
          scene,
          self.get_ability(&abid)?,
          target,
          self.change(),
          in_combat,
        )
      } else {
        Err(GameErrorEnum::CreatureLacksAbility(creature.id(), abid).into())
      }
    } else {
      Err(GameErrorEnum::CannotAct(creature.id()).into())
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
            SceneTarget::RangedVolume { range, volume },
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
          _ => bail!(GameErrorEnum::BuggyProgram("Ugh".to_string())),
        }
        change
      }
      _ => bail!(GameErrorEnum::InvalidTargetForAction(ability.action.clone(), target)),
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
      (CreatureTarget::Melee, DecidedTarget::Creature(cid)) => if self
        .tile_system
        .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, MELEE_RANGE)
      {
        Ok(vec![cid])
      } else {
        Err(GameErrorEnum::CreatureOutOfRange(cid).into())
      },
      (CreatureTarget::Range(max), DecidedTarget::Creature(cid)) => if self
        .tile_system
        .points_within_distance(scene.get_pos(creature.id())?, scene.get_pos(cid)?, max)
      {
        Ok(vec![cid])
      } else {
        Err(GameErrorEnum::CreatureOutOfRange(cid).into())
      },
      (CreatureTarget::Actor, DecidedTarget::Actor) => Ok(vec![creature.id()]),
      (_, DecidedTarget::Point(pt)) => {
        self.volume_creature_targets(scene, creature.creature.id, target, pt)
      }
      (spec, decided) => Err(GameErrorEnum::InvalidTargetForTargetSpec(spec, decided).into()),
    }
  }

  // TODO: Honor terrain!
  // 1. `pt` must be visible to the caster
  // 2. volumes must not go through blocked terrain
  // 3. volumes must (generally) not go around corners
  fn volume_creature_targets(
    &self, scene: &Scene, actor_id: CreatureID, target: CreatureTarget, pt: Point3
  ) -> Result<Vec<CreatureID>, GameError> {
    match target {
      CreatureTarget::AllCreaturesInVolumeInRange { volume, range } => {
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
      _ => bail!(GameErrorEnum::InvalidTargetForTargetSpec(target, DecidedTarget::Point(pt))),
    }
  }

  /// Calculate which *points* and which *creatures* will be affected by an ability targeted at a
  /// point.
  pub fn preview_volume_targets(
    &self, scene: &Scene, actor_id: CreatureID, ability_id: AbilityID, pt: Point3
  ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
    let terrain = self.get_map(scene.map)?.terrain.iter();
    let all_tiles = terrain.map(|pt| (*pt, *pt)).collect();
    let ability = self.get_ability(&ability_id)?;

    let cids = match ability.action {
      Action::Creature { target, .. } => self.volume_creature_targets(scene, actor_id, target, pt)?,
      Action::SceneVolume { target: SceneTarget::RangedVolume { volume, .. }, .. } => {
        scene.creatures_in_volume(self.tile_system, pt, volume)
      }
      _ => vec![],
    };
    let tiles = match ability.action {
      Action::Creature {
        target: CreatureTarget::AllCreaturesInVolumeInRange { volume, range },
        ..
      } |
      Action::SceneVolume { target: SceneTarget::RangedVolume { volume, range }, .. } => {
        self.tile_system.items_within_volume(volume, pt, &all_tiles)
      }
      Action::Creature { target: CreatureTarget::LineFromActor { distance }, .. } => {
        let actor_pos = scene.get_pos(actor_id)?;
        let volume = line_through_point(actor_pos, pt, distance);
        self.tile_system.items_within_volume(volume, actor_pos, &all_tiles)
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
        self.get_map(scene.map)?,
        Volume::AABB(creature.creature.size),
        creature.speed(),
      ))
    } else {
      Err(GameErrorEnum::CannotAct(creature.id()).into())
    }
  }

  /// Get a list of possible targets for an ability being used by a creature.
  pub fn get_target_options(
    &self, scene: SceneID, creature_id: CreatureID, ability_id: AbilityID
  ) -> Result<PotentialTargets, GameError> {
    let ability = self.get_ability(&ability_id)?;

    use types::Action as A;
    use types::CreatureTarget as CT;
    Ok(match ability.action {
      A::Creature { target: CT::Melee, .. } => {
        self.creatures_in_range(scene, creature_id, MELEE_RANGE)?
      }
      A::Creature { target: CT::Range(distance), .. } => {
        self.creatures_in_range(scene, creature_id, distance)?
      }
      A::Creature { target: CT::Actor, .. } => PotentialTargets::CreatureIDs(vec![creature_id]),
      A::Creature { target: CT::AllCreaturesInVolumeInRange { range, .. }, .. } |
      A::SceneVolume { target: SceneTarget::RangedVolume { range, .. }, .. } => {
        self.open_terrain_in_range(scene, creature_id, range)?
      }
      A::Creature { target: CT::LineFromActor { distance }, .. } => {
        self.open_terrain_in_range(scene, creature_id, distance)?
      }
      A::Creature {
        target: CT::SomeCreaturesInVolumeInRange { volume, maximum, range }, ..
      } => panic!(),
    })
  }

  fn open_terrain_in_range(
    &self, scene: SceneID, creature_id: CreatureID, range: Distance
  ) -> Result<PotentialTargets, GameError> {
    let scene = self.get_scene(scene)?;
    let creature_pos = scene.get_pos(creature_id)?;
    let map = self.get_map(scene.map)?;
    let pts = self.tile_system.open_points_in_range(creature_pos, map, range);
    Ok(PotentialTargets::Points(pts))
  }

  fn creatures_in_range(
    &self, scene: SceneID, creature_id: CreatureID, distance: Distance
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

  pub fn get_class(&self, class: &str) -> Result<&Class, GameError> {
    self.classes.get(class).ok_or_else(|| GameErrorEnum::ClassNotFound(class.to_string()).into())
  }

  pub fn change(&self) -> ChangedGame {
    ChangedGame { game: self.clone(), logs: vec![] }
  }

  pub fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError> {
    let game = self.apply_log(&log)?;
    Ok(ChangedGame { game: game, logs: vec![log] })
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
    F: FnOnce(DynamicCombat<'game>)
      -> Result<ChangedCombat<'game>, GameError>,
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
    F: FnOnce(DynamicCreature)
      -> Result<ChangedCreature, GameError>,
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
  use std::collections::HashSet;
  use std::iter::FromIterator;

  use game::*;
  use combat::test::*;
  use types::test::*;
  use grid::test::*;
  use creature::test::*;

  pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    t_perform(game, GameCommand::StartCombat(t_scene_id(), combatants))
  }

  pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    t_perform(game, GameCommand::CombatAct(ability_id, target))
  }

  pub fn t_game() -> Game {
    let mut game = Game::new(t_classes(), t_abilities());
    game.maps.insert(huge_box());
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

  pub fn t_classes() -> HashMap<String, Class> {
    let rogue_abs = vec![abid("punch")];
    let ranger_abs = vec![abid("shoot"), abid("piercing_shot")];
    let cleric_abs = vec![abid("heal"), abid("fireball")];
    HashMap::from_iter(vec![
      (
        "rogue".to_string(),
        Class { abilities: rogue_abs, conditions: vec![], color: "purple".to_string() },
      ),
      (
        "ranger".to_string(),
        Class { abilities: ranger_abs, conditions: vec![], color: "darkgreen".to_string() },
      ),
      (
        "cleric".to_string(),
        Class { abilities: cleric_abs, conditions: vec![], color: "lightgreen".to_string() },
      ),
    ])
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
    let game = t_perform(
      &game,
      GameCommand::CombatAct(abid("punch"), DecidedTarget::Creature(cid_ranger())),
    );
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
    assert_eq!(
      game.get_combat().unwrap().combat.creature_ids(),
      vec![cid_rogue(), cid_ranger(), cid_cleric()]
    );
    // move ranger to have an initiative higher than the rogue
    let game = t_perform(&game, GameCommand::ChangeCreatureInitiative(cid_ranger(), 30));
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
    let game =
      t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), (11, 0, 0)));
    let game = t_perform(
      &game,
      GameCommand::ActCreature(
        t_scene_id(),
        cid_cleric(),
        abid("fireball"),
        DecidedTarget::Point((0, 0, 0)),
      ),
    );
    assert_eq!(game.get_creature(cid_rogue()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_ranger()).unwrap().creature.cur_health, HP(7));
    assert_eq!(game.get_creature(cid_cleric()).unwrap().creature.cur_health, HP(10));
  }

  #[test]
  fn test_creatures_in_sphere() {
    let game = t_game();
    let volume = Volume::Sphere(Distance::from_meters(2.0));
    let pt = (5, 0, 0);

    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), (5, 0, 0)));
    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), (6, 0, 0)));
    let scene = game.get_scene(t_scene_id()).unwrap();

    let cids = scene.creatures_in_volume(game.tile_system, pt, volume);
    let cids = HashSet::<CreatureID>::from_iter(cids);
    assert_eq!(cids, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn test_sphere_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::AllCreaturesInVolumeInRange {
      range: Distance::from_meters(10.0),
      volume: Volume::Sphere(Distance::from_meters(2.0)),
    };
    let pt = (5, 0, 0);

    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), (5, 0, 0)));
    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), (6, 0, 0)));

    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game.volume_creature_targets(scene, cid_ranger(), target_spec, pt).unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
  }

  #[test]
  fn test_line_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::LineFromActor { distance: Distance::from_meters(10.0) };
    let pt = (1, 0, 0);

    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_rogue(), (1, 0, 0)));
    let game = t_perform(&game, GameCommand::SetCreaturePos(t_scene_id(), cid_cleric(), (2, 0, 0)));
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
    let ability_id = abid("thorn_patch");
    let preview = game.preview_volume_targets(scene, cleric, ability_id, (0, 0, 0)).unwrap();
    let expected = hashset!{cid_cleric(), cid_ranger(), cid_rogue()};
    assert_eq!(HashSet::from_iter(preview.0), expected);
  }
}
