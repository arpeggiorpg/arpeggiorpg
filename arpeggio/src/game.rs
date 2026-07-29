use std::{
    cmp,
    collections::{HashMap, HashSet},
};

use crate::{
    combat::*,
    creature::{ChangedCreature, CreatureExt},
    grid::{line_through_point, TileSystemExt},
    scene::SceneExt,
    types::*,
};
#[cfg(test)]
pub mod test;

pub trait GameExt {
    fn validate_collections(&self) -> Result<(), GameError>;

    fn creatures(&self) -> Result<HashMap<CreatureID, DynamicCreature<'_, '_>>, GameError>;

    fn get_item(&self, iid: ItemID) -> Result<&Item, GameError>;

    fn get_ability(&self, abid: AbilityID) -> Result<&Ability, GameError>;

    /// Perform a PlayerCommand on the current Game.
    fn perform_player_command(
        &self,
        player_id: PlayerID,
        cmd: PlayerCommand,
    ) -> Result<ChangedGame, GameError>;

    /// Check that the player controls the current combat creature.
    fn auth_combat(&self, player: &Player) -> Result<(), GameError>;

    /// Perform a GMCommand on the current Game.
    ///
    /// The result includes a new Game instance and a Vec of GameLogs. These GameLogs should be a
    /// deterministic representation of the changes made to the Game, so they can be used to replay
    /// history and get the same exact result. An Undo operation can be implemented by rolling back to
    /// a previous game Snapshot and replaying until the desired GameLog.
    fn perform_gm_command(&self, cmd: GMCommand) -> Result<ChangedGame, GameError>;

    fn start_combat(
        &self,
        scene_id: SceneID,
        cids: Vec<CreatureID>,
    ) -> Result<ChangedGame, GameError>;

    fn add_creature_to_combat(&self, creature_id: CreatureID) -> Result<ChangedGame, GameError>;

    fn attribute_check(
        &self,
        creature_id: CreatureID,
        check: &AttributeCheck,
    ) -> Result<ChangedGame, GameError>;

    fn path_creature(
        &self,
        scene: SceneID,
        cid: CreatureID,
        pt: Point3,
    ) -> Result<(ChangedGame, u32units::Length), GameError>;

    fn path_creature_distance(
        &self,
        scene_id: SceneID,
        creature_id: CreatureID,
        pt: Point3,
        max_distance: u32units::Length,
    ) -> Result<(ChangedGame, u32units::Length), GameError>;

    fn next_turn(&self) -> Result<ChangedGame, GameError>;

    fn apply_log(&self, log: &GameLog) -> Result<Game, GameError>;

    fn mutate_owner_inventory<F>(
        &mut self,
        owner_id: InventoryOwner,
        f: F,
    ) -> Result<(), GameError>
    where
        F: FnOnce(&mut Inventory);

    fn get_owner_inventory(&self, owner_id: InventoryOwner) -> Result<&Inventory, GameError>;

    /// Remove some number of items from an inventory, returning the actual number removed.
    fn remove_inventory(
        &mut self,
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
    ) -> Result<u64, GameError>;

    fn set_item_count(
        &mut self,
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
    ) -> Result<(), GameError>;

    /// Apply a log to a *mutable* Game.
    // This is done so that we don't have to worry about `self` vs `newgame` -- all
    // manipulations here work on &mut self.
    fn apply_log_mut(&mut self, log: &GameLog) -> Result<(), GameError>;

    fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError>;

    fn check_scene(&self, scene: SceneID) -> Result<(), GameError>;

    fn is_in_combat(&self, cid: CreatureID) -> bool;

    fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature<'_, '_>, GameError>;

    /// Only pub for tests.
    fn dyn_creature<'creature, 'game: 'creature>(
        &'game self,
        creature: &'creature Creature,
    ) -> Result<DynamicCreature<'creature, 'game>, GameError>;

    fn get_combat(&self) -> Result<DynamicCombat<'_>, GameError>;

    // ** CONSIDER ** moving this chunk of code to... Scene.rs?

    fn combat_act(&self, abid: AbilityID, target: DecidedTarget) -> Result<ChangedGame, GameError>;

    fn ooc_act(
        &self,
        scene: SceneID,
        cid: CreatureID,
        abid: AbilityID,
        target: DecidedTarget,
    ) -> Result<ChangedGame, GameError>;

    fn _act(
        &self,
        scene: &Scene,
        cid: CreatureID,
        abid: AbilityID,
        target: DecidedTarget,
        in_combat: bool,
    ) -> Result<ChangedGame, GameError>;

    fn creature_act(
        &self,
        creature: &DynamicCreature,
        scene: &Scene,
        ability: &Ability,
        target: DecidedTarget,
        change: ChangedGame,
        in_combat: bool,
    ) -> Result<ChangedGame, GameError>;

    fn resolve_creature_targets(
        &self,
        creature: &DynamicCreature,
        scene: &Scene,
        target: CreatureTarget,
        decision: DecidedTarget,
    ) -> Result<Vec<CreatureID>, GameError>;

    // TODO: unimplemented! Honor terrain!
    // 1. `pt` must be visible to the caster
    // 2. volumes must not go through blocked terrain
    // 3. volumes must (generally) not go around corners
    fn volume_creature_targets(
        &self,
        scene: &Scene,
        actor_id: CreatureID,
        target: CreatureTarget,
        pt: Point3,
    ) -> Result<Vec<CreatureID>, GameError>;

    /// Calculate which *points* and which *creatures* will be affected by an ability targeted at a
    /// point.
    fn preview_volume_targets(
        &self,
        scene: &Scene,
        actor_id: CreatureID,
        ability_id: AbilityID,
        pt: Point3,
    ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError>;

    fn get_movement_options(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
    ) -> Result<Vec<Point3>, GameError>;

    /// Get a list of possible targets for an ability being used by a creature.
    fn get_target_options(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        ability_id: AbilityID,
    ) -> Result<PotentialTargets, GameError>;

    fn open_terrain_in_range(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        range: u32units::Length,
    ) -> Result<PotentialTargets, GameError>;

    fn creatures_in_range(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        distance: u32units::Length,
    ) -> Result<PotentialTargets, GameError>;

    // ** END CONSIDERATION **

    fn get_class(&self, class: ClassID) -> Result<&Class, GameError>;

    fn change(&self) -> ChangedGame;

    fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError>;

    fn change_with_logs(&self, logs: Vec<GameLog>) -> Result<ChangedGame, GameError>;
}

fn remove_resource_from_collections(game: &mut Game, resource: ResourceRef) {
    for collection_id in game.collections.keys().copied().collect::<Vec<_>>() {
        game.collections
            .mutate(&collection_id, |collection| match resource {
                ResourceRef::Scene(id) => collection.scenes.retain(|candidate| *candidate != id),
                ResourceRef::Creature(id) => {
                    collection.creatures.retain(|candidate| *candidate != id)
                }
                ResourceRef::Note(id) => collection.notes.retain(|candidate| *candidate != id),
                ResourceRef::Item(id) => collection.items.retain(|candidate| *candidate != id),
                ResourceRef::Ability(id) => {
                    collection.abilities.retain(|candidate| *candidate != id)
                }
                ResourceRef::Class(id) => collection.classes.retain(|candidate| *candidate != id),
            });
    }
}

fn extend_unique<T: Copy + Eq>(destination: &mut Vec<T>, additions: &[T]) {
    for addition in additions {
        if !destination.contains(addition) {
            destination.push(*addition);
        }
    }
}

fn remove_members<T: Eq>(destination: &mut Vec<T>, removals: &[T]) {
    destination.retain(|candidate| !removals.contains(candidate));
}

fn add_collection_resources(collection: &mut Collection, resources: &CollectionResources) {
    extend_unique(&mut collection.scenes, &resources.scenes);
    extend_unique(&mut collection.creatures, &resources.creatures);
    extend_unique(&mut collection.notes, &resources.notes);
    extend_unique(&mut collection.items, &resources.items);
    extend_unique(&mut collection.abilities, &resources.abilities);
    extend_unique(&mut collection.classes, &resources.classes);
}

fn remove_collection_resources(collection: &mut Collection, resources: &CollectionResources) {
    remove_members(&mut collection.scenes, &resources.scenes);
    remove_members(&mut collection.creatures, &resources.creatures);
    remove_members(&mut collection.notes, &resources.notes);
    remove_members(&mut collection.items, &resources.items);
    remove_members(&mut collection.abilities, &resources.abilities);
    remove_members(&mut collection.classes, &resources.classes);
}

fn merge_collections(
    game: &mut Game,
    destination_id: CollectionID,
    source_ids: &[CollectionID],
) -> Result<(), GameError> {
    if !game.collections.contains_key(&destination_id) {
        return Err(GameError::CollectionNotFound(destination_id));
    }

    let mut seen = HashSet::new();
    let mut sources = Vec::new();
    for source_id in source_ids {
        if *source_id == destination_id || !seen.insert(*source_id) {
            continue;
        }
        sources.push(
            game.collections
                .get(source_id)
                .cloned()
                .ok_or(GameError::CollectionNotFound(*source_id))?,
        );
    }

    game.collections.mutate(&destination_id, |destination| {
        for source in &sources {
            add_collection_resources(
                destination,
                &CollectionResources {
                    scenes: source.scenes.clone(),
                    creatures: source.creatures.clone(),
                    notes: source.notes.clone(),
                    items: source.items.clone(),
                    abilities: source.abilities.clone(),
                    classes: source.classes.clone(),
                },
            );
        }
    });
    for source in sources {
        game.collections.remove(&source.id);
    }
    game.validate_collections()
}

fn delete_resource(game: &mut Game, resource: ResourceRef) -> Result<(), GameError> {
    remove_resource_from_collections(game, resource);
    match resource {
        ResourceRef::Note(id) => {
            game.notes.remove(&id).ok_or(GameError::NoteNotFound(id))?;
        }
        ResourceRef::Item(id) => {
            for creature_id in game.creatures.keys().copied().collect::<Vec<_>>() {
                game.creatures.mutate(&creature_id, |creature| {
                    creature.inventory.remove(&id);
                });
            }
            for scene_id in game.scenes.keys().copied().collect::<Vec<_>>() {
                game.scenes.mutate(&scene_id, |scene| {
                    scene.inventory.remove(&id);
                });
            }
            game.items.remove(&id).ok_or(GameError::ItemNotFound(id))?;
        }
        ResourceRef::Creature(id) => {
            for scene_id in game.scenes.keys().copied().collect::<Vec<_>>() {
                game.scenes.mutate(&scene_id, |scene| {
                    scene.creatures.remove(&id);
                });
            }
            for player_id in game.players.keys().cloned().collect::<Vec<_>>() {
                game.players.mutate(&player_id, |player| {
                    player.creatures.remove(&id);
                });
            }
            game.current_combat = if let Ok(combat) = game.get_combat() {
                combat.remove_from_combat(id)?
            } else {
                None
            };
            game.creatures
                .remove(&id)
                .ok_or_else(|| GameError::CreatureNotFound(id.to_string()))?;
        }
        ResourceRef::Scene(id) => {
            if game.get_combat().is_ok_and(|combat| combat.scene.id == id) {
                return Err(GameError::SceneInUse(id));
            }
            game.scenes
                .remove(&id)
                .ok_or(GameError::SceneNotFound(id))?;
            if game.active_scene == Some(id) {
                game.active_scene = None;
            }
            for player_id in game.players.keys().cloned().collect::<Vec<_>>() {
                game.players.mutate(&player_id, |player| {
                    if player.scene == Some(id) {
                        player.scene = None;
                    }
                });
            }
        }
        ResourceRef::Ability(id) => {
            for class_id in game.classes.keys().copied().collect::<Vec<_>>() {
                game.classes.mutate(&class_id, |class| {
                    class.abilities.retain(|candidate| *candidate != id);
                });
            }
            for creature_id in game.creatures.keys().copied().collect::<Vec<_>>() {
                game.creatures.mutate(&creature_id, |creature| {
                    creature.abilities.remove(&id);
                });
            }
            game.abilities.remove(&id).ok_or(GameError::NoAbility(id))?;
        }
        ResourceRef::Class(id) => {
            if game.creatures.values().any(|creature| creature.class == id) {
                return Err(GameError::BuggyProgram("Class in use!".to_string()));
            }
            game.classes
                .remove(&id)
                .ok_or(GameError::ClassNotFound(id))?;
        }
    }
    Ok(())
}

fn rename_resource(
    game: &mut Game,
    resource: ResourceRef,
    new_name: &str,
) -> Result<(), GameError> {
    let found = match resource {
        ResourceRef::Scene(id) => game
            .scenes
            .mutate(&id, |value| value.name = new_name.to_string()),
        ResourceRef::Creature(id) => game
            .creatures
            .mutate(&id, |value| value.name = new_name.to_string()),
        ResourceRef::Note(id) => game
            .notes
            .mutate(&id, |value| value.name = new_name.to_string()),
        ResourceRef::Item(id) => game
            .items
            .mutate(&id, |value| value.name = new_name.to_string()),
        ResourceRef::Ability(id) => game
            .abilities
            .mutate(&id, |value| value.name = new_name.to_string()),
        ResourceRef::Class(id) => game
            .classes
            .mutate(&id, |value| value.name = new_name.to_string()),
    };
    if found.is_none() {
        return Err(match resource {
            ResourceRef::Scene(id) => GameError::SceneNotFound(id),
            ResourceRef::Creature(id) => GameError::CreatureNotFound(id.to_string()),
            ResourceRef::Note(id) => GameError::NoteNotFound(id),
            ResourceRef::Item(id) => GameError::ItemNotFound(id),
            ResourceRef::Ability(id) => GameError::NoAbility(id),
            ResourceRef::Class(id) => GameError::ClassNotFound(id),
        });
    }
    Ok(())
}

fn copy_resource(
    game: &mut Game,
    source: ResourceRef,
    destination: ResourceRef,
) -> Result<(), GameError> {
    match (source, destination) {
        (ResourceRef::Scene(source), ResourceRef::Scene(destination)) => {
            let mut value = game.get_scene(source)?.clone();
            value.id = destination;
            game.scenes
                .try_insert(value)
                .ok_or(GameError::SceneAlreadyExists(destination))?;
        }
        (ResourceRef::Creature(source), ResourceRef::Creature(destination)) => {
            let mut value = game.get_creature(source)?.creature.clone();
            value.id = destination;
            game.creatures
                .try_insert(value)
                .ok_or(GameError::CreatureAlreadyExists(destination))?;
        }
        (ResourceRef::Note(source), ResourceRef::Note(destination)) => {
            let mut value = game
                .notes
                .get(&source)
                .ok_or(GameError::NoteNotFound(source))?
                .clone();
            value.id = destination;
            game.notes
                .try_insert(value)
                .ok_or(GameError::NoteAlreadyExists(destination))?;
        }
        (ResourceRef::Item(source), ResourceRef::Item(destination)) => {
            let mut value = game.get_item(source)?.clone();
            value.id = destination;
            game.items
                .try_insert(value)
                .ok_or(GameError::ItemAlreadyExists(destination))?;
        }
        (ResourceRef::Ability(source), ResourceRef::Ability(destination)) => {
            let mut value = game.get_ability(source)?.clone();
            value.id = destination;
            game.abilities
                .try_insert(value)
                .ok_or(GameError::AbilityAlreadyExists(destination))?;
        }
        (ResourceRef::Class(source), ResourceRef::Class(destination)) => {
            let mut value = game.get_class(source)?.clone();
            value.id = destination;
            game.classes
                .try_insert(value)
                .ok_or(GameError::ClassAlreadyExists(destination))?;
        }
        _ => {
            return Err(GameError::BuggyProgram(
                "Cannot copy a resource into a different resource type".to_string(),
            ))
        }
    }
    Ok(())
}

impl GameExt for Game {
    fn validate_collections(&self) -> Result<(), GameError> {
        fn validate_ids<ID>(
            collection_id: CollectionID,
            kind: &str,
            ids: &[ID],
            exists: impl Fn(&ID) -> bool,
            not_found: impl Fn(ID) -> GameError,
        ) -> Result<(), GameError>
        where
            ID: Copy + Eq + std::hash::Hash + ToString,
        {
            let mut seen = HashSet::new();
            for id in ids {
                if !seen.insert(*id) {
                    return Err(GameError::DuplicateCollectionResource(
                        collection_id,
                        kind.to_string(),
                        id.to_string(),
                    ));
                }
                if !exists(id) {
                    return Err(not_found(*id));
                }
            }
            Ok(())
        }

        for collection in self.collections.values() {
            validate_ids(
                collection.id,
                "scene",
                &collection.scenes,
                |id| self.scenes.contains_key(id),
                GameError::SceneNotFound,
            )?;
            validate_ids(
                collection.id,
                "creature",
                &collection.creatures,
                |id| self.creatures.contains_key(id),
                |id| GameError::CreatureNotFound(id.to_string()),
            )?;
            validate_ids(
                collection.id,
                "note",
                &collection.notes,
                |id| self.notes.contains_key(id),
                GameError::NoteNotFound,
            )?;
            validate_ids(
                collection.id,
                "item",
                &collection.items,
                |id| self.items.contains_key(id),
                GameError::ItemNotFound,
            )?;
            validate_ids(
                collection.id,
                "ability",
                &collection.abilities,
                |id| self.abilities.contains_key(id),
                GameError::NoAbility,
            )?;
            validate_ids(
                collection.id,
                "class",
                &collection.classes,
                |id| self.classes.contains_key(id),
                GameError::ClassNotFound,
            )?;
        }
        Ok(())
    }

    fn creatures(&self) -> Result<HashMap<CreatureID, DynamicCreature<'_, '_>>, GameError> {
        let mut map = HashMap::new();
        for creature in self.creatures.values() {
            map.insert(creature.id, self.dyn_creature(creature)?);
        }
        Ok(map)
    }

    fn get_item(&self, iid: ItemID) -> Result<&Item, GameError> {
        self.items
            .get(&iid)
            .ok_or_else(|| GameError::ItemNotFound(iid))
    }

    fn get_ability(&self, abid: AbilityID) -> Result<&Ability, GameError> {
        self.abilities
            .get(&abid)
            .ok_or_else(|| GameError::NoAbility(abid))
    }

    /// Perform a PlayerCommand on the current Game.
    fn perform_player_command(
        &self,
        player_id: PlayerID,
        cmd: PlayerCommand,
    ) -> Result<ChangedGame, GameError> {
        use self::PlayerCommand::*;
        let player = self
            .players
            .get(&player_id)
            .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;

        match cmd {
            ChatFromPlayer { message } => self.change_with(GameLog::ChatFromPlayer {
                player_id,
                message: message.to_owned(),
            }),
            CreateNote { name, content } => self.change_with(GameLog::CreateNote {
                note: Note {
                    id: NoteID::gen(),
                    name,
                    content,
                    owner: NoteOwner::Player(player_id),
                    visibility: NoteVisibility::OwnerOnly,
                },
            }),
            EditNote {
                note_id,
                name,
                content,
            } => {
                let existing = self
                    .notes
                    .get(&note_id)
                    .filter(|note| note.owner == NoteOwner::Player(player_id.clone()))
                    .ok_or(GameError::NoteNotFound(note_id))?;
                self.change_with(GameLog::EditNote {
                    note: Note {
                        id: note_id,
                        name,
                        content,
                        owner: existing.owner.clone(),
                        visibility: existing.visibility.clone(),
                    },
                })
            }
            PathCreature {
                creature_id,
                destination,
            } => {
                let scene_id = player.scene.ok_or(GameError::BuggyProgram(
                    "Player isn't in a scene".to_string(),
                ))?;
                let scene = self.get_scene(scene_id)?;
                Ok(self.path_creature(scene.id, creature_id, destination)?.0)
            }
            CombatAct { ability_id, target } => {
                self.auth_combat(player)?;
                self.combat_act(ability_id, target)
            }
            PathCurrentCombatCreature { destination } => {
                self.auth_combat(player)?;
                self.get_combat()?.get_movement()?.move_current(destination)
            }
            EndTurn => {
                self.auth_combat(player)?;
                self.next_turn()
            }
            GiveItem {
                from_creature_id,
                to_creature_id,
                item_id,
                count,
            } => {
                // Validate that the player owns the source creature
                if !player.creatures.contains(&from_creature_id) {
                    return Err(GameError::CreatureNotFound(from_creature_id.to_string()));
                }

                // Validate that both creatures exist
                self.check_creature_id(from_creature_id)?;
                self.check_creature_id(to_creature_id)?;

                // Validate that the player is in a scene
                let scene_id = player.scene.ok_or(GameError::BuggyProgram(
                    "Player isn't in a scene".to_string(),
                ))?;
                let scene = self.get_scene(scene_id)?;

                // Validate that both creatures are in the player's current scene
                if !scene.creatures.contains_key(&from_creature_id) {
                    return Err(GameError::CreatureNotFound(from_creature_id.to_string()));
                }
                if !scene.creatures.contains_key(&to_creature_id) {
                    return Err(GameError::CreatureNotFound(to_creature_id.to_string()));
                }

                self.change_with(GameLog::TransferItem {
                    from: InventoryOwner::Creature(from_creature_id),
                    to: InventoryOwner::Creature(to_creature_id),
                    item_id,
                    count,
                })
            }
            DropItem {
                creature_id,
                item_id,
                count,
            } => {
                // Validate that the player owns the creature
                if !player.creatures.contains(&creature_id) {
                    return Err(GameError::CreatureNotFound(creature_id.to_string()));
                }

                // Validate that the creature exists
                self.check_creature_id(creature_id)?;

                // Validate that the player is in a scene
                let scene_id = player.scene.ok_or(GameError::BuggyProgram(
                    "Player isn't in a scene".to_string(),
                ))?;
                let scene = self.get_scene(scene_id)?;

                // Validate that the creature is in the player's current scene
                if !scene.creatures.contains_key(&creature_id) {
                    return Err(GameError::CreatureNotFound(creature_id.to_string()));
                }

                self.change_with(GameLog::TransferItem {
                    from: InventoryOwner::Creature(creature_id),
                    to: InventoryOwner::Scene(scene_id),
                    item_id,
                    count,
                })
            }
        }
    }

    /// Check that the player controls the current combat creature.
    fn auth_combat(&self, player: &Player) -> Result<(), GameError> {
        if !player
            .creatures
            .contains(&self.get_combat()?.combat.current_creature_id())
        {
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
    fn perform_gm_command(&self, cmd: GMCommand) -> Result<ChangedGame, GameError> {
        use self::GMCommand::*;
        let change = match cmd {
            SetActiveScene { id } => self.change_with(GameLog::SetActiveScene { id }),
            // ** Player Management **
            RegisterPlayer { ref id } => {
                self.change_with(GameLog::RegisterPlayer { id: id.clone() })
            }
            GiveCreaturesToPlayer {
                ref player_id,
                ref creature_ids,
            } => self.change_with(GameLog::GiveCreaturesToPlayer {
                player_id: player_id.clone(),
                creature_ids: creature_ids.clone(),
            }),
            UnregisterPlayer { ref id } => {
                self.change_with(GameLog::UnregisterPlayer { id: id.clone() })
            }
            RemoveCreaturesFromPlayer {
                ref player_id,
                ref creature_ids,
            } => self.change_with(GameLog::RemoveCreaturesFromPlayer {
                player_id: player_id.clone(),
                creature_ids: creature_ids.clone(),
            }),
            SetPlayerScene {
                ref player_id,
                scene_id,
            } => self.change_with(GameLog::SetPlayerScene {
                player_id: player_id.clone(),
                scene_id,
            }),

            // ** Chat **
            ChatFromGM { ref message } => self.change_with(GameLog::ChatFromGM {
                message: message.to_owned(),
            }),

            // ** Attribute checks **
            AttributeCheck {
                creature_id,
                attribute_check,
            } => self.attribute_check(creature_id, &attribute_check),
            CreateCollection { name } => self.change_with(GameLog::CreateCollection {
                collection: Collection {
                    id: CollectionID::gen(),
                    name,
                    scenes: vec![],
                    creatures: vec![],
                    notes: vec![],
                    items: vec![],
                    abilities: vec![],
                    classes: vec![],
                },
            }),
            EditCollection { collection } => {
                self.change_with(GameLog::EditCollection { collection })
            }
            RenameCollection {
                collection_id,
                name,
            } => self.change_with(GameLog::RenameCollection {
                collection_id,
                name,
            }),
            AddResourcesToCollection {
                collection_id,
                resources,
            } => self.change_with(GameLog::AddResourcesToCollection {
                collection_id,
                resources,
            }),
            RemoveResourcesFromCollection {
                collection_id,
                resources,
            } => self.change_with(GameLog::RemoveResourcesFromCollection {
                collection_id,
                resources,
            }),
            MergeCollections {
                destination_id,
                source_ids,
            } => self.change_with(GameLog::MergeCollections {
                destination_id,
                source_ids,
            }),
            DeleteCollection { collection_id } => {
                self.change_with(GameLog::DeleteCollection { collection_id })
            }
            DeleteResource { resource } => self.change_with(GameLog::DeleteResource { resource }),
            RenameResource { resource, new_name } => {
                self.change_with(GameLog::RenameResource { resource, new_name })
            }
            CopyResource { source } => {
                let destination = match source {
                    ResourceRef::Scene(_) => ResourceRef::Scene(SceneID::gen()),
                    ResourceRef::Creature(_) => ResourceRef::Creature(CreatureID::gen()),
                    ResourceRef::Note(_) => ResourceRef::Note(NoteID::gen()),
                    ResourceRef::Item(_) => ResourceRef::Item(ItemID::gen()),
                    ResourceRef::Ability(_) => ResourceRef::Ability(AbilityID::gen()),
                    ResourceRef::Class(_) => ResourceRef::Class(ClassID::gen()),
                };
                self.change_with(GameLog::CopyResource {
                    source,
                    destination,
                })
            }

            CreateItem { name } => {
                let item = Item {
                    id: ItemID::gen(),
                    name,
                };
                self.change_with(GameLog::CreateItem { item })
            }
            EditItem { item } => self.change_with(GameLog::EditItem { item }),

            CreateNote {
                name,
                content,
                visibility,
            } => self.change_with(GameLog::CreateNote {
                note: Note {
                    id: NoteID::gen(),
                    name,
                    content,
                    owner: NoteOwner::Game,
                    visibility,
                },
            }),
            EditNote { note } => self.change_with(GameLog::EditNote { note }),

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

            CreateScene { scene } => {
                let scene = Scene::create(scene);
                self.change_with(GameLog::CreateScene { scene })
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
                visibility,
            }),
            AddCreatureToScene {
                scene_id,
                creature_id,
                ref visibility,
            } => self.change_with(GameLog::AddCreatureToScene {
                scene_id,
                creature_id,
                visibility: *visibility,
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

            // ** Classes & Abilities **
            CreateClass { class } => {
                let id = ClassID::gen();
                let class = Class {
                    id,
                    name: class.name.clone(),
                    abilities: class.abilities.clone(),
                    conditions: class.conditions.clone(),
                    color: class.color.clone(),
                    emoji: class.emoji.clone(),
                };
                self.change_with(GameLog::CreateClass { class })
            }
            EditClass { class } => self.change_with(GameLog::EditClass { class }),
            CreateAbility { ability } => {
                let id = AbilityID::gen();
                let ability = Ability {
                    id,
                    name: ability.name.clone(),
                    cost: ability.cost,
                    action: ability.action.clone(),
                    usable_ooc: ability.usable_ooc,
                };
                self.change_with(GameLog::CreateAbility { ability })
            }
            EditAbility { ability } => self.change_with(GameLog::EditAbility { ability }),

            CreateCreature { creature } => {
                let creature = Creature::create(&creature);
                self.change_with(GameLog::CreateCreature { creature })
            }
            EditCreatureDetails { creature } => {
                self.change_with(GameLog::EditCreature { creature })
            }
            PathCreature {
                scene_id,
                creature_id,
                destination,
            } => Ok(self.path_creature(scene_id, creature_id, destination)?.0),
            SetCreaturePos {
                scene_id,
                creature_id,
                pos,
            } => self.change_with(GameLog::SetCreaturePos {
                scene_id,
                creature_id,
                pos,
            }),
            PathCurrentCombatCreature { destination } => {
                self.get_combat()?.get_movement()?.move_current(destination)
            }
            CombatAct { ability_id, target } => self.combat_act(ability_id, target),
            ActCreature {
                scene_id,
                creature_id,
                ability_id,
                target,
            } => self.ooc_act(scene_id, creature_id, ability_id, target),
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
            EditSceneRelatedScenes {
                scene_id,
                ref related_scenes,
            } => self.change_with(GameLog::EditSceneRelatedScenes {
                scene_id,
                related_scenes: related_scenes.clone(),
            }),
            EditSceneSceneHotspots {
                scene_id,
                ref scene_hotspots,
            } => self.change_with(GameLog::EditSceneSceneHotspots {
                scene_id,
                scene_hotspots: scene_hotspots.clone(),
            }),
            StartCombat {
                scene_id,
                combatants,
            } => self.start_combat(scene_id, combatants),
            StopCombat => self.change_with(GameLog::StopCombat),
            AddCreatureToCombat { creature_id } => self.add_creature_to_combat(creature_id),
            RemoveCreatureFromCombat { creature_id } => {
                self.change_with(GameLog::RemoveCreatureFromCombat { creature_id })
            }
            RerollCombatInitiative => self.change().apply_combat(|c| c.reroll_initiative()),
            ChangeCreatureInitiative {
                creature_id,
                initiative,
            } => self.change_with(GameLog::CombatLog {
                log: CombatLog::ChangeCreatureInitiative {
                    creature_id,
                    initiative,
                },
            }),
            ForceNextTurn => self.change_with(GameLog::CombatLog {
                log: CombatLog::ForceNextTurn,
            }),
            ForcePrevTurn => self.change_with(GameLog::CombatLog {
                log: CombatLog::ForcePrevTurn,
            }),
            EndTurn => self.next_turn(),
        }?;
        Ok(change)
    }

    fn start_combat(
        &self,
        scene_id: SceneID,
        cids: Vec<CreatureID>,
    ) -> Result<ChangedGame, GameError> {
        let combatants = Combat::roll_initiative(self, cids)?;
        self.change_with(GameLog::StartCombat {
            scene_id,
            combatants,
        })
    }

    fn add_creature_to_combat(&self, creature_id: CreatureID) -> Result<ChangedGame, GameError> {
        let creature = self.get_creature(creature_id)?;
        let initiative = creature.creature.initiative.roll().1 as i16;
        self.change_with(GameLog::AddCreatureToCombat {
            creature_id,
            initiative,
        })
    }

    fn attribute_check(
        &self,
        creature_id: CreatureID,
        check: &AttributeCheck,
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

    fn path_creature(
        &self,
        scene: SceneID,
        cid: CreatureID,
        pt: Point3,
    ) -> Result<(ChangedGame, u32units::Length), GameError> {
        let creature = self.get_creature(cid)?;
        self.path_creature_distance(scene, cid, pt, creature.speed())
    }

    fn path_creature_distance(
        &self,
        scene_id: SceneID,
        creature_id: CreatureID,
        pt: Point3,
        max_distance: u32units::Length,
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

        let change = self.change_with(GameLog::PathCreature {
            scene_id,
            creature_id,
            path,
        })?;
        Ok((change, distance))
    }

    fn next_turn(&self) -> Result<ChangedGame, GameError> {
        let change = self.change().apply_combat(|c| c.next_turn())?;
        change.apply_creature(
            self.current_combat.as_ref().unwrap().current_creature_id(),
            |c| c.tick(),
        )
    }

    fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
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
        &mut self,
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
    ) -> Result<u64, GameError> {
        let actually_has = *self.get_owner_inventory(owner)?.get(&item_id).unwrap_or(&0);
        self.set_item_count(owner, item_id, actually_has - count)?;
        Ok(cmp::min(actually_has, count))
    }

    fn set_item_count(
        &mut self,
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
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
        // HEY! Maintainer note! Don't use a call to *ID::gen(), or any other random or
        // side-effecting functions! All of that stuff should be resolved in perform_*_command. This
        // function MUST be purely deterministic.
        use self::GameLog::*;
        match *log {
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
                self.players
                    .remove(id)
                    .ok_or_else(|| GameError::PlayerNotFound(id.clone()))?;
            }

            GiveCreaturesToPlayer {
                ref player_id,
                ref creature_ids,
            } => {
                for cid in creature_ids {
                    self.check_creature_id(*cid)?;
                }
                self.players
                    .mutate(player_id, |p| p.creatures.extend(creature_ids))
                    .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
            }

            RemoveCreaturesFromPlayer {
                ref player_id,
                ref creature_ids,
            } => {
                self.players
                    .mutate(player_id, |p| {
                        for cid in creature_ids {
                            p.creatures.remove(cid);
                        }
                    })
                    .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
            }

            SetPlayerScene {
                ref player_id,
                scene_id,
            } => {
                self.players
                    .mutate(player_id, move |p| p.scene = scene_id)
                    .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;
            }

            // purely informational
            ChatFromGM { .. } | ChatFromPlayer { .. } | AttributeCheckResult { .. } => {}

            CreateCollection { ref collection } => {
                self.collections
                    .try_insert(collection.clone())
                    .ok_or(GameError::CollectionAlreadyExists(collection.id))?;
                self.validate_collections()?;
            }
            EditCollection { ref collection } => {
                if !self.collections.contains_key(&collection.id) {
                    return Err(GameError::CollectionNotFound(collection.id));
                }
                self.collections.insert(collection.clone());
                self.validate_collections()?;
            }
            RenameCollection {
                collection_id,
                ref name,
            } => {
                self.collections
                    .mutate(&collection_id, |collection| collection.name = name.clone())
                    .ok_or(GameError::CollectionNotFound(collection_id))?;
            }
            AddResourcesToCollection {
                collection_id,
                ref resources,
            } => {
                self.collections
                    .mutate(&collection_id, |collection| {
                        add_collection_resources(collection, resources)
                    })
                    .ok_or(GameError::CollectionNotFound(collection_id))?;
                self.validate_collections()?;
            }
            RemoveResourcesFromCollection {
                collection_id,
                ref resources,
            } => {
                self.collections
                    .mutate(&collection_id, |collection| {
                        remove_collection_resources(collection, resources)
                    })
                    .ok_or(GameError::CollectionNotFound(collection_id))?;
                self.validate_collections()?;
            }
            MergeCollections {
                destination_id,
                ref source_ids,
            } => merge_collections(self, destination_id, source_ids)?,
            DeleteCollection { collection_id } => {
                self.collections
                    .remove(&collection_id)
                    .ok_or(GameError::CollectionNotFound(collection_id))?;
            }
            DeleteResource { resource } => delete_resource(self, resource)?,
            RenameResource {
                resource,
                ref new_name,
            } => rename_resource(self, resource, new_name)?,
            CopyResource {
                source,
                destination,
            } => copy_resource(self, source, destination)?,

            CreateItem { item: ref ritem } => {
                let item = ritem.clone();
                self.items
                    .try_insert(item)
                    .ok_or_else(|| GameError::ItemAlreadyExists(ritem.id))?;
            }
            EditItem { ref item } => {
                self.items
                    .mutate(&item.id, move |i| *i = item.clone())
                    .ok_or_else(|| GameError::ItemNotFound(item.id))?;
            }

            CreateNote { ref note } => {
                self.notes
                    .try_insert(note.clone())
                    .ok_or(GameError::NoteAlreadyExists(note.id))?;
            }
            EditNote { ref note } => {
                self.notes
                    .mutate(&note.id, |existing| *existing = note.clone())
                    .ok_or(GameError::NoteNotFound(note.id))?;
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

            // ** Scenes **
            CreateScene { scene: ref rscene } => {
                let scene = rscene.clone();
                self.scenes
                    .try_insert(scene)
                    .ok_or_else(|| GameError::SceneAlreadyExists(rscene.id))?;
            }
            EditSceneDetails {
                scene_id,
                ref details,
            } => {
                self.scenes
                    .mutate(&scene_id, move |scene| {
                        scene.name = details.name.clone();
                        scene.background_image_url = details.background_image_url.clone();
                        scene.background_image_offset = details.background_image_offset;
                        scene.background_image_scale = details.background_image_scale;
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
                    return Err(GameError::CreatureNotFound(creature_id.to_string()));
                }
                self.scenes
                    .mutate(&scene_id, move |scene| {
                        let entry = scene.creatures.get_mut(&creature_id);
                        let entry = entry.expect("Already checked that creature exists?!");
                        entry.1 = *visibility;
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            AddCreatureToScene {
                scene_id,
                creature_id,
                ref visibility,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.creatures
                            .insert(creature_id, (Point3::new(0, 0, 0), *visibility));
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            RemoveCreatureFromScene {
                scene_id,
                creature_id,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.creatures.remove(&creature_id);
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            AddSceneChallenge {
                scene_id,
                ref description,
                ref challenge,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.attribute_checks
                            .insert(description.clone(), challenge.clone());
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            RemoveSceneChallenge {
                scene_id,
                ref description,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.attribute_checks.remove(description);
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }

            SetFocusedSceneCreatures {
                scene_id,
                ref creatures,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| s.focused_creatures = creatures.clone())
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }

            RemoveSceneVolumeCondition {
                scene_id,
                condition_id,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.volume_conditions.remove(&condition_id);
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }

            EditSceneTerrain {
                scene_id,
                ref terrain,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| s.terrain = terrain.clone())
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            EditSceneHighlights {
                scene_id,
                ref highlights,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| s.highlights = highlights.clone())
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            EditSceneAnnotations {
                scene_id,
                ref annotations,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| s.annotations = annotations.clone())
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            EditSceneRelatedScenes {
                scene_id,
                ref related_scenes,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.related_scenes = related_scenes.clone()
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }
            EditSceneSceneHotspots {
                scene_id,
                ref scene_hotspots,
            } => {
                self.scenes
                    .mutate(&scene_id, move |s| {
                        s.scene_hotspots = scene_hotspots.clone()
                    })
                    .ok_or_else(|| GameError::SceneNotFound(scene_id))?;
            }

            // ** Classes & Abilities **
            CreateClass { ref class } => {
                self.classes.insert(class.clone());
            }
            EditClass { ref class } => {
                self.classes.mutate(&class.id, move |c| {
                    c.name = class.name.clone();
                    c.abilities = class.abilities.clone();
                    c.conditions = class.conditions.clone();
                    c.color = class.color.clone();
                    c.emoji = class.emoji.clone();
                });
            }
            CreateAbility { ref ability } => {
                self.abilities.insert(ability.clone());
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
            CreateCreature {
                creature: ref rcreature,
            } => {
                let creature = rcreature.clone();
                self.creatures
                    .try_insert(creature)
                    .ok_or_else(|| GameError::CreatureAlreadyExists(rcreature.id()))?;
            }
            EditCreatureDetails {
                creature_id,
                ref details,
            } => {
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
            EditCreature { ref creature } => {
                if !self.creatures.contains_key(&creature.id) {
                    return Err(GameError::CreatureNotFound(creature.id.to_string()));
                }
                self.creatures.insert(creature.clone());
            }
            AddCreatureToCombat {
                creature_id,
                initiative,
            } => {
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
            CreatureLog {
                creature_id,
                ref log,
            } => {
                let creature = self.get_creature(creature_id)?.creature.apply_log(log)?;
                self.creatures.mutate(&creature_id, |c| *c = creature);
            }
            StartCombat {
                ref scene_id,
                ref combatants,
            } => {
                for &(cid, _) in combatants {
                    self.check_creature_id(cid)?;
                }
                self.check_scene(*scene_id)?;
                self.current_combat = Some(Combat::new(*scene_id, combatants.clone())?);
            }
            StopCombat => {
                self.current_combat.take().ok_or(GameError::NotInCombat)?;
            }
            SetCreaturePos {
                ref scene_id,
                ref creature_id,
                ref pos,
            } => {
                let scene = self.get_scene(*scene_id)?.set_pos(*creature_id, *pos)?;
                self.scenes.insert(scene);
            }
            PathCreature {
                ref scene_id,
                ref creature_id,
                ref path,
            } => {
                let scene = {
                    let scene = self.get_scene(*scene_id)?;
                    let current_pos = scene.get_pos(*creature_id)?;
                    let dest = path.last().cloned().unwrap_or(current_pos);
                    scene.set_pos(*creature_id, dest)?
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
        }
        Ok(())
    }

    fn check_creature_id(&self, cid: CreatureID) -> Result<(), GameError> {
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

    fn is_in_combat(&self, cid: CreatureID) -> bool {
        match self.get_combat() {
            Ok(combat) => combat.combat.contains_creature(cid),
            Err(_) => false,
        }
    }

    fn get_creature(&self, cid: CreatureID) -> Result<DynamicCreature<'_, '_>, GameError> {
        self.dyn_creature(
            self.creatures
                .get(&cid)
                .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?,
        )
    }

    /// Only pub for tests.
    fn dyn_creature<'creature, 'game: 'creature>(
        &'game self,
        creature: &'creature Creature,
    ) -> Result<DynamicCreature<'creature, 'game>, GameError> {
        DynamicCreature::new(creature, self)
    }

    fn get_combat(&self) -> Result<DynamicCombat<'_>, GameError> {
        let combat = self.current_combat.as_ref().ok_or(GameError::NotInCombat)?;
        let scene = self.get_scene(combat.scene)?;
        Ok(DynamicCombat {
            scene,
            combat,
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
        &self,
        scene: SceneID,
        cid: CreatureID,
        abid: AbilityID,
        target: DecidedTarget,
    ) -> Result<ChangedGame, GameError> {
        let scene = self.get_scene(scene)?;
        self._act(scene, cid, abid, target, false)
    }

    fn _act(
        &self,
        scene: &Scene,
        cid: CreatureID,
        abid: AbilityID,
        target: DecidedTarget,
        in_combat: bool,
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

    fn creature_act(
        &self,
        creature: &DynamicCreature,
        scene: &Scene,
        ability: &Ability,
        target: DecidedTarget,
        mut change: ChangedGame,
        in_combat: bool,
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
            change =
                change.apply_creature(creature.id(), |c| c.creature.reduce_energy(ability.cost))?;
        }
        Ok(change)
    }

    fn resolve_creature_targets(
        &self,
        creature: &DynamicCreature,
        scene: &Scene,
        target: CreatureTarget,
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
        &self,
        scene: &Scene,
        actor_id: CreatureID,
        target: CreatureTarget,
        pt: Point3,
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
            _ => Err(GameError::InvalidTargetForTargetSpec(
                target,
                DecidedTarget::Point(pt),
            )),
        }
    }

    /// Calculate which *points* and which *creatures* will be affected by an ability targeted at a
    /// point.
    fn preview_volume_targets(
        &self,
        scene: &Scene,
        actor_id: CreatureID,
        ability_id: AbilityID,
        pt: Point3,
    ) -> Result<(Vec<CreatureID>, Vec<Point3>), GameError> {
        let ability = self.get_ability(ability_id)?;

        let cids = match ability.action {
            Action::Creature { target, .. } => {
                self.volume_creature_targets(scene, actor_id, target, pt)?
            }
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

    fn get_movement_options(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
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
    fn get_target_options(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        ability_id: AbilityID,
    ) -> Result<PotentialTargets, GameError> {
        let ability = self.get_ability(ability_id)?;

        use crate::types::{Action as A, CreatureTarget as CT};
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
            } => {
                unimplemented!("SomeCreaturesInVolumeInRange not implemented")
            }
        })
    }

    fn open_terrain_in_range(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        range: u32units::Length,
    ) -> Result<PotentialTargets, GameError> {
        let scene = self.get_scene(scene)?;
        let creature_pos = scene.get_pos(creature_id)?;
        let pts = self
            .tile_system
            .open_points_in_range(creature_pos, &scene.terrain, range);
        Ok(PotentialTargets::Points(pts))
    }

    fn creatures_in_range(
        &self,
        scene: SceneID,
        creature_id: CreatureID,
        distance: u32units::Length,
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

    fn get_class(&self, class: ClassID) -> Result<&Class, GameError> {
        self.classes
            .get(&class)
            .ok_or_else(|| GameError::ClassNotFound(class))
    }

    fn change(&self) -> ChangedGame {
        ChangedGame {
            game: self.clone(),
            logs: vec![],
        }
    }

    fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError> {
        let game = self.apply_log(&log)?;
        Ok(ChangedGame {
            game,
            logs: vec![log],
        })
    }

    fn change_with_logs(&self, logs: Vec<GameLog>) -> Result<ChangedGame, GameError> {
        let mut game = self.clone();
        for log in logs.iter() {
            game = game.apply_log(log)?;
        }
        Ok(ChangedGame { game, logs })
    }
}

pub trait ChangedGameExt {
    fn apply(&self, log: &GameLog) -> Result<ChangedGame, GameError>;

    fn apply_combat<'game, F>(&'game self, f: F) -> Result<ChangedGame, GameError>
    where
        F: FnOnce(DynamicCombat<'game>) -> Result<ChangedCombat<'game>, GameError>;

    fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedGame, GameError>
    where
        F: FnOnce(DynamicCreature) -> Result<ChangedCreature, GameError>;

    fn done(self) -> (Game, Vec<GameLog>);
}

impl ChangedGameExt for ChangedGame {
    fn apply(&self, log: &GameLog) -> Result<ChangedGame, GameError> {
        let mut new = self.clone();
        new.game = self.game.apply_log(log)?;
        Ok(new)
    }

    fn apply_combat<'game, F>(&'game self, f: F) -> Result<ChangedGame, GameError>
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

    fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedGame, GameError>
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

    fn done(self) -> (Game, Vec<GameLog>) {
        (self.game, self.logs)
    }
}
