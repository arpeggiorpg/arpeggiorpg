use std::collections::{HashMap, HashSet};

use foldertree::FolderPath;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::types::*;

/// Top-level commands that can be sent from a Player
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "t")]
pub enum PlayerCommand {
    // should these enums include PlayerID? probably not...
    ChatFromPlayer {
        message: String,
    },

    CreateNote {
        path: FolderPath,
        note: Note,
    },

    EditNote {
        // This FolderPath is scoped to the player's folder.
        path: FolderPath,
        original_name: String,
        note: Note,
    },

    // Out-of-combat actions:
    // /// Use an Ability out of combat.
    // ActCreature { // NYI
    //   creature_id: CreatureID,
    //   ability_id: AbilityID,
    //   target: DecidedTarget,
    // },
    /// Move a creature along a path within a scene.
    /// There must be a clear path according to the current loaded map. It doesn't matter whether
    /// the creature is in combat.
    PathCreature {
        creature_id: CreatureID,
        destination: Point3,
    },

    // In-combat actions:
    /// Make the current creature use an ability.
    CombatAct {
        ability_id: AbilityID,
        target: DecidedTarget,
    },
    /// Move the current creature in combat to a point.
    /// There must be a clear path according to the current loaded map.
    PathCurrentCombatCreature {
        destination: Point3,
    },
    /// End the current creature's turn.
    EndTurn,

    // ** Inventory management **
    /// Give an item from one of the player's creatures to another creature in the scene
    GiveItem {
        from_creature_id: CreatureID,
        to_creature_id: CreatureID,
        item_id: ItemID,
        count: u64,
    },
    /// Drop an item from one of the player's creatures into the current scene
    DropItem {
        creature_id: CreatureID,
        item_id: ItemID,
        count: u64,
    },
}

/// Top-level commands that can be sent from a GM to affect the state of the game.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[serde(tag = "t")]
pub enum GMCommand {
    LoadModule {
        name: String,
        source: ModuleSource,
        game: Game,
        path: FolderPath,
    },

    ChatFromGM {
        message: String,
    },

    AttributeCheck {
        creature_id: CreatureID,
        attribute_check: AttributeCheck,
    },

    /// Create a folder, given segments leading to it.
    CreateFolder {
        path: FolderPath,
    },
    /// Rename a folder. DEPRECATED (I think?)
    RenameFolder {
        path: FolderPath,
        new_name: String,
    },

    /// Move some object from one folder to another.
    MoveFolderItem {
        source: FolderPath,
        item_id: FolderItemID,
        destination: FolderPath,
    },
    /// Copy an object to a folder. It's okay to copy it to the same folder.
    CopyFolderItem {
        source: FolderPath,
        item_id: FolderItemID,
        dest: FolderPath,
    },
    DeleteFolderItem {
        path: FolderPath,
        item_id: FolderItemID,
    },
    RenameFolderItem {
        path: FolderPath,
        item_id: FolderItemID,
        new_name: String,
    },

    /// Create an Item in a folder. (this will probably take an ItemCreation in the future)
    CreateItem {
        path: FolderPath,
        name: String,
    },
    /// Edit an Item. The ID in the given Item must match an existing Item.
    EditItem {
        item: Item,
    },

    /// Create a Note inside of a Folder.
    CreateNote {
        path: FolderPath,
        note: Note,
    },
    /// Edit a Note inside of a Folder.
    EditNote {
        path: FolderPath,
        original_name: String,
        note: Note,
    },

    // ** Inventory management **
    // These work for creatures or scenes
    TransferItem {
        from: InventoryOwner,
        to: InventoryOwner,
        item_id: ItemID,
        count: u64,
    },
    RemoveItem {
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
    },
    SetItemCount {
        owner: InventoryOwner,
        item_id: ItemID,
        count: u64,
    },

    // ** Scene management **
    /// Create a Scene.
    CreateScene {
        path: FolderPath,
        scene: SceneCreation,
    },
    EditSceneDetails {
        scene_id: SceneID,
        details: SceneCreation,
    },
    SetSceneCreatureVisibility {
        scene_id: SceneID,
        creature_id: CreatureID,
        visibility: Visibility,
    },
    AddCreatureToScene {
        scene_id: SceneID,
        creature_id: CreatureID,
        visibility: Visibility,
    },
    RemoveCreatureFromScene {
        scene_id: SceneID,
        creature_id: CreatureID,
    },
    AddSceneChallenge {
        scene_id: SceneID,
        description: String,
        challenge: AttributeCheck,
    },
    RemoveSceneChallenge {
        scene_id: SceneID,
        description: String,
    },
    SetFocusedSceneCreatures {
        scene_id: SceneID,
        creatures: Vec<CreatureID>,
    },
    // AddSceneVolumeCondition {
    //   scene_id: SceneID,
    //   point: Point3,
    //   volume: Volume,
    //   condition: Condition,
    //   duration: Duration,
    // },
    RemoveSceneVolumeCondition {
        scene_id: SceneID,
        condition_id: ConditionID,
    },
    EditSceneTerrain {
        scene_id: SceneID,
        #[ts(type = "Terrain")]
        terrain: Vec<Point3>,
    },
    EditSceneHighlights {
        scene_id: SceneID,
        #[ts(type = "Highlights")]
        highlights: HashMap<Point3, (Color, Visibility)>,
    },
    EditSceneAnnotations {
        scene_id: SceneID,
        #[ts(type = "Annotations")]
        annotations: HashMap<Point3, (String, Visibility)>,
    },
    EditSceneRelatedScenes {
        scene_id: SceneID,
        #[ts(type = "RelatedScenes")]
        related_scenes: HashSet<SceneID>,
    },
    EditSceneSceneHotspots {
        scene_id: SceneID,
        #[ts(type = "SceneHotspots")]
        scene_hotspots: HashMap<Point3, SceneID>,
    },

    // ** Combat management **
    /// Start a combat with the specified creatures.
    StartCombat {
        scene_id: SceneID,
        combatants: Vec<CreatureID>,
    },
    /// Stop the current combat.
    StopCombat,
    /// Add a creature to combat.
    AddCreatureToCombat {
        creature_id: CreatureID,
    },
    /// Remove a creature from combat.
    RemoveCreatureFromCombat {
        creature_id: CreatureID,
    },
    /// Modify a creature's order in the combat list.
    ChangeCreatureInitiative {
        creature_id: CreatureID,
        initiative: i16,
    },
    /// Reroll initiative for all creatures in combat, and sort the combat list
    RerollCombatInitiative,
    /// Move to the next creature in the initiative list. This does *not* run any end-of-turn or
    /// start-turn events.
    ForceNextTurn,
    /// Move to the previous creature in the initiative list. This does *not* run any end-of-turn or
    /// start-turn events.
    ForcePrevTurn,

    // ** Combat **
    /// Use an Ability out of combat.
    ActCreature {
        scene_id: SceneID,
        creature_id: CreatureID,
        ability_id: AbilityID,
        target: DecidedTarget,
    },
    /// Make the current creature use an ability.
    CombatAct {
        ability_id: AbilityID,
        target: DecidedTarget,
    },
    /// Move the current creature in combat to a point.
    /// There must be a clear path according to the current loaded map.
    PathCurrentCombatCreature {
        destination: Point3,
    },
    /// End the current creature's turn.
    EndTurn,

    // ** Classes & Abilities **
    CreateClass {
        path: FolderPath,
        class: ClassCreation,
    },
    EditClass {
        class: Class,
    },
    CreateAbility {
        path: FolderPath,
        ability: AbilityCreation,
    },
    EditAbility {
        ability: Ability,
    },

    // ** Creature Manipulation **
    /// Create a new creature.
    CreateCreature {
        path: FolderPath,
        creature: CreatureCreation,
    },
    /// Edit an existing creature.
    EditCreatureDetails {
        creature: Creature,
    },
    /// Assign a creature's position within a scene.
    SetCreaturePos {
        scene_id: SceneID,
        creature_id: CreatureID,
        pos: Point3,
    },
    /// Move a creature along a path within a scene.
    /// There must be a clear path according to the current loaded map. It doesn't matter whether
    /// the creature is in combat.
    PathCreature {
        scene_id: SceneID,
        creature_id: CreatureID,
        destination: Point3,
    },

    // ** Player Manipulation **
    /// Register a player as available for controlling a creature.
    RegisterPlayer {
        id: PlayerID,
    },
    /// Give control of a creature to a player.
    GiveCreaturesToPlayer {
        player_id: PlayerID,
        creature_ids: Vec<CreatureID>,
    },
    /// Remove a player from the game, allowing all of their creatures to be given to other players.
    UnregisterPlayer {
        id: PlayerID,
    },
    /// Remove control of a creature from a player.
    RemoveCreaturesFromPlayer {
        player_id: PlayerID,
        creature_ids: Vec<CreatureID>,
    },
    /// Move a player to a particular scene, so they only see what's happening in that scene.
    /// Note that this doesn't have any affect on a player's *characters*.
    /// The scene name can be None (null) to not show any scene to the player.
    SetPlayerScene {
        player_id: PlayerID,
        scene_id: Option<SceneID>,
    },

    SetActiveScene {
        id: Option<SceneID>,
    },

    /// Roll back to a specific snapshot + log index
    Rollback {
        snapshot_index: usize,
        log_index: usize,
    },
}
