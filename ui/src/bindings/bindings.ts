import type { Map } from 'immutable';
  import type {
     Point3, Highlights, NonEmpty, Annotations, SceneHotspots, RelatedScenes, SceneAttributeChecks, SceneCreatures, SceneInventory, SceneVolumeConditions, Terrain, SceneFocusedCreatures,
     GameAbilities, GameCreatures, GameClasses, GameScenes, GameItems, GamePlayers,
     CreatureAttributes, CreatureConditions, CreatureInventory

  } from '../PTTypes';

  export type FolderPath = Array<string>;

export interface FolderTree<T> {
        data: T;
        children: Map<string, FolderTree<T>>;
      }

export type GameID = string;

export interface GameIndex { game_idx: number, log_idx: number, }

export interface GameList { games: Array<[GameProfile, GameMetadata]>, }

export interface GameMetadata { name: string, }

export interface GameProfile { user_id: UserID, game_id: GameID, profile_name: PlayerID, role: Role, }

export type InvitationID = string;

export type Role = "GM" | "Player";

export type RPIGameRequest = { "t": "GMGetGame" } | { "t": "GMCommand", command: GMCommand, } | { "t": "GMGenerateInvitation" } | { "t": "GMListInvitations" } | { "t": "GMDeleteInvitation", invitation_id: InvitationID, } | { "t": "UploadImageFromURL", url: string, purpose: ImageType, } | { "t": "RequestUploadImage", purpose: ImageType, } | { "t": "PlayerCommand", command: PlayerCommand, } | { "t": "MovementOptions", scene_id: SceneID, creature_id: CreatureID, } | { "t": "CombatMovementOptions" } | { "t": "TargetOptions", scene_id: SceneID, creature_id: CreatureID, ability_id: AbilityID, } | { "t": "PreviewVolumeTargets", scene_id: SceneID, creature_id: CreatureID, ability_id: AbilityID, point: Point3, };

export type UserID = string;

export type ImageType = { "t": "BackgroundImage" } | { "t": "CreatureIcon" };

export interface AABB { x: number, y: number, z: number, }

export interface Ability { id: AbilityID, name: string, cost: Energy, action: Action, usable_ooc: boolean, }

export interface AbilityCreation { name: string, cost: Energy, action: Action, usable_ooc: boolean, }

export type AbilityID = string;

export interface AbilityStatus { ability_id: AbilityID, cooldown: number, }

export type Action = { "Creature": { effect: CreatureEffect, target: CreatureTarget, } } | { "SceneVolume": { effect: SceneEffect, target: SceneTarget, } };

export interface AppliedCondition { remaining: Duration, condition: Condition, }

export interface AttributeCheck { reliable: boolean, attr: AttrID, target: SkillLevel, }

export type AttrID = string;

export interface ChangedGame { game: Game, logs: Array<GameLog>, }

export interface Class { id: ClassID, name: string, abilities: Array<AbilityID>, conditions: Array<Condition>, color: string, }

export interface ClassCreation { name: string, abilities: Array<AbilityID>, conditions: Array<Condition>, color: string, }

export type ClassID = string;

export interface Combat { scene: SceneID, creatures: NonEmpty, movement_used: number, }

export type CombatLog = { "t": "ConsumeMovement", distance: number, } | { "t": "ChangeCreatureInitiative", creature_id: CreatureID, initiative: number, } | { "t": "EndTurn", creature_id: CreatureID, } | { "t": "ForceNextTurn" } | { "t": "ForcePrevTurn" } | { "t": "RerollInitiative", combatants: Array<[CreatureID, number]>, };

export type Condition = { "RecurringEffect": CreatureEffect } | "Dead" | "Incapacitated" | { "AddDamageBuff": HP } | "DoubleMaxMovement" | { "ActivateAbility": AbilityID };

export type ConditionID = string;

export interface CreatureData { id: CreatureID, name: string, speed: number, max_energy: Energy, cur_energy: Energy, abilities: Record<AbilityID, AbilityStatus>, class: ClassID, max_health: HP, cur_health: HP, conditions: CreatureConditions, note: string, bio: string, portrait_url: string, icon_url: string, attributes: CreatureAttributes, initiative: Dice, size: AABB, inventory: CreatureInventory, }

export interface CreatureCreation { name: string, class: ClassID, portrait_url: string, icon_url: string, note: string, bio: string, initiative: Dice, size: AABB, }

export type CreatureEffect = { "ApplyCondition": [Duration, Condition] } | { "Heal": Dice } | { "Damage": Dice } | { "MultiEffect": Array<CreatureEffect> } | { "GenerateEnergy": Energy };

export type CreatureID = string;

export type CreatureLog = { "t": "Damage", hp: HP, rolls: Array<number>, } | { "t": "Heal", hp: HP, rolls: Array<number>, } | { "t": "GenerateEnergy", energy: Energy, } | { "t": "ReduceEnergy", energy: Energy, } | { "t": "ApplyCondition", id: ConditionID, duration: Duration, condition: Condition, } | { "t": "DecrementConditionRemaining", id: ConditionID, } | { "t": "RemoveCondition", id: ConditionID, };

export type CreatureTarget = "Melee" | { "Range": number } | "Actor" | { "LineFromActor": { distance: number, } } | { "SomeCreaturesInVolumeInRange": { volume: Volume, maximum: number, range: number, } } | { "AllCreaturesInVolumeInRange": { volume: Volume, range: number, } };

export type DecidedTarget = { "Creature": CreatureID } | { "Creatures": Array<CreatureID> } | "Actor" | { "Point": Point3 };

export type Dice = { "Expr": { num: number, size: number, } } | { "Plus": [Dice, Dice] } | { "Flat": { value: number, } } | { "BestOf": [number, Dice] };

export type Duration = "Interminate" | { "Rounds": number };

export type Energy = number;

export interface FolderNode { scenes: Array<SceneID>, creatures: Array<CreatureID>, notes: Record<string, Note>, items: Array<ItemID>, abilities: Array<AbilityID>, classes: Array<ClassID>, }

export type FolderItemID = { "SceneID": SceneID } | { "CreatureID": CreatureID } | { "NoteID": string } | { "ItemID": ItemID } | { "AbilityID": AbilityID } | { "ClassID": ClassID } | { "SubfolderID": string };

export interface Game { current_combat: Combat | null, abilities: GameAbilities, creatures: GameCreatures, classes: GameClasses, tile_system: TileSystem, scenes: GameScenes, items: GameItems, campaign: FolderTree<FolderNode>, players: GamePlayers, active_scene: SceneID | null, }

export type GameLog = { "t": "LoadModule", name: string, source: ModuleSource, path: FolderPath, } | { "t": "SetActiveScene", id: SceneID | null, } | { "t": "RegisterPlayer", id: PlayerID, } | { "t": "GiveCreaturesToPlayer", player_id: PlayerID, creature_ids: Array<CreatureID>, } | { "t": "UnregisterPlayer", id: PlayerID, } | { "t": "RemoveCreaturesFromPlayer", player_id: PlayerID, creature_ids: Array<CreatureID>, } | { "t": "SetPlayerScene", player_id: PlayerID, scene_id: SceneID | null, } | { "t": "ChatFromGM", message: string, } | { "t": "ChatFromPlayer", player_id: PlayerID, message: string, } | { "t": "AttributeCheckResult", creature_id: CreatureID, attribute_check: AttributeCheck, actual: number, success: boolean, } | { "t": "CreateFolder", path: FolderPath, } | { "t": "RenameFolder", path: FolderPath, new_name: string, } | { "t": "MoveFolderItem", source: FolderPath, item_id: FolderItemID, destination: FolderPath, } | { "t": "CopyFolderItem", source: FolderPath, item_id: FolderItemID, dest: FolderPath, new_item_id: FolderItemID, } | { "t": "DeleteFolderItem", path: FolderPath, item_id: FolderItemID, } | { "t": "CreateItem", path: FolderPath, item: Item, } | { "t": "EditItem", item: Item, } | { "t": "CreateNote", path: FolderPath, note: Note, } | { "t": "EditNote", path: FolderPath, original_name: string, note: Note, } | { "t": "TransferItem", from: InventoryOwner, to: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "RemoveItem", owner: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "SetItemCount", owner: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "CreateScene", path: FolderPath, scene: Scene, } | { "t": "EditSceneDetails", scene_id: SceneID, details: SceneCreation, } | { "t": "SetSceneCreatureVisibility", scene_id: SceneID, creature_id: CreatureID, visibility: Visibility, } | { "t": "AddCreatureToScene", scene_id: SceneID, creature_id: CreatureID, visibility: Visibility, } | { "t": "RemoveCreatureFromScene", scene_id: SceneID, creature_id: CreatureID, } | { "t": "AddSceneChallenge", scene_id: SceneID, description: string, challenge: AttributeCheck, } | { "t": "RemoveSceneChallenge", scene_id: SceneID, description: string, } | { "t": "SetFocusedSceneCreatures", scene_id: SceneID, creatures: Array<CreatureID>, } | { "t": "RemoveSceneVolumeCondition", scene_id: SceneID, condition_id: ConditionID, } | { "t": "EditSceneTerrain", scene_id: SceneID, terrain: Terrain, } | { "t": "EditSceneHighlights", scene_id: SceneID, highlights: Highlights, } | { "t": "EditSceneAnnotations", scene_id: SceneID, annotations: Annotations, } | { "t": "EditSceneRelatedScenes", scene_id: SceneID, related_scenes: RelatedScenes, } | { "t": "EditSceneSceneHotspots", scene_id: SceneID, scene_hotspots: SceneHotspots, } | { "t": "CombatLog", log: CombatLog, } | { "t": "CreatureLog", creature_id: CreatureID, log: CreatureLog, } | { "t": "SetCreaturePos", scene_id: SceneID, creature_id: CreatureID, pos: Point3, } | { "t": "PathCreature", scene_id: SceneID, creature_id: CreatureID, path: Array<Point3>, } | { "t": "AddVolumeCondition", scene_id: SceneID, point: Point3, volume: Volume, condition_id: ConditionID, condition: Condition, duration: Duration, } | { "t": "StartCombat", scene_id: SceneID, combatants: Array<[CreatureID, number]>, } | { "t": "StopCombat" } | { "t": "CreateClass", path: FolderPath, class: Class, } | { "t": "EditClass", class: Class, } | { "t": "CreateAbility", path: FolderPath, ability: Ability, } | { "t": "EditAbility", ability: Ability, } | { "t": "CreateCreature", path: FolderPath, creature: CreatureData, } | { "t": "EditCreatureDetails", creature_id: CreatureID, details: CreatureCreation, } | { "t": "EditCreature", creature: CreatureData, } | { "t": "AddCreatureToCombat", creature_id: CreatureID, initiative: number, } | { "t": "RemoveCreatureFromCombat", creature_id: CreatureID, } | { "t": "Rollback", snapshot_index: number, log_index: number, };

export type GMCommand = { "t": "LoadModule", name: string, source: ModuleSource, game: Game, path: FolderPath, } | { "t": "ChatFromGM", message: string, } | { "t": "AttributeCheck", creature_id: CreatureID, attribute_check: AttributeCheck, } | { "t": "CreateFolder", path: FolderPath, } | { "t": "RenameFolder", path: FolderPath, new_name: string, } | { "t": "MoveFolderItem", source: FolderPath, item_id: FolderItemID, destination: FolderPath, } | { "t": "CopyFolderItem", source: FolderPath, item_id: FolderItemID, dest: FolderPath, } | { "t": "DeleteFolderItem", path: FolderPath, item_id: FolderItemID, } | { "t": "CreateItem", path: FolderPath, name: string, } | { "t": "EditItem", item: Item, } | { "t": "CreateNote", path: FolderPath, note: Note, } | { "t": "EditNote", path: FolderPath, original_name: string, note: Note, } | { "t": "TransferItem", from: InventoryOwner, to: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "RemoveItem", owner: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "SetItemCount", owner: InventoryOwner, item_id: ItemID, count: bigint, } | { "t": "CreateScene", path: FolderPath, scene: SceneCreation, } | { "t": "EditSceneDetails", scene_id: SceneID, details: SceneCreation, } | { "t": "SetSceneCreatureVisibility", scene_id: SceneID, creature_id: CreatureID, visibility: Visibility, } | { "t": "AddCreatureToScene", scene_id: SceneID, creature_id: CreatureID, visibility: Visibility, } | { "t": "RemoveCreatureFromScene", scene_id: SceneID, creature_id: CreatureID, } | { "t": "AddSceneChallenge", scene_id: SceneID, description: string, challenge: AttributeCheck, } | { "t": "RemoveSceneChallenge", scene_id: SceneID, description: string, } | { "t": "SetFocusedSceneCreatures", scene_id: SceneID, creatures: Array<CreatureID>, } | { "t": "RemoveSceneVolumeCondition", scene_id: SceneID, condition_id: ConditionID, } | { "t": "EditSceneTerrain", scene_id: SceneID, terrain: Terrain, } | { "t": "EditSceneHighlights", scene_id: SceneID, highlights: Highlights, } | { "t": "EditSceneAnnotations", scene_id: SceneID, annotations: Annotations, } | { "t": "EditSceneRelatedScenes", scene_id: SceneID, related_scenes: RelatedScenes, } | { "t": "EditSceneSceneHotspots", scene_id: SceneID, scene_hotspots: SceneHotspots, } | { "t": "StartCombat", scene_id: SceneID, combatants: Array<CreatureID>, } | { "t": "StopCombat" } | { "t": "AddCreatureToCombat", creature_id: CreatureID, } | { "t": "RemoveCreatureFromCombat", creature_id: CreatureID, } | { "t": "ChangeCreatureInitiative", creature_id: CreatureID, initiative: number, } | { "t": "RerollCombatInitiative" } | { "t": "ForceNextTurn" } | { "t": "ForcePrevTurn" } | { "t": "ActCreature", scene_id: SceneID, creature_id: CreatureID, ability_id: AbilityID, target: DecidedTarget, } | { "t": "CombatAct", ability_id: AbilityID, target: DecidedTarget, } | { "t": "PathCurrentCombatCreature", destination: Point3, } | { "t": "EndTurn" } | { "t": "CreateClass", path: FolderPath, class: ClassCreation, } | { "t": "EditClass", class: Class, } | { "t": "CreateAbility", path: FolderPath, ability: AbilityCreation, } | { "t": "EditAbility", ability: Ability, } | { "t": "CreateCreature", path: FolderPath, creature: CreatureCreation, } | { "t": "EditCreatureDetails", creature: CreatureData, } | { "t": "SetCreaturePos", scene_id: SceneID, creature_id: CreatureID, pos: Point3, } | { "t": "PathCreature", scene_id: SceneID, creature_id: CreatureID, destination: Point3, } | { "t": "RegisterPlayer", id: PlayerID, } | { "t": "GiveCreaturesToPlayer", player_id: PlayerID, creature_ids: Array<CreatureID>, } | { "t": "UnregisterPlayer", id: PlayerID, } | { "t": "RemoveCreaturesFromPlayer", player_id: PlayerID, creature_ids: Array<CreatureID>, } | { "t": "SetPlayerScene", player_id: PlayerID, scene_id: SceneID | null, } | { "t": "SetActiveScene", id: SceneID | null, } | { "t": "Rollback", snapshot_index: number, log_index: number, };

export type HP = number;

export type InventoryOwner = { "Scene": SceneID } | { "Creature": CreatureID };

export interface Item { id: ItemID, name: string, }

export type ItemID = string;

export type ModuleSource = "Module" | "SavedGame";

export interface Note { name: string, content: string, }

export interface Player { player_id: PlayerID, scene: SceneID | null, creatures: Array<CreatureID>, }

export type PlayerCommand = { "t": "ChatFromPlayer", message: string, } | { "t": "CreateNote", path: FolderPath, note: Note, } | { "t": "EditNote", path: FolderPath, original_name: string, note: Note, } | { "t": "PathCreature", creature_id: CreatureID, destination: Point3, } | { "t": "CombatAct", ability_id: AbilityID, target: DecidedTarget, } | { "t": "PathCurrentCombatCreature", destination: Point3, } | { "t": "EndTurn" };

export type PlayerID = string;

export type PotentialTargets = { "CreatureIDs": Array<CreatureID> } | { "Points": Array<Point3> };

export interface Scene { id: SceneID, name: string, terrain: Terrain, highlights: Highlights, annotations: Annotations, scene_hotspots: SceneHotspots, related_scenes: RelatedScenes, background_image_url: string, background_image_offset: [number, number] | null, background_image_scale: [number, number], creatures: SceneCreatures, attribute_checks: SceneAttributeChecks, inventory: SceneInventory, volume_conditions: SceneVolumeConditions, focused_creatures: SceneFocusedCreatures, }

export interface SceneCreation { name: string, background_image_url: string, background_image_offset: [number, number] | null, background_image_scale: [number, number], }

export type SceneEffect = { "CreateVolumeCondition": { duration: Duration, condition: Condition, } };

export type SceneID = string;

export type SceneTarget = { "RangedVolume": { volume: Volume, range: number, } };

export interface DynamicCreature { id: CreatureID, name: string, max_energy: Energy, cur_energy: Energy, class: ClassID, max_health: HP, cur_health: HP, note: string, bio: string, portrait_url: string, icon_url: string, attributes: CreatureAttributes, initiative: Dice, size: AABB, inventory: CreatureInventory, conditions: CreatureConditions, abilities: Record<AbilityID, AbilityStatus>, speed: number, own_conditions: CreatureConditions, volume_conditions: CreatureConditions, can_act: boolean, can_move: boolean, }

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";

export type TileSystem = "Realistic" | "DnD";

export type Visibility = "GMOnly" | "AllPlayers";

export type Volume = { "Sphere": number } | { "Line": { vector: Point3, } } | { "VerticalCylinder": { radius: number, height: number, } } | { "AABB": AABB };

export interface VolumeCondition { point: Point3, volume: Volume, remaining: Duration, condition: Condition, }

