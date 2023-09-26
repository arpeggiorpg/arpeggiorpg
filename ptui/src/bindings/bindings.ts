import type { Map } from 'immutable';
  import type {
     Point3, Highlights, NonEmpty, Annotations, SceneHotspots, RelatedScenes, SceneAttributeChecks, SceneCreatures, SceneInventory, SceneVolumeConditions, Terrain, SceneFocusedCreatures, GameAbilities, GameCreatures, GameClasses, GameScenes, GameItems, GamePlayers
  } from '../PTTypes';

  export interface AABB { x: number, y: number, z: number, }

export interface Ability { id: AbilityID, name: string, cost: Energy, action: Action, usable_ooc: boolean, }

export type AbilityID = string;

export interface AbilityStatus { ability_id: AbilityID, cooldown: number, }

export type Action = { Creature: { effect: CreatureEffect, target: CreatureTarget, } } | { SceneVolume: { effect: SceneEffect, target: SceneTarget, } };

export interface AppliedCondition { remaining: Duration, condition: Condition, }

export interface AttributeCheck { reliable: boolean, attr: AttrID, target: SkillLevel, }

export type AttrID = string;

export interface Class { id: ClassID, name: string, abilities: Array<AbilityID>, conditions: Array<Condition>, color: string, }

export type ClassID = string;

export interface Combat { scene: SceneID, creatures: NonEmpty, movement_used: number, }

export type CombatLog = { ConsumeMovement: number } | { ChangeCreatureInitiative: { creature_id: CreatureID, new_initiative: number, } } | { EndTurn: CreatureID } | "ForceNextTurn" | "ForcePrevTurn" | { RerollInitiative: Array<[CreatureID, number]> };

export type Condition = { RecurringEffect: CreatureEffect } | "Dead" | "Incapacitated" | { AddDamageBuff: HP } | "DoubleMaxMovement" | { ActivateAbility: AbilityID };

export type ConditionID = string;

export interface CreatureCreation { name: string, class: ClassID, portrait_url: string, icon_url: string, note: string, bio: string, initiative: Dice, size: AABB, }

export type CreatureEffect = { ApplyCondition: [Duration, Condition] } | { Heal: Dice } | { Damage: Dice } | { MultiEffect: Array<CreatureEffect> } | { GenerateEnergy: Energy };

export type CreatureID = string;

export type CreatureLog = { Damage: { hp: HP, rolls: Array<number>, } } | { Heal: { hp: HP, rolls: Array<number>, } } | { GenerateEnergy: Energy } | { ReduceEnergy: Energy } | { ApplyCondition: { id: ConditionID, duration: Duration, condition: Condition, } } | { DecrementConditionRemaining: ConditionID } | { RemoveCondition: ConditionID };

export type CreatureTarget = "Melee" | { Range: number } | "Actor" | { LineFromActor: { distance: number, } } | { SomeCreaturesInVolumeInRange: { volume: Volume, maximum: number, range: number, } } | { AllCreaturesInVolumeInRange: { volume: Volume, range: number, } };

export type DecidedTarget = { Creature: CreatureID } | { Creatures: Array<CreatureID> } | "Actor" | { Point: Point3 };

export type Dice = { Expr: { num: number, size: number, } } | { Plus: [Dice, Dice] } | { Flat: { value: number, } } | { BestOf: [number, Dice] };

export type Duration = "Interminate" | { Rounds: number };

export type Energy = number;

export interface FolderTree<T> {
        data: T;
        children: Map<string, FolderTree<T>>;
      }

export interface FolderNode { scenes: Array<SceneID>, creatures: Array<CreatureID>, notes: Record<string, Note>, items: Array<ItemID>, abilities: Array<AbilityID>, classes: Array<ClassID>, }

export type FolderItemID = { SceneID: SceneID } | { CreatureID: CreatureID } | { NoteID: string } | { ItemID: ItemID } | { AbilityID: AbilityID } | { ClassID: ClassID } | { SubfolderID: string };

export type FolderPath = Array<string>;

export interface Game { current_combat: Combat | null, abilities: GameAbilities, creatures: GameCreatures, classes: GameClasses, tile_system: TileSystem, scenes: GameScenes, items: GameItems, campaign: FolderTree<FolderNode>, players: GamePlayers, active_scene: SceneID | null, }

export type HP = number;

export type InventoryOwner = { Scene: SceneID } | { Creature: CreatureID };

export interface Item { id: ItemID, name: string, }

export type ItemID = string;

export type ModuleSource = "Module" | "SavedGame";

export interface Note { name: string, content: string, }

export interface Player { player_id: PlayerID, scene: SceneID | null, creatures: Array<CreatureID>, }

export type PlayerID = string;

export type PotentialTargets = { CreatureIDs: Array<CreatureID> } | { Points: Array<Point3> };

export interface Scene { id: SceneID, name: string, terrain: Terrain, highlights: Highlights, annotations: Annotations, scene_hotspots: SceneHotspots, related_scenes: RelatedScenes, background_image_url: string, background_image_offset: [number, number] | null, background_image_scale: [number, number], creatures: SceneCreatures, attribute_checks: SceneAttributeChecks, inventory: SceneInventory, volume_conditions: SceneVolumeConditions, focused_creatures: SceneFocusedCreatures, }

export interface SceneCreation { name: string, background_image_url: string, background_image_offset: [number, number] | null, background_image_scale: [number, number], }

export type SceneEffect = { CreateVolumeCondition: { duration: Duration, condition: Condition, } };

export type SceneID = string;

export type SceneTarget = { RangedVolume: { volume: Volume, range: number, } };

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";

export type TileSystem = "Realistic" | "DnD";

export type Visibility = "GMOnly" | "AllPlayers";

export type Volume = { Sphere: number } | { Line: { vector: Point3, } } | { VerticalCylinder: { radius: number, height: number, } } | { AABB: AABB };

export interface VolumeCondition { point: Point3, volume: Volume, remaining: Duration, condition: Condition, }

