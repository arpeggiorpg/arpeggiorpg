import { Set, Map, List, ValueObject } from 'immutable';
import * as Z from "zod";

import type {
  AbilityID,
  ClassID,
  CreatureID,
  PlayerID,
  SceneID,
  ItemID,
  AttrID,
  ConditionID,
  HP,
  Energy,
  Note,
  Dice,
  AABB,
  AttributeCheck,
  Duration,
  Condition,
  CreatureEffect,
  SkillLevel,
  Volume
} from "./bindings/bindings";

export {
  AABB, AbilityID, AttributeCheck, AttrID, ClassID, Condition, ConditionID, CreatureEffect,
  CreatureID, Dice, Duration, HP, ItemID, Note, PlayerID, SceneID, SkillLevel, Volume
};

export type Color = string;
export type Distance = number;
export type FolderPath = Array<string>;
export type Terrain = Set<Point3>;
export type Highlights = Map<Point3, [Color, Visibility]>;
export type Annotations = Map<Point3, [string, Visibility]>;

export function folderPathToString(path: FolderPath): string {
  if (path.length === 0) {
    return "Campaign Root";
  }
  return encodeFolderPath(path);
}

export class Point3 implements ValueObject {
  constructor(public x: number, public y: number, public z: number) { }

  equals(other: Point3): boolean {
    return this.x === other.x && this.y === other.y && this.z === other.z;
  }

  hashCode(): number {
    return List([this.x, this.y, this.z]).hashCode();
  }

  toString(): string {
    return `${this.x}/${this.y}/${this.z}`;
  }
}

// Idea for a nicer constructor syntax, if I ever implement auto-generating this file:
//     const target = T.MkDecidedTarget.Creature({creature_id});
// as equivalent to
//     const target = {t: "DecidedTarget", creature_id};


export interface App {
  snapshots: Array<Snapshot>;
  current_game: Game;
}

export interface Snapshot { snapshot: {}; logs: Array<GameLog>; }

export interface Game {
  current_combat?: Combat | undefined;
  creatures: Map<CreatureID, Creature>;
  classes: Map<ClassID, Class>;
  items: { [index: string]: Item };
  scenes: Map<SceneID, Scene>;
  abilities: { [index: string]: Ability };
  campaign: Folder;
  players: Map<PlayerID, Player>;
}

export interface Combat {
  scene: SceneID;
  creatures: { cursor: number; data: Array<[CreatureID, number]> };
  movement_used: number;
}

export interface Ability {
  name: string;
  id: AbilityID;
  action: Action;
  cost: Energy;
  usable_ooc: boolean;
}

export type Action =
  // these variants also have an `effect` field but we don't use it in the client
  | { t: "Creature"; target: CreatureTarget }
  | { t: "SceneVolume"; target: SceneTarget }
  ;

export type CreatureTarget =
  | { t: "Melee" }
  | { t: "Range"; distance: Distance }
  | { t: "Actor" }
  | { t: "SomeCreaturesInVolumeInRange"; volume: Volume; maximum: number; range: Distance }
  | { t: "AllCreaturesInVolumeInRange"; volume: Volume; range: Distance }
  | { t: "LineFromActor"; distance: Distance }
  ;

export type SceneTarget =
  | { t: "RangedVolume"; volume: Volume; range: Distance; }

export type DecidedTarget =
  | { t: "Creature"; creature_id: CreatureID }
  | { t: "Creatures"; creature_ids: Array<CreatureID> }
  | { t: "Actor" }
  | { t: "Point"; point: Point3 };

export interface Folder {
  data: FolderNode;
  children: Map<string, Folder>;
}

export interface FolderNode {
  scenes: Array<SceneID>;
  creatures: Array<CreatureID>;
  notes: { [index: string]: Note };
  items: Array<ItemID>;
  abilities: Array<AbilityID>;
  classes: Array<ClassID>;
}

export type InventoryOwner =
  | { Creature: CreatureID }
  | { Scene: SceneID }
  ;

export type GameCommand =
  | { t: "SetActiveScene"; scene_id?: SceneID | undefined }
  | { t: "ChatFromGM"; message: string }
  | { t: "ChatFromPlayer"; player_id: PlayerID; message: string }
  | { t: "RegisterPlayer"; player_id: PlayerID }
  | { t: "GiveCreaturesToPlayer"; player_id: PlayerID; creature_ids: Array<CreatureID> }
  | { t: "CreateFolder"; path: FolderPath }
  | { t: "MoveFolderItem"; source: FolderPath; item_id: FolderItemID; dest: FolderPath }
  | { t: "CopyFolderItem"; source: FolderPath; item_id: FolderItemID; dest: FolderPath }
  | { t: "DeleteFolderItem"; location: FolderPath; item_id: FolderItemID }
  | { t: "CreateCreature"; path: FolderPath; spec: CreatureCreation }
  | { t: "EditCreatureDetails"; creature_id: CreatureID; details: CreatureCreation }
  | { t: "CreateItem"; path: FolderPath; name: string }
  | { t: "EditItem"; item: Item }
  | { t: "CreateNote"; path: FolderPath; note: Note }
  | { t: "EditNote"; path: FolderPath; name: string; note: Note }
  | { t: "TransferItem"; from: InventoryOwner; to: InventoryOwner; item_id: ItemID; count: number }
  | { t: "RemoveItem"; owner: InventoryOwner; item_id: ItemID; count: number }
  | { t: "SetItemCount"; owner: InventoryOwner; item_id: ItemID; count: number }
  | { t: "CreateScene"; path: FolderPath; spec: SceneCreation }
  | { t: "EditSceneDetails"; scene_id: SceneID; details: SceneCreation }
  | {
    t: "SetSceneCreatureVisibility";
    scene_id: SceneID; creature_id: CreatureID; visibility: Visibility;
  }
  | { t: "AddCreatureToScene"; scene_id: SceneID; creature_id: CreatureID; visibility: Visibility }
  | { t: "RemoveCreatureFromScene"; scene_id: SceneID; creature_id: CreatureID }
  | { t: "AddSceneChallenge"; scene_id: SceneID; description: string; challenge: AttributeCheck }
  | { t: "RemoveSceneChallenge"; scene_id: SceneID; description: string }
  | { t: "SetFocusedSceneCreatures"; scene_id: SceneID; creatures: List<CreatureID> }
  | { t: "RemoveSceneVolumeCondition"; scene_id: SceneID; condition_id: ConditionID }
  | { t: "EditSceneTerrain"; scene_id: SceneID; terrain: Terrain }
  | { t: "EditSceneHighlights"; scene_id: SceneID; highlights: Highlights }
  | { t: "EditSceneAnnotations"; scene_id: SceneID; annotations: Annotations }
  | { t: "EditSceneRelatedScenes"; scene_id: SceneID; related_scenes: Set<SceneID> }
  | { t: "EditSceneSceneHotspots"; scene_id: SceneID; scene_hotspots: Map<Point3, SceneID> }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatAct"; ability_id: AbilityID; target: DecidedTarget }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; dest: Point3 }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; dest: Point3 }
  | { t: "PathCurrentCombatCreature"; dest: Point3 }
  | { t: "Done" }
  | { t: "ChangeCreatureInitiative"; creature_id: CreatureID; init: number }
  | { t: "StartCombat"; scene_id: SceneID; creature_ids: Array<CreatureID> }
  | { t: "StopCombat" }
  | { t: "AddCreatureToCombat"; creature_id: CreatureID }
  | { t: "AttributeCheck"; creature_id: CreatureID; check: AttributeCheck }
  | { t: "SetPlayerScene"; player_id: PlayerID; scene_id?: SceneID | undefined }
  | { t: "Rollback"; snapshot_index: number; log_index: number }
  | { t: "LoadModule"; source: ModuleSource; name: string; path: FolderPath }
  ;

export type ModuleSource = 'Module' | 'SavedGame';

export interface CreatureCreation {
  name: string;
  class_: string;
  portrait_url: string;
  icon_url: string;
  note: string;
  bio: string;
  initiative: Dice;
  size: AABB;
}

export interface Class {
  id: ClassID;
  name: string;
  // abilities, conditions
  color: string;
}

export interface Player {
  player_id: PlayerID;
  scene?: SceneID;
  creatures: Array<CreatureID>;
}

export type GameLog =
  | { t: "SetActiveScene"; scene_id?: SceneID | undefined }
  | { t: "RegisterPlayer"; player_id: string }
  | { t: "UnregisterPlayer"; player_id: string }
  | { t: "GiveCreaturesToPlayer"; player_id: string; creature_ids: Array<CreatureID> }
  | { t: "RemoveCreaturesFromPlayer"; player_id: string; creature_ids: Array<CreatureID> }
  | { t: "SetPlayerScene"; player_id: string; scene_id?: SceneID | undefined }

  | { t: "ChatFromGM"; message: string }
  | { t: "ChatFromPlayer"; player_id: PlayerID; message: string }
  | {
    t: "AttributeCheckResult";
    cid: CreatureID;
    check: AttributeCheck;
    actual: number;
    success: boolean;
  }
  | { t: "CreateFolder"; path: FolderPath }
  | { t: "RenameFolder"; path: FolderPath; newName: string }
  | { t: "DeleteFolderItem"; path: FolderPath; item: FolderItemID }
  | { t: "MoveFolderItem"; path: FolderPath; item: FolderItemID; newPath: FolderPath }
  | { t: "CopyFolderItem"; source: FolderPath; item_id: FolderItemID; dest: FolderPath }
  | { t: "CreateItem"; path: FolderPath; item: Item }
  | { t: "EditItem"; item: Item }
  | { t: "CreateNote"; path: FolderPath; note: Note }
  | { t: "EditNote"; path: FolderPath; name: string; newNote: Note }
  | { t: "TransferItem"; from: InventoryOwner; to: InventoryOwner; item_id: ItemID; count: number }
  | { t: "RemoveItem"; owner: InventoryOwner; item_id: ItemID; count: number }
  | { t: "SetItemCount"; owner: InventoryOwner; item_id: ItemID; count: number }
  | { t: "CreateScene"; path: FolderPath; scene: Scene }
  | { t: "EditSceneDetails"; scene_id: SceneID; details: SceneCreation }
  | {
    t: "SetSceneCreatureVisibility";
    scene_id: SceneID; creature_id: CreatureID; visibility: Visibility;
  }
  | { t: "AddCreatureToScene"; scene_id: SceneID; creature_id: CreatureID; visibility: Visibility }
  | { t: "RemoveCreatureFromScene"; scene_id: SceneID; creature_id: CreatureID }
  | { t: "AddSceneChallenge"; scene_id: SceneID; description: string; challenge: AttributeCheck }
  | { t: "RemoveSceneChallenge"; scene_id: SceneID; description: string }
  | { t: "SetFocusedSceneCreatures"; scene_id: SceneID; creatures: List<CreatureID> }
  | { t: "RemoveSceneVolumeCondition"; scene_id: SceneID; condition_id: ConditionID }
  | { t: "EditSceneTerrain"; scene_id: SceneID; terrain: Terrain }
  | { t: "EditSceneHighlights"; scene_id: SceneID; highlights: Highlights }
  | { t: "EditSceneAnnotations"; scene_id: SceneID; annotations: Annotations }
  | { t: "EditSceneRelatedScenes"; scene_id: SceneID; related_scenes: Set<SceneID> }
  | { t: "EditSceneSceneHotspots"; scene_id: SceneID; scene_hotspots: Map<Point3, SceneID> }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; pos: Point3 }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; path: Array<Point3> }
  | { t: "CreateCreature"; path: FolderPath; creature: CreatureData }
  | { t: "EditCreatureDetails"; creature_id: CreatureID; details: CreatureCreation }
  | { t: "StartCombat"; scene: SceneID; creatures: Array<{ cid: CreatureID; init: number }> }
  | { t: "AddCreatureToCombat"; creature_id: CreatureID; init: number }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatLog"; log: CombatLog }
  | { t: "CreatureLog"; creature_id: CreatureID; log: CreatureLog }
  | { t: "StopCombat" }
  | { t: "Rollback"; snapshot_index: number; log_index: number }
  | { t: "LoadModule"; source: ModuleSource; name: string; path: FolderPath } // `module` is left out
  ;

export type CombatLog =
  | { t: "ConsumeMovement"; distance: Distance }
  | { t: "ChangeCreatureInitiative"; creature_id: CreatureID; init: number }
  | { t: "EndTurn"; creature_id: CreatureID }
  | { t: "ForceNextTurn" }
  | { t: "ForcePrevTurn" }
  | { t: "RerollInitiative"; combatants: Array<[CreatureID, number]> };

export type CreatureLog =
  | { t: "Damage"; hp: HP; rolls: Array<number> }
  | { t: "Heal"; hp: HP; rolls: Array<number> }
  | { t: "GenerateEnergy"; energy: Energy }
  | { t: "ReduceEnergy"; energy: Energy }
  | { t: "ApplyCondition"; condition_id: ConditionID; duration: Duration }
  | { t: "DecrementConditionRemaining"; condition_id: ConditionID }
  | { t: "RemoveCondition"; condition_id: ConditionID };

export interface Item {
  id: ItemID;
  name: string;
}

export interface AppliedCondition {
  remaining: Duration;
  condition: Condition;
}

export interface AbilityStatus {
  ability_id: AbilityID;
  cooldown: number;
}

export class Creature {
  constructor(
    public id: CreatureID,
    public name: string,
    public speed: Distance,
    public max_energy: Energy,
    public cur_energy: Energy,
    public abilities: { [index: string]: AbilityStatus },
    public class_: string,
    public max_health: HP,
    public cur_health: HP,
    public own_conditions: Map<ConditionID, AppliedCondition>,
    public volume_conditions: Map<ConditionID, AppliedCondition>,
    public note: string,
    public bio: string,
    public portrait_url: string,
    public icon_url: string,
    public attributes: Map<AttrID, SkillLevel>,
    public initiative: Dice,
    public inventory: Map<ItemID, number>,
    public size: AABB,
  ) { }

  dynamic_conditions(): Map<ConditionID, AppliedCondition> {
    return this.own_conditions.merge(this.volume_conditions);
  }
}

export interface CreatureData {
  name: string;
}

export type FolderItemID =
  | { t: "SceneID"; id: SceneID }
  | { t: "CreatureID"; id: CreatureID }
  | { t: "NoteID"; id: string }
  | { t: "SubfolderID"; id: string }
  | { t: "ItemID"; id: ItemID }
  | { t: "AbilityID"; id: AbilityID }
  | { t: "ClassID"; id: ClassID }
  ;

export const SKILL_LEVELS: Array<SkillLevel> =
  ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

export interface SceneCreation {
  name: string;
  background_image_url: string;
  background_image_offset?: [number, number] | undefined;
  background_image_scale: [number, number];
}

export interface Scene {
  id: SceneID;
  name: string;
  terrain: Terrain;
  highlights: Highlights;
  annotations: Annotations;
  scene_hotspots: Map<Point3, SceneID>;
  related_scenes: Set<SceneID>;
  creatures: Map<CreatureID, [Point3, Visibility]>;
  attribute_checks: Map<string, AttributeCheck>;
  inventory: Map<ItemID, number>;
  background_image_url: string;
  background_image_offset?: [number, number] | undefined;
  background_image_scale: [number, number];
  volume_conditions: Map<ConditionID, VolumeCondition>;
  focused_creatures: List<CreatureID>;
}

export interface VolumeCondition {
  point: Point3;
  volume: Volume;
  remaining: Duration;
  condition: Condition;
}

export type Visibility =
  | { t: "GMOnly" }
  | { t: "AllPlayers" };

export type PotentialTargets =
  | { t: "CreatureIDs"; cids: Array<CreatureID> }
  | { t: "Points"; points: Array<Point3> }
  ;

export type RustResult<T, E> =
  | { t: "Ok"; result: T }
  | { t: "Err"; error: E }
  ;

// ** Decoders **

type Decoder<T> = Z.ZodType<T, Z.ZodTypeDef, any>;

export function parsePoint3(str: string): Point3 {
  const segments = str.split("/");
  if (segments.length !== 3) {
    throw new Error(`${str} did not have three segments separated by /`);
  }
  const [xs, ys, zs] = segments;
  return new Point3(Number(xs), Number(ys), Number(zs));
}


export const decodePoint3 = Z.string().transform(parsePoint3);
export const arrayOfPoint3 = Z.array(decodePoint3);

export const decodePotentialTargets: Decoder<PotentialTargets> = Z.union([
  Z.object({CreatureIDs: Z.array(Z.string())}).transform(({CreatureIDs: cids}): PotentialTargets => ({t: "CreatureIDs", cids})),
  Z.object({Points: Z.array(decodePoint3)}).transform(({Points: points}): PotentialTargets => ({t: "Points", points}))
]);

const decodeDice: Decoder<Dice> = Z.lazy(() => Z.union([
  Z.object({Flat: Z.object({value: Z.number()})}),
  Z.object({Expr: Z.object({num: Z.number(), size: Z.number()})}),
  Z.object({Plus: Z.tuple([decodeDice, decodeDice])}),
  Z.object({BestOf: Z.tuple([Z.number(), decodeDice])}),
]));

const decodeDuration: Decoder<Duration> = Z.union([
  Z.literal("Interminate"),
  Z.object({Rounds: Z.number()}),
]);

const decodeEffect: Decoder<CreatureEffect> = Z.union([
  Z.object({ApplyCondition: Z.tuple([decodeDuration, Z.lazy(() => decodeCondition)])}),
  Z.object({Damage: decodeDice}),
  Z.object({GenerateEnergy: Z.number()}),
  Z.object({Heal: decodeDice}),
  Z.object({MultiEffect: Z.array(Z.lazy(() => decodeEffect))}),
]);

const decodeCondition: Decoder<Condition> = Z.union([
  Z.literal("Dead"),
  Z.literal("DoubleMaxMovement"),
  Z.literal("Incapacitated"),
  Z.object({ActivateAbility: Z.string()}),
  Z.object({RecurringEffect: decodeEffect}),
]);


const decodeAppliedCondition: Decoder<AppliedCondition> = Z.object({ remaining: decodeDuration, condition: decodeCondition});

export const decodeSkillLevel: Decoder<SkillLevel> = Z.union([
  Z.literal("Inept"),
  Z.literal("Unskilled"),
  Z.literal("Skilled"),
  Z.literal("Expert"),
  Z.literal("Supernatural"),
]);

const decodeAbilityStatus: Decoder<AbilityStatus> = Z.object({
  ability_id: Z.string(),
  cooldown: Z.number(),
});

const decodeAABB: Decoder<AABB> = Z.object({
  x: Z.number(),
  y: Z.number(),
  z: Z.number(),
});

export const decodeCreatureData: Decoder<CreatureData> = Z.object({name: Z.string()});

export const decodeCreature: Decoder<Creature> = Z.object({
  id: Z.string(),
  name: Z.string(),
  speed: Z.number(),
  max_energy: Z.number(),
  cur_energy: Z.number(),
  abilities: Z.record(decodeAbilityStatus),
  class: Z.string(),
  max_health: Z.number(),
  cur_health: Z.number(),
  own_conditions: Z.record(decodeAppliedCondition).transform<Creature['own_conditions']>(Map),
  volume_conditions: Z.record(decodeAppliedCondition).transform<Creature['volume_conditions']>(Map),
  note: Z.string(),
  bio: Z.string(),
  portrait_url: Z.string(),
  icon_url: Z.string(),
  attributes: Z.record(decodeSkillLevel).transform<Creature['attributes']>(Map),
  initiative: decodeDice,
  inventory: Z.record(Z.number()).transform<Creature['inventory']>(Map),
  size: decodeAABB,
}).transform(({
    id, name, speed, max_energy, cur_energy, abilities, class: class_, max_health, cur_health,
    own_conditions, volume_conditions, note, bio, portrait_url, icon_url, attributes, initiative,
    inventory, size}) =>
    new Creature(
      id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health,
      own_conditions, volume_conditions, note, bio, portrait_url, icon_url, attributes, initiative,
      inventory, size
    )
);

const decodeCreatureCreation: Decoder<CreatureCreation> = Z.object({
  name: Z.string(),
  class: Z.string(),
  portrait_url: Z.string(),
  icon_url: Z.string(),
  note: Z.string(),
  bio: Z.string(),
  initiative: decodeDice,
  size: decodeAABB,
}).transform(({class: class_, ...o}): CreatureCreation => ({...o, class_}));

export const decodeVisibility: Decoder<Visibility> = Z.union([
  Z.literal("GMOnly"),
  Z.literal("AllPlayers")
]).transform((s): Visibility => ({t: s}));

export const decodeAttributeCheck: Decoder<AttributeCheck> = Z.object({
  reliable: Z.boolean(),
  attr: Z.string(),
  target: decodeSkillLevel,
});

const decodeSceneCreation: Decoder<SceneCreation> = Z.object({
  name: Z.string(),
  background_image_url: Z.string(),
  background_image_scale: Z.tuple([Z.number(), Z.number()]),
  background_image_offset: maybe(Z.tuple([Z.number(), Z.number()])),
});

const decodeVolume: Decoder<Volume> = Z.union([
  Z.object({Sphere: Z.number()}),
  Z.object({Line: Z.object({vector: decodePoint3})}),
  Z.object({VerticalCylinder: Z.object({ radius: Z.number(), height: Z.number()})}),
  Z.object({AABB: decodeAABB}),
]);

export const decodeVolumeCondition: Decoder<VolumeCondition> = Z.object({
  point: decodePoint3,
  volume: decodeVolume,
  remaining: decodeDuration,
  condition: decodeCondition,
});

const decodeTerrain: Decoder<Terrain> = decodeSet(decodePoint3);
const decodeHighlights: Decoder<Highlights> =
  decodeIMap(decodePoint3, Z.tuple([Z.string(), decodeVisibility]));
const decodeAnnotations: Decoder<Annotations> = decodeHighlights;

export const decodeScene: Decoder<Scene> = Z.object({
  id: Z.string(),
  name: Z.string(),
  terrain: decodeTerrain,
  highlights: decodeHighlights,
  annotations: decodeAnnotations,
  scene_hotspots: decodeIMap(decodePoint3, Z.string()),
  related_scenes: decodeSet(Z.string()),
  creatures: Z.record(Z.tuple([decodePoint3, decodeVisibility])).transform<Scene['creatures']>(Map),
  attribute_checks: Z.record(decodeAttributeCheck).transform<Scene['attribute_checks']>(Map),
  inventory: Z.record(Z.number()).transform<Scene['inventory']>(Map),
  background_image_url: Z.string(),
  background_image_offset: maybe(Z.tuple([Z.number(), Z.number()])),
  background_image_scale: Z.tuple([Z.number(), Z.number()]),
  volume_conditions: Z.record(decodeVolumeCondition).transform<Scene['volume_conditions']>(Map),
  focused_creatures: Z.array(Z.string()).transform<Scene['focused_creatures']>(List),
});

function _mkFolderItem(t: FolderItemID['t']): Decoder<FolderItemID> {
  // "!": the type of `o` in the transform is only {[key: string]: string}, which means typescript
  // cannot know that o[t] exists in the face of noUncheckedIndexAccess. There may be a way to type
  // `o` better but that might also require a type ascription.
  return Z.object({[t]: Z.string()}).transform((o): FolderItemID => ({t, id: o[t]!}))
}

const decodeFolderItemID: Decoder<FolderItemID> = Z.union([
  _mkFolderItem("SceneID"),
  _mkFolderItem("CreatureID"),
  _mkFolderItem("NoteID"),
  _mkFolderItem("ItemID"),
  _mkFolderItem("AbilityID"),
  _mkFolderItem("ClassID"),
  _mkFolderItem("SubfolderID"),
]);

const decodeFolderPath: Decoder<FolderPath> = Z.string().transform(strpath => {
  if (strpath === "") {
    return [];
  } else if (strpath.startsWith("/")) {
    return strpath.split("/").slice(1);
  } else {
    throw new Error(`Not a path: ${strpath}.`);
  }
});

const decodeItem: Decoder<Item> = Z.object({
  id: Z.string(),
  name: Z.string(),
});

const decodeNote: Decoder<Note> = Z.object({
  name: Z.string(),
  content: Z.string(),
})

export const decodeInventoryOwner: Decoder<InventoryOwner> = Z.union([
  Z.object({Scene: Z.string()}),
  Z.object({Creature: Z.string()}),
]);

const decodeModuleSource: Decoder<ModuleSource> = Z.union([Z.literal("Module"), Z.literal("SavedGame")]);

const decodeCreatureLog: Decoder<CreatureLog> = Z.union([
  Z.object({Damage: Z.tuple([Z.number(), Z.array(Z.number())])}).transform(
    ({Damage: [hp, rolls]}): CreatureLog => ({ t: "Damage", hp, rolls })),
  Z.object({Heal: Z.tuple([Z.number(), Z.array(Z.number())])}).transform(
    ({Heal: [hp, rolls]}): CreatureLog => ({ t: "Heal", hp, rolls })),
  Z.object({GenerateEnergy: Z.number()}).transform(
    ({GenerateEnergy: energy}): CreatureLog => ({ t: "GenerateEnergy", energy })),
  Z.object({ReduceEnergy: Z.number()}).transform(({ReduceEnergy: energy}): CreatureLog => ({ t: "ReduceEnergy", energy })),
  Z.object({ApplyCondition: Z.tuple([Z.string(), decodeDuration])}).transform(
    ({ApplyCondition: [condition_id, duration]}): CreatureLog => ({ t: "ApplyCondition", condition_id, duration })),
  Z.object({DecrementConditionRemaining: Z.string()}).transform(
    ({DecrementConditionRemaining: condition_id}): CreatureLog => ({ t: "DecrementConditionRemaining", condition_id })),
  Z.object({RemoveCondition: Z.string()}).transform(
    ({RemoveCondition: condition_id}): CreatureLog => ({ t: "RemoveCondition", condition_id })),
]);

const decodeCombatLog: Decoder<CombatLog> = Z.union([
  Z.literal("ForceNextTurn").transform((): CombatLog => ({t: "ForceNextTurn"})),
  Z.literal("ForcePrevTurn").transform((): CombatLog => ({t: "ForcePrevTurn"})),
  Z.object({ConsumeMovement: Z.number()}).transform(
    ({ConsumeMovement: distance}): CombatLog => ({ t: "ConsumeMovement", distance})),
  Z.object({ChangeCreatureInitiative: Z.tuple([Z.string(), Z.number()])}).transform(
    ({ChangeCreatureInitiative: [creature_id, init]}): CombatLog => ({ t: "ChangeCreatureInitiative", creature_id, init })),
  Z.object({EndTurn: Z.string()}).transform(
    ({EndTurn: creature_id}): CombatLog => ({ t: "EndTurn", creature_id })),
  Z.object({RerollInitiative: Z.array(Z.tuple([Z.string(), Z.number()]))}).transform(
    ({RerollInitiative: combatants}): CombatLog => ({ t: "RerollInitiative", combatants })),
]);

const decodeCreatureIDAndInit = Z.tuple([Z.string(), Z.number()]).transform(([cid, init]) => ({cid, init}));

export const decodeGameLog: Decoder<GameLog> = Z.union([
  Z.literal("StopCombat").transform((): GameLog => ({t: "StopCombat"})),
  Z.object({SetActiveScene: Z.string()}).transform(({SetActiveScene: scene_id}): GameLog => ({ t: "SetActiveScene", scene_id })),
  Z.object({RegisterPlayer: Z.string()}).transform(({RegisterPlayer: player_id}): GameLog => ({ t: "RegisterPlayer", player_id })),
  Z.object({UnregisterPlayer: Z.string()}).transform(({UnregisterPlayer: player_id}): GameLog => ({ t: "UnregisterPlayer", player_id })),
  Z.object({GiveCreaturesToPlayer: Z.tuple([Z.string(), Z.array(Z.string())])}).transform(
    ({GiveCreaturesToPlayer: [player_id, creature_ids]}): GameLog =>
      ({ t: "GiveCreaturesToPlayer", player_id, creature_ids })),
  Z.object({RemoveCreaturesFromPlayer: Z.tuple([Z.string(), Z.array(Z.string())])}).transform(
    ({RemoveCreaturesFromPlayer: [player_id, creature_ids]}): GameLog =>
      ({ t: "RemoveCreaturesFromPlayer", player_id, creature_ids })),
  Z.object({SetPlayerScene: Z.tuple([Z.string(), maybe(Z.string())])}).transform(
    ({SetPlayerScene: [player_id, scene_id]}): GameLog => ({ t: "SetPlayerScene", player_id, scene_id })),
  Z.object({ChatFromGM: Z.string()}).transform(({ChatFromGM: message}): GameLog => ({ t: "ChatFromGM", message })),
  Z.object({ChatFromPlayer: Z.tuple([Z.string(), Z.string()])}).transform(
    ({ChatFromPlayer: [player_id, message]}): GameLog => ({ t: "ChatFromPlayer", player_id, message })),
  Z.object({StartCombat: Z.tuple([ Z.string(), Z.array(decodeCreatureIDAndInit) ])}).transform(
      ({StartCombat: [scene, creatures]}): GameLog => ({ t: "StartCombat", scene, creatures })),
  Z.object({CreateFolder: decodeFolderPath}).transform(({CreateFolder: path}): GameLog => ({ t: "CreateFolder", path })),
  Z.object({RenameFolder: Z.tuple([decodeFolderPath, Z.string()])}).transform(
    ({RenameFolder: [path, newName]}): GameLog => ({ t: "RenameFolder", path, newName })),
  Z.object({DeleteFolderItem: Z.tuple([decodeFolderPath, decodeFolderItemID])}).transform(
    ({DeleteFolderItem: [path, item]}): GameLog => ({ t: "DeleteFolderItem", path, item })),
  Z.object({MoveFolderItem: Z.tuple([decodeFolderPath, decodeFolderItemID, decodeFolderPath])}).transform(
    ({MoveFolderItem: [path, item, newPath]}): GameLog => ({ t: "MoveFolderItem", path, item, newPath })),
  Z.object({
    CopyFolderItem: Z.object({
      source: decodeFolderPath, item_id: decodeFolderItemID, dest: decodeFolderPath
    })
  }).transform(({CopyFolderItem: o}): GameLog => ({ t: "CopyFolderItem", ...o })),
  Z.object({CreateItem: Z.tuple([decodeFolderPath, decodeItem])}).transform(
    ({CreateItem: [path, item]}): GameLog => ({ t: "CreateItem", path, item })),
  Z.object({EditItem: decodeItem}).transform(({EditItem: item}): GameLog => ({ t: "EditItem", item })),
  Z.object({CreateNote: Z.tuple([decodeFolderPath, decodeNote])}).transform(
    ({CreateNote: [path, note]}): GameLog => ({ t: "CreateNote", path, note })),
  Z.object({EditNote: Z.tuple([decodeFolderPath, Z.string(), decodeNote])}).transform(
    ({EditNote: [path, name, newNote]}): GameLog => ({ t: "EditNote", path, name, newNote })),
  Z.object({TransferItem: Z.object({
      from: decodeInventoryOwner,
      to: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.number(),
  })}).transform(({TransferItem: o}): GameLog => ({t: "TransferItem", ...o})),
  Z.object({RemoveItem: Z.object({
      owner: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.number(),
  })}).transform(({RemoveItem: o}): GameLog => ({t: "RemoveItem", ...o})),
  Z.object({SetItemCount: Z.object({
      owner: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.number(),
  })}).transform(({SetItemCount: o}): GameLog => ({t: "SetItemCount", ...o})),
  Z.object({CreateScene: Z.tuple([decodeFolderPath, decodeScene])}).transform(
      ({CreateScene: [path, scene]}): GameLog => ({ t: "CreateScene", path, scene })),
  Z.object({EditSceneDetails: Z.object({ scene_id: Z.string(), details: decodeSceneCreation })}).transform(
    ({EditSceneDetails: o}): GameLog => ({t: "EditSceneDetails", ...o})),
  Z.object({SetSceneCreatureVisibility: Z.object({
      scene_id: Z.string(),
      creature_id: Z.string(),
      visibility: decodeVisibility
  })}).transform(
      ({SetSceneCreatureVisibility: o}): GameLog => ({ t: "SetSceneCreatureVisibility", ...o })),
  Z.object({AddCreatureToScene: Z.object({
    scene_id: Z.string(),
    creature_id: Z.string(),
    visibility: decodeVisibility,
  })}).transform(({AddCreatureToScene: o}): GameLog => ({t: "AddCreatureToScene", ...o})),
  Z.object({RemoveCreatureFromScene: Z.object({
    scene_id: Z.string(),
    creature_id: Z.string(),
  })}).transform(
    ({RemoveCreatureFromScene: o}): GameLog => ({ t: "RemoveCreatureFromScene", ...o })
  ),
  Z.object({AddSceneChallenge: Z.object({
    scene_id: Z.string(),
    description: Z.string(),
    challenge: decodeAttributeCheck,
  })}).transform(
    ({AddSceneChallenge: o}): GameLog => ({ t: "AddSceneChallenge", ...o })),
  Z.object({RemoveSceneChallenge: Z.object({
    scene_id: Z.string(),
    description: Z.string(),
  })}).transform(
    ({RemoveSceneChallenge: o}): GameLog => ({ t: "RemoveSceneChallenge", ...o })),
  Z.object({SetFocusedSceneCreatures: Z.object({
    scene_id: Z.string(),
    creatures: Z.array(Z.string()).transform(List),
  })}).transform(
    ({SetFocusedSceneCreatures: o}): GameLog => ({ t: "SetFocusedSceneCreatures", ...o })),
  Z.object({RemoveSceneVolumeCondition: Z.object({
    scene_id: Z.string(),
    condition_id: Z.string(),
  })}).transform(
    ({RemoveSceneVolumeCondition: o}): GameLog => ({ t: "RemoveSceneVolumeCondition", ...o })),
  Z.object({EditSceneTerrain: Z.object({
    scene_id: Z.string(),
    terrain: decodeTerrain,
  })}).transform(
    ({EditSceneTerrain: o}): GameLog => ({ t: "EditSceneTerrain", ...o})),
  Z.object({EditSceneHighlights: Z.object({
    scene_id: Z.string(),
    highlights: decodeHighlights,
  })}).transform(
    ({EditSceneHighlights: o}): GameLog => ({ t: "EditSceneHighlights", ...o})),
  Z.object({EditSceneAnnotations: Z.object({
    scene_id: Z.string(),
    annotations: decodeAnnotations,
  })}).transform(
    ({EditSceneAnnotations: o}): GameLog => ({ t: "EditSceneAnnotations", ...o})),
  Z.object({EditSceneRelatedScenes: Z.object({
    scene_id: Z.string(),
    related_scenes: decodeSet(Z.string()),
  })}).transform(
    ({EditSceneRelatedScenes: o}): GameLog => ({ t: "EditSceneRelatedScenes", ...o})),
  Z.object({EditSceneSceneHotspots: Z.object({
    scene_id: Z.string(),
    scene_hotspots: decodeIMap(decodePoint3, Z.string()),
  })}).transform(
    ({EditSceneSceneHotspots: o}): GameLog => ({ t: "EditSceneSceneHotspots", ...o})),

  Z.object({SetCreaturePos: Z.tuple([Z.string(), Z.string(), decodePoint3])}).transform(
    ({SetCreaturePos: [scene_id, creature_id, pos]}): GameLog => ({ t: "SetCreaturePos", scene_id, creature_id, pos})),
  Z.object({PathCreature: Z.tuple([Z.string(), Z.string(), Z.array(decodePoint3)])}).transform(
    ({PathCreature: [scene_id, creature_id, path]}): GameLog => ({ t: "PathCreature", scene_id, creature_id, path })),

  Z.object({CreateCreature: Z.tuple([decodeFolderPath, decodeCreatureData])}).transform(
    ({CreateCreature: [path, creature]}): GameLog => ({ t: "CreateCreature", path, creature})),
  Z.object({EditCreatureDetails: Z.object({creature_id: Z.string(), details: decodeCreatureCreation})}).transform(
    ({EditCreatureDetails: o}): GameLog => ({ t: "EditCreatureDetails", ...o })),
  Z.object({AddCreatureToCombat: Z.tuple([Z.string(), Z.number()])}).transform(
    ({AddCreatureToCombat: [creature_id, init]}): GameLog => ({ t: "AddCreatureToCombat", creature_id, init})),
  Z.object({RemoveCreatureFromCombat: Z.string()}).transform(
    ({RemoveCreatureFromCombat: creature_id}): GameLog => ({ t: "RemoveCreatureFromCombat", creature_id})),
  Z.object({CombatLog: decodeCombatLog}).transform(
    ({CombatLog: log}): GameLog => ({ t: "CombatLog", log})),
  Z.object({CreatureLog: Z.tuple([Z.string(), decodeCreatureLog])}).transform(
    ({CreatureLog: [creature_id, log]}): GameLog => ({ t: "CreatureLog", creature_id, log})),
  Z.object({AttributeCheckResult: Z.tuple([Z.string(), decodeAttributeCheck, Z.number(), Z.boolean()])}).transform(
    ({AttributeCheckResult: [cid, check, actual, success]}): GameLog => ({ t: "AttributeCheckResult", cid, check, actual, success})),
  Z.object({Rollback: Z.tuple([Z.number(), Z.number()])}).transform(
    ({Rollback: [snapshot_index, log_index]}): GameLog => ({ t: "Rollback", snapshot_index, log_index})),

  Z.object({LoadModule: Z.object({name: Z.string(), path: decodeFolderPath, source: decodeModuleSource})}).transform(
    ({LoadModule: o}): GameLog => ({ t: "LoadModule", ...o })),
]);

const decodePlayer: Decoder<Player> = Z.object({
  player_id: Z.string(),
  scene: maybe(Z.string()),
  creatures: Z.array(Z.string()),
});

const decodeClass: Decoder<Class> = Z.object({
  id: Z.string(),
  name: Z.string(),
  color: Z.string(),
});

function decodeNonEmpty<T>(valueDecoder: Decoder<T>): Decoder<{ cursor: number; data: Array<T> }> {
  return Z.object({
    cursor: Z.number(),
    data: Z.array(valueDecoder),
  });
}

const decodeCombat: Decoder<Combat> = Z.object({
  scene: Z.string(),
  creatures: decodeNonEmpty(Z.tuple([Z.string(), Z.number()])),
  movement_used: Z.number(),
});

const decodeFolderNode: Decoder<FolderNode> = Z.object({
  scenes: Z.array(Z.string()),
  creatures: Z.array(Z.string()),
  items: Z.array(Z.string()),
  notes: Z.record(decodeNote),
  abilities: Z.array(Z.string()),
  classes: Z.array(Z.string()),
});

const decodeFolder: Decoder<Folder> = Z.object({
  data: decodeFolderNode,
  children: Z.record(Z.lazy(() => decodeFolder)).transform<Folder["children"]>(Map)
});

const decodeCreatureTarget: Decoder<CreatureTarget> = Z.union([
  Z.literal("Actor").transform((): CreatureTarget => ({t: "Actor"})),
  Z.literal("Melee").transform((): CreatureTarget => ({t: "Melee"})),
  // | { t: "Volume"; volume: Volume; range: Distance }

  Z.object({Range: Z.number()}).transform(({Range: distance}): CreatureTarget => ({ t: "Range", distance })),

  Z.object({SomeCreaturesInVolumeInRange: Z.object({
      volume: decodeVolume,
      maximum: Z.number(),
      range: Z.number(),
    })}).transform(
      ({SomeCreaturesInVolumeInRange: o}): CreatureTarget => ({ t: "SomeCreaturesInVolumeInRange", ...o})),
    Z.object({AllCreaturesInVolumeInRange: Z.object({
      volume: decodeVolume,
      range: Z.number(),
    })}).transform(
      ({AllCreaturesInVolumeInRange: o}): CreatureTarget => ({ t: "AllCreaturesInVolumeInRange", ...o })),
    Z.object({LineFromActor: Z.object({
      distance: Z.number(),
    })}).transform(
      ({LineFromActor: o}): CreatureTarget => ({ t: "LineFromActor", ...o })),
]);

export const decodeSceneTarget: Decoder<SceneTarget> =
// Z.union([
  Z.object({RangedVolume: Z.object({ volume: decodeVolume, range: Z.number()})}).transform(
    ({RangedVolume: o}): SceneTarget => ({t: "RangedVolume", ...o}))
// ]);


export const decodeAction: Decoder<Action> = Z.union([
  Z.object({Creature: Z.object({target: decodeCreatureTarget})}).transform(
    ({Creature: o}): Action => ({ t: "Creature", ...o })),
  Z.object({SceneVolume: Z.object({target: decodeSceneTarget})}).transform(
    ({SceneVolume: o}): Action => ({ t: "SceneVolume", ...o }))
]);

const decodeAbility: Decoder<Ability> = Z.object({
  name: Z.string(),
  id: Z.string(),
  action: decodeAction,
  cost: Z.number(),
  usable_ooc: Z.boolean(),
});

const decodeGame: Decoder<Game> = Z.object({
  current_combat: maybe(decodeCombat),
  creatures: Z.record(decodeCreature).transform<Game["creatures"]>(Map),
  classes: Z.record(decodeClass).transform<Game["classes"]>(Map),
  items: Z.record(decodeItem),
  scenes: Z.record(decodeScene).transform<Game["scenes"]>(Map),
  abilities: Z.record(decodeAbility),
  campaign: decodeFolder,
  players: Z.record(decodePlayer).transform<Game["players"]>(Map),
});

export const decodeApp: Decoder<App> = Z.object({
  snapshots: Z.array(
    Z.tuple([Z.any(), Z.array(decodeGameLog)]).transform(([g, logs]): Snapshot => ({snapshot: {}, logs}))
  ),
  current_game: decodeGame,
});

export const decodeSendCommandResult: Decoder<[Game, Array<GameLog>]> = Z.tuple([
  decodeGame,
  Z.array(decodeGameLog)
]);


export function decodeRustResult<T, E>(decode_ok: Decoder<T>, decode_err: Decoder<E>): Decoder<RustResult<T, E>> {
  return Z.union(
    [
      Z.object({Ok: decode_ok}).transform(({Ok}): RustResult<T, E> => ({ t: "Ok", result: Ok as T})),
      Z.object({Err: decode_err}).transform(({Err}): RustResult<T, E> => ({ t: "Err", error: Err as E})),
    ]);
}

export function encodeGameCommand(cmd: GameCommand): object | string {
  switch (cmd.t) {
    case "SetActiveScene": return { SetActiveScene: cmd.scene_id };
    case "ChatFromGM": return { ChatFromGM: cmd.message };
    case "ChatFromPlayer": return { ChatFromPlayer: [cmd.player_id, cmd.message] };
    case "RegisterPlayer": return { RegisterPlayer: cmd.player_id };
    case "GiveCreaturesToPlayer":
      return { GiveCreaturesToPlayer: [cmd.player_id, cmd.creature_ids] };
    case "CreateFolder": return { CreateFolder: encodeFolderPath(cmd.path) };
    case "MoveFolderItem":
      return {
        MoveFolderItem:
          [encodeFolderPath(cmd.source),
          encodeFolderItemID(cmd.item_id),
          encodeFolderPath(cmd.dest)],
      };
    case "CopyFolderItem":
      return {
        CopyFolderItem: {
          source: encodeFolderPath(cmd.source),
          item_id: encodeFolderItemID(cmd.item_id),
          dest: encodeFolderPath(cmd.dest),
        },
      };
    case "DeleteFolderItem":
      return { DeleteFolderItem: [encodeFolderPath(cmd.location), encodeFolderItemID(cmd.item_id)] };
    case "EditCreatureDetails":
      return {
        EditCreatureDetails:
          { creature_id: cmd.creature_id, details: encodeCreatureCreation(cmd.details) },
      };
    case "CreateCreature":
      return { CreateCreature: [encodeFolderPath(cmd.path), encodeCreatureCreation(cmd.spec)] };
    case "CreateItem": return { CreateItem: [encodeFolderPath(cmd.path), cmd.name] };
    case "EditItem": return { EditItem: encodeItem(cmd.item) };
    case "CreateNote": return { CreateNote: [encodeFolderPath(cmd.path), encodeNote(cmd.note)] };
    case "EditNote":
      return { EditNote: [encodeFolderPath(cmd.path), cmd.name, encodeNote(cmd.note)] };
    case "TransferItem":
      return {
        TransferItem: {
          from: encodeInventoryOwner(cmd.from),
          to: encodeInventoryOwner(cmd.to),
          item_id: cmd.item_id, count: cmd.count,
        },
      };
    case "RemoveItem":
      return {
        RemoveItem:
          { owner: encodeInventoryOwner(cmd.owner), item_id: cmd.item_id, count: cmd.count },
      };
    case "SetItemCount":
      return {
        SetItemCount:
          { owner: encodeInventoryOwner(cmd.owner), item_id: cmd.item_id, count: cmd.count },
      };
    case "CreateScene":
      return { CreateScene: [encodeFolderPath(cmd.path), encodeSceneCreation(cmd.spec)] };
    case "EditSceneDetails":
      return {
        EditSceneDetails: { scene_id: cmd.scene_id, details: encodeSceneCreation(cmd.details) },
      };
    case "SetSceneCreatureVisibility":
      return {
        SetSceneCreatureVisibility: {
          scene_id: cmd.scene_id, creature_id: cmd.creature_id,
          visibility: encodeVisibility(cmd.visibility),
        },
      };
    case "AddCreatureToScene":
      return {
        AddCreatureToScene: {
          scene_id: cmd.scene_id, creature_id: cmd.creature_id,
          visibility: encodeVisibility(cmd.visibility),
        },
      };
    case "RemoveCreatureFromScene":
      return { RemoveCreatureFromScene: { scene_id: cmd.scene_id, creature_id: cmd.creature_id } };
    case "AddSceneChallenge":
      return {
        AddSceneChallenge: {
          scene_id: cmd.scene_id, description: cmd.description,
          challenge: encodeAttributeCheck(cmd.challenge),
        },
      };
    case "RemoveSceneChallenge":
      return { RemoveSceneChallenge: { scene_id: cmd.scene_id, description: cmd.description } };
    case "SetFocusedSceneCreatures":
      return {
        SetFocusedSceneCreatures: { scene_id: cmd.scene_id, creatures: cmd.creatures.toArray() },
      };
    case "RemoveSceneVolumeCondition":
      return {
        RemoveSceneVolumeCondition: { scene_id: cmd.scene_id, condition_id: cmd.condition_id },
      };
    case "EditSceneTerrain":
      return {
        EditSceneTerrain: { scene_id: cmd.scene_id, terrain: cmd.terrain.map(encodePoint3) },
      };
    case "EditSceneHighlights":
      return {
        EditSceneHighlights: {
          scene_id: cmd.scene_id,
          highlights: cmd.highlights.mapEntries(
            ([point, [color, vis]]) => [encodePoint3(point), [color, encodeVisibility(vis)]]).toJS(),
        },
      };
    case "EditSceneAnnotations":
      return {
        EditSceneAnnotations: {
          scene_id: cmd.scene_id,
          annotations: cmd.annotations.mapEntries(
            ([point, [annotation, vis]]) =>
              [encodePoint3(point), [annotation, encodeVisibility(vis)]]
          ).toJS(),
        },
      };
    case "EditSceneRelatedScenes":
      return {
        EditSceneRelatedScenes: {
          scene_id: cmd.scene_id,
          related_scenes: cmd.related_scenes.toArray(),
        },
      };
    case "EditSceneSceneHotspots":
      return {
        EditSceneSceneHotspots: {
          scene_id: cmd.scene_id, scene_hotspots: cmd.scene_hotspots.mapKeys(encodePoint3).toJS(),
        },
      };
    case "RemoveCreatureFromCombat":
      return { RemoveCreatureFromCombat: cmd.creature_id };
    case "CombatAct": return { CombatAct: [cmd.ability_id, encodeDecidedTarget(cmd.target)] };
    case "PathCreature":
      return { PathCreature: [cmd.scene_id, cmd.creature_id, encodePoint3(cmd.dest)] };
    case "SetCreaturePos":
      return { SetCreaturePos: [cmd.scene_id, cmd.creature_id, encodePoint3(cmd.dest)] };
    case "PathCurrentCombatCreature":
      return { PathCurrentCombatCreature: encodePoint3(cmd.dest) };
    case "Done": return "Done";
    case "ChangeCreatureInitiative":
      return { ChangeCreatureInitiative: [cmd.creature_id, cmd.init] };
    case "StartCombat":
      return { StartCombat: [cmd.scene_id, cmd.creature_ids] };
    case "StopCombat":
      return "StopCombat";
    case "AddCreatureToCombat":
      return { AddCreatureToCombat: cmd.creature_id };
    case "AttributeCheck":
      return { AttributeCheck: [cmd.creature_id, encodeAttributeCheck(cmd.check)] };
    case "SetPlayerScene":
      return { SetPlayerScene: [cmd.player_id, cmd.scene_id] };
    case "Rollback":
      return { Rollback: [cmd.snapshot_index, cmd.log_index] };
    case "LoadModule":
      return {
        LoadModule: {
          name: cmd.name, path: encodeFolderPath(cmd.path),
          source: encodeModuleSource(cmd.source),
        },
      };
  }
}

function encodeModuleSource(s: ModuleSource): string {
  return s;
}

function encodeFolderItemID(fid: FolderItemID): object {
  return { [fid.t]: fid.id };
}

function encodeInventoryOwner(owner: InventoryOwner): object {
  return owner;
}

function encodeCreatureCreation(cc: CreatureCreation): object {
  return {
    name: cc.name,
    class: cc.class_,
    portrait_url: cc.portrait_url,
    icon_url: cc.icon_url,
    note: cc.note,
    bio: cc.bio,
    initiative: encodeDice(cc.initiative),
    size: encodeAABB(cc.size),
  };
}

function encodeAABB(box: AABB): object {
  return box;
}

function encodeItem(item: Item): object {
  return { id: item.id, name: item.name };
}

function encodeSceneCreation(sc: SceneCreation): object {
  return {
    name: sc.name,
    background_image_url: sc.background_image_url,
    background_image_offset: sc.background_image_offset,
    background_image_scale: sc.background_image_scale,
  };
}

function encodeVisibility(vis: Visibility): string {
  return vis.t;
}

function encodeAttributeCheck(check: AttributeCheck): object {
  return {
    reliable: check.reliable,
    attr: check.attr,
    target: encodeSkillLevel(check.target),
  };
}

function encodeSkillLevel(sl: SkillLevel): string {
  return sl;
}


export function encodeFolderPath(path: FolderPath): string {
  if (path.length === 0) {
    return "";
  } else {
    return "/" + path.join("/");
  }
}

function encodeNote(note: Note): object {
  return note;
}

function encodeDice(d: Dice): object {
  return d;
}

function encodeDecidedTarget(dt: DecidedTarget): object | string {
  switch (dt.t) {
    case "Actor": return "Actor";
    case "Creature": return { Creature: dt.creature_id };
    case "Creatures": return { Creatures: dt.creature_ids };
    case "Point": return { Point: encodePoint3(dt.point) };
  }
}

export function encodePoint3(pt: Point3): string {
  return `${pt.x}/${pt.y}/${pt.z}`;
}

// Utility Functions for Decoding
function decodeIMap<K, V>(keyDecoder: Decoder<K>, valueDecoder: Decoder<V>): Decoder<Map<K, V>> {
  return Z.record(valueDecoder).transform(o => Map(o).mapKeys(k => keyDecoder.parse(k)));
}

function decodeSet<T>(d: Decoder<T>): Decoder<Set<T>> {
  return Z.array(d).transform<Set<T>>(Set);
}

// We use undefined for missing data in our typescript types, not null. This way
// we can *parse* null but *produce* undefined.
function maybe<T>(d: Decoder<T>): Decoder<T | undefined> {
  return d.nullish().transform((x: T|undefined|null): T | undefined => x ?? undefined);
}
