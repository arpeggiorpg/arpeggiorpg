import { Set, Map, List, ValueObject } from 'immutable';
import * as Z from "zod";

import type {
  AABB, Ability, AbilityID, AbilityStatus, Action, AppliedCondition, AttributeCheck, AttrID, Class,
  ClassID, Combat, CombatLog, Condition, ConditionID, CreatureCreation, CreatureData, CreatureEffect,
  CreatureID, CreatureLog, CreatureTarget, DecidedTarget, Dice, Duration,  Energy, FolderItemID,
  FolderNode, FolderPath, FolderTree, Game, GameLog, HP, InventoryOwner, Item, ItemID, ModuleSource,
  Note, Player, PlayerID, PotentialTargets, Scene, SceneCreation, SceneEffect, SceneID, SceneTarget,
  SkillLevel, TileSystem, Visibility, Volume, VolumeCondition,
} from "./bindings/bindings";
export {
  AABB, Ability, AbilityID, AbilityStatus, Action, AppliedCondition, AttributeCheck, AttrID, Class,
  ClassID, Combat, CombatLog, Condition, ConditionID, CreatureCreation, CreatureData, CreatureEffect,
  CreatureID, CreatureLog, CreatureTarget, DecidedTarget, Dice, Duration,  Energy, FolderItemID,
  FolderNode, FolderPath, FolderTree, Game, GameLog, HP, InventoryOwner, Item, ItemID, ModuleSource,
  Note, Player, PlayerID, PotentialTargets, Scene, SceneCreation, SceneEffect, SceneID, SceneTarget,
  SkillLevel, TileSystem, Visibility, Volume, VolumeCondition,
};
import { DynamicCreature as Creature } from "./bindings/bindings";
export type { Creature };

export type Color = string;
export type Distance = number;

// Various types that aren't the obvious types that ts-rs would generate for a
// Rust type, mostly because we use immutablejs for a bunch of stuff.
export type Terrain = Set<Point3>;
export type SceneHotspots = Map<Point3, SceneID>;
export type Highlights = Map<Point3, [Color, Visibility]>;
export type Annotations = Map<Point3, [string, Visibility]>;
export type RelatedScenes = Set<SceneID>;
export type SceneCreatures = Map<CreatureID, [Point3, Visibility]>;
export type SceneInventory = Map<ItemID, number>;
export type SceneVolumeConditions = Map<ConditionID, VolumeCondition>;
export type SceneAttributeChecks = Map<string, AttributeCheck>;
export type SceneFocusedCreatures = List<CreatureID>;
export type GameAbilities = Record<AbilityID, Ability>;
export type GameCreatures = Map<CreatureID, Creature>;
export type GameClasses = Map<ClassID, Class>;
export type GameScenes = Map<SceneID, Scene>;
export type GameItems = Record<ItemID, Item>;
export type GamePlayers = Map<PlayerID, Player>;

export type CreatureAttributes = Map<AttrID, SkillLevel>;
export type CreatureInventory = Map<ItemID, number>;
export type CreatureConditions = Map<ConditionID, AppliedCondition>;

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

export type Folder = FolderTree<FolderNode>;

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

export const SKILL_LEVELS: Array<SkillLevel> =
  ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];


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
  Z.object({CreatureIDs: Z.array(Z.string())}),
  Z.object({Points: Z.array(decodePoint3)})
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

const decodeCommonCreatureData = {
  id: Z.string(),
  name: Z.string(),
  speed: Z.number(),
  max_energy: Z.number(),
  cur_energy: Z.number(),
  abilities: Z.record(decodeAbilityStatus),
  class: Z.string(),
  max_health: Z.number(),
  cur_health: Z.number(),
  note: Z.string(),
  bio: Z.string(),
  portrait_url: Z.string(),
  icon_url: Z.string(),
  attributes: Z.record(decodeSkillLevel).transform<Creature['attributes']>(Map),
  initiative: decodeDice,
  inventory: Z.record(Z.number()).transform<Creature['inventory']>(Map),
  size: decodeAABB,
};

export const decodeCreatureData: Decoder<CreatureData> = Z.object({
  ...decodeCommonCreatureData,
  conditions: Z.record(decodeAppliedCondition).transform<Creature['own_conditions']>(Map),
});

export const decodeDynamicCreature: Decoder<Creature> = Z.object({
  ...decodeCommonCreatureData,
  can_act: Z.boolean(),
  can_move: Z.boolean(),
  own_conditions: Z.record(decodeAppliedCondition).transform<Creature['own_conditions']>(Map),
  volume_conditions: Z.record(decodeAppliedCondition).transform<Creature['volume_conditions']>(Map),
});

const decodeCreatureCreation: Decoder<CreatureCreation> = Z.object({
  name: Z.string(),
  class: Z.string(),
  portrait_url: Z.string(),
  icon_url: Z.string(),
  note: Z.string(),
  bio: Z.string(),
  initiative: decodeDice,
  size: decodeAABB,
});

export const decodeVisibility: Decoder<Visibility> = Z.union([
  Z.literal("GMOnly"),
  Z.literal("AllPlayers")
]);

export const decodeAttributeCheck: Decoder<AttributeCheck> = Z.object({
  reliable: Z.boolean(),
  attr: Z.string(),
  target: decodeSkillLevel,
});

const decodeSceneCreation: Decoder<SceneCreation> = Z.object({
  name: Z.string(),
  background_image_url: Z.string(),
  background_image_scale: Z.tuple([Z.number(), Z.number()]),
  background_image_offset: Z.tuple([Z.number(), Z.number()]).nullable(),
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
  background_image_offset: Z.tuple([Z.number(), Z.number()]).nullable(),
  background_image_scale: Z.tuple([Z.number(), Z.number()]),
  volume_conditions: Z.record(decodeVolumeCondition).transform<Scene['volume_conditions']>(Map),
  focused_creatures: Z.array(Z.string()).transform<Scene['focused_creatures']>(List),
});

const decodeFolderItemID: Decoder<FolderItemID> = Z.union([
  Z.object({"SceneID": Z.string()}),
  Z.object({"CreatureID": Z.string()}),
  Z.object({"NoteID": Z.string()}),
  Z.object({"ItemID": Z.string()}),
  Z.object({"AbilityID": Z.string()}),
  Z.object({"ClassID": Z.string()}),
  Z.object({"SubfolderID": Z.string()}),
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
  Z.object({Damage: Z.object({hp: Z.number(), rolls: Z.array(Z.number())})}),
  Z.object({Heal: Z.object({hp: Z.number(), rolls: Z.array(Z.number())})}),
  Z.object({GenerateEnergy: Z.number()}),
  Z.object({ReduceEnergy: Z.number()}),
  Z.object({ApplyCondition: Z.object({id: Z.string(), duration: decodeDuration, condition: decodeCondition})}),
  Z.object({DecrementConditionRemaining: Z.string()}),
  Z.object({RemoveCondition: Z.string()}),
]);

const decodeCombatLog: Decoder<CombatLog> = Z.union([
  Z.literal("ForceNextTurn"),
  Z.literal("ForcePrevTurn"),
  Z.object({ConsumeMovement: Z.number()}),
  Z.object({ChangeCreatureInitiative: Z.object({creature_id: Z.string(), new_initiative: Z.number()})}),
  Z.object({EndTurn: Z.string()}),
  Z.object({RerollInitiative: Z.array(Z.tuple([Z.string(), Z.number()]))}),
]);

export const decodeGameLog: Decoder<GameLog> = Z.union([
  Z.literal("StopCombat"),
  Z.object({SetActiveScene: Z.string()}),
  Z.object({RegisterPlayer: Z.string()}),
  Z.object({UnregisterPlayer: Z.string()}),
  Z.object({GiveCreaturesToPlayer: Z.tuple([Z.string(), Z.array(Z.string())])}),
  Z.object({RemoveCreaturesFromPlayer: Z.tuple([Z.string(), Z.array(Z.string())])}),
  Z.object({SetPlayerScene: Z.tuple([Z.string(), Z.string().nullable()])}),
  Z.object({ChatFromGM: Z.string()}),
  Z.object({ChatFromPlayer: Z.tuple([Z.string(), Z.string()])}),
  Z.object({StartCombat: Z.tuple([ Z.string(), Z.array(Z.tuple([Z.string(), Z.number()]))])}),
  Z.object({CreateFolder: decodeFolderPath}),
  Z.object({RenameFolder: Z.tuple([decodeFolderPath, Z.string()])}),
  Z.object({DeleteFolderItem: Z.tuple([decodeFolderPath, decodeFolderItemID])}),
  Z.object({MoveFolderItem: Z.tuple([decodeFolderPath, decodeFolderItemID, decodeFolderPath])}),
  Z.object({
    CopyFolderItem: Z.object({
      source: decodeFolderPath,
      item_id: decodeFolderItemID,
      dest: decodeFolderPath,
      new_item_id: decodeFolderItemID,
    })
  }),
  Z.object({CreateItem: Z.tuple([decodeFolderPath, decodeItem])}),
  Z.object({EditItem: decodeItem}),
  Z.object({CreateNote: Z.tuple([decodeFolderPath, decodeNote])}),
  Z.object({EditNote: Z.tuple([decodeFolderPath, Z.string(), decodeNote])}),
  Z.object({TransferItem: Z.object({
      from: decodeInventoryOwner,
      to: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.bigint(),
  })}),
  Z.object({RemoveItem: Z.object({
      owner: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.bigint(),
  })}),
  Z.object({SetItemCount: Z.object({
      owner: decodeInventoryOwner,
      item_id: Z.string(),
      count: Z.bigint(),
  })}),
  Z.object({CreateScene: Z.tuple([decodeFolderPath, decodeScene])}),
  Z.object({EditSceneDetails: Z.object({ scene_id: Z.string(), details: decodeSceneCreation })}),
  Z.object({SetSceneCreatureVisibility: Z.object({
      scene_id: Z.string(),
      creature_id: Z.string(),
      visibility: decodeVisibility
  })}),
  Z.object({AddCreatureToScene: Z.object({
    scene_id: Z.string(),
    creature_id: Z.string(),
    visibility: decodeVisibility,
  })}),
  Z.object({RemoveCreatureFromScene: Z.object({
    scene_id: Z.string(),
    creature_id: Z.string(),
  })}),
  Z.object({AddSceneChallenge: Z.object({
    scene_id: Z.string(),
    description: Z.string(),
    challenge: decodeAttributeCheck,
  })}),
  Z.object({RemoveSceneChallenge: Z.object({
    scene_id: Z.string(),
    description: Z.string(),
  })}),
  Z.object({SetFocusedSceneCreatures: Z.object({
    scene_id: Z.string(),
    creatures: Z.array(Z.string()),
  })}),
  Z.object({RemoveSceneVolumeCondition: Z.object({
    scene_id: Z.string(),
    condition_id: Z.string(),
  })}),
  Z.object({EditSceneTerrain: Z.object({
    scene_id: Z.string(),
    terrain: decodeTerrain,
  })}),
  Z.object({EditSceneHighlights: Z.object({
    scene_id: Z.string(),
    highlights: decodeHighlights,
  })}),
  Z.object({EditSceneAnnotations: Z.object({
    scene_id: Z.string(),
    annotations: decodeAnnotations,
  })}),
  Z.object({EditSceneRelatedScenes: Z.object({
    scene_id: Z.string(),
    related_scenes: decodeSet(Z.string()),
  })}),
  Z.object({EditSceneSceneHotspots: Z.object({
    scene_id: Z.string(),
    scene_hotspots: decodeIMap(decodePoint3, Z.string()),
  })}),

  Z.object({SetCreaturePos: Z.tuple([Z.string(), Z.string(), decodePoint3])}),
  Z.object({PathCreature: Z.tuple([Z.string(), Z.string(), Z.array(decodePoint3)])}),

  Z.object({CreateCreature: Z.tuple([decodeFolderPath, decodeCreatureData])}),
  Z.object({EditCreatureDetails: Z.object({creature_id: Z.string(), details: decodeCreatureCreation})}),
  Z.object({AddCreatureToCombat: Z.tuple([Z.string(), Z.number()])}),
  Z.object({RemoveCreatureFromCombat: Z.string()}),
  Z.object({CombatLog: decodeCombatLog}),
  Z.object({CreatureLog: Z.tuple([Z.string(), decodeCreatureLog])}),
  Z.object({AttributeCheckResult: Z.object({creature_id: Z.string(), attribute_check: decodeAttributeCheck, actual: Z.number(), success: Z.boolean()})}),
  Z.object({Rollback: Z.tuple([Z.number(), Z.number()])}),

  Z.object({LoadModule: Z.object({name: Z.string(), path: decodeFolderPath, source: decodeModuleSource, module: Z.lazy(() => decodeGame) })}),
]);

if (typeof window !== 'undefined') { (window as any).decodeGameLog = decodeGameLog;}

const decodePlayer: Decoder<Player> = Z.object({
  player_id: Z.string(),
  scene: Z.string().nullable(),
  creatures: Z.array(Z.string()),
});

const decodeClass: Decoder<Class> = Z.object({
  id: Z.string(),
  name: Z.string(),
  color: Z.string(),
  abilities: Z.array(Z.string()),
  conditions: Z.array(decodeCondition)
});

export interface NonEmpty {
  cursor: number;
  data: Array<[CreatureID, number]>
}
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
  Z.literal("Actor"),
  Z.literal("Melee"),
  Z.object({Range: Z.number()}),
  Z.object({SomeCreaturesInVolumeInRange: Z.object({
      volume: decodeVolume,
      maximum: Z.number(),
      range: Z.number(),
    })}),
    Z.object({AllCreaturesInVolumeInRange: Z.object({
      volume: decodeVolume,
      range: Z.number(),
    })}),
    Z.object({LineFromActor: Z.object({ distance: Z.number() })})
]);

export const decodeSceneTarget: Decoder<SceneTarget> =
// Z.union([
  Z.object({RangedVolume: Z.object({ volume: decodeVolume, range: Z.number()})});
// ]);

const decodeCreatureEffect: Decoder<CreatureEffect> = Z.union([
  Z.object({ApplyCondition: Z.tuple([decodeDuration, decodeCondition])}),
  Z.object({Heal: decodeDice}),
  Z.object({Damage: decodeDice}),
  // GenerateEnergy should be dice, not number...
  Z.object({GenerateEnergy: Z.number()}),
  Z.object({MultiEffect: Z.array(Z.lazy(() => decodeCreatureEffect))}),
]);
const decodeSceneEffect = // Z.union([
  Z.object({CreateVolumeCondition: Z.object({duration: decodeDuration, condition: decodeCondition})});
// ]);

export const decodeAction: Decoder<Action> = Z.union([
  Z.object({Creature: Z.object({effect: decodeCreatureEffect, target: decodeCreatureTarget})}),
  Z.object({SceneVolume: Z.object({effect: decodeSceneEffect, target: decodeSceneTarget})}),
]);


const decodeAbility: Decoder<Ability> = Z.object({
  name: Z.string(),
  id: Z.string(),
  action: decodeAction,
  cost: Z.number(),
  usable_ooc: Z.boolean(),
});

const decodeTileSystem: Decoder<TileSystem> = Z.union([
  Z.literal("Realistic"),
  Z.literal("DnD")
]);

const decodeGame: Decoder<Game> = Z.object({
  current_combat: decodeCombat.nullable(),
  creatures: Z.record(decodeDynamicCreature).transform<Game["creatures"]>(Map),
  classes: Z.record(decodeClass).transform<Game["classes"]>(Map),
  items: Z.record(decodeItem),
  scenes: Z.record(decodeScene).transform<Game["scenes"]>(Map),
  abilities: Z.record(decodeAbility),
  campaign: decodeFolder,
  players: Z.record(decodePlayer).transform<Game["players"]>(Map),
  tile_system: decodeTileSystem,
  active_scene: Z.string().nullable(),
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
          cmd.item_id,
          encodeFolderPath(cmd.dest)],
      };
    case "CopyFolderItem":
      return {
        CopyFolderItem: {
          source: encodeFolderPath(cmd.source),
          item_id: cmd.item_id,
          dest: encodeFolderPath(cmd.dest),
        },
      };
    case "DeleteFolderItem":
      return { DeleteFolderItem: [encodeFolderPath(cmd.location), cmd.item_id] };
    case "EditCreatureDetails":
      return {
        EditCreatureDetails:
          { creature_id: cmd.creature_id, details: cmd.details },
      };
    case "CreateCreature":
      return { CreateCreature: [encodeFolderPath(cmd.path), cmd.spec] };
    case "CreateItem": return { CreateItem: [encodeFolderPath(cmd.path), cmd.name] };
    case "EditItem": return { EditItem: cmd.item };
    case "CreateNote": return { CreateNote: [encodeFolderPath(cmd.path), cmd.note] };
    case "EditNote":
      return { EditNote: [encodeFolderPath(cmd.path), cmd.name, cmd.note] };
    case "TransferItem":
      return {
        TransferItem: {
          from: cmd.from,
          to: cmd.to,
          item_id: cmd.item_id, count: cmd.count,
        },
      };
    case "RemoveItem":
      return {
        RemoveItem:
          { owner: cmd.owner, item_id: cmd.item_id, count: cmd.count },
      };
    case "SetItemCount":
      return {
        SetItemCount:
          { owner: cmd.owner, item_id: cmd.item_id, count: cmd.count },
      };
    case "CreateScene":
      return { CreateScene: [encodeFolderPath(cmd.path), cmd.spec] };
    case "EditSceneDetails":
      return {
        EditSceneDetails: { scene_id: cmd.scene_id, details: cmd.details },
      };
    case "SetSceneCreatureVisibility":
      return {
        SetSceneCreatureVisibility: {
          scene_id: cmd.scene_id, creature_id: cmd.creature_id,
          visibility: cmd.visibility,
        },
      };
    case "AddCreatureToScene":
      return {
        AddCreatureToScene: {
          scene_id: cmd.scene_id, creature_id: cmd.creature_id,
          visibility: cmd.visibility,
        },
      };
    case "RemoveCreatureFromScene":
      return { RemoveCreatureFromScene: { scene_id: cmd.scene_id, creature_id: cmd.creature_id } };
    case "AddSceneChallenge":
      return {
        AddSceneChallenge: {
          scene_id: cmd.scene_id, description: cmd.description,
          challenge: cmd.challenge,
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
            ([point, [color, vis]]) => [encodePoint3(point), [color, vis]]).toJS(),
        },
      };
    case "EditSceneAnnotations":
      return {
        EditSceneAnnotations: {
          scene_id: cmd.scene_id,
          annotations: cmd.annotations.mapEntries(
            ([point, [annotation, vis]]) =>
              [encodePoint3(point), [annotation, vis]]
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
      return { AttributeCheck: [cmd.creature_id, cmd.check] };
    case "SetPlayerScene":
      return { SetPlayerScene: [cmd.player_id, cmd.scene_id] };
    case "Rollback":
      return { Rollback: [cmd.snapshot_index, cmd.log_index] };
    case "LoadModule":
      return {
        LoadModule: {
          name: cmd.name, path: encodeFolderPath(cmd.path),
          source: cmd.source,
        },
      };
  }
}

export function encodeFolderPath(path: FolderPath): string {
  if (path.length === 0) {
    return "";
  } else {
    return "/" + path.join("/");
  }
}

function encodeDecidedTarget(dt: DecidedTarget): object | string {
  if (typeof dt !== "string" && "Point" in dt) {
    return {Point: encodePoint3(dt.Point)};
  }
  return dt;
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
