import I from 'immutable';
import LD from "lodash";
import * as JD from "type-safe-json-decoder";
import * as Z from "zod";
import { Decoder } from "type-safe-json-decoder";

import type { AbilityID } from "./bindings/AbilityID";
import type { ClassID } from "./bindings/ClassID";
import type { CreatureID } from "./bindings/CreatureID";
import type { PlayerID } from "./bindings/PlayerID";
import type { SceneID } from "./bindings/SceneID";
import type { ItemID } from "./bindings/ItemID";
import type { AttrID } from "./bindings/AttrID";
import type { ConditionID } from "./bindings/ConditionID";
import type { HP } from "./bindings/HP";
import type { Energy } from "./bindings/Energy";
import type { Note } from "./bindings/Note";

export { AbilityID, ClassID, CreatureID, PlayerID, SceneID, ItemID, AttrID, ConditionID, HP, Note};

export type Color = string;
export type Distance = number;
export type FolderPath = Array<string>;
export type Terrain = I.Set<Point3>;
export type Highlights = I.Map<Point3, [Color, Visibility]>;
export type Annotations = I.Map<Point3, [string, Visibility]>;

export function folderPathToString(path: FolderPath): string {
  if (path.length === 0) {
    return "Campaign Root";
  }
  return encodeFolderPath(path);
}

export class Point3 implements I.ValueObject {
  constructor(public x: number, public y: number, public z: number) { }

  equals(other: Point3): boolean {
    return this.x === other.x && this.y === other.y && this.z === other.z;
  }

  hashCode(): number {
    return I.List([this.x, this.y, this.z]).hashCode();
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
  current_combat: Combat | undefined;
  creatures: I.Map<CreatureID, Creature>;
  classes: I.Map<ClassID, Class>;
  items: { [index: string]: Item };
  scenes: I.Map<SceneID, Scene>;
  abilities: { [index: string]: Ability };
  campaign: Folder;
  players: I.Map<PlayerID, Player>;
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

export interface SceneTarget { t: "RangedVolume"; volume: Volume; range: Distance; }

export type DecidedTarget =
  | { t: "Creature"; creature_id: CreatureID }
  | { t: "Creatures"; creature_ids: Array<CreatureID> }
  | { t: "Actor" }
  | { t: "Point"; point: Point3 };

export type Volume =
  | { t: "Sphere"; radius: Distance }
  | { t: "Line"; vector: Point3 }
  | { t: "VerticalCylinder"; radius: Distance; height: Distance }
  | { t: "AABB"; aabb: AABB };

export interface Folder {
  data: FolderNode;
  children: I.Map<string, Folder>;
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
  | { t: "SetActiveScene"; scene_id: SceneID | undefined }
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
  | { t: "SetFocusedSceneCreatures"; scene_id: SceneID; creatures: I.List<CreatureID> }
  | { t: "RemoveSceneVolumeCondition"; scene_id: SceneID; condition_id: ConditionID }
  | { t: "EditSceneTerrain"; scene_id: SceneID; terrain: Terrain }
  | { t: "EditSceneHighlights"; scene_id: SceneID; highlights: Highlights }
  | { t: "EditSceneAnnotations"; scene_id: SceneID; annotations: Annotations }
  | { t: "EditSceneRelatedScenes"; scene_id: SceneID; related_scenes: I.Set<SceneID> }
  | { t: "EditSceneSceneHotspots"; scene_id: SceneID; scene_hotspots: I.Map<Point3, SceneID> }
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
  | { t: "SetPlayerScene"; player_id: PlayerID; scene_id: SceneID | undefined }
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
  | { t: "SetActiveScene"; scene_id: SceneID | undefined }
  | { t: "RegisterPlayer"; player_id: string }
  | { t: "UnregisterPlayer"; player_id: string }
  | { t: "GiveCreaturesToPlayer"; player_id: string; creature_ids: Array<CreatureID> }
  | { t: "RemoveCreaturesFromPlayer"; player_id: string; creature_ids: Array<CreatureID> }
  | { t: "SetPlayerScene"; player_id: string; scene_id: SceneID | undefined }

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
  | { t: "SetFocusedSceneCreatures"; scene_id: SceneID; creatures: I.List<CreatureID> }
  | { t: "RemoveSceneVolumeCondition"; scene_id: SceneID; condition_id: ConditionID }
  | { t: "EditSceneTerrain"; scene_id: SceneID; terrain: Terrain }
  | { t: "EditSceneHighlights"; scene_id: SceneID; highlights: Highlights }
  | { t: "EditSceneAnnotations"; scene_id: SceneID; annotations: Annotations }
  | { t: "EditSceneRelatedScenes"; scene_id: SceneID; related_scenes: I.Set<SceneID> }
  | { t: "EditSceneSceneHotspots"; scene_id: SceneID; scene_hotspots: I.Map<Point3, SceneID> }
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
  | { t: "ApplyCondition"; condition_id: ConditionID; duration: Duration } // TODO Condition
  | { t: "DecrementConditionRemaining"; condition_id: ConditionID }
  | { t: "RemoveCondition"; condition_id: ConditionID };

export interface Item {
  id: ItemID;
  name: string;
}

export type CreatureEffect =
  | { t: "ApplyCondition"; duration: Duration; condition: Condition }
  | { t: "Heal"; dice: Dice }
  | { t: "Damage"; dice: Dice }
  | { t: "MultiEffect"; effects: Array<CreatureEffect> }
  | { t: "GenerateEnergy"; energy: Energy };

export type Duration =
  | { t: "Interminate" }
  | { t: "Rounds"; duration: number };

export type Condition =
  | { t: "RecurringEffect"; effect: CreatureEffect }
  | { t: "Dead" }
  | { t: "Incapacitated" }
  | { t: "AddDamageBuff"; hp: HP }
  | { t: "DoubleMaxMovement" }
  | { t: "ActivateAbility"; ability_id: AbilityID };

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
    public own_conditions: I.Map<ConditionID, AppliedCondition>,
    public volume_conditions: I.Map<ConditionID, AppliedCondition>,
    public note: string,
    public bio: string,
    public portrait_url: string,
    public icon_url: string,
    public attributes: I.Map<AttrID, SkillLevel>,
    public initiative: Dice,
    public inventory: I.Map<ItemID, number>,
    public size: AABB,
  ) { }

  dynamic_conditions(): I.Map<ConditionID, AppliedCondition> {
    return this.own_conditions.merge(this.volume_conditions);
  }
}

export interface CreatureData {
  name: string;
}

export interface AABB { x: number; y: number; z: number; }

export type FolderItemID =
  | { t: "SceneID"; id: SceneID }
  | { t: "CreatureID"; id: CreatureID }
  | { t: "NoteID"; id: string }
  | { t: "SubfolderID"; id: string }
  | { t: "ItemID"; id: ItemID }
  | { t: "AbilityID"; id: AbilityID }
  | { t: "ClassID"; id: ClassID }
  ;

export interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";
export const SKILL_LEVELS: Array<SkillLevel> =
  ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

export interface SceneCreation {
  name: string;
  background_image_url: string;
  background_image_offset: [number, number] | undefined;
  background_image_scale: [number, number];
}

export interface Scene {
  id: SceneID;
  name: string;
  terrain: Terrain;
  highlights: Highlights;
  annotations: Annotations;
  scene_hotspots: I.Map<Point3, SceneID>;
  related_scenes: I.Set<SceneID>;
  creatures: I.Map<CreatureID, [Point3, Visibility]>;
  attribute_checks: I.Map<string, AttributeCheck>;
  inventory: I.Map<ItemID, number>;
  background_image_url: string;
  background_image_offset: [number, number] | undefined;
  background_image_scale: [number, number];
  volume_conditions: I.Map<ConditionID, VolumeCondition>;
  focused_creatures: I.List<CreatureID>;
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

export type Dice =
  | { t: "Flat"; val: number }
  | { t: "Expr"; num: number; size: number }
  | { t: "Plus"; left: Dice; right: Dice }
  | { t: "BestOf"; num: number; dice: Dice };

export type PotentialTargets =
  | { t: "CreatureIDs"; cids: Array<CreatureID> }
  | { t: "Points"; points: Array<Point3> }
  ;

export type RustResult<T, E> =
  | { t: "Ok"; result: T }
  | { t: "Err"; error: E }
  ;

// ** Decoders **

export function parsePoint3(str: string): Point3 {
  const segments = str.split("/");
  if (segments.length !== 3) {
    throw new Error(`${str} did not have three segments separated by /`);
  }
  const [xs, ys, zs] = segments;
  return new Point3(Number(xs), Number(ys), Number(zs));
}

export const decodePoint3: Decoder<Point3> = JD.map(parsePoint3, JD.string());

export const zecodePoint3 = Z.string().transform(parsePoint3);
export const arrayOfPoint3 = Z.array(zecodePoint3);

(window as any).zecodePoint3 = zecodePoint3;
(window as any).arrayOfPoint3 = arrayOfPoint3;
(window as any).Z = Z;

export const decodePotentialTargets = sum<PotentialTargets>("PotentialTargets", {}, {
  CreatureIDs: JD.map((cids): PotentialTargets =>
    ({ t: "CreatureIDs", cids }), JD.array(JD.string())),
  Points: JD.map((points): PotentialTargets => ({ t: "Points", points }), JD.array(decodePoint3)),
});

const decodeDiceLazy = JD.lazy(() => decodeDice);
const decodeConditionLazy = JD.lazy(() => decodeCondition);
const decodeEffectLazy = JD.lazy(() => decodeEffect);

const decodeDice: Decoder<Dice> = sum<Dice>("Dice", {}, {
  BestOf: JD.map(
    ([num, dice]): Dice => ({ t: "BestOf", num, dice }),
    JD.tuple(JD.number(), decodeDiceLazy)),
  Expr: JD.map(
    ({ num, size }): Dice => ({ t: "Expr", num, size }),
    JD.object(["num", JD.number()], ["size", JD.number()], (num, size) => ({ num, size }))),
  Flat: JD.map((val): Dice => ({ t: "Flat", val }), JD.number()),
  Plus: JD.map(
    ([left, right]): Dice => ({ t: "Plus", left, right }),
    JD.tuple(decodeDiceLazy, decodeDiceLazy)),
});

const decodeDuration: Decoder<Duration> =
  sum<Duration>("Duration", { Interminate: { t: "Interminate" } },
    {
      Rounds: JD.map(
        (duration): Duration => ({ t: "Rounds", duration }),
        JD.number()),
    });

const decodeEffect: Decoder<CreatureEffect> = sum<CreatureEffect>("CreatureEffect", {},
  {
    ApplyCondition: JD.map(
      ([duration, condition]): CreatureEffect => ({ t: "ApplyCondition", duration, condition }),
      JD.tuple(decodeDuration, decodeConditionLazy)),
    Damage: JD.map((dice): CreatureEffect => ({ t: "Damage", dice }), decodeDice),
    GenerateEffect: JD.map(
      (energy): CreatureEffect => ({ t: "GenerateEnergy", energy }),
      JD.number()),
    Heal: JD.map((dice): CreatureEffect => ({ t: "Heal", dice }), decodeDice),
    MultiEffect: JD.map(
      (effects): CreatureEffect => ({ t: "MultiEffect", effects }),
      JD.array(decodeEffectLazy)),
  });

const decodeCondition: Decoder<Condition> = sum<Condition>("Condition",
  {
    Dead: { t: "Dead" },
    DoubleMaxMovement: { t: "DoubleMaxMovement" },
    Incapacitated: { t: "Incapacitated" },
  }, {
  ActivateAbility: JD.map(
    (ability_id): Condition => ({ t: "ActivateAbility", ability_id }),
    JD.string()),
  RecurringEffect: JD.map(
    (effect): Condition => ({ t: "RecurringEffect", effect }),
    decodeEffect),
}
);

const decodeAppliedCondition: Decoder<AppliedCondition> = JD.object(
  ["remaining", decodeDuration],
  ["condition", decodeCondition],
  (remaining, condition) => ({ remaining, condition })
);

export const decodeSkillLevel: Decoder<SkillLevel> =
  JD.oneOf.apply(null, SKILL_LEVELS.map(JD.equal));

const decodeAbilityStatus: Decoder<AbilityStatus> = JD.object(
  ["ability_id", JD.string()],
  ["cooldown", JD.number()],
  (ability_id, cooldown) => ({ ability_id, cooldown })
);

const decodeAABB: Decoder<AABB> = JD.object(
  ["x", JD.number()],
  ["y", JD.number()],
  ["z", JD.number()],
  (x, y, z) => ({ x, y, z })
);

export const decodeCreatureData: Decoder<CreatureData> = JD.object(
  ["name", JD.string()],
  name => ({ name })
);

function objectBig<T, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O>(
  _ad: JD.EntryDecoder<A>, _bd: JD.EntryDecoder<B>, _cd: JD.EntryDecoder<C>, _dd: JD.EntryDecoder<D>,
  _ed: JD.EntryDecoder<E>, _fd: JD.EntryDecoder<F>, _gd: JD.EntryDecoder<G>, _hd: JD.EntryDecoder<H>,
  _id: JD.EntryDecoder<I>, _jd: JD.EntryDecoder<J>, _kd: JD.EntryDecoder<K>, _ld: JD.EntryDecoder<L>,
  _md: JD.EntryDecoder<M>, _nd: JD.EntryDecoder<N>, _od: JD.EntryDecoder<O>,
  _cons: (
    a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O,
  ) => T): Decoder<T>;
function objectBig<T, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R>(
  _ad: JD.EntryDecoder<A>, _bd: JD.EntryDecoder<B>, _cd: JD.EntryDecoder<C>, _dd: JD.EntryDecoder<D>,
  _ed: JD.EntryDecoder<E>, _fd: JD.EntryDecoder<F>, _gd: JD.EntryDecoder<G>, _hd: JD.EntryDecoder<H>,
  _id: JD.EntryDecoder<I>, _jd: JD.EntryDecoder<J>, _kd: JD.EntryDecoder<K>, _ld: JD.EntryDecoder<L>,
  _md: JD.EntryDecoder<M>, _nd: JD.EntryDecoder<N>, _od: JD.EntryDecoder<O>, _pd: JD.EntryDecoder<P>,
  _qd: JD.EntryDecoder<Q>, _rd: JD.EntryDecoder<R>,
  _cons: (
    a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O,
    p: P, q: Q, r: R) => T): Decoder<T>;
function objectBig<T, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S>(
  _ad: JD.EntryDecoder<A>, _bd: JD.EntryDecoder<B>, _cd: JD.EntryDecoder<C>, _dd: JD.EntryDecoder<D>,
  _ed: JD.EntryDecoder<E>, _fd: JD.EntryDecoder<F>, _gd: JD.EntryDecoder<G>, _hd: JD.EntryDecoder<H>,
  _id: JD.EntryDecoder<I>, _jd: JD.EntryDecoder<J>, _kd: JD.EntryDecoder<K>, _ld: JD.EntryDecoder<L>,
  _md: JD.EntryDecoder<M>, _nd: JD.EntryDecoder<N>, _od: JD.EntryDecoder<O>, _pd: JD.EntryDecoder<P>,
  _qd: JD.EntryDecoder<Q>, _rd: JD.EntryDecoder<R>, _sd: JD.EntryDecoder<S>,
  _cons: (
    a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O,
    p: P, q: Q, r: R, s: S) => T): Decoder<T>;
function objectBig<T>(...args: Array<any>): Decoder<T> {
  return JD.object.apply(undefined, args);
}

export const decodeCreature: Decoder<Creature> = objectBig(
  ["id", JD.string()],
  ["name", JD.string()],
  ["speed", JD.number()],
  ["max_energy", JD.number()],
  ["cur_energy", JD.number()],
  ["abilities", JD.dict(decodeAbilityStatus)],
  ["class", JD.string()],
  ["max_health", JD.number()],
  ["cur_health", JD.number()],
  ["own_conditions", JD.map(I.Map, JD.dict(decodeAppliedCondition))],
  ["volume_conditions", JD.map(I.Map, JD.dict(decodeAppliedCondition))],
  ["note", JD.string()],
  ["bio", JD.string()],
  ["portrait_url", JD.string()],
  ["icon_url", JD.string()],
  ["attributes", JD.map(I.Map, JD.dict(decodeSkillLevel))],
  ["initiative", decodeDice],
  ["inventory", JD.map(I.Map, JD.dict(JD.number()))],
  ["size", decodeAABB],
  (
    id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health,
    own_conditions, volume_conditions, note, bio, portrait_url, icon_url, attributes, initiative,
    inventory, size) =>
    new Creature(
      id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health,
      own_conditions, volume_conditions, note, bio, portrait_url, icon_url, attributes, initiative,
      inventory, size
    )
);

const decodeCreatureCreation: Decoder<CreatureCreation> = JD.object(
  ["name", JD.string()],
  ["class", JD.string()],
  ["portrait_url", JD.string()],
  ["icon_url", JD.string()],
  ["note", JD.string()],
  ["bio", JD.string()],
  ["initiative", decodeDice],
  ["size", decodeAABB],
  (name, class_, portrait_url, icon_url, note, bio, initiative, size) =>
    ({ name, class_, portrait_url, icon_url, note, bio, initiative, size })
);

export const decodeVisibility: Decoder<Visibility> = JD.map((x): Visibility => {
  switch (x) {
    case "GMOnly": return { t: "GMOnly" };
    case "AllPlayers": return { t: "AllPlayers" };
    default: throw new Error(`Not a Visibility: ${x}.`);
  }
}, JD.string());

export const decodeAttributeCheck: Decoder<AttributeCheck> =
  JD.object(["reliable", JD.boolean()], ["attr", JD.string()], ["target", decodeSkillLevel],
    (reliable, attr, target) => ({ reliable, attr, target }));

const decodeSceneCreation: Decoder<SceneCreation> = JD.object(
  ["name", JD.string()],
  ["background_image_url", JD.string()],
  ["background_image_scale", JD.tuple(JD.number(), JD.number())],
  ["background_image_offset", maybe(JD.tuple(JD.number(), JD.number()))],
  (name, background_image_url, background_image_scale, background_image_offset) =>
    ({ name, background_image_url, background_image_scale, background_image_offset })
);

const decodeVolume: Decoder<Volume> = sum("Volume", {},
  {
    Sphere: JD.map((radius): Volume => ({ t: "Sphere", radius }), JD.number()),
    Line: JD.map((vector): Volume => ({ t: "Line", vector }), decodePoint3),
    VerticalCylinder: JD.object(
      ["radius", JD.number()],
      ["height", JD.number()],
      (radius, height): Volume => ({ t: "VerticalCylinder", radius, height })
    ),
    AABB: JD.map((aabb): Volume => ({ t: "AABB", aabb }), decodeAABB),
  });

export const decodeVolumeCondition: Decoder<VolumeCondition> = JD.object(
  ["point", decodePoint3],
  ["volume", decodeVolume],
  ["remaining", decodeDuration],
  ["condition", decodeCondition],
  (point, volume, remaining, condition): VolumeCondition => ({ point, volume, remaining, condition })
);

const decodeTerrain: Decoder<Terrain> = decodeSet(decodePoint3);
const decodeHighlights: Decoder<Highlights> =
  decodeIMap(decodePoint3, JD.tuple(JD.string(), decodeVisibility));
const decodeAnnotations: Decoder<Annotations> = decodeHighlights;

function decodeSet<T>(d: Decoder<T>): Decoder<I.Set<T>> {
  // In the upgrade to a latest typescript (and many other dependencies), for some reason
  // I needed to explicitly cast the `I.Set` constructor in this way, when previously I did not.
  return JD.map(I.Set as ((arr: Array<T>) => I.Set<T>), JD.array(d));
}


export const decodeScene: Decoder<Scene> =
  objectBig(
    ["id", JD.string()],
    ["name", JD.string()],
    ["terrain", decodeTerrain],
    ["highlights", decodeHighlights],
    ["annotations", decodeAnnotations],
    ["scene_hotspots", decodeIMap(decodePoint3, JD.string())],
    ["related_scenes", decodeSet(JD.string())],
    ["creatures", JD.map(I.Map, JD.dict(JD.tuple(decodePoint3, decodeVisibility)))],
    ["attribute_checks", JD.map(I.Map, JD.dict(decodeAttributeCheck))],
    ["inventory", JD.map(I.Map, JD.dict(JD.number()))],
    ["background_image_url", JD.string()],
    ["background_image_offset", maybe(JD.tuple(JD.number(), JD.number()))],
    ["background_image_scale", JD.tuple(JD.number(), JD.number())],
    ["volume_conditions", JD.map(I.Map, JD.dict(decodeVolumeCondition))],
    ["focused_creatures",
      JD.map(I.List as ((arr: Array<string>) => I.List<CreatureID>), JD.array(JD.string()))],
    (
      id, name, terrain, highlights, annotations, scene_hotspots, related_scenes, creatures,
      attribute_checks, inventory, background_image_url, background_image_offset,
      background_image_scale, volume_conditions, focused_creatures): Scene => ({
        id, name, terrain, highlights, annotations, scene_hotspots, related_scenes, creatures,
        attribute_checks, inventory, background_image_url, background_image_offset,
        background_image_scale, volume_conditions, focused_creatures,
      }));

function _mkFolderItem(t: string): Decoder<FolderItemID> {
  return JD.map(id => ({ t, id } as FolderItemID), JD.string());
}
const decodeFolderItemID: Decoder<FolderItemID> =
  sum<FolderItemID>("FolderItemID", {}, {
    SceneID: _mkFolderItem("SceneID"),
    MapID: _mkFolderItem("MapID"),
    CreatureID: _mkFolderItem("CreatureID"),
    NoteID: _mkFolderItem("NoteID"),
    ItemID: _mkFolderItem("ItemID"),
    AbilityID: _mkFolderItem("AbilityID"),
    ClassID: _mkFolderItem("ClassID"),
    SubfolderID: _mkFolderItem("SubfolderID"),
  });

const decodeFolderPath: Decoder<FolderPath> =
  JD.map(strpath => {
    if (strpath === "") {
      return [];
    } else if (LD.startsWith(strpath, "/")) {
      return LD.slice(LD.split(strpath, "/"), 1);
    } else {
      throw new Error(`Not a path: ${strpath}.`);
    }
  }, JD.string());

const decodeItem: Decoder<Item> =
  JD.object(
    ["id", JD.string()],
    ["name", JD.string()],
    (id, name) => ({ id, name })
  );

const decodeNote: Decoder<Note> =
  JD.object(
    ["name", JD.string()],
    ["content", JD.string()],
    (name, content) => ({ name, content })
  );

export const decodeInventoryOwner: Decoder<InventoryOwner> = sum<InventoryOwner>("InventoryOwner",
  {},
  {
    Scene: JD.map(Scene => ({ Scene }), JD.string()),
    Creature: JD.map(Creature => ({ Creature }), JD.string()),
  });

const decodeModuleSource: Decoder<ModuleSource> =
  JD.oneOf(JD.equal('Module' as ModuleSource), JD.equal('SavedGame' as ModuleSource));

const decodeCreatureLog: Decoder<CreatureLog> =
  sum<CreatureLog>("CreatureLog", {}, {
    Damage: JD.map(
      ([hp, rolls]): CreatureLog => ({ t: "Damage", hp, rolls }),
      JD.tuple(JD.number(), JD.array(JD.number()))),
    Heal: JD.map(([hp, rolls]): CreatureLog => ({ t: "Heal", hp, rolls }),
      JD.tuple(JD.number(), JD.array(JD.number()))),
    GenerateEnergy: JD.map((energy): CreatureLog => ({ t: "GenerateEnergy", energy }),
      JD.number()),
    ReduceEnergy: JD.map((energy): CreatureLog => ({ t: "ReduceEnergy", energy }),
      JD.number()),
    ApplyCondition: JD.map(
      ([condition_id, duration]): CreatureLog => ({ t: "ApplyCondition", condition_id, duration }),
      JD.tuple(JD.string(), decodeDuration)),
    DecrementConditionRemaining: JD.map(
      (condition_id): CreatureLog => ({ t: "DecrementConditionRemaining", condition_id }),
      JD.string()),
    RemoveCondition: JD.map((condition_id): CreatureLog => ({ t: "RemoveCondition", condition_id }),
      JD.string()),
  });

const decodeCombatLog: Decoder<CombatLog> =
  sum<CombatLog>("CombatLog",
    {
      ForceNextTurn: { t: "ForceNextTurn" },
      ForcePrevTurn: { t: "ForcePrevTurn" },
    },
    {
      ConsumeMovement: JD.map(
        (distance): CombatLog => ({ t: "ConsumeMovement", distance }),
        JD.number()),
      ChangeCreatureInitiative: JD.map(
        ([creature_id, init]): CombatLog => ({ t: "ChangeCreatureInitiative", creature_id, init }),
        JD.tuple(JD.string(), JD.number())),
      EndTurn: JD.map((creature_id): CombatLog => ({ t: "EndTurn", creature_id }), JD.string()),
      RerollInitiative: JD.map((combatants): CombatLog => ({ t: "RerollInitiative", combatants }),
        JD.array(JD.tuple(JD.string(), JD.number()))),
    });

export const decodeGameLog: Decoder<GameLog> =
  sum<GameLog>("GameLog", { StopCombat: { t: "StopCombat" } }, {
    SetActiveScene: JD.map((scene_id): GameLog => ({ t: "SetActiveScene", scene_id }), JD.string()),
    RegisterPlayer: JD.map(
      (player_id): GameLog => ({ t: "RegisterPlayer", player_id }),
      JD.string()),
    UnregisterPlayer: JD.map(
      (player_id): GameLog => ({ t: "UnregisterPlayer", player_id }),
      JD.string(),
    ),
    GiveCreaturesToPlayer: JD.map(
      ([player_id, creature_ids]): GameLog =>
        ({ t: "GiveCreaturesToPlayer", player_id, creature_ids }),
      JD.tuple(JD.string(), JD.array(JD.string())),
    ),
    RemoveCreaturesFromPlayer: JD.map(
      ([player_id, creature_ids]): GameLog =>
        ({ t: "RemoveCreaturesFromPlayer", player_id, creature_ids }),
      JD.tuple(JD.string(), JD.array(JD.string())),
    ),
    SetPlayerScene: JD.map(
      ([player_id, scene_id]): GameLog => ({ t: "SetPlayerScene", player_id, scene_id }),
      JD.tuple(JD.string(), maybe(JD.string())),
    ),
    ChatFromGM: JD.map((message): GameLog => ({ t: "ChatFromGM", message }), JD.string()),
    ChatFromPlayer: JD.map(
      ([player_id, message]): GameLog => ({ t: "ChatFromPlayer", player_id, message }),
      JD.tuple(JD.string(), JD.string())),
    StartCombat: JD.map(
      ([scene, creatures]): GameLog => ({ t: "StartCombat", scene, creatures }),
      JD.tuple(
        JD.string(),
        JD.array(JD.map(([cid, init]) => ({ cid, init }), JD.tuple(JD.string(), JD.number())))
      )),
    CreateFolder: JD.map((p): GameLog => ({ t: "CreateFolder", path: p }), decodeFolderPath),
    RenameFolder: JD.map(
      ([path, newName]): GameLog => ({ t: "RenameFolder", path, newName }),
      JD.tuple(decodeFolderPath, JD.string())),
    DeleteFolderItem: JD.map(([path, item]): GameLog => ({ t: "DeleteFolderItem", path, item }),
      JD.tuple(decodeFolderPath, decodeFolderItemID)),
    MoveFolderItem: JD.map(
      ([path, item, newPath]): GameLog => ({ t: "MoveFolderItem", path, item, newPath }),
      JD.tuple(decodeFolderPath, decodeFolderItemID, decodeFolderPath)),
    CopyFolderItem: JD.object(
      ["source", decodeFolderPath],
      ["item_id", decodeFolderItemID],
      ["dest", decodeFolderPath],
      (source, item_id, dest): GameLog => ({ t: "CopyFolderItem", source, item_id, dest }),
    ),
    CreateItem: JD.map(
      ([path, item]): GameLog => ({ t: "CreateItem", path, item }),
      JD.tuple(decodeFolderPath, decodeItem)),
    EditItem: JD.map((item): GameLog => ({ t: "EditItem", item }), decodeItem),
    CreateNote: JD.map(
      ([path, note]): GameLog => ({ t: "CreateNote", path, note }),
      JD.tuple(decodeFolderPath, decodeNote)),
    EditNote: JD.map(
      ([path, name, newNote]): GameLog => ({ t: "EditNote", path, name, newNote }),
      JD.tuple(decodeFolderPath, JD.string(), decodeNote)),
    TransferItem: JD.object(
      ["from", decodeInventoryOwner],
      ["to", decodeInventoryOwner],
      ["item_id", JD.string()],
      ["count", JD.number()],
      (from, to, item_id, count): GameLog => ({ t: "TransferItem", from, to, item_id, count })),
    RemoveItem: JD.object(
      ["owner", decodeInventoryOwner],
      ["item_id", JD.string()],
      ["count", JD.number()],
      (owner, item_id, count): GameLog => ({ t: "RemoveItem", owner, item_id, count })),
    SetItemCount: JD.object(
      ["owner", decodeInventoryOwner],
      ["item_id", JD.string()],
      ["count", JD.number()],
      (owner, item_id, count): GameLog => ({ t: "SetItemCount", owner, item_id, count })),
    CreateScene: JD.map(
      ([path, scene]): GameLog => ({ t: "CreateScene", path, scene }),
      JD.tuple(decodeFolderPath, decodeScene)),
    EditSceneDetails: JD.object(
      ["scene_id", JD.string()], ["details", decodeSceneCreation],
      (scene_id, details): GameLog => ({ t: "EditSceneDetails", scene_id, details })),
    SetSceneCreatureVisibility: JD.object(
      ["scene_id", JD.string()], ["creature_id", JD.string()], ["visibility", decodeVisibility],
      (scene_id, creature_id, visibility): GameLog =>
        ({ t: "SetSceneCreatureVisibility", scene_id, creature_id, visibility })
    ),
    AddCreatureToScene: JD.object(
      ["scene_id", JD.string()], ["creature_id", JD.string()], ["visibility", decodeVisibility],
      (scene_id, creature_id, visibility): GameLog =>
        ({ t: "AddCreatureToScene", scene_id, creature_id, visibility })
    ),
    RemoveCreatureFromScene: JD.object(
      ["scene_id", JD.string()], ["creature_id", JD.string()],
      (scene_id, creature_id): GameLog => ({ t: "RemoveCreatureFromScene", scene_id, creature_id })
    ),
    AddSceneChallenge: JD.object(
      ["scene_id", JD.string()], ["description", JD.string()], ["challenge", decodeAttributeCheck],
      (scene_id, description, challenge): GameLog =>
        ({ t: "AddSceneChallenge", scene_id, description, challenge })
    ),
    RemoveSceneChallenge: JD.object(
      ["scene_id", JD.string()], ["description", JD.string()],
      (scene_id, description): GameLog => ({ t: "RemoveSceneChallenge", scene_id, description })
    ),
    SetFocusedSceneCreatures: JD.object(
      ["scene_id", JD.string()],
      ["creatures", JD.map(I.List as ((arr: Array<string>) => I.List<CreatureID>),
        JD.array(JD.string()))],
      (scene_id, creatures): GameLog => ({ t: "SetFocusedSceneCreatures", scene_id, creatures })
    ),
    RemoveSceneVolumeCondition: JD.object(
      ["scene_id", JD.string()],
      ["condition_id", JD.string()],
      (scene_id, condition_id): GameLog =>
        ({ t: "RemoveSceneVolumeCondition", scene_id, condition_id })
    ),
    EditSceneTerrain: JD.object(
      ["scene_id", JD.string()],
      ["terrain", decodeTerrain],
      (scene_id, terrain): GameLog => ({ t: "EditSceneTerrain", scene_id, terrain })),
    EditSceneHighlights: JD.object(
      ["scene_id", JD.string()],
      ["highlights", decodeHighlights],
      (scene_id, highlights): GameLog => ({ t: "EditSceneHighlights", scene_id, highlights })),
    EditSceneAnnotations: JD.object(
      ["scene_id", JD.string()],
      ["annotations", decodeAnnotations],
      (scene_id, annotations): GameLog => ({ t: "EditSceneAnnotations", scene_id, annotations })),
    EditSceneRelatedScenes: JD.object(
      ["scene_id", JD.string()],
      ["related_scenes", decodeSet(JD.string())],
      (scene_id, related_scenes): GameLog =>
        ({ t: "EditSceneRelatedScenes", scene_id, related_scenes })),
    EditSceneSceneHotspots: JD.object(
      ["scene_id", JD.string()],
      ["scene_hotspots", decodeIMap(decodePoint3, JD.string())],
      (scene_id, scene_hotspots): GameLog =>
        ({ t: "EditSceneSceneHotspots", scene_id, scene_hotspots })),
    SetCreaturePos: JD.map(
      ([scene_id, creature_id, pos]): GameLog =>
        ({ t: "SetCreaturePos", scene_id, creature_id, pos }),
      JD.tuple(JD.string(), JD.string(), decodePoint3)),
    PathCreature: JD.map(
      ([scene_id, creature_id, path]): GameLog =>
        ({ t: "PathCreature", scene_id, creature_id, path }),
      JD.tuple(JD.string(), JD.string(), JD.array(decodePoint3))),
    CreateCreature: JD.map(
      ([path, creature]): GameLog => ({ t: "CreateCreature", path, creature }),
      JD.tuple(decodeFolderPath, decodeCreatureData)),
    EditCreatureDetails: JD.object(["creature_id", JD.string()], ["details", decodeCreatureCreation],
      (creature_id, details): GameLog => ({ t: "EditCreatureDetails", creature_id, details })),
    AddCreatureToCombat: JD.map(
      ([creature_id, init]): GameLog => ({ t: "AddCreatureToCombat", creature_id, init }),
      JD.tuple(JD.string(), JD.number())),
    RemoveCreatureFromCombat: JD.map(
      (creature_id): GameLog => ({ t: "RemoveCreatureFromCombat", creature_id }),
      JD.string()),
    CombatLog: JD.map((log): GameLog => ({ t: "CombatLog", log }), decodeCombatLog),
    CreatureLog: JD.map(([creature_id, log]): GameLog => ({ t: "CreatureLog", creature_id, log }),
      JD.tuple(JD.string(), decodeCreatureLog)),
    AttributeCheckResult: JD.map(
      ([cid, check, actual, success]): GameLog =>
        ({ t: "AttributeCheckResult", cid, check, actual, success }),
      JD.tuple(JD.string(), decodeAttributeCheck, JD.number(), JD.boolean())
    ),
    Rollback: JD.map(
      ([snapshot_index, log_index]): GameLog => ({ t: "Rollback", snapshot_index, log_index }),
      JD.tuple(JD.number(), JD.number())),
    LoadModule: JD.object(
      ["name", JD.string()], ["path", decodeFolderPath], ["source", decodeModuleSource],
      (name, path, source): GameLog => ({ t: "LoadModule", name, path, source })),
  });

const decodePlayer: Decoder<Player> = JD.object(
  ["player_id", JD.string()],
  ["scene", maybe(JD.string())],
  ["creatures", JD.array(JD.string())],
  (player_id, scene, creatures) => ({ player_id, scene, creatures })
);

const decodeClass: Decoder<Class> = JD.object(
  ["id", JD.string()],
  ["name", JD.string()],
  ["color", JD.string()],
  (id, name, color) => ({ id, name, color })
);

function decodeNonEmpty<T>(valueDecoder: Decoder<T>): Decoder<{ cursor: number; data: Array<T> }> {
  return JD.object(
    ["cursor", JD.number()],
    ["data", JD.array(valueDecoder)],
    (cursor, data) => ({ cursor, data }));
}

const decodeCombat: Decoder<Combat> = JD.object(
  ["scene", JD.string()],
  ["creatures", decodeNonEmpty(JD.tuple(JD.string(), JD.number()))],
  ["movement_used", JD.number()],
  (scene, creatures, movement_used) => ({ scene, creatures, movement_used })
);


const decodeFolderNode: Decoder<FolderNode> = JD.object(
  ["scenes", JD.array(JD.string())],
  ["creatures", JD.array(JD.string())],
  ["items", JD.array(JD.string())],
  ["notes", JD.dict(decodeNote)],
  ["abilities", JD.array(JD.string())],
  ["classes", JD.array(JD.string())],
  (scenes, creatures, items, notes, abilities, classes) =>
    ({ scenes, creatures, items, notes, abilities, classes })
);

const decodeFolderLazy: Decoder<Folder> = JD.lazy(() => decodeFolder);

const decodeFolder: Decoder<Folder> = JD.object(
  ["data", decodeFolderNode],
  ["children", JD.map(I.Map, JD.dict(decodeFolderLazy))],
  (data, children) => ({ data, children })
);

const decodeCreatureTarget: Decoder<CreatureTarget> = sum<CreatureTarget>("TargetSpec",
  {
    Actor: { t: "Actor" },
    Melee: { t: "Melee" },
  },
  // | { t: "Volume"; volume: Volume; range: Distance }

  {
    Range: JD.map((distance): CreatureTarget => ({ t: "Range", distance }), JD.number()),
    SomeCreaturesInVolumeInRange: JD.object(
      ["volume", decodeVolume], ["maximum", JD.number()], ["range", JD.number()],
      (volume, maximum, range): CreatureTarget =>
        ({ t: "SomeCreaturesInVolumeInRange", volume, maximum, range })),
    AllCreaturesInVolumeInRange: JD.object(
      ["volume", decodeVolume],
      ["range", JD.number()],
      (volume, range): CreatureTarget => ({ t: "AllCreaturesInVolumeInRange", volume, range })),
    LineFromActor: JD.object(
      ["distance", JD.number()],
      (distance): CreatureTarget => ({ t: "LineFromActor", distance })),
  });

export const decodeSceneTarget: Decoder<SceneTarget> = sum<SceneTarget>("SceneTarget", {}, {
  RangedVolume: JD.object(["volume", decodeVolume],
    ["range", JD.number()],
    (volume, range): SceneTarget => ({ t: "RangedVolume", volume, range })),
});


export const decodeAction: Decoder<Action> = sum<Action>("Action", {},
  {
    Creature: JD.object(["target", decodeCreatureTarget],
      (target): Action => ({ t: "Creature", target })),
    SceneVolume: JD.object(["target", decodeSceneTarget],
      (target): Action => ({ t: "SceneVolume", target })),
  }
);

const decodeAbility: Decoder<Ability> = JD.object(
  ["name", JD.string()],
  ["id", JD.string()],
  ["action", decodeAction],
  ["cost", JD.number()],
  ["usable_ooc", JD.boolean()],
  (name, id, action, cost, usable_ooc) => ({ name, id, action, cost, usable_ooc })
);

const decodeGame: Decoder<Game> = JD.object(
  ["current_combat", maybe(decodeCombat)],
  ["creatures", JD.map(I.Map, JD.dict(decodeCreature))],
  ["classes", JD.map(I.Map, JD.dict(decodeClass))],
  ["items", JD.dict(decodeItem)],
  ["scenes", JD.map(I.Map, JD.dict(decodeScene))],
  ["abilities", JD.dict(decodeAbility)],
  ["campaign", decodeFolder],
  ["players", JD.map(I.Map, JD.dict(decodePlayer))],
  (current_combat, creatures, classes, items, scenes, abilities, campaign, players) =>
    ({ current_combat, creatures, classes, items, scenes, abilities, campaign, players })
);

export const decodeApp: Decoder<App> = JD.object(
  ["snapshots", JD.array(JD.map(
    ls => ({ snapshot: {}, logs: ls }),
    JD.at([1], JD.array(decodeGameLog))))],
  ["current_game", decodeGame],
  (snapshots, current_game) => ({ snapshots, current_game })
);

export const decodeSendCommandResult: Decoder<[Game, Array<GameLog>]> = JD.tuple(
  decodeGame,
  JD.array(decodeGameLog));


export function decodeRustResult<T, E>(decode_ok: Decoder<T>, decode_err: Decoder<E>
): Decoder<RustResult<T, E>> {
  return sum<RustResult<T, E>>("Result", {},
    {
      Ok: JD.map((result): RustResult<T, E> => ({ t: "Ok", result }), decode_ok),
      Err: JD.map((error): RustResult<T, E> => ({ t: "Err", error }), decode_err),
    });
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
    return "/" + LD.join(path, "/");
  }
}

function encodeNote(note: Note): object {
  return note;
}

function encodeDice(d: Dice): object {
  switch (d.t) {
    case "Flat": return { Flat: d.val };
    case "Expr": return { Expr: { num: d.num, size: d.size } };
    case "Plus": return { Plus: [encodeDice(d.left), encodeDice(d.right)] };
    case "BestOf": return { BestOf: [d.num, encodeDice(d.dice)] };
  }
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

export function maybe<T>(d: Decoder<T>): Decoder<T | undefined> {
  return JD.oneOf(JD.map(_ => undefined, JD.equal(null)), d);
}

export function sum<T>(
  name: string,
  nullaryValues: { [index: string]: T },
  decoders: { [index: string]: Decoder<T> }): Decoder<T> {
  /// This decoder is specific to the Serde-serialized JSON format:
  /// Nullary variants are just strings like "VariantName"
  /// Unary variants are {"VariantName": value}
  /// "tuple" variants are {"VariantName": [values, ...]}
  /// record variants are {"VariantName": {...}}

  function nullary(variant: string): T {
    if (nullaryValues.hasOwnProperty(variant)) {
      return nullaryValues[variant];
    } else {
      throw new Error(`Variant ${variant} is not a valid constructor for ${name}.`);
    }
  }

  const variants = Object.keys(decoders);
  const mapped_decoders: Array<Decoder<T>> =
    variants.map(variant => JD.at([variant], decoders[variant]));

  return JD.oneOf(
    JD.map(nullary, JD.string()),
    JD.oneOf.apply(null, mapped_decoders),
  );
}

function decodeIMap<K, V>(keyDecoder: Decoder<K>, valueDecoder: Decoder<V>): Decoder<I.Map<K, V>> {
  return JD.map(obj => I.Map(obj).mapKeys(k => keyDecoder.decodeAny(k)),
    JD.dict(valueDecoder));
}
