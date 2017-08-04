import * as I from 'immutable';
import * as LD from "lodash";
import * as JD from "type-safe-json-decoder";
import { Decoder } from "type-safe-json-decoder";

export type AbilityID = string;
export type CreatureID = string;
export type PlayerID = string;
export type SceneID = string;
export type ItemID = string;
export type AttrID = string;
export type MapID = string;
export type Color = string;
export type Distance = number;
export type HP = number;
export type Energy = number;
export type ConditionID = string;
export type FolderPath = Array<string>;
export type SpecialTile = [Point3, Color, string, Visibility];
export type SpecialTileData = [Color, string, Visibility];
export type Point3 = [number, number, number];
export type VectorCM = [number, number, number];

// Idea for a nicer constructor syntax, if I ever implement auto-generating this file:
//     const target = T.MkDecidedTarget.Creature({creature_id});
// as equivalent to
//     const target: T.DecidedTarget = {t: "DecidedTarget", creature_id};


export interface App {
  snapshots: Array<{ snapshot: {}, logs: Array<GameLog> }>;
  players: I.Map<PlayerID, Player>;
  current_game: Game;
}

export interface Game {
  current_combat: Combat | undefined;
  creatures: I.Map<CreatureID, Creature>;
  classes: I.Map<string, Class>;
  items: { [index: string]: Item };
  scenes: { [index: string]: Scene };
  abilities: { [index: string]: Ability };
  maps: { [index: string]: Map };
  campaign: Folder;
}

export interface Combat {
  scene: SceneID;
  creatures: { cursor: number; data: Array<[CreatureID, number]> };
  movement_used: number;
}

export interface Ability {
  name: string;
  action: Action;
  // cost: Energy;
  usable_ooc: boolean;
}

export type Action =
  // these variants also have an `effect` field but we don't use it in the client
  | { t: "Creature", target: CreatureTarget }
  | { t: "SceneVolume", target: SceneTarget }
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
  | { t: "Creature"; creature_id: CreatureID; }
  | { t: "Creatures"; creature_ids: Array<CreatureID>; }
  | { t: "Actor" }
  | { t: "Point"; point: Point3 };

export type Volume =
  | { t: "Sphere"; radius: Distance }
  | { t: "Line"; vector: VectorCM }
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
  maps: Array<MapID>;
  items: Array<ItemID>;
}

export type InventoryOwner =
  | { Creature: CreatureID }
  | { Scene: SceneID }
  ;

interface MapCreation {
  name: string;
  background_image_url: string;
  background_image_offset: [number, number];
  background_image_scale: [number, number];
}

export type GameCommand =
  | { t: "RegisterPlayer"; player_id: PlayerID }
  | { t: "GiveCreaturesToPlayer"; player_id: PlayerID; creature_ids: Array<CreatureID>; }
  | { t: "CreateFolder"; path: FolderPath }
  | { t: "MoveFolderItem"; source: FolderPath; item_id: FolderItemID; dest: FolderPath; }
  | { t: "CopyFolderItem"; source: FolderPath; item_id: FolderItemID; dest: FolderPath; }
  | { t: "DeleteFolderItem"; location: FolderPath; item_id: FolderItemID; }
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
  | { t: "AddCreatureToScene"; scene_id: SceneID; creature_id: CreatureID; visibility: Visibility; }
  | { t: "RemoveCreatureFromScene"; scene_id: SceneID; creature_id: CreatureID; }
  | { t: "AddSceneChallenge"; scene_id: SceneID; description: string; challenge: AttributeCheck; }
  | { t: "RemoveSceneChallenge"; scene_id: SceneID; description: string; }
  | { t: "CreateMap"; path: FolderPath; map: MapCreation }
  | { t: "EditMap"; map: Map }
  | { t: "EditMapDetails"; id: MapID; details: MapCreation }
  | { t: "EditMapTerrain"; id: MapID; terrain: Array<Point3>; specials: Array<SpecialTile> }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatAct"; ability_id: AbilityID; target: DecidedTarget }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; dest: Point3 }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; dest: Point3 }
  | { t: "PathCurrentCombatCreature"; dest: Point3 }
  | { t: "Done" }
  | { t: "ChangeCreatureInitiative"; creature_id: CreatureID; init: number }
  | { t: "StartCombat"; scene_id: SceneID; creature_ids: Array<CreatureID>; }
  | { t: "StopCombat" }
  | { t: "AddCreatureToCombat"; creature_id: CreatureID }
  | { t: "AttributeCheck"; creature_id: CreatureID; check: AttributeCheck; }
  | { t: "SetPlayerScene"; player_id: PlayerID, scene_id: SceneID | undefined }
  | { t: "Rollback"; snapshot_index: number; log_index: number; }
  ;


// AttributeCheck(CreatureID, AttributeCheck),
// CreateFolder(FolderPath),
// RenameFolder(FolderPath, String),
// MoveFolderItem(FolderPath, FolderItemID, FolderPath),
// DeleteFolderItem(FolderPath, FolderItemID),
// CreateItem(FolderPath, String),
// EditItem(Item),
// CreateNote(FolderPath, Note),
// EditNote(FolderPath, String, Note),
// CreateScene(FolderPath, SceneCreation),
// CreateMap(FolderPath, MapCreation),
// EditMap(Map),
// StartCombat(SceneID, Vec<CreatureID>),
// StopCombat,
// AddCreatureToCombat(CreatureID),
// RemoveCreatureFromCombat(CreatureID),
// ChangeCreatureInitiative(CreatureID, i16),
// RerollCombatInitiative,
// ForceNextTurn,
// ForcePrevTurn,
// ActCreature(SceneID, CreatureID, AbilityID, DecidedTarget),
// CombatAct(AbilityID, DecidedTarget),
// PathCurrentCombatCreature(Point3),
// Done,
// CreateCreature(FolderPath, CreatureCreation),
// SetCreaturePos(SceneID, CreatureID, Point3),
// PathCreature(SceneID, CreatureID, Point3),
// RegisterPlayer(PlayerID),
// GiveCreaturesToPlayer(PlayerID, Vec<CreatureID>),
// UnregisterPlayer(PlayerID),
// RemoveCreaturesFromPlayer(PlayerID, Vec<CreatureID>),
// SetPlayerScene(PlayerID, Option<SceneID>),
// Rollback(usize, usize),


export interface SceneCreation {
  name: string;
  map: MapID;
  background_image_url: string;
}

export interface CreatureCreation {
  name: string;
  class_: string;
  portrait_url: string;
  note: string;
  bio: string;
  initiative: Dice;
  size: AABB;
}

export interface Class {
  // abilities, conditions
  color: string;
}

export interface Player {
  player_id: PlayerID;
  scene?: SceneID;
  creatures: Array<CreatureID>;
}

export type GameLog =
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
  | { t: "AddCreatureToScene"; scene_id: SceneID; creature_id: CreatureID; visibility: Visibility; }
  | { t: "RemoveCreatureFromScene"; scene_id: SceneID; creature_id: CreatureID; }
  | { t: "AddSceneChallenge"; scene_id: SceneID; description: string; challenge: AttributeCheck; }
  | { t: "RemoveSceneChallenge"; scene_id: SceneID; description: string; }
  | { t: "CreateMap"; path: FolderPath; map: Map }
  | { t: "EditMap"; map: Map }
  | { t: "EditMapDetails"; id: MapID; details: MapCreation }
  | { t: "EditMapTerrain"; id: MapID; terrain: Array<Point3>; specials: Array<SpecialTile>; }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; pos: Point3 }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; path: Array<Point3> }
  | { t: "CreateCreature"; path: FolderPath; creature: Creature }
  | { t: "EditCreatureDetails"; creature_id: CreatureID; details: CreatureCreation; }
  | { t: "StartCombat"; scene: SceneID; creatures: Array<{ cid: CreatureID; init: number }> }
  | { t: "AddCreatureToCombat"; creature_id: CreatureID; init: number }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatLog"; log: CombatLog }
  | { t: "CreatureLog"; creature_id: CreatureID; log: CreatureLog }
  | { t: "StopCombat" }
  | { t: "Rollback"; snapshot_index: number; log_index: number };

export type CombatLog =
  | { t: "ConsumeMovement"; distance: Distance }
  | { t: "ChangeCreatureInitiative"; creature_id: CreatureID; init: number }
  | { t: "EndTurn", creature_id: CreatureID }
  | { t: "ForceNextTurn" }
  | { t: "ForcePrevTurn" }
  | { t: "RerollInitiative", combatants: Array<[CreatureID, number]> };

export type CreatureLog =
  | { t: "Damage"; hp: HP; rolls: Array<number> }
  | { t: "Heal"; hp: HP; rolls: Array<number> }
  | { t: "GenerateEnergy"; energy: Energy }
  | { t: "ReduceEnergy"; energy: Energy }
  | { t: "ApplyCondition"; condition_id: ConditionID, duration: Duration } // TODO Condition
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
  | { t: "AddDamageBuff", hp: HP }
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

export interface Creature {
  id: CreatureID;
  name: string;
  speed: Distance;
  max_energy: Energy;
  cur_energy: Energy;
  abilities: { [index: string]: AbilityStatus };
  class_: string;
  max_health: HP;
  cur_health: HP;
  conditions: I.Map<ConditionID, AppliedCondition>;
  note: string;
  bio: string;
  portrait_url: string;
  attributes: I.Map<AttrID, SkillLevel>;
  initiative: Dice;
  inventory: I.Map<ItemID, number>;
  size: AABB;
}

export interface AABB { x: number; y: number; z: number; }

export type FolderItemID =
  | { t: "SceneID"; id: SceneID }
  | { t: "MapID"; id: MapID }
  | { t: "CreatureID"; id: CreatureID }
  | { t: "NoteID"; id: string }
  | { t: "SubfolderID"; id: string }
  | { t: "ItemID"; id: ItemID };

export interface Map {
  id: MapID;
  name: string;
  terrain: Array<Point3>;
  specials: Array<SpecialTile>;
  background_image_url: string;
  background_image_scale: [number, number];
  background_image_offset: [number, number];
}

export interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";
export const SKILL_LEVELS: Array<SkillLevel> =
  ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

export interface Note {
  name: string;
  content: string;
}

export interface Scene {
  id: SceneID;
  name: string;
  map: MapID;
  creatures: I.Map<CreatureID, [Point3, Visibility]>;
  attribute_checks: I.Map<string, AttributeCheck>;
  inventory: I.Map<ItemID, number>;
  background_image_url: string;
}

export type Visibility =
  | { t: "GMOnly" }
  | { t: "AllPlayers" };

export type Dice =
  | { t: "Flat", val: number }
  | { t: "Expr", num: number, size: number }
  | { t: "Plus", left: Dice, right: Dice }
  | { t: "BestOf", num: number, dice: Dice };

export type PotentialTargets =
  | { t: "CreatureIDs"; cids: Array<CreatureID> }
  | { t: "Points"; points: Array<Point3> }
  ;

export type RustResult<T, E> =
  | { t: "Ok", result: T }
  | { t: "Err", error: E }
  ;

// ** Decoders **

export const decodePoint3: Decoder<Point3> = JD.tuple(JD.number(), JD.number(), JD.number());

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

function object17<T, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q>(
  _ad: JD.EntryDecoder<A>, _bd: JD.EntryDecoder<B>, _cd: JD.EntryDecoder<C>, _dd: JD.EntryDecoder<D>,
  _ed: JD.EntryDecoder<E>, _fd: JD.EntryDecoder<F>, _gd: JD.EntryDecoder<G>, _hd: JD.EntryDecoder<H>,
  _id: JD.EntryDecoder<I>, _jd: JD.EntryDecoder<J>, _kd: JD.EntryDecoder<K>, _ld: JD.EntryDecoder<L>,
  _md: JD.EntryDecoder<M>, _nd: JD.EntryDecoder<N>, _od: JD.EntryDecoder<O>, _pd: JD.EntryDecoder<P>,
  _qd: JD.EntryDecoder<Q>,
  _cons: (
    a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O,
    p: P, q: Q) => T): Decoder<T> {
  return JD.object.apply(undefined, arguments);
}

export const decodeCreature: Decoder<Creature> = object17(
  ["id", JD.string()],
  ["name", JD.string()],
  ["speed", JD.number()],
  ["max_energy", JD.number()],
  ["cur_energy", JD.number()],
  ["abilities", JD.dict(decodeAbilityStatus)],
  ["class", JD.string()],
  ["max_health", JD.number()],
  ["cur_health", JD.number()],
  ["note", JD.string()],
  ["bio", JD.string()],
  ["portrait_url", JD.string()],
  ["attributes", JD.map(I.Map, JD.dict(decodeSkillLevel))],
  ["inventory", JD.map(I.Map, JD.dict(JD.number()))],
  ["conditions", JD.map(I.Map, JD.dict(decodeAppliedCondition))],
  ["initiative", decodeDice],
  ["size", decodeAABB],
  (
    id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health, note, bio,
    portrait_url, attributes, inventory, conditions, initiative, size) =>
    ({
      id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health, note, bio,
      portrait_url, attributes, inventory, conditions, initiative, size,
    })
);

const decodeCreatureCreation: Decoder<CreatureCreation> = JD.object(
  ["name", JD.string()],
  ["class", JD.string()],
  ["portrait_url", JD.string()],
  ["note", JD.string()],
  ["bio", JD.string()],
  ["initiative", decodeDice],
  ["size", decodeAABB],
  (name, class_, portrait_url, note, bio, initiative, size) =>
    ({ name, class_, portrait_url, note, bio, initiative, size })
);

export const decodeVisibility: Decoder<Visibility> = JD.map((x): Visibility => {
  switch (x) {
    case "GMOnly": return { t: "GMOnly" };
    case "AllPlayers": return { t: "AllPlayers" };
    default: throw new Error(`Not a Visibility: ${x}.`);
  }
}, JD.string());

const decodeMapCreation: Decoder<MapCreation> = JD.object(
  ["name", JD.string()],
  ["background_image_url", JD.string()],
  ["background_image_offset", JD.tuple(JD.number(), JD.number())],
  ["background_image_scale", JD.tuple(JD.number(), JD.number())],
  (name, background_image_url, background_image_offset, background_image_scale) =>
    ({ name, background_image_url, background_image_offset, background_image_scale }),
);

const decodeSpecialTile: Decoder<SpecialTile> =
  JD.tuple(decodePoint3, JD.string(), JD.string(), decodeVisibility);

const decodeMap: Decoder<Map> = JD.object(
  ["id", JD.string()],
  ["name", JD.string()],
  ["terrain", JD.array(decodePoint3)],
  ["specials", JD.array(decodeSpecialTile)],
  ["background_image_url", JD.string()],
  ["background_image_offset", JD.tuple(JD.number(), JD.number())],
  ["background_image_scale", JD.tuple(JD.number(), JD.number())],
  (
    id, name, terrain, specials,
    background_image_url, background_image_offset, background_image_scale) =>
    ({
      id, name, terrain, specials,
      background_image_url, background_image_offset, background_image_scale,
    })
);

export const decodeAttributeCheck: Decoder<AttributeCheck> =
  JD.object(["reliable", JD.boolean()], ["attr", JD.string()], ["target", decodeSkillLevel],
    (reliable, attr, target) => ({ reliable, attr, target }));

const decodeSceneCreation: Decoder<SceneCreation> = JD.object(
  ["name", JD.string()],
  ["map", JD.string()],
  ["background_image_url", JD.string()],
  (name, map, background_image_url) => ({ name, map, background_image_url })
);

export const decodeScene: Decoder<Scene> =
  JD.object(
    ["id", JD.string()],
    ["name", JD.string()],
    ["map", JD.string()],
    ["creatures", JD.map(I.Map, JD.dict(JD.tuple(decodePoint3, decodeVisibility)))],
    ["attribute_checks", JD.map(I.Map, JD.dict(decodeAttributeCheck))],
    ["inventory", JD.map(I.Map, JD.dict(JD.number()))],
    ["background_image_url", JD.string()],
    (id, name, map, creatures, attribute_checks, inventory, background_image_url): Scene =>
      ({ id, name, map, creatures, attribute_checks, inventory, background_image_url }));

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
    CreateMap: JD.map(
      ([path, map]): GameLog => ({ t: "CreateMap", path, map }),
      JD.tuple(decodeFolderPath, decodeMap)),
    EditMap: JD.map((map): GameLog => ({ t: "EditMap", map }), decodeMap),
    EditMapDetails: JD.object(["id", JD.string()], ["details", decodeMapCreation],
      (id, details): GameLog => ({ t: "EditMapDetails", id, details })),
    EditMapTerrain: JD.object(
      ["id", JD.string()],
      ["terrain", JD.array(decodePoint3)],
      ["specials", JD.array(decodeSpecialTile)],
      (id, terrain, specials): GameLog => ({ t: "EditMapTerrain", id, terrain, specials })),
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
      JD.tuple(decodeFolderPath, decodeCreature)),
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
  });

const decodePlayer: Decoder<Player> = JD.object(
  ["player_id", JD.string()],
  ["scene", maybe(JD.string())],
  ["creatures", JD.array(JD.string())],
  (player_id, scene, creatures) => ({ player_id, scene, creatures })
);

const decodeClass: Decoder<Class> = JD.object(
  ["color", JD.string()],
  color => ({ color })
);

function decodeNonEmpty<T>(valueDecoder: Decoder<T>): Decoder<{ cursor: number, data: Array<T> }> {
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
  ["maps", JD.array(JD.string())],
  ["items", JD.array(JD.string())],
  ["notes", JD.dict(decodeNote)],
  (scenes, creatures, maps, items, notes) => ({ scenes, creatures, maps, items, notes })
);

const decodeFolderLazy: Decoder<Folder> = JD.lazy(() => decodeFolder);

const decodeFolder: Decoder<Folder> = JD.object(
  ["data", decodeFolderNode],
  ["children", JD.map(I.Map, JD.dict(decodeFolderLazy))],
  (data, children) => ({ data, children })
);

const decodeVectorCM: Decoder<VectorCM> = JD.tuple(JD.number(), JD.number(), JD.number());

const decodeVolume: Decoder<Volume> = sum("Volume", {},
  {
    Sphere: JD.map((radius): Volume => ({ t: "Sphere", radius }), JD.number()),
    Line: JD.map((vector): Volume => ({ t: "Line", vector }), decodeVectorCM),
    VerticalCylinder: JD.object(
      ["radius", JD.number()],
      ["height", JD.number()],
      (radius, height): Volume => ({ t: "VerticalCylinder", radius, height })
    ),
    AABB: JD.map((aabb): Volume => ({ t: "AABB", aabb }), decodeAABB),
  });

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
  ["action", decodeAction],
  ["usable_ooc", JD.boolean()],
  (name, action, usable_ooc) => ({ name, action, usable_ooc })
);

const decodeGame: Decoder<Game> = JD.object(
  ["current_combat", JD.oneOf(decodeCombat, JD.map(_ => undefined, JD.equal(null)))],
  ["creatures", JD.map(I.Map, JD.dict(decodeCreature))],
  ["classes", JD.map(I.Map, JD.dict(decodeClass))],
  ["items", JD.dict(decodeItem)],
  ["scenes", JD.dict(decodeScene)],
  ["abilities", JD.dict(decodeAbility)],
  ["maps", JD.dict(decodeMap)],
  ["campaign", decodeFolder],
  (current_combat, creatures, classes, items, scenes, abilities, maps, campaign) =>
    ({ current_combat, creatures, classes, items, scenes, abilities, maps, campaign })
);

export const decodeApp: Decoder<App> = JD.object(
  ["snapshots", JD.array(JD.map(
    ls => ({ snapshot: {}, logs: ls }),
    JD.at([1], JD.array(decodeGameLog))))],
  ["players", JD.map(I.Map, JD.dict(decodePlayer))],
  ["current_game", decodeGame],
  (snapshots, players, current_game) => ({ snapshots, players, current_game })
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
    case "RegisterPlayer": return { RegisterPlayer: cmd.player_id };
    case "GiveCreaturesToPlayer":
      return { GiveCreaturesToPlayer: [cmd.player_id, cmd.creature_ids] };
    case "CreateFolder": return { CreateFolder: encodeFolderPath(cmd.path) };
    case "MoveFolderItem":
      return {
        MoveFolderItem:
        [encodeFolderPath(cmd.source), encodeFolderItemID(cmd.item_id), encodeFolderPath(cmd.dest)],
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
    case "CreateMap":
      return { CreateMap: [encodeFolderPath(cmd.path), encodeMapCreation(cmd.map)] };
    case "EditMap":
      return { EditMap: encodeMap(cmd.map) };
    case "EditMapDetails":
      return { EditMapDetails: { id: cmd.id, details: encodeMapCreation(cmd.details) } };
    case "EditMapTerrain":
      return {
        EditMapTerrain: {
          id: cmd.id, terrain: cmd.terrain.map(encodePoint3),
          specials: cmd.specials.map(encodeSpecialTile),
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
  }
}

function encodeFolderItemID(fid: FolderItemID): object {
  return { [fid.t]: fid.id };
}

function encodeMapCreation(mc: MapCreation): object {
  return {
    name: mc.name,
    background_image_url: mc.background_image_url,
    background_image_offset: mc.background_image_offset,
    background_image_scale: mc.background_image_scale,
  };
}

function encodeInventoryOwner(owner: InventoryOwner): object {
  return owner;
}

function encodeCreatureCreation(cc: CreatureCreation): object {
  return {
    name: cc.name,
    class: cc.class_,
    portrait_url: cc.portrait_url,
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
    map: sc.map,
    background_image_url: sc.background_image_url,
  };
}

function encodeMap(map: Map): object {
  return {
    id: map.id,
    name: map.name,
    terrain: map.terrain.map(encodePoint3),
    specials: map.specials.map(
      ([pt, color, note, vis]) => [encodePoint3(pt), color, note, encodeVisibility(vis)]),
    background_image_url: map.background_image_url,
    background_image_scale: map.background_image_scale,
    background_image_offset: map.background_image_offset,
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

function encodePoint3(pt: Point3): Point3 {
  return pt;
}

function encodeSpecialTile(t: SpecialTile): Array<object | string> {
  return [encodePoint3(t[0]), t[1], t[2], encodeVisibility(t[3])];
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
