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
export type ConditionID = number;
export type FolderPath = Array<string>;


// Idea for a nicer constructor syntax, if I ever implement auto-generating this file:
//     const target = T.MkDecidedTarget.Creature({creature_id});
// as equivalent to
//     const target: T.DecidedTarget = {t: "DecidedTarget", creature_id};


export interface App {
  snapshots: AppSnapshots;
  players: AppPlayers;
  current_game: Game;
}

export interface Game {
  current_combat: Combat | undefined;
  creatures: { [index: string]: Creature };
  classes: { [index: string]: Class };
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
  target: TargetSpec;
  // cost: Energy;
  // effects: Array<Effect>;
  usable_ooc: boolean;
}

export type TargetSpec =
  | { t: "Melee" }
  | { t: "Range"; distance: Distance }
  | { t: "Actor" }
  | { t: "SomeCreaturesInVolumeInRange"; volume: Volume; maximum: number; range: Distance }
  | { t: "AllCreaturesInVolumeInRange"; volume: Volume; range: Distance }
  | { t: "Volume"; volume: Volume; range: Distance };

export type DecidedTarget =
  | { t: "Creature"; creature_id: CreatureID; }
  | { t: "Creatures"; creature_ids: Array<CreatureID>; }
  | { t: "Actor" }
  | { t: "Point"; point: Point3 };

export type Volume =
  | { t: "Sphere"; radius: Distance }
  | { t: "Line"; length: Distance }
  | { t: "VerticalCylinder"; radius: Distance; height: Distance }
  | { t: "AABB"; aabb: AABB };

export interface Folder {
  data: FolderNode;
  children: { [index: string]: Folder };
}

export interface FolderNode {
  scenes: Array<SceneID>;
  creatures: Array<CreatureID>;
  notes: { [index: string]: Note };
  maps: Array<MapID>;
  items: Array<ItemID>;
}

export type GameCommand =
  | { t: "RegisterPlayer"; player_id: PlayerID }
  | { t: "EditCreature"; creature: Creature }
  | { t: "CreateNote"; path: FolderPath; note: Note }
  | { t: "EditNote"; path: FolderPath; name: string; note: Note }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatAct"; ability_id: AbilityID; target: DecidedTarget }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; dest: Point3 }
  | { t: "PathCurrentCombatCreature"; dest: Point3 }
  | { t: "Done" }
  | { t: "ChangeCreatureInitiative", creature_id: CreatureID; init: number }
  | { t: "SetPlayerScene"; player_id: PlayerID, scene_id: SceneID | undefined }
  | { t: "Rollback"; snapshot_index: number; log_index: number; }
  ;


// AttributeCheck(CreatureID, AttributeCheck),
// CreateFolder(FolderPath),
// RenameFolder(FolderPath, String),
// DeleteFolder(FolderPath),
// MoveFolderItem(FolderPath, FolderItemID, FolderPath),
// DeleteFolderItem(FolderPath, FolderItemID),
// CreateItem(FolderPath, String),
// EditItem(Item),
// CreateNote(FolderPath, Note),
// EditNote(FolderPath, String, Note),
// DeleteNote(FolderPath, String),
// CreateScene(FolderPath, SceneCreation),
// EditScene(Scene),
// DeleteScene(SceneID),
// CreateMap(FolderPath, MapCreation),
// EditMap(Map),
// DeleteMap(MapID),
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
// EditCreature(Creature),
// SetCreaturePos(SceneID, CreatureID, Point3),
// PathCreature(SceneID, CreatureID, Point3),
// DeleteCreature(CreatureID),
// RegisterPlayer(PlayerID),
// GiveCreaturesToPlayer(PlayerID, Vec<CreatureID>),
// UnregisterPlayer(PlayerID),
// RemoveCreaturesFromPlayer(PlayerID, Vec<CreatureID>),
// SetPlayerScene(PlayerID, Option<SceneID>),
// Rollback(usize, usize),


export interface Class {
  // abilities, conditions
  color: string;
}

export interface Player {
  player_id: PlayerID;
  scene?: SceneID;
  creatures: Array<CreatureID>;
}

export type AppSnapshots = Array<{ snapshot: {}, logs: Array<GameLog> }>;
export interface AppPlayers { [index: string]: Player; }

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
  | { t: "DeleteFolder"; path: FolderPath }
  | { t: "DeleteFolderItem"; path: FolderPath; item: FolderItemID }
  | { t: "MoveFolderItem"; path: FolderPath; item: FolderItemID; newPath: FolderPath }
  | { t: "CreateItem"; path: FolderPath; item: Item }
  | { t: "EditItem"; item: Item }
  | { t: "CreateNote"; path: FolderPath; note: Note }
  | { t: "EditNote"; path: FolderPath; name: string; newNote: Note }
  | { t: "DeleteNote"; path: FolderPath; name: string }
  | { t: "CreateScene"; path: FolderPath; scene: Scene }
  | { t: "EditScene"; scene: Scene }
  | { t: "DeleteScene"; scene_id: SceneID }
  | { t: "CreateMap"; path: FolderPath; map: Map }
  | { t: "EditMap"; map: Map }
  | { t: "DeleteMap"; map_id: MapID }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; pos: Point3 }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; path: Array<Point3> }
  | { t: "CreateCreature"; path: FolderPath; creature: Creature }
  | { t: "EditCreature"; creature: Creature }
  | { t: "DeleteCreature"; creature_id: CreatureID }
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
  | { t: "ApplyCondition"; condition_id: ConditionID, duration: ConditionDuration } // TODO Condition
  | { t: "DecrementConditionRemaining"; condition_id: ConditionID }
  | { t: "RemoveCondition"; condition_id: ConditionID };

export interface Item {
  id: ItemID;
  name: string;
}

export type Effect =
  | { t: "ApplyCondition"; duration: ConditionDuration; condition: Condition }
  | { t: "Heal"; dice: Dice }
  | { t: "Damage"; dice: Dice }
  | { t: "MultiEffect"; effects: Array<Effect> }
  | { t: "GenerateEnergy"; energy: Energy };

export type ConditionDuration =
  | { t: "Interminate" }
  | { t: "Duration"; duration: number };

export type Condition =
  | { t: "RecurringEffect"; effect: Effect }
  | { t: "Dead" }
  | { t: "Incapacitated" }
  | { t: "AddDamageBuff", hp: HP }
  | { t: "DoubleMaxMovement" }
  | { t: "ActivateAbility"; ability_id: AbilityID };

export interface AppliedCondition {
  remaining: ConditionDuration;
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
  conditions: { [index: string]: AppliedCondition }; // key: ConditionID
  note: string;
  portrait_url: string;
  attributes: { [index: string]: SkillLevel }; // key: AttrID
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
  specials: Array<[Point3, Color, string, Visibility]>;
}

export interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";
const SKILL_LEVELS: Array<SkillLevel> = ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

export interface Note {
  name: string;
  content: string;
}

export interface Scene {
  id: SceneID;
  name: string;
  map: MapID;
  creatures: { [index: string]: [Point3, Visibility] };
  attribute_checks: { [index: string]: AttributeCheck };
}

export type Point3 = [number, number, number];

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

// ** Decoders **

export const decodePoint3: Decoder<Point3> = JD.tuple(JD.number(), JD.number(), JD.number());

export const decodePotentialTargets = sum<PotentialTargets>("PotentialTargets", {}, {
  CreatureIDs: JD.map((cids): PotentialTargets =>
    ({ t: "CreatureIDs", cids }), JD.array(JD.string())),
  Points: JD.map((points): PotentialTargets => ({ t: "Points", points }), JD.array(decodePoint3)),
});

export const decodeDiceLazy = JD.lazy(() => decodeDice);
export const decodeConditionLazy = JD.lazy(() => decodeCondition);
export const decodeEffectLazy = JD.lazy(() => decodeEffect);

export const decodeDice: Decoder<Dice> = sum<Dice>("Dice", {}, {
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

export const decodeConditionDuration: Decoder<ConditionDuration> =
  sum<ConditionDuration>("ConditionDuration", { Interminate: { t: "Interminate" } },
    {
      Duration: JD.map(
        (duration): ConditionDuration => ({ t: "Duration", duration }),
        JD.number()),
    });

export const decodeEffect: Decoder<Effect> = sum<Effect>("Effect", {},
  {
    ApplyCondition: JD.map(
      ([duration, condition]): Effect => ({ t: "ApplyCondition", duration, condition }),
      JD.tuple(decodeConditionDuration, decodeConditionLazy)),
    Damage: JD.map((dice): Effect => ({ t: "Damage", dice }), decodeDice),
    GenerateEffect: JD.map(
      (energy): Effect => ({ t: "GenerateEnergy", energy }),
      JD.number()),
    Heal: JD.map((dice): Effect => ({ t: "Heal", dice }), decodeDice),
    MultiEffect: JD.map(
      (effects): Effect => ({ t: "MultiEffect", effects }),
      JD.array(decodeEffectLazy)),
  });

export const decodeCondition: Decoder<Condition> = sum<Condition>("Condition",
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

export const decodeAppliedCondition: Decoder<AppliedCondition> = JD.object(
  ["remaining", decodeConditionDuration],
  ["condition", decodeCondition],
  (remaining, condition) => ({ remaining, condition })
);

export const decodeSkillLevel: Decoder<SkillLevel> =
  JD.oneOf.apply(null, SKILL_LEVELS.map(JD.equal));

export const decodeAbilityStatus: Decoder<AbilityStatus> = JD.object(
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

export function object16<T, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P>(
  ad: JD.EntryDecoder<A>, bd: JD.EntryDecoder<B>, cd: JD.EntryDecoder<C>, dd: JD.EntryDecoder<D>,
  ed: JD.EntryDecoder<E>, fd: JD.EntryDecoder<F>, gd: JD.EntryDecoder<G>, hd: JD.EntryDecoder<H>,
  id: JD.EntryDecoder<I>, jd: JD.EntryDecoder<J>, kd: JD.EntryDecoder<K>, ld: JD.EntryDecoder<L>,
  md: JD.EntryDecoder<M>, nd: JD.EntryDecoder<N>, od: JD.EntryDecoder<O>, pd: JD.EntryDecoder<P>,
  cons: (
    a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O,
    p: P) => T): Decoder<T> {
  return JD.object.apply(undefined, arguments);
}

export const decodeCreature: Decoder<Creature> = object16(
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
  ["portrait_url", JD.string()],
  ["attributes", JD.dict(decodeSkillLevel)],
  ["inventory", JD.map(I.Map, JD.dict(JD.number()))],
  ["conditions", JD.dict(decodeAppliedCondition)],
  ["initiative", decodeDice],
  ["size", decodeAABB],
  (
    id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health, note,
    portrait_url, attributes, inventory, conditions, initiative, size) =>
    ({
      id, name, speed, max_energy, cur_energy, abilities, class_, max_health, cur_health, note,
      portrait_url, attributes, inventory, conditions, initiative, size,
    })
);

export const decodeVisibility: Decoder<Visibility> = JD.map((x): Visibility => {
  switch (x) {
    case "GMOnly": return { t: "GMOnly" };
    case "AllPlayers": return { t: "AllPlayers" };
    default: throw new Error(`Not a Visibility: ${x}.`);
  }
}, JD.string());

export const decodeMap: Decoder<Map> = JD.object(
  ["id", JD.string()],
  ["name", JD.string()],
  ["terrain", JD.array(decodePoint3)],
  ["specials", JD.array(JD.tuple(decodePoint3, JD.string(), JD.string(), decodeVisibility))],
  (id, name, terrain, specials) => ({ id, name, terrain, specials })
);

export const decodeAttributeCheck: Decoder<AttributeCheck> =
  JD.object(["reliable", JD.boolean()], ["attr", JD.string()], ["target", decodeSkillLevel],
    (reliable, attr, target) => ({ reliable, attr, target }));

export const decodeScene: Decoder<Scene> =
  JD.object(
    ["id", JD.string()],
    ["name", JD.string()],
    ["map", JD.string()],
    ["creatures", JD.dict(JD.tuple(decodePoint3, decodeVisibility))],
    ["attribute_checks", JD.dict(decodeAttributeCheck)],
    (id, name, map, creatures, attribute_checks): Scene =>
      ({ id, name, map, creatures, attribute_checks }));

function _mkFolderItem(t: string): Decoder<FolderItemID> {
  return JD.map(id => ({ t, id } as FolderItemID), JD.string());
}
export const decodeFolderItemID: Decoder<FolderItemID> =
  sum<FolderItemID>("FolderItemID", {}, {
    SceneID: _mkFolderItem("SceneID"),
    MapID: _mkFolderItem("MapID"),
    CreatureID: _mkFolderItem("CreatureID"),
    NoteID: _mkFolderItem("NoteID"),
    ItemID: _mkFolderItem("ItemID"),
    SubfolderID: _mkFolderItem("SubfolderID"),
  });

export const decodeFolderPath: Decoder<FolderPath> =
  JD.map(strpath => {
    if (strpath === "") {
      return [];
    } else if (LD.startsWith(strpath, "/")) {
      return LD.slice(LD.split(strpath, "/"), 1);
    } else {
      throw new Error(`Not a path: ${strpath}.`);
    }
  }, JD.string());

export const decodeItem: Decoder<Item> =
  JD.object(
    ["id", JD.string()],
    ["name", JD.string()],
    (id, name) => ({ id, name })
  );

export const decodeNote: Decoder<Note> =
  JD.object(
    ["name", JD.string()],
    ["content", JD.string()],
    (name, content) => ({ name, content })
  );

export const decodeCreatureLog: Decoder<CreatureLog> =
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
      JD.tuple(JD.number(), decodeConditionDuration)),
    DecrementConditionRemaining: JD.map(
      (condition_id): CreatureLog => ({ t: "DecrementConditionRemaining", condition_id }),
      JD.number()),
    RemoveCondition: JD.map((condition_id): CreatureLog => ({ t: "RemoveCondition", condition_id }),
      JD.number()),
  });

export const decodeCombatLog: Decoder<CombatLog> =
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
    DeleteFolder: JD.map((path): GameLog => ({ t: "DeleteFolder", path }), decodeFolderPath),
    DeleteFolderItem: JD.map(([path, item]): GameLog => ({ t: "DeleteFolderItem", path, item }),
      JD.tuple(decodeFolderPath, decodeFolderItemID)),
    MoveFolderItem: JD.map(
      ([path, item, newPath]): GameLog => ({ t: "MoveFolderItem", path, item, newPath }),
      JD.tuple(decodeFolderPath, decodeFolderItemID, decodeFolderPath)),
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
    DeleteNote: JD.map(
      ([path, name]): GameLog => ({ t: "DeleteNote", path, name }),
      JD.tuple(decodeFolderPath, JD.string())),
    CreateScene: JD.map(
      ([path, scene]): GameLog => ({ t: "CreateScene", path, scene }),
      JD.tuple(decodeFolderPath, decodeScene)),
    EditScene: JD.map((scene): GameLog => ({ t: "EditScene", scene }), decodeScene),
    DeleteScene: JD.map((scene_id): GameLog => ({ t: "DeleteScene", scene_id }), JD.string()),
    CreateMap: JD.map(
      ([path, map]): GameLog => ({ t: "CreateMap", path, map }),
      JD.tuple(decodeFolderPath, decodeMap)),
    EditMap: JD.map((map): GameLog => ({ t: "EditMap", map }), decodeMap),
    DeleteMap: JD.map((map_id): GameLog => ({ t: "DeleteMap", map_id }), JD.string()),
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
    EditCreature: JD.map((creature): GameLog => ({ t: "EditCreature", creature }), decodeCreature),
    DeleteCreature: JD.map(
      (creature_id): GameLog => ({ t: "DeleteCreature", creature_id }),
      JD.string()),
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

export const decodeAppSnapshots: Decoder<AppSnapshots> =
  JD.array(JD.map(
    ls => ({ snapshot: {}, logs: ls }),
    JD.at([1], JD.array(decodeGameLog))));

export const decodePlayer: Decoder<Player> = JD.object(
  ["player_id", JD.string()],
  ["scene", maybe(JD.string())],
  ["creatures", JD.array(JD.string())],
  (player_id, scene, creatures) => ({ player_id, scene, creatures })
);

export const decodeAppPlayers: Decoder<AppPlayers> = JD.dict(decodePlayer);

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

export const decodeCombat: Decoder<Combat> = JD.object(
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
  ["children", JD.dict(decodeFolderLazy)],
  (data, children) => ({ data, children })
);

const decodeVolume: Decoder<Volume> = sum("Volume", {},
  {
    Sphere: JD.map((radius): Volume => ({ t: "Sphere", radius }), JD.number()),
    Line: JD.map((length): Volume => ({ t: "Line", length }), JD.number()),
    VerticalCylinder: JD.object(
      ["radius", JD.number()],
      ["height", JD.number()],
      (radius, height): Volume => ({ t: "VerticalCylinder", radius, height })
    ),
    AABB: JD.map((aabb): Volume => ({ t: "AABB", aabb }), decodeAABB),
  });

const decodeTargetSpec: Decoder<TargetSpec> = sum<TargetSpec>("TargetSpec",
  {
    Actor: { t: "Actor" },
    Melee: { t: "Melee" },
  },
  // | { t: "Volume"; volume: Volume; range: Distance }

  {
    Range: JD.map((distance): TargetSpec => ({ t: "Range", distance }), JD.number()),
    SomeCreaturesInVolumeInRange: JD.object(
      ["volume", decodeVolume], ["maximum", JD.number()], ["range", JD.number()],
      (volume, maximum, range): TargetSpec =>
        ({ t: "SomeCreaturesInVolumeInRange", volume, maximum, range })),
    AllCreaturesInVolumeInRange: JD.object(
      ["volume", decodeVolume],
      ["range", JD.number()],
      (volume, range): TargetSpec => ({ t: "AllCreaturesInVolumeInRange", volume, range })),
    Volume: JD.object(
      ["volume", decodeVolume],
      ["range", JD.number()],
      (volume, range): TargetSpec => ({ t: "Volume", volume, range })
    ),
  });

const decodeAbility: Decoder<Ability> = JD.object(
  ["name", JD.string()],
  ["target", decodeTargetSpec],
  ["usable_ooc", JD.boolean()],
  (name, target, usable_ooc) => ({ name, target, usable_ooc })
);

export const decodeGame: Decoder<Game> = JD.object(
  ["current_combat", JD.oneOf(decodeCombat, JD.map(_ => undefined, JD.equal(null)))],
  ["creatures", JD.dict(decodeCreature)],
  ["classes", JD.dict(decodeClass)],
  ["items", JD.dict(decodeItem)],
  ["scenes", JD.dict(decodeScene)],
  ["abilities", JD.dict(decodeAbility)],
  ["maps", JD.dict(decodeMap)],
  ["campaign", decodeFolder],
  (current_combat, creatures, classes, items, scenes, abilities, maps, campaign) =>
    ({ current_combat, creatures, classes, items, scenes, abilities, maps, campaign })
);

export const decodeApp: Decoder<App> = JD.object(
  ["snapshots", decodeAppSnapshots],
  ["players", decodeAppPlayers],
  ["current_game", decodeGame],
  (snapshots, players, current_game) => ({ snapshots, players, current_game })
);

export function encodeGameCommand(cmd: GameCommand): object | string {
  switch (cmd.t) {
    case "RegisterPlayer": return { RegisterPlayer: cmd.player_id };
    case "EditCreature": return { EditCreature: encodeCreature(cmd.creature) };
    case "CreateNote": return { CreateNote: [encodeFolderPath(cmd.path), encodeNote(cmd.note)] };
    case "EditNote":
      return { EditNote: [encodeFolderPath(cmd.path), cmd.name, encodeNote(cmd.note)] };
    case "RemoveCreatureFromCombat":
      return { RemoveCreatureFromCombat: cmd.creature_id };
    case "CombatAct": return { CombatAct: [cmd.ability_id, encodeDecidedTarget(cmd.target)] };
    case "PathCreature":
      return { PathCreature: [cmd.scene_id, cmd.creature_id, encodePoint3(cmd.dest)] };
    case "PathCurrentCombatCreature":
      return { PathCurrentCombatCreature: encodePoint3(cmd.dest) };
    case "Done": return "Done";
    case "ChangeCreatureInitiative":
      return { ChangeCreatureInitiative: [cmd.creature_id, cmd.init] };
    case "SetPlayerScene":
      return { SetPlayerScene: [cmd.player_id, cmd.scene_id] };
    case "Rollback":
      return { Rollback: [cmd.snapshot_index, cmd.log_index] };
  }
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

function encodeConditionDuration(cd: ConditionDuration): string | object {
  switch (cd.t) {
    case "Interminate": return "Interminate";
    case "Duration": return { Duration: cd.duration };
  }
}

function encodeDice(d: Dice): object {
  switch (d.t) {
    case "Flat": return { Flat: d.val };
    case "Expr": return { Expr: { num: d.num, size: d.size } };
    case "Plus": return { Plus: [encodeDice(d.left), encodeDice(d.right)] };
    case "BestOf": return { BestOf: [d.num, encodeDice(d.dice)] };
  }
}

function encodeEffect(eff: Effect): object {
  switch (eff.t) {
    case "ApplyCondition":
      return {
        ApplyCondition: [
          encodeConditionDuration(eff.duration),
          encodeCondition(eff.condition)],
      };
    case "Heal": return { Heal: encodeDice(eff.dice) };
    case "Damage": return { Damage: encodeDice(eff.dice) };
    case "MultiEffect": return { MultiEffect: eff.effects.map(encodeEffect) };
    case "GenerateEnergy": return { GenerateEnergy: eff.energy };
  }
}

function encodeCondition(c: Condition): string | object {
  switch (c.t) {
    case "RecurringEffect": return { RecurringEffect: encodeEffect(c.effect) };
    case "Dead": return "Dead";
    case "Incapacitated": return "Incapacitated";
    case "AddDamageBuff": return { AddDamageBuff: c.hp };
    case "DoubleMaxMovement": return "DoubleMaxMovement";
    case "ActivateAbility": return { ActivateAbility: c.ability_id };
  }
}

function encodeAppliedCondition(ac: AppliedCondition): object {
  return {
    remaining: encodeConditionDuration(ac.remaining),
    condition: encodeCondition(ac.condition),
  };
}

function encodeAbilityStatus(as: AbilityStatus): object {
  return as;
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

export function encodeCreature(c: Creature): object {
  return {
    id: c.id,
    name: c.name,
    speed: c.speed,
    max_energy: c.max_energy,
    cur_energy: c.cur_energy,
    abilities: LD.mapValues(c.abilities, encodeAbilityStatus),
    class: c.class_,
    max_health: c.max_health,
    cur_health: c.cur_health,
    conditions: LD.mapValues(c.conditions, encodeAppliedCondition),
    note: c.note,
    portrait_url: c.portrait_url,
    attributes: c.attributes,
    initiative: encodeDice(c.initiative),
    inventory: c.inventory.toJS(),
    size: c.size,
  };
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
