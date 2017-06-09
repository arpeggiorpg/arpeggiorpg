import * as JD from 'type-safe-json-decoder';
import { Decoder } from 'type-safe-json-decoder';

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

export interface App {
  snapshots: AppSnapshots,
  players: AppPlayers,
  current_game: Game,
};

export interface Game {
  creatures: { [index: string]: Creature };
  classes: {[index: string]: Class};
}

export interface Class{
  // abilities, conditions
  color: string;
}

export interface Player {
  player_id: PlayerID;
  scene?: SceneID;
  creatures: Array<CreatureID>;
}

export type AppSnapshots = Array<{ snapshot: GameSnapshot, logs: Array<GameLog> }>
export type AppPlayers = { [index: string]: Player }

export interface GameSnapshot { };

export type GameLog =
  | {
    t: "AttributeCheckResult";
    cid: CreatureID;
    check: AttributeCheck;
    actual: number;
    success: boolean;
  }
  | { t: "CreateFolder"; path: string }
  | { t: "RenameFolder"; path: string; newName: string }
  | { t: "DeleteFolder"; path: string }
  | { t: "DeleteFolderItem"; path: string; item: FolderItemID }
  | { t: "MoveFolderItem"; path: string; item: FolderItemID; newPath: string }
  | { t: "CreateItem"; path: string; item: Item }
  | { t: "EditItem"; item: Item }
  | { t: "CreateNote"; path: string; note: Note }
  | { t: "EditNote"; path: string; name: string; newNote: Note }
  | { t: "DeleteNote"; path: string; name: string }
  | { t: "CreateScene"; path: string; scene: Scene }
  | { t: "EditScene"; scene: Scene }
  | { t: "DeleteScene"; scene_id: SceneID }
  | { t: "CreateMap"; path: string; map: Map }
  | { t: "EditMap"; map: Map }
  | { t: "DeleteMap"; map_id: MapID }
  | { t: "SetCreaturePos"; scene_id: SceneID; creature_id: CreatureID; pos: Point3 }
  | { t: "PathCreature"; scene_id: SceneID; creature_id: CreatureID; path: Array<Point3> }
  | { t: "CreateCreature"; path: string; creature: Creature }
  | { t: "EditCreature"; creature: Creature }
  | { t: "DeleteCreature"; creature_id: CreatureID }
  | { t: "StartCombat"; scene: SceneID; creatures: Array<{ cid: CreatureID; init: number }> }
  | { t: "AddCreatureToCombat"; creature_id: CreatureID; init: number }
  | { t: "RemoveCreatureFromCombat"; creature_id: CreatureID }
  | { t: "CombatLog"; log: CombatLog }
  | { t: "CreatureLog"; creature_id: CreatureID; log: CreatureLog }
  | { t: "StopCombat" }
  | { t: "Rollback"; snapshot_index: number; log_index: number }

export type CombatLog =
  | { t: "ConsumeMovement"; distance: Distance }
  | { t: "ChangeCreatureInitiative"; creature_id: CreatureID; init: number }
  | { t: "EndTurn", creature_id: CreatureID }
  | { t: "ForceNextTurn" }
  | { t: "ForcePrevTurn" }
  | { t: "RerollInitiative", combatants: Array<[CreatureID, number]> }

export type CreatureLog =
  | { t: "Damage"; hp: HP; rolls: Array<number> }
  | { t: "Heal"; hp: HP; rolls: Array<number> }
  | { t: "GenerateEnergy"; energy: Energy }
  | { t: "ReduceEnergy"; energy: Energy }
  | { t: "ApplyCondition"; condition_id: ConditionID, duration: ConditionDuration } // TODO: Condition
  | { t: "DecrementConditionRemaining"; condition_id: ConditionID }
  | { t: "RemoveCondition"; condition_id: ConditionID }

export interface Item {
  id: ItemID;
  name: string;
}

export type ConditionDuration =
  | { t: "Interminate" }
  | { t: "Duration"; duration: number }

export type Condition =
  | { t: "Condition" }

export interface Creature {
  id: CreatureID;
  name: string;
  speed: Distance;
  max_energy: Energy;
  cur_energy: Energy;
  //   pub abilities: IndexedHashMap<AbilityStatus>,
  class_: string;
  max_health: HP;
  cur_health: HP;
  //   pub conditions: HashMap<ConditionID, AppliedCondition>,
  note: string;
  portrait_url: string;
  //   pub attributes: HashMap<AttrID, SkillLevel>,
  //   pub initiative: Dice,
}

export type FolderItemID =
  | { t: "SceneID"; id: SceneID }
  | { t: "MapID"; id: MapID }
  | { t: "CreatureID"; id: CreatureID }
  | { t: "NoteID"; id: string }
  | { t: "SubfolderID"; id: string }
  | { t: "ItemID"; id: ItemID }

export interface Map {
  id: MapID,
  name: string,
  terrain: Array<Point3>,
  specials: Array<[Point3, Color, string, Visibility]>,
}

export interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural"
let SkillLevel_values: Array<SkillLevel> = ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

export interface Note {
  name: string,
  content: string,
}

export interface Scene {
  id: SceneID,
  name: string,
  map: MapID,
  creatures: { [index: string]: [Point3, Visibility] },
  attribute_checks: { [index: string]: AttributeCheck },
}

export type Point3 = [number, number, number];

export type Visibility =
  | { t: "GMOnly" }
  | { t: "AllPlayers" }

export type Dice =
  | { t: "Flat", val: number }
  | { t: "Expr", num: number, size: number }
  | { t: "Plus", left: Dice, right: Dice }
  | { t: "BestOf", num: number, dice: Dice }

// ** Decoders **

export const decodeConditionDuration: Decoder<ConditionDuration> =
  sum<ConditionDuration>("ConditionDuration", { "Interminate": { t: "Interminate" } },
    {
      "Duration": JD.map(
        (duration): ConditionDuration => ({ t: "Duration", duration }),
        JD.number())
    })

export const decodeCreature: Decoder<Creature> = JD.object(
  ["id", JD.string()],
  ["name", JD.string()],
  ["speed", JD.number()],
  ["max_energy", JD.number()],
  ["cur_energy", JD.number()],
  ["class", JD.string()],
  ["max_health", JD.number()],
  ["cur_health", JD.number()],
  ["note", JD.string()],
  ["portrait_url", JD.string()],
  (id, name, speed, max_energy, cur_energy, class_, max_health, cur_health, note, portrait_url) =>
    ({ id, name, speed, max_energy, cur_energy, class_, max_health, cur_health, note, portrait_url })
);

export const decodePoint3: Decoder<Point3> = JD.tuple(JD.number(), JD.number(), JD.number());

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

export const decodeSkillLevel: Decoder<SkillLevel> =
  JD.oneOf.apply(null, SkillLevel_values.map(JD.equal));

export const decodeAttributeCheck: Decoder<AttributeCheck> =
  JD.object(["reliable", JD.boolean()], ["attr", JD.string()], ["target", decodeSkillLevel],
    (reliable, attr, target) => ({ reliable, attr, target }))

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
  return JD.map((id) => ({ t, id } as FolderItemID), JD.string());
}
export const decodeFolderItemID: Decoder<FolderItemID> =
  sum<FolderItemID>("FolderItemID", {}, {
    "SceneID": _mkFolderItem("SceneID"),
    "MapID": _mkFolderItem("MapID"),
    "CreatureID": _mkFolderItem("CreatureID"),
    "NoteID": _mkFolderItem("NoteID"),
    "ItemID": _mkFolderItem("ItemID"),
    "SubfolderID": _mkFolderItem("SubfolderID"),
  });

export const decodeItem: Decoder<Item> =
  JD.object(
    ["id", JD.string()],
    ["name", JD.string()],
    (id, name) => ({ id, name })
  )

export const decodeNote: Decoder<Note> =
  JD.object(
    ["name", JD.string()],
    ["content", JD.string()],
    (name, content) => ({ name, content })
  );

export const decodeCreatureLog: Decoder<CreatureLog> =
  sum<CreatureLog>("CreatureLog", {}, {
    "Damage": JD.map(
      ([hp, rolls]): CreatureLog => ({ t: "Damage", hp, rolls }),
      JD.tuple(JD.number(), JD.array(JD.number()))),
    "Heal": JD.map(([hp, rolls]): CreatureLog => ({ t: "Heal", hp, rolls }),
      JD.tuple(JD.number(), JD.array(JD.number()))),
    "GenerateEnergy": JD.map((energy): CreatureLog => ({ t: "GenerateEnergy", energy }),
      JD.number()),
    "ReduceEnergy": JD.map((energy): CreatureLog => ({ t: "ReduceEnergy", energy }),
      JD.number()),
    "ApplyCondition": JD.map(
      ([condition_id, duration]): CreatureLog => ({ t: "ApplyCondition", condition_id, duration }),
      JD.tuple(JD.number(), decodeConditionDuration)),
    "DecrementConditionRemaining": JD.map(
      (condition_id): CreatureLog => ({ t: "DecrementConditionRemaining", condition_id }),
      JD.number()),
    "RemoveCondition": JD.map((condition_id): CreatureLog => ({ t: "RemoveCondition", condition_id }),
      JD.number()),
  })

export const decodeCombatLog: Decoder<CombatLog> =
  sum<CombatLog>("CombatLog",
    {
      "ForceNextTurn": { t: "ForceNextTurn" },
      "ForcePrevTurn": { t: "ForcePrevTurn" }
    },
    {
      "ConsumeMovement": JD.map(
        (distance): CombatLog => ({ t: "ConsumeMovement", distance }),
        JD.number()),
      "ChangeCreatureInitiative": JD.map(
        ([creature_id, init]): CombatLog => ({ t: "ChangeCreatureInitiative", creature_id, init }),
        JD.tuple(JD.string(), JD.number())),
      "EndTurn": JD.map((creature_id): CombatLog => ({ t: "EndTurn", creature_id }), JD.string()),
      "RerollInitiative": JD.map((combatants): CombatLog => ({ t: "RerollInitiative", combatants }),
        JD.array(JD.tuple(JD.string(), JD.number()))),
    });

export const decodeGameLog: Decoder<GameLog> =
  sum<GameLog>("GameLog", { "StopCombat": { t: "StopCombat" } }, {
    "StartCombat": JD.map(
      ([scene, creatures]): GameLog => ({ t: "StartCombat", scene, creatures }),
      JD.tuple(
        JD.string(),
        JD.array(JD.map(([cid, init]) => ({ cid, init }), JD.tuple(JD.string(), JD.number())))
      )),
    "CreateFolder": JD.map((p): GameLog => ({ t: "CreateFolder", path: p }), JD.string()),
    "RenameFolder": JD.map(
      ([path, newName]): GameLog => ({ t: "RenameFolder", path, newName }),
      JD.tuple(JD.string(), JD.string())),
    "DeleteFolder": JD.map((path): GameLog => ({ t: "DeleteFolder", path }), JD.string()),
    "DeleteFolderItem": JD.map(([path, item]): GameLog => ({ t: "DeleteFolderItem", path, item }),
      JD.tuple(JD.string(), decodeFolderItemID)),
    "MoveFolderItem": JD.map(
      ([path, item, newPath]): GameLog => ({ t: "MoveFolderItem", path, item, newPath }),
      JD.tuple(JD.string(), decodeFolderItemID, JD.string())),
    "CreateItem": JD.map(
      ([path, item]): GameLog => ({ t: "CreateItem", path, item }),
      JD.tuple(JD.string(), decodeItem)),
    "EditItem": JD.map((item): GameLog => ({ t: "EditItem", item }), decodeItem),
    "CreateNote": JD.map(
      ([path, note]): GameLog => ({ t: "CreateNote", path, note }),
      JD.tuple(JD.string(), decodeNote)),
    "EditNote": JD.map(
      ([path, name, newNote]): GameLog => ({ t: "EditNote", path, name, newNote }),
      JD.tuple(JD.string(), JD.string(), decodeNote)),
    "DeleteNote": JD.map(
      ([path, name]): GameLog => ({ t: "DeleteNote", path, name }),
      JD.tuple(JD.string(), JD.string())),
    "CreateScene": JD.map(
      ([path, scene]): GameLog => ({ t: "CreateScene", path, scene }),
      JD.tuple(JD.string(), decodeScene)),
    "EditScene": JD.map((scene): GameLog => ({ t: "EditScene", scene }), decodeScene),
    "DeleteScene": JD.map((scene_id): GameLog => ({ t: "DeleteScene", scene_id }), JD.string()),
    "CreateMap": JD.map(
      ([path, map]): GameLog => ({ t: "CreateMap", path, map }),
      JD.tuple(JD.string(), decodeMap)),
    "EditMap": JD.map((map): GameLog => ({ t: "EditMap", map }), decodeMap),
    "DeleteMap": JD.map((map_id): GameLog => ({ t: "DeleteMap", map_id }), JD.string()),
    "SetCreaturePos": JD.map(
      ([scene_id, creature_id, pos]): GameLog =>
        ({ t: "SetCreaturePos", scene_id, creature_id, pos }),
      JD.tuple(JD.string(), JD.string(), decodePoint3)),
    "PathCreature": JD.map(
      ([scene_id, creature_id, path]): GameLog => ({ t: "PathCreature", scene_id, creature_id, path }),
      JD.tuple(JD.string(), JD.string(), JD.array(decodePoint3))),
    "CreateCreature": JD.map(
      ([path, creature]): GameLog => ({ t: "CreateCreature", path, creature }),
      JD.tuple(JD.string(), decodeCreature)),
    "EditCreature": JD.map((creature): GameLog => ({ t: "EditCreature", creature }), decodeCreature),
    "DeleteCreature": JD.map(
      (creature_id): GameLog => ({ t: "DeleteCreature", creature_id }),
      JD.string()),
    "AddCreatureToCombat": JD.map(
      ([creature_id, init]): GameLog => ({ t: "AddCreatureToCombat", creature_id, init }),
      JD.tuple(JD.string(), JD.number())),
    "RemoveCreatureFromCombat": JD.map(
      (creature_id): GameLog => ({ t: "RemoveCreatureFromCombat", creature_id }),
      JD.string()),
    "CombatLog": JD.map((log): GameLog => ({ t: "CombatLog", log }), decodeCombatLog),
    "CreatureLog": JD.map(([creature_id, log]): GameLog => ({ t: "CreatureLog", creature_id, log }),
      JD.tuple(JD.string(), decodeCreatureLog)),
    "AttributeCheckResult": JD.map(
      ([cid, check, actual, success]): GameLog =>
        ({ t: "AttributeCheckResult", cid, check, actual, success }),
      JD.tuple(JD.string(), decodeAttributeCheck, JD.number(), JD.boolean())
    ),
    "Rollback": JD.map(
      ([snapshot_index, log_index]): GameLog => ({ t: "Rollback", snapshot_index, log_index }),
      JD.tuple(JD.number(), JD.number())),
  });

export const decodeAppSnapshots: Decoder<AppSnapshots> =
  JD.array(JD.map(
    (ls) => ({ snapshot: {} as GameSnapshot, logs: ls }),
    JD.at([1], JD.array(decodeGameLog))))

export const decodePlayer: Decoder<Player> = JD.object(
  ["player_id", JD.string()],
  ["scene", maybe(JD.string())],
  ["creatures", JD.array(JD.string())],
  (player_id, scene, creatures) => ({ player_id, scene, creatures })
);

export const decodeAppPlayers: Decoder<AppPlayers> = JD.dict(decodePlayer);

const decodeClass: Decoder<Class> = JD.object(
  ["color", JD.string()],
  (color) => ({color})
);

export const decodeGame: Decoder<Game> = JD.object(
  ["creatures", JD.dict(decodeCreature)],
  ["classes", JD.dict(decodeClass)],
  (creatures, classes) => ({ creatures, classes })
);

export const decodeApp: Decoder<App> = JD.object(
  ["snapshots", decodeAppSnapshots],
  ["players", decodeAppPlayers],
  ["current_game", decodeGame],
  (snapshots, players, current_game) => ({ snapshots, players, current_game })
);


// Utility Functions for Decoding

export function maybe<T>(d: Decoder<T>): Decoder<T | undefined> {
  return JD.oneOf(JD.map((_) => undefined, JD.equal(null)), d);
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

  let variants = Object.keys(decoders);
  let _decoders: Array<Decoder<T>> = variants.map(variant => JD.at([variant], decoders[variant]));

  return JD.oneOf(
    JD.map(nullary, JD.string()),
    JD.oneOf.apply(null, _decoders),
  );
}

// Utility functions for interacting with the model

export function filterMap<T, R>(coll: Array<T>, f: (t: T) => R | undefined): Array<R> {
  // I can't "naturally" convince TypeScript that this filter makes an
  // Array<R> instead of Array<R|undefined>, hence the typecast
  return coll.map(f).filter((el) => (el)) as Array<R>;
}

export function getCreature(app: App, cid: CreatureID): Creature | undefined {
  return app.current_game.creatures[cid];
}

export function getCreatures(app: App, cids: Array<CreatureID>) {
  return filterMap(cids, getCreature.bind(undefined, app));
}
