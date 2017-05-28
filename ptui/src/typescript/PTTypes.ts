import * as JD from 'type-safe-json-decoder';
import { Decoder } from 'type-safe-json-decoder';
import * as Lodash from 'lodash';

type CreatureID = string;
type SceneID = string;
type AttrID = string;


export interface App {
  snapshots: AppSnapshots
};

export type AppSnapshots = Array<{ snapshot: GameSnapshot, logs: Array<GameLog> }>

export interface GameSnapshot { };

interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export type GameLog =
  | {
    t: "AttributeCheckResult";
    cid: CreatureID;
    check: AttributeCheck;
    actual: number;
    success: boolean;
  }
  | { t: "CreateFolder"; path: string }
  | { t: "StartCombat"; scene: SceneID; creatures: Array<{ cid: CreatureID; init: number }> }
  | { t: "StopCombat" }

type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural"
let SkillLevel_values: Array<SkillLevel> = ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

/// Decoders

const decodeSkillLevel: Decoder<SkillLevel> =
  JD.oneOf.apply(null, SkillLevel_values.map(JD.equal));

const decodeAttributeCheck: Decoder<AttributeCheck> =
  JD.object(["reliable", JD.boolean()], ["attr", JD.string()], ["target", decodeSkillLevel],
    (reliable, attr, target) => ({ reliable, attr, target }))

export const decodeGameLog: Decoder<GameLog> =
  sum<GameLog>("GameLog", {
    "StopCombat": { t: "StopCombat" }
  }, {
      "StartCombat": JD.map(
        ([scene, creatures]): GameLog => ({
          t: "StartCombat",
          scene: scene,
          creatures: creatures,
        }),
        JD.tuple(JD.string(),
          JD.array(JD.map(([cid, init]) => ({ cid: cid, init: init }),
            JD.tuple(JD.string(), JD.number())
          )
          ))),
      "CreateFolder": JD.map((p: string): GameLog => ({ t: "CreateFolder", path: p }), JD.string()),
      "AttributeCheckResult":
      JD.map(
        ([cid, check, actual, success]): GameLog =>
          ({ t: "AttributeCheckResult", cid, check, actual, success }),
        JD.tuple(JD.string(), decodeAttributeCheck, JD.number(), JD.boolean())
      )
    });

export const decodeAppSnapshots: Decoder<AppSnapshots> =
  JD.array(JD.map(
    (ls) => ({ snapshot: {} as GameSnapshot, logs: ls }),
    JD.at([1], JD.array(decodeGameLog))))

// Utility Functions

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

function assertEq<T>(a: T, b: T, msg?: string) {
  if (!Lodash.isEqual(a, b)) {
    console.log("Not equal", a, "!==", b, msg);
    throw new Error(`Not equal (${msg}) ${a} !== ${b}`)
  }
}

function assertRaises(f: () => void, msg?: string) {
  try {
    f()
  } catch (e) {
    return;
  }
  throw new Error(`function did not raise (${msg}): ${f}.`)
}

export function test() {
  assertRaises(() => decodeSkillLevel.decodeAny("Foo"));
  assertEq(decodeSkillLevel.decodeAny("Skilled"), "Skilled");

  let exAttrCheck: AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    decodeAttributeCheck.decodeAny(exAttrCheck as any),
    exAttrCheck);

  let gameLogTests: [[any, any]] = [
    ["StopCombat", { t: "StopCombat" }],
    [
      { "StartCombat": ["coolScene", [["coolCreature", 5]]] },
      { t: "StartCombat", scene: "coolScene", creatures: [{ cid: "coolCreature", init: 5 }] }],
    [{ "CreateFolder": "foo/bar" }, { t: "CreateFolder", path: "foo/bar" }],
    [
      { "AttributeCheckResult": ["coolCreature", exAttrCheck, 50, true] },
      { t: "AttributeCheckResult", cid: "coolCreature", check: exAttrCheck, actual: 50, success: true }]
  ];
  for (let [x, y] of gameLogTests) {
    assertEq<GameLog>(decodeGameLog.decodeAny(x), y);
  }
  console.log("OK");
}

// test()
