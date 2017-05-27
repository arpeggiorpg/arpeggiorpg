import * as JD from './JsonDecode';
import { Decoder } from './JsonDecode';

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

const decodeAttributeCheck: Decoder<AttributeCheck> = JD.map3(
  (r, a, t): AttributeCheck => ({ reliable: r, attr: a, target: t }),
  JD.field("reliable", JD.bool),
  JD.field("attr", JD.string),
  JD.field("target", decodeSkillLevel),
)

export const decodeGameLog: Decoder<GameLog> =
  JD.sum<GameLog>("GameLog", {
    "StopCombat": { t: "StopCombat" }
  }, {
      "StartCombat": JD.map2(
        (scene, creatures): GameLog => ({
          t: "StartCombat",
          scene: scene,
          creatures: creatures,
        }),
        JD.index(0, JD.string),
        JD.index(1, JD.array(JD.map2((cid, init) => ({ cid: cid, init: init }),
          JD.index(0, JD.string),
          JD.index(1, JD.number))))
      ),
      "CreateFolder": JD.map((p: string): GameLog => ({ t: "CreateFolder", path: p }), JD.string),
      "AttributeCheckResult":
      JD.map4(
        (cid, check, actual, success): GameLog => ({
          t: "AttributeCheckResult", cid: cid, check: check, actual: actual, success: success
        }),
        JD.index(0, JD.string),
        JD.index(1, decodeAttributeCheck),
        JD.index(2, JD.number),
        JD.index(3, JD.bool)
      )
    });

export const decodeAppSnapshots: Decoder<AppSnapshots> =
  JD.array(JD.map(
    (ls) => ({ snapshot: {} as GameSnapshot, logs: ls }),
    JD.index(1, JD.array(decodeGameLog))))

function decodeSkillLevel(i: any): SkillLevel {
  if (SkillLevel_values.indexOf(i) > -1) {
    return i;
  }
  throw new Error(`Expected a SkillLevel, got ${i}.`);
}
