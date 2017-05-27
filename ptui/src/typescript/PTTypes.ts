import * as JD from './JsonDecode';

type CreatureID = string;
type AttrID = string;


export interface App {
  snapshots: AppSnapshots
};

export type AppSnapshots = Array<{snapshot: GameSnapshot, logs: Array<GameLog>}>

export interface GameSnapshot {};

export function decodeAppSnapshots(obj: any): AppSnapshots {
  let decodeArrayGameLogs = (l:any): Array<GameLog> => { console.log("Log Array:", l); return JD.array(l, decodeGameLog) };
  let gs: GameSnapshot = {};
  console.log("Top-level:", obj);
  return JD.array(obj, (item) => {
    console.log("Two-Tuple:", item);
    return ({snapshot: gs, logs: JD.index(item, 1, decodeArrayGameLogs)}) } );
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

export function decodeGameLog(obj: object): GameLog {
  return JD.sum<GameLog>("GameLog", obj, {}, {
      "CreateFolder": (path) => ({ t: "CreateFolder", path: JD.string(path) }),
      "AttributeCheckResult": (data) => ({
        t: "AttributeCheckResult",
        cid: JD.index(data, 0, JD.string),
        check: JD.index(data, 1, decodeAttributeCheck),
        actual: JD.index(data, 2, JD.number),
        success: JD.index(data, 3, JD.bool)
      })
    });
}

interface AttributeCheck {
  reliable: boolean;
  attr: AttrID;
  target: SkillLevel;
}

export function decodeAttributeCheck(obj: object): AttributeCheck {
  return {
    reliable: JD.field(obj, "reliable", JD.bool),
    attr: JD.field(obj, "attr", JD.string),
    target: JD.field(obj, "target", decodeSkillLevel),
  };
}

type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural"
let SkillLevel_values: Array<SkillLevel> = ["Inept", "Unskilled", "Skilled", "Expert", "Supernatural"];

function decodeSkillLevel(i: any): SkillLevel {
  if (SkillLevel_values.indexOf(i) > -1) {
    return i;
  }
  throw new Error(`Expected a SkillLevel, got ${i}.`);
}
