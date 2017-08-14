import * as I from 'immutable';
import 'jasmine';
import * as LD from 'lodash';


import * as M from '../Model';
import * as T from '../PTTypes';

function assertEq<T>(a: T, b: T, msg?: string) {
  console.log();
  if (!LD.isEqual(a, b)) {
    console.log("Not equal", JSON.stringify(a, null, 2), "!==", JSON.stringify(b, null, 2), msg);
    throw new Error(`Not equal (${msg}) ${a} !== ${b}`);
  }
}

function assertRaises(f: () => void, msg?: string) {
  try {
    f();
  } catch (e) {
    return;
  }
  throw new Error(`function did not raise (${msg}): ${f}.`);
}

export function test(): boolean {
  assertRaises(() => T.decodeSkillLevel.decodeAny("Foo"));
  assertEq(T.decodeSkillLevel.decodeAny("Skilled"), "Skilled");

  const exAttrCheck: T.AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    T.decodeAttributeCheck.decodeAny(exAttrCheck as any),
    exAttrCheck);

  const gameLogTests: [[any, any]] = [
    ["StopCombat", { t: "StopCombat" }],
    [
      { "StartCombat": ["coolScene", [["coolCreature", 5]]] },
      { t: "StartCombat", scene: "coolScene", creatures: [{ cid: "coolCreature", init: 5 }] }],
    [{ "CreateFolder": "/foo/bar" }, { t: "CreateFolder", path: ["foo", "bar"] }],
    [
      { "AttributeCheckResult": ["coolCreature", exAttrCheck, 50, true] },
      {
        t: "AttributeCheckResult", cid: "coolCreature", check: exAttrCheck, actual: 50,
        success: true,
      }]
  ];
  for (const [x, y] of gameLogTests) {
    assertEq<T.GameLog>(T.decodeGameLog.decodeAny(x), y);
  }

  assertEq<T.Visibility>(T.decodeVisibility.decodeAny("GMOnly"), { t: "GMOnly" });
  assertEq<T.Visibility>(T.decodeVisibility.decodeAny("AllPlayers"), { t: "AllPlayers" });

  const sceneJSON = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": [[0, 0, 0], "GMOnly"] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
    inventory: {},
    background_image_url: "",
  };
  const exScene: T.Scene = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: I.Map<T.CreatureID, [T.Point3, T.Visibility]>().set(
      "Creature ID" as T.CreatureID, [[0, 0, 0], { t: "GMOnly" }]),
    attribute_checks: I.Map({ "Do a backflip": exAttrCheck }),
    inventory: I.Map(),
    background_image_url: "",
    volume_conditions: I.Map(),
  };
  assertEq<T.Scene>(
    I.fromJS(T.decodeScene.decodeAny(sceneJSON)).toJS(),
    I.fromJS(exScene).toJS());
  console.log("OK");
  return true;
}

// test()

describe("PTTypes", () => {
  const ranged_volume_target = { "RangedVolume": { "volume": { "Sphere": 200 }, "range": 1000 } };

  it("decodeSceneTarget", () => {
    T.decodeSceneTarget.decodeAny(ranged_volume_target);
  });

  it("decodeAction", () => {
    const action = { "SceneVolume": { "target": ranged_volume_target } };
    T.decodeAction.decodeAny(action);
  });

  it("decodeInventoryOwner", () => {
    assertEq(
      T.decodeInventoryOwner.decodeAny({ Creature: "FOO" }),
      { Creature: "FOO" }
    );

  });

  it("Decoding a super-basic creature", () => {
    const sample = {
      id: "0x00",
      name: "Elron",
      speed: 600,
      max_energy: 10,
      cur_energy: 10,
      abilities: {
        dash: {
          ability_id: "dash",
          cooldown: 0,
        },
      },
      "class": "creature",
      max_health: 10,
      cur_health: 10,
      conditions: {},
      note: "AC15",
      portrait_url: "",
      icon_url: "",
      attributes: {},
      initiative: { "BestOf": [2, { "Plus": [{ "Expr": { num: 1, size: 20 } }, { "Flat": 4 }] }] },
      size: { x: 1, y: 1, z: 1 },
      inventory: {},
      bio: "",
    };
    const creature = T.decodeCreature.decodeAny(sample);
    expect(creature.initiative).toEqual({
      t: "BestOf",
      num: 2,
      dice: {
        t: "Plus",
        left: { t: "Expr", num: 1, size: 20 },
        right: { t: "Flat", val: 4 },
      },
    });
  });
});

describe("filterMap", () => {
  it("filters and maps", () => {
    expect(
      M.filterMap(
        ["0", "one", "2", "3"],
        Number)
    ).toEqual([2, 3]);
  });
});

describe("getCreatures", () => {
  it("Gets creatures", () => {
    const creature = { id: "0x00", name: "Bob" } as T.Creature;
    const app = { current_game: { creatures: I.Map({ "0x00": creature }) } } as any as T.App; // lol
    const ptui = new M.PTUI("http://example.com/", app);
    expect(ptui.getCreatures(["0x00", "0x01"])).toEqual([creature]);
  });
});

