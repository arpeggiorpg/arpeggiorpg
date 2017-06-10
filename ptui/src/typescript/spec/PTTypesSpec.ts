import * as Lodash from 'lodash';
import * as T from '../PTTypes';
import * as M from '../Model';
import J = require("jasmine");

function assertEq<T>(a: T, b: T, msg?: string) {
  if (!Lodash.isEqual(a, b)) {
    console.log("Not equal", JSON.stringify(a, null, 2), "!==", JSON.stringify(b, null, 2), msg);
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

export function test(): boolean {
  assertRaises(() => T.decodeSkillLevel.decodeAny("Foo"));
  assertEq(T.decodeSkillLevel.decodeAny("Skilled"), "Skilled");

  let exAttrCheck: T.AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    T.decodeAttributeCheck.decodeAny(exAttrCheck as any),
    exAttrCheck);

  let gameLogTests: [[any, any]] = [
    ["StopCombat", { t: "StopCombat" }],
    [
      { "StartCombat": ["coolScene", [["coolCreature", 5]]] },
      { t: "StartCombat", scene: "coolScene", creatures: [{ cid: "coolCreature", init: 5 }] }],
    [{ "CreateFolder": "/foo/bar" }, { t: "CreateFolder", path: ["foo", "bar"] }],
    [
      { "AttributeCheckResult": ["coolCreature", exAttrCheck, 50, true] },
      { t: "AttributeCheckResult", cid: "coolCreature", check: exAttrCheck, actual: 50, success: true }]
  ];
  for (let [x, y] of gameLogTests) {
    assertEq<T.GameLog>(T.decodeGameLog.decodeAny(x), y);
  }

  assertEq<T.Visibility>(T.decodeVisibility.decodeAny("GMOnly"), { t: "GMOnly" });
  assertEq<T.Visibility>(T.decodeVisibility.decodeAny("AllPlayers"), { t: "AllPlayers" });

  let sceneJSON = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": [[0, 0, 0], "GMOnly"] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
  };
  let exScene: T.Scene = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": [[0, 0, 0], { t: "GMOnly" }] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
  };
  assertEq<T.Scene>(T.decodeScene.decodeAny(sceneJSON), exScene);
  console.log("OK");
  return true;
}

// test()

describe("PTTypes decoding", function () {
  it("original test function works", function () {
    expect(test()).toBe(true);
  });

  it("Decoding a super-basic creature", function() {
    let sample = {
      id: "0x00",
      name: "Elron",
      speed: 600,
      max_energy: 10,
      cur_energy: 10,
      abilities: {
        dash: {
          ability_id: "dash",
          cooldown: 0,
        }
      },
      "class": "creature",
      max_health: 10,
      cur_health: 10,
      conditions: {},
      note: "AC15",
      portrait_url: "",
      attributes: {},
      initiative: {"BestOf": [2, {"Plus": [{"Expr": {num: 1, size: 20}}, {"Flat": 4}]}]},
      size: {x: 1, y: 1, z: 1},
      inventory: {},
    };
    let creature = T.decodeCreature.decodeAny(sample);
    expect(creature.initiative).toEqual({
      t: "BestOf",
      num: 2,
      dice: {
        t: "Plus",
        left: {t: "Expr", num: 1, size: 20},
        right: {t: "Flat", val: 4},
      }
    });
    expect(T.encodeCreature(creature)).toEqual(sample);
  });
});

describe("filterMap", function () {
  it("filters and maps", function () {
    expect(
      M.filterMap(
        ["0", "one", "2", "3"],
        Number)
    ).toEqual([2, 3]);
  });
});

describe("getCreatures", function() {
  it("Gets creatures", function() {
    let creature = {id: "0x00", name: "Bob"};
    let app = {current_game: {creatures: {"0x00": creature}}} as any as T.App; // lol
    // expect(T.getCreatures(app, ["0x00", "0x01"])).toEqual([creature]);
  });
});
