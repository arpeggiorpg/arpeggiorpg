import * as Lodash from 'lodash';
import * as PTTypes from '../PTTypes';

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
  assertRaises(() => PTTypes.decodeSkillLevel.decodeAny("Foo"));
  assertEq(PTTypes.decodeSkillLevel.decodeAny("Skilled"), "Skilled");

  let exAttrCheck: PTTypes.AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    PTTypes.decodeAttributeCheck.decodeAny(exAttrCheck as any),
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
    assertEq<PTTypes.GameLog>(PTTypes.decodeGameLog.decodeAny(x), y);
  }

  assertEq<PTTypes.Visibility>(PTTypes.decodeVisibility.decodeAny("GMOnly"), { t: "GMOnly" });
  assertEq<PTTypes.Visibility>(PTTypes.decodeVisibility.decodeAny("AllPlayers"), { t: "AllPlayers" });

  let sceneJSON = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": [[0, 0, 0], "GMOnly"] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
  };
  let exScene: PTTypes.Scene = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": [[0, 0, 0], { t: "GMOnly" }] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
  };
  assertEq<PTTypes.Scene>(PTTypes.decodeScene.decodeAny(sceneJSON), exScene);
  console.log("OK");
  return true;
}

// test()

describe("PTTypes serialization", function () {
  it("is cool", function () {
    expect(test()).toBe(true);
  })
}
)
