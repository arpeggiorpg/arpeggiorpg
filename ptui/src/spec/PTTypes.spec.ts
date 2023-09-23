import * as I from 'immutable';
import * as LD from 'lodash';
import { expect, describe, test } from 'vitest'

import * as M from '../Model';
import * as T from '../PTTypes';

function assertEq<T>(a: T, b: T, msg?: string) {
  expect(a).toEqual(b);
}

function assertRaises(f: () => void, msg?: string) {
  expect(f).toThrow();
}

test("SkillLevel", () => {
  assertRaises(() => T.decodeSkillLevel.parse("Foo"));
  assertEq(T.decodeSkillLevel.parse("Skilled"), "Skilled");
})

test("random junk", () => {
  const exAttrCheck: T.AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    T.decodeAttributeCheck.parse(exAttrCheck as any),
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
    assertEq<T.GameLog>(T.decodeGameLog.parse(x), y);
  }

  assertEq<T.Visibility>(T.decodeVisibility.parse("GMOnly"), { t: "GMOnly" });
  assertEq<T.Visibility>(T.decodeVisibility.parse("AllPlayers"), { t: "AllPlayers" });

  const sceneJSON = {
    id: "Scene ID",
    name: "Scene Name",
    map: "Map ID",
    creatures: { "Creature ID": ["0/0/0", "GMOnly"] },
    attribute_checks: {
      "Do a backflip": exAttrCheck,
    },
    inventory: {},
    background_image_url: "",
    related_scenes: [],
    scene_hotspots: {},
    terrain: [],
    highlights: {},
    annotations: {},
    background_image_scale: [0, 0],
    volume_conditions: {},
    focused_creatures: []
  };
  const exScene: T.Scene = {
    id: "Scene ID",
    name: "Scene Name",
    terrain: I.Set(),
    highlights: I.Map(),
    annotations: I.Map(),
    creatures: I.Map<T.CreatureID, [T.Point3, T.Visibility]>().set(
      "Creature ID" as T.CreatureID, [new T.Point3(0, 0, 0), { t: "GMOnly" }]),
    attribute_checks: I.Map({ "Do a backflip": exAttrCheck }),
    inventory: I.Map(),
    background_image_url: "",
    background_image_offset: undefined,
    background_image_scale: [0, 0],
    volume_conditions: I.Map(),
    focused_creatures: I.List(),
    scene_hotspots: I.Map(),
    related_scenes: I.Set(),
  };
  assertEq(T.decodeScene.parse(sceneJSON), exScene);

});

describe("PTTypes", () => {
  const ranged_volume_target = { "RangedVolume": { "volume": { "Sphere": 200 }, "range": 1000 } };

  test("decodeSceneTarget", () => {
    T.decodeSceneTarget.parse(ranged_volume_target);
  });

  test("decodeAction", () => {
    const action = { "SceneVolume": { "target": ranged_volume_target } };
    T.decodeAction.parse(action);
  });

  test("decodeInventoryOwner", () => {
    assertEq(
      T.decodeInventoryOwner.parse({ Creature: "FOO" }),
      { Creature: "FOO" }
    );

  });

  test("Decoding a super-basic creature", () => {
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
      own_conditions: {},
      volume_conditions: {},
      note: "AC15",
      portrait_url: "",
      icon_url: "",
      attributes: {},
      initiative: { "BestOf": [2, { "Plus": [{ "Expr": { num: 1, size: 20 } }, { "Flat": 4 }] }] },
      size: { x: 1, y: 1, z: 1 },
      inventory: {},
      bio: "",
    };
    const creature = T.decodeCreature.parse(sample);
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
  test("filters and maps", () => {
    expect(
      M.filterMap(
        ["0", "one", "2", "3"],
        Number)
    ).toEqual([2, 3]);
  });
});

