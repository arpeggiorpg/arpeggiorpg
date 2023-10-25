import { List, Map, Set } from "immutable";
import { describe, expect, test } from "vitest";

import * as M from "../Model";
import * as T from "../PTTypes";

function assertEq<T>(a: T, b: T, msg?: string) {
  expect(a).toEqual(b);
}

function assertRaises(f: () => void, msg?: string) {
  expect(f).toThrow();
}

test("SkillLevel", () => {
  assertRaises(() => T.decodeSkillLevel.parse("Foo"));
  assertEq(T.decodeSkillLevel.parse("Skilled"), "Skilled");
});

test("random junk", () => {
  const exAttrCheck: T.AttributeCheck = { reliable: false, attr: "finesse", target: "Skilled" };
  assertEq(
    T.decodeAttributeCheck.parse(exAttrCheck),
    exAttrCheck,
  );
  const gameLogTests: [any, T.GameLog][] = [
    // ["StopCombat", "StopCombat"],
    [
      { "StartCombat": ["coolScene", [["coolCreature", 5]]] },
      { "StartCombat": ["coolScene", [["coolCreature", 5]]] },
    ],
    [{ "CreateFolder": "/foo/bar" }, { "CreateFolder": ["foo", "bar"] }],
    [
      {
        "AttributeCheckResult": {
          creature_id: "coolCreature",
          attribute_check: exAttrCheck,
          actual: 50,
          success: true,
        },
      },
      {
        "AttributeCheckResult": {
          creature_id: "coolCreature",
          attribute_check: exAttrCheck,
          actual: 50,
          success: true,
        },
      },
    ],
  ];
  for (const [x, y] of gameLogTests) {
    assertEq<T.GameLog>(T.decodeGameLog.parse(x), y);
  }

  assertEq<T.Visibility>(T.decodeVisibility.parse("GMOnly"), "GMOnly");
  assertEq<T.Visibility>(T.decodeVisibility.parse("AllPlayers"), "AllPlayers");

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
    background_image_offset: null,
    related_scenes: [],
    scene_hotspots: {},
    terrain: [],
    highlights: {},
    annotations: {},
    background_image_scale: [0, 0],
    volume_conditions: {},
    focused_creatures: [],
  };
  const exScene: T.Scene = {
    id: "Scene ID",
    name: "Scene Name",
    terrain: Set(),
    highlights: Map(),
    annotations: Map(),
    creatures: Map<T.CreatureID, [T.Point3, T.Visibility]>().set(
      "Creature ID",
      [new T.Point3(0, 0, 0), "GMOnly"],
    ),
    attribute_checks: Map({ "Do a backflip": exAttrCheck }),
    inventory: Map(),
    background_image_url: "",
    background_image_offset: null,
    background_image_scale: [0, 0],
    volume_conditions: Map(),
    focused_creatures: List(),
    scene_hotspots: Map(),
    related_scenes: Set(),
  };
  assertEq(T.decodeScene.parse(sceneJSON), exScene);
});

describe("PTTypes", () => {
  const ranged_volume_target = { "RangedVolume": { "volume": { "Sphere": 200 }, "range": 1000 } };

  test("decodeSceneTarget", () => {
    T.decodeSceneTarget.parse(ranged_volume_target);
  });

  test("decodeAction", () => {
    const action = {
      "SceneVolume": {
        "effect": {
          CreateVolumeCondition: { duration: "Interminate", condition: "DoubleMaxMovement" },
        },
        "target": ranged_volume_target,
      },
    };
    T.decodeAction.parse(action);
  });

  test("decodeInventoryOwner", () => {
    assertEq(
      T.decodeInventoryOwner.parse({ Creature: "FOO" }),
      { Creature: "FOO" },
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
      initiative: {
        BestOf: [2, { Plus: [{ Expr: { num: 1, size: 20 } }, { Flat: { value: 4 } }] }],
      },
      size: { x: 1, y: 1, z: 1 },
      inventory: {},
      bio: "",
      can_act: true,
      can_move: true,
    };
    const creature = T.decodeDynamicCreature.parse(sample);
    expect(creature.initiative).toEqual({
      BestOf: [2, { Plus: [{ Expr: { num: 1, size: 20 } }, { Flat: { value: 4 } }] }],
    });
  });
});

describe("filterMap", () => {
  test("filters and maps", () => {
    expect(
      M.filterMap(
        ["0", "one", "2", "3"],
        Number,
      ),
    ).toEqual([2, 3]);
  });
});
