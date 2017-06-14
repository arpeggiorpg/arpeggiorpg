import * as I from 'immutable';
import * as LD from 'lodash';
import * as reactRedux from 'react-redux';
import * as redux from 'redux';

import * as T from './PTTypes';


export class PTUI {
  readonly app: T.App;
  readonly elm_app: any;

  constructor(elm_app: any, app: T.App) {
    this.app = app;
    this.elm_app = elm_app;
  }

  sendCommand(cmd: T.GameCommand) {
    console.log("[sendCommand:TS]", cmd);
    const json = T.encodeGameCommand(cmd);
    console.log("[sendCommand:JSON]", json);
    this.elm_app.ports.sendCommand.send(json);
  }

  requestCombatMovement() {
    console.log("[requestMovement]");
    this.elm_app.ports.requestCombatMovement.send(null);
  }
  selectAbility(scene_id: T.SceneID, cid: T.CreatureID, abid: T.AbilityID) {
    return this.elm_app.ports.selectAbility.send([scene_id, cid, abid]);
  }

  requestCombatAbility(
    cid: T.CreatureID, ability_id: T.AbilityID, ability: T.Ability, scene_id: T.SceneID) {
    switch (ability.target.t) {
      case "Actor": return this.sendCommand({ t: "CombatAct", ability_id, target: { t: "Actor" } });
      default: this.selectAbility(scene_id, cid, ability_id);
    }
  }

  // Utility functions for interacting with the model
  // TODO: Consider making Game, Combat, Folder classes and moving these methods to those classes.
  // But I'm not sure it'd really matter -- if I find myself really needing to increase isolation
  // then it would be a good way forward, but I'm not sure it will be necessary.
  getCreature(cid: T.CreatureID): T.Creature | undefined {
    return get(this.app.current_game.creatures, cid);
  }

  getCreatures(cids: Array<T.CreatureID>): Array<T.Creature> {
    return filterMap(cids, this.getCreature.bind(this)) as Array<T.Creature>;
  }

  getItem(iid: T.ItemID): T.Item | undefined {
    return get(this.app.current_game.items, iid);
  }

  getItems(iids: Array<T.ItemID>): Array<T.Item> {
    return filterMap(iids, this.getItem.bind(this)) as Array<T.Item>;
  }

  getFolderNode(path: T.FolderPath): T.FolderNode | undefined {
    let cur: T.Folder | undefined = this.app.current_game.campaign;
    for (const seg of path) {
      cur = get(cur.children, seg);
      if (!cur) { return undefined; }
    }
    return cur.data;
  }

  getCurrentCombatCreatureID(combat: T.Combat): T.CreatureID {
    const entry = idx(combat.creatures.data, combat.creatures.cursor);
    if (!entry) { throw new Error(`No combat creature at ${combat.creatures.cursor}`); }
    return entry[0];
  }

  getCurrentCombatCreature(combat: T.Combat): T.Creature {
    const cid = this.getCurrentCombatCreatureID(combat);
    const creature = this.getCreature(cid);
    if (!creature) { throw new Error(`Current combat creature does not exist: ${cid}`); }
    return creature;
  }
}

export function filterMap<T, R>(coll: Array<T>, f: (t: T) => R | undefined): Array<R> {
  // I can't "naturally" convince TypeScript that this filter makes an
  // Array<R> instead of Array<R|undefined>, hence the assertion
  return coll.map(f).filter(el => (el)) as Array<R>;
}

type Inventory = I.Map<T.ItemID, number>;

// TODO: these functions should be replaced by GameCommands so the backend handles this stuff
export function addToInventory(inventory: Inventory, item_id: T.ItemID, count: number): Inventory {
  return inventory.set(item_id, inventory.get(item_id, 0) + count);
}

// TODO: this allows over-withdrawing from the inventory.
export function removeFromInventory(inventory: Inventory, item_id: T.ItemID, count: number):
  Inventory {
  const new_count = inventory.get(item_id, 0) - count;
  if (new_count <= 0) {
    return inventory.delete(item_id);
  } else {
    return inventory.set(item_id, new_count);
  }
}

export function folderPathToString(path: T.FolderPath): string {
  return T.encodeFolderPath(path);
}

export function get<T>(obj: { [index: string]: T }, key: string): T | undefined {
  return obj[key];
}

export function idx<T>(arr: Array<T>, index: number): T | undefined {
  return arr[index];
}

/// A version of isEqual that requires both arguments to be the same type.
export function isEqual<T>(l: T, r: T): boolean {
  return LD.isEqual(l, r);
}
