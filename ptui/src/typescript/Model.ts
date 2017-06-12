import * as T from './PTTypes';
import * as LD from 'lodash';

export class PTUI {
  app: T.App;
  elm_app: any;

  constructor(elm_app: any, app: T.App) {
    this.app = app;
    this.elm_app = elm_app;
  }

  sendCommand(cmd: T.GameCommand) {
    console.log("[sendCommand:TS]", cmd);
    let json = T.encodeGameCommand(cmd);
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

  requestCombatAbility(cid: T.CreatureID, ability_id: T.AbilityID, ability: T.Ability, scene_id: T.SceneID) {
    switch (ability.target.t) {
      case "Actor": return this.sendCommand({ t: "CombatAct", ability_id: ability_id, target: { t: "Actor" } });
      default: this.selectAbility(scene_id, cid, ability_id);
    }
  }

  // Utility functions for interacting with the model
  // TODO: Make Creature, Combat, App classes and move these methods to them

  getCreature(cid: T.CreatureID): T.Creature | undefined {
    return this.app.current_game.creatures[cid];
  }

  getCreatures(cids: Array<T.CreatureID>): Array<T.Creature> {
    return filterMap(cids, this.getCreature.bind(this)) as Array<T.Creature>;
  }

  getItem(iid: T.ItemID): T.Item | undefined {
    return this.app.current_game.items[iid];
  }

  getItems(iids: Array<T.ItemID>): Array<T.Item> {
    return filterMap(iids, this.getItem.bind(this)) as Array<T.Item>;
  }

  getFolderNode(path: T.FolderPath): T.FolderNode | undefined {
    let cur = this.app.current_game.campaign;
    for (let seg of path) {
      cur = cur.children[seg];
      if (!cur) { return undefined; }
    }
    return cur.data;
  }

  getCurrentCombatCreatureID(combat: T.Combat): T.CreatureID {
    let cid = combat.creatures.data[combat.creatures.cursor][0];
    if (!cid) { throw new Error(`No combat creature at ${combat.creatures.cursor}`) }
    return cid;
  }

  getCurrentCombatCreature(combat: T.Combat): T.Creature {
    let cid = this.getCurrentCombatCreatureID(combat);
    let creature = this.getCreature(cid);
    if (!creature) { throw new Error(`Current combat creature does not exist: ${cid}`) }
    return creature;
  }
}

export function filterMap<T, R>(coll: Array<T>, f: (t: T) => R | undefined): Array<R> {
  // I can't "naturally" convince TypeScript that this filter makes an
  // Array<R> instead of Array<R|undefined>, hence the assertion
  return coll.map(f).filter((el) => (el)) as Array<R>;
}

type Inventory = { [index: string]: number };

// TODO: these functions should be replaced by GameCommands so the backend handles this stuff
export function addToInventory(inventory: Inventory, item_id: T.ItemID, count: number): Inventory {
  let new_count = LD.get(inventory, item_id, 0) + count;
  let x: Inventory = {}; x[item_id] = new_count;
  return LD.assign({}, inventory, x);
}

export function removeFromInventory(inventory: Inventory, item_id: T.ItemID, count: number): Inventory {
  let new_count = LD.get(inventory, item_id, 0) - count;
  if (new_count <= 0) {
    return LD.omit(inventory, [item_id]) as Inventory; // I'm not sure why I need this `as`, the typedef for `omit` may be insufficient
  } else {
    let x: Inventory = {}; x[item_id] = new_count;
    return LD.assign({}, inventory, x);
  }
}
