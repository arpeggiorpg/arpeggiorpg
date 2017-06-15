import * as I from 'immutable';
import "isomorphic-fetch";
import * as LD from 'lodash';
import * as ReactRedux from 'react-redux';
import * as Redux from 'redux';
import * as JD from "type-safe-json-decoder";

import * as T from './PTTypes';

export type Action =
  | { type: "RefreshApp"; app: T.App }
  | { type: "ActivateGridCreature"; cid: T.CreatureID; rect: Rect; }
  | { type: "DisplayMovementOptions"; cid?: T.CreatureID; options: Array<T.Point3> }
  | { type: "ClearMovementOptions" }
  | { type: "SetPlayerID"; pid: T.PlayerID }
  | { type: "DisplayError"; error: string }
  | { type: "ClearError" }
  | { type: "ToggleAnnotation"; pt: T.Point3; rect: Rect }
  | { type: "@@redux/INIT" };

export function update(ptui: PTUI, action: Action): PTUI {
  console.log("[Model.update]", action.type);
  switch (action.type) {
    case "RefreshApp":
      return new PTUI(ptui.rpi_url, ptui.elm_app, action.app, ptui.state);
    case "ActivateGridCreature":
      const new_active =
        (ptui.state.grid.active_menu && ptui.state.grid.active_menu.cid === action.cid)
          ? undefined
          : { cid: action.cid, rect: action.rect };
      return ptui.updateState(
        state => ({ ...state, grid: { ...ptui.state.grid, active_menu: new_active } }));
    case "SetPlayerID":
      return ptui.updateState(state => ({ ...state, player_id: action.pid }));


    // Grid-related
    case "ToggleAnnotation":
      const curDisplay = ptui.state.grid.display_annotation;
      const nv =
        curDisplay && isEqual(curDisplay.pt, action.pt)
          ? undefined
          : { pt: action.pt, rect: action.rect };
      return ptui.updateGridState(grid => ({ ...grid, display_annotation: nv }));
    case "DisplayMovementOptions":
      return ptui.updateGridState(
        grid => ({
          ...ptui.state.grid,
          movement_options: { cid: action.cid, options: action.options },
        }));
    case "ClearMovementOptions":
      return ptui.updateGridState(grid => ({ ...grid, movement_options: undefined }));
    case "DisplayError":
      return ptui.updateState(state => ({ ...state, error: action.error }));
    case "ClearError":
      return ptui.updateState(state => ({ ...state, error: undefined }));
    case "@@redux/INIT":
      return ptui;
  }
}

export interface Rect { nw: SVGPoint; ne: SVGPoint; se: SVGPoint; sw: SVGPoint; }

export interface GridModel {
  active_menu?: { cid: T.CreatureID; rect: Rect };
  movement_options?: { cid?: T.CreatureID; options: Array<T.Point3> };
  display_annotation?: { pt: T.Point3, rect: Rect };
}

export interface PTUIState {
  grid: GridModel;
  player_id?: T.PlayerID;
  error?: string;
}

function ptfetch<T>(
  dispatch: Dispatch, url: string, init: RequestInit | undefined,
  decoder: JD.Decoder<T>, then: (result: T) => void)
  : Promise<void> {
  return fetch(url, init)
    .then(response => response.json())
    .then(json => {
      try {
        return decoder.decodeAny(json);
      } catch (e) {
        dispatch({ type: "DisplayError", error: "Failed to decode JSON: " + e.toString() });
      }
    })
    .then(then)
    .catch(e => dispatch({ type: "DisplayError", error: e.toString() }));

}

export class PTUI {
  readonly app: T.App;
  readonly elm_app: any;
  readonly state: PTUIState;
  rpi_url: string;

  constructor(rpi_url: string, elm_app: any, app: T.App, state: PTUIState = { grid: {} }) {
    this.elm_app = elm_app;
    this.app = app;
    this.state = state;
    this.rpi_url = rpi_url;
  }

  updateState(updater: (state: PTUIState) => PTUIState): PTUI {
    return new PTUI(this.rpi_url, this.elm_app, this.app, updater(this.state));
  }
  updateGridState(updater: (state: GridModel) => GridModel): PTUI {
    return this.updateState(state => ({ ...state, grid: updater(state.grid) }));
  }

  requestMove(dispatch: Dispatch, cid: T.CreatureID) {
    const scene = this.focused_scene();
    if (scene) {
      return ptfetch(dispatch, this.rpi_url + "/movement_options/" + scene.id + "/" + cid,
        undefined,
        JD.array(T.decodePoint3),
        options => dispatch({
          type: "DisplayMovementOptions",
          cid,
          options,
        }));
    }
  }

  moveCreature(dispatch: Dispatch, creature_id: T.CreatureID, dest: T.Point3) {
    dispatch({ type: "ClearMovementOptions" });
    const scene = this.focused_scene();
    if (scene) {
      this.sendCommand(dispatch, { t: "PathCreature", scene_id: scene.id, creature_id, dest });
    } else {
      throw new Error(`Tried moving when there is no scene`);
    }
  }

  moveCombatCreature(dispatch: Dispatch, dest: T.Point3) {
    dispatch({ type: "ClearMovementOptions" });
    this.sendCommand(dispatch, { t: "PathCurrentCombatCreature", dest });
  }

  sendCommand(dispatch: Dispatch, cmd: T.GameCommand) {
    const json = T.encodeGameCommand(cmd);
    console.log("[sendCommand:JSON]", json);
    ptfetch(
      dispatch, this.rpi_url,
      {
        method: "POST",
        body: JSON.stringify(json),
        headers: { "content-type": "application/json" },
      },
      JD.succeed(undefined),
      x => x);
  }

  focused_scene(): T.Scene | undefined {
    // oh for Rust's "?" operator
    const pid = this.state.player_id;
    if (pid) {
      const player = this.app.players[pid];
      if (player && player.scene) {
        return this.app.current_game.scenes[player.scene];
      }
    }
  }

  requestCombatMovement(dispatch: Dispatch) {
    return ptfetch(
      dispatch, this.rpi_url + "/combat_movement_options", undefined,
      JD.array(T.decodePoint3),
      options => dispatch({ type: "DisplayMovementOptions", options }));
  }

  selectAbility(scene_id: T.SceneID, cid: T.CreatureID, abid: T.AbilityID) {
    return this.elm_app.ports.selectAbility.send([scene_id, cid, abid]);
  }

  requestCombatAbility(
    dispatch: Dispatch,
    cid: T.CreatureID, ability_id: T.AbilityID, ability: T.Ability, scene_id: T.SceneID) {
    switch (ability.target.t) {
      case "Actor": return this.sendCommand(
        dispatch, { t: "CombatAct", ability_id, target: { t: "Actor" } });
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


interface StoreProps { ptui: PTUI; }
interface DispatchProps { dispatch: (a: Action) => Action; }
export type Dispatch = (action: Action) => Action;

export type ReduxProps = StoreProps & DispatchProps;

export function connectRedux<BaseProps>(
  x: React.ComponentType<BaseProps & StoreProps & DispatchProps>)
  : React.ComponentType<BaseProps> {
  return ReactRedux.connect((ptui, op) => ({ ptui }), dispatch => ({ dispatch }))(x);
}
