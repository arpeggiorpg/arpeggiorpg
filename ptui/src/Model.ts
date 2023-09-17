import LD from 'lodash';
import I from 'immutable';
import { createWithEqualityFn } from "zustand/traditional";
import type { StateCreator } from "zustand";
import { shallow } from 'zustand/shallow';

import * as T from "./PTTypes";

export type FetchStatus = "Unfetched" | "Ready" | "Error";
export interface AppState {
  app: T.App;
  fetchStatus: FetchStatus;
  setFetchStatus: (s: FetchStatus) => void;
  refresh: (app: T.App) => void;
  refreshGame: (game: T.Game) => void;

  // utility functions for fetching state
  getItem: (iid: T.ItemID) => T.Item | undefined,
  getItems: (iids: T.ItemID[]) => T.Item[],
  getScenes: (sceneIds: T.SceneID[]) => T.Scene[],
  getCurrentCombatCreatureID: () => T.CreatureID | undefined,
  getNote: (path: T.FolderPath, name: string | undefined) => T.Note | undefined,
  getFolderNode: (path: T.FolderPath) => T.FolderNode | undefined,
  getFolder: (path: T.FolderPath) => T.Folder | undefined,
  getScene: (sceneId: T.SceneID) => T.Scene | undefined,

  creatureIsInCombat: (creatureId: T.CreatureID) => boolean,
  getSceneCreatures: (scene: T.Scene) => T.Creature[],
  getCreatures: (cids: T.CreatureID[]) => T.Creature[],
  getCreature: (cid: T.CreatureID) => T.Creature | undefined,
  getCombat: () => T.Combat | undefined,
  getGame: () => T.Game,

  getAbility: (abid: T.AbilityID) => T.Ability | undefined;
  getAbilities: (abids: T.AbilityID[]) => T.Ability[];
  getClass: (classid: T.ClassID) => T.Class | undefined;
  getClasses: (classids: T.ClassID[]) => T.Class[];
  getSceneInventory: (scene: T.Scene) => I.List<[T.Item, number]>;
}

const appSlice: Slice<AppState> = (set, get) => ({
  app: initialApp,
  fetchStatus: "Unfetched",
  setFetchStatus: fetchStatus => set(() => ({ fetchStatus })),
  refresh: app => set(state => {
    const pid = state.playerId;
    if (pid) {
      // we always want to force the focus on the players to whatever scene they're focused on
      const playerScene = app.current_game.players.get(pid)?.scene;
      if (playerScene) {
        getState().setGridFocus(playerScene);
      }
    }
    return { app, fetchStatus: "Ready" };
  }),
  refreshGame: game => set(state => {
    state.refresh({ ...state.app, current_game: game });
    return state;
  }),

  getItem: iid => get().getGame().items[iid],
  getItems: iids => LD.sortBy(
    filterMap(iids, iid => get().getItem(iid)),
    i => i.name
  ),

  getScenes: (sceneIds) => LD.sortBy(filterMap(sceneIds, s => get().getGame().scenes.get(s)), s => s.name),
  getScene: (scene_id) => get().getGame().scenes.get(scene_id),

  getCurrentCombatCreatureID: () => {
    const combat = get().getCombat();
    if (!combat) return;
    const entry = idx(combat.creatures.data, combat.creatures.cursor);
    if (!entry) { throw new Error(`No combat creature at ${combat.creatures.cursor}`); }
    return entry[0];
  },

  getNote: (path, name) => {
    if (!name) return;
    const fnode = get().getFolderNode(path);
    if (fnode && fnode.notes.hasOwnProperty(name)) {
      return fnode.notes[name];
    }
  },

  getFolderNode: path => get().getFolder(path)?.data,

  getFolder: path => {
    let cur: T.Folder | undefined = get().getGame().campaign;
    for (const seg of path) {
      cur = cur.children.get(seg);
      if (!cur) { return undefined; }
    }
    return cur;
  },

  creatureIsInCombat: creatureId =>
    LD.find(
      get().getCombat()?.creatures.data,
      ([cid, _]) => cid === creatureId
    ) !== undefined,
  getSceneCreatures: scene => get().getCreatures(scene.creatures.keySeq().toArray()),
  getCreatures: cids => LD.sortBy(filterMap(cids, cid => get().getCreature(cid)), (c: T.Creature) => c.name),
  getCreature: cid => get().getGame().creatures.get(cid),
  getCombat: () => get().getGame().current_combat,
  getGame: () => get().app.current_game,

  getAbility: abid => get().getGame().abilities[abid],
  getAbilities: abids => LD.sortBy( filterMap(abids, abid => get().getAbility(abid)), i => i.name),
  getClass: classid => get().getGame().classes.get(classid),
  getClasses: classids => LD.sortBy(
      filterMap(classids, classid => get().getClass(classid)),
      c => c.name,
    ),

  getSceneInventory: scene => {
    const arr = filterMap(scene.inventory.entrySeq().toArray(),
      ([iid, count]) => optMap(get().getItem(iid), (i): [T.Item, number] => [i, count]));
    const list = I.List(arr);
    return list.sortBy(([i, _]) => i.name);
  }
});

const initialApp: T.App = {
  snapshots: [],
  current_game: {
    players: I.Map(),
    current_combat: undefined,
    creatures: I.Map(),
    classes: I.Map(),
    items: {},
    scenes: I.Map(),
    abilities: {},
    campaign: { children: I.Map(), data: { scenes: [], creatures: [], notes: {}, items: [], abilities: [], classes: [] } }
  }
};


interface SecondaryFocusState {
  secondaryFocus?: SecondaryFocus;
  setSecondaryFocus: (f: SecondaryFocus) => void;
}

const secondaryFocusSlice: Slice<SecondaryFocusState> = set => ({
  secondaryFocus: undefined,
  setSecondaryFocus: secondaryFocus => set(() => ({ secondaryFocus }))
});

interface GridState {
  grid: GridModel;
  gridFocus?: GridFocus;
  // resetGrid is used in New Game
  resetGrid: () => void;
  setGridFocus: (s: T.SceneID, l?: SceneLayerType) => void;
  activateObjects: (objects: GridObject[], coords: [number, number]) => void;
  activateContextMenu: (pt: T.Point3, coords: [number, number]) => void;
  clearContextMenu: () => void;
  setHighlightColor: (color: T.Color) => void;
  setObjectVisibility: (v: T.Visibility) => void;
  setTerrain: (t: T.Terrain) => void;
  setHighlights: (h: T.Highlights) => void;
  displayMovementOptions: (options: T.Point3[], cid?: T.CreatureID, teleport?: boolean) => void;
  displayPotentialTargets: (cid: T.CreatureID, ability_id: T.AbilityID, options: T.PotentialTargets) => void;
  clearPotentialTargets: () => void;
  clearMovementOptions: () => void;

  // accessors
  getFocusedScene: () => T.Scene | undefined;
}

export type AllStates = PlayerState & GridState & AppState & ErrorState & SecondaryFocusState;
type Slice<T> = StateCreator<AllStates, [], [], T>;

const gridSlice: Slice<GridState> = (set, get) => ({
  grid: defaultGrid,
  gridFocus: undefined,
  resetGrid: () => set(() => ({ grid: defaultGrid })),
  setGridFocus: (scene_id: T.SceneID, t?: SceneLayerType) => set(() => {
    const scene = get().getGame().scenes.get(scene_id);
    let layer: SceneLayer | undefined = undefined;
    switch (t) {
      case "Terrain":
        // When switching to the Terrain layer, create a copy of the terrain data for editing.
        layer = { t, terrain: scene ? scene.terrain : I.Set() };
        break;
      case "Highlights":
        layer = { t, highlights: scene ? scene.highlights : I.Map() };
        break;
      case "Volumes":
        layer = { t };
        break;
    }
    return { gridFocus: { scene_id, layer } };
  }),
  activateObjects: (objects, coords) => set(({ grid }) => ({ grid: { ...grid, active_objects: { objects, coords } } })),
  activateContextMenu: (pt, coords) => set(({ grid }) => ({ grid: { ...grid, context_menu: { pt, coords } } })),
  clearContextMenu: () => set(({ grid }) => ({ grid: { ...grid, context_menu: undefined, active_objects: { ...grid.active_objects, objects: [] } } })),
  setHighlightColor: highlight_color => set(({ grid }) => ({ grid: { ...grid, highlight_color } })),
  setObjectVisibility: object_visibility => set(({ grid }) => ({ grid: { ...grid, object_visibility } })),
  setTerrain: terrain => set(({ gridFocus }) => {
    // TODO: do we really need to do this conditional?
    if (gridFocus?.layer?.t === "Terrain") {
      return { gridFocus: { ...gridFocus, layer: { ...gridFocus.layer, terrain } } }
    }
    return {};
  }),
  setHighlights: highlights => set(({ gridFocus }) => {
    if (gridFocus?.layer?.t === "Highlights") {
      return { gridFocus: { ...gridFocus, layer: { ...gridFocus.layer, highlights } } };
    }
    return {}
  }),
  displayMovementOptions: (options, cid, teleport) => set(({ grid }) => ({ grid: { ...grid, movement_options: { cid, options, teleport: !!teleport } } })),
  displayPotentialTargets: (cid, ability_id, options) => set(({ grid }) => ({ grid: { ...grid, target_options: { cid, ability_id, options } } })),
  clearPotentialTargets: () => set(({ grid }) => ({ grid: { ...grid, target_options: undefined } })),
  clearMovementOptions: () => set(({ grid }) => ({ grid: { ...grid, movement_options: undefined } })),

  getFocusedScene: () => {
    const state = get();
    if (!state.gridFocus) return;
    return state.getGame().scenes.get(state.gridFocus.scene_id);
  }
});

const defaultGrid = {
  highlight_color: "#FF0000",
  object_visibility: { t: "AllPlayers" } as T.Visibility,
  active_objects: { objects: [], coords: [0, 0] as [number, number] },
};

interface PlayerState {
  playerId: T.PlayerID | undefined;
  setPlayerId: (id: T.PlayerID) => void;
}
const playerSlice: Slice<PlayerState> = set => ({
  playerId: undefined,
  setPlayerId: playerId => set(() => ({playerId}))
});

interface ErrorState {
  error: string | undefined;
  setError: (e: string) => void,
  clearError: () => void,
}

const errorSlice: Slice<ErrorState> = set => ({
  error: undefined,
  setError: error => set(() => ({ error })),
  clearError: () => set(() => ({ error: undefined }))
});

export const useState = createWithEqualityFn<AllStates>()((...a) => ({
  ...errorSlice(...a),
  ...playerSlice(...a),
  ...gridSlice(...a),
  ...appSlice(...a),
  ...secondaryFocusSlice(...a),
}),
  // There may be an argument for *deep* comparison here. The app is 100%
  // replaced on every refresh, and selectors will often return structures deeper than just 1 level.
  shallow);

export const getState = useState.getState;

// Just for debugging
(window as any).getState = getState;



export interface GridModel {
  active_objects: {
    objects: Array<GridObject>;
    coords: [number, number];
  };
  context_menu?: { pt: T.Point3; coords: [number, number] };
  movement_options?: {
    cid?: T.CreatureID; // undefined when we're moving in combat
    options: Array<T.Point3>;
    teleport: boolean;
  };
  target_options?: { cid: T.CreatureID; ability_id: T.AbilityID; options: T.PotentialTargets };
  highlight_color: T.Color;
  object_visibility: T.Visibility;
}

export type GridObject =
  | { t: "VolumeCondition"; id: T.ConditionID }
  | { t: "Creature"; id: T.CreatureID }
  | { t: "Annotation"; pt: T.Point3 }
  | { t: "SceneHotSpot"; scene_id: T.SceneID; pt: T.Point3 }
  ;

export type SceneLayer =
  | { t: "Terrain"; terrain: T.Terrain }
  | { t: "Highlights"; highlights: T.Highlights }
  | { t: "Volumes" }
  | { t: "LinkedScenes" }
  ;

export type SceneLayerType =
  | "Terrain"
  | "Highlights"
  | "Volumes"
  | "LinkedScenes"
  ;

export interface GridFocus { scene_id: T.SceneID; layer?: SceneLayer; }

export type SecondaryFocus =
  | { t: "Note"; path: T.FolderPath; name: string | undefined }
  | { t: "Creature"; creature_id: T.CreatureID }
  | { t: "Item"; item_id: T.ItemID }
  ;


export function filterMap<T, R>(coll: Array<T>, f: (t: T) => R | undefined): Array<R> {
  // I can't "naturally" convince TypeScript that this filter makes an
  // Array<R> instead of Array<R|undefined>, hence the assertion
  return coll.map(f).filter(el => el) as Array<R>;
}

export function filterMapValues<T, R>
  (obj: { [index: string]: T }, f: (val: T) => R | undefined): { [index: string]: R } {
  const result: { [index: string]: R } = {};
  for (const key of LD.keys(obj)) {
    const new_val = f(obj[key]);
    if (new_val !== undefined) { result[key] = new_val; }
  }
  return result;
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

export function optMap<T, R>(x: T | undefined, f: ((t: T) => R)): R | undefined {
  if (x !== undefined) {
    return f(x);
  }
}
