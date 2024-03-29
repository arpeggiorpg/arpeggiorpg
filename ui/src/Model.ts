import { Map, Set } from "immutable";
import deepEqual from "lodash/isEqual";
import sortBy from "lodash/sortBy";
import type { StateCreator } from "zustand";
import { shallow } from "zustand/shallow";
import { createWithEqualityFn } from "zustand/traditional";

import { getCookie } from "react-use-cookie";
import * as T from "./PTTypes";

export const ID_TOKEN_NAME = "arpeggio-token";

export type FetchStatus = "Unfetched" | "Ready" | "Error";
export type SocketStatus = "unconnected" | "open" | "closed";
export interface AppState {
  userToken: string | undefined;
  game: T.Game;
  gameId: T.GameID | undefined;
  gameName: string | undefined;
  recentLogs: [T.GameIndex, T.GameLog][];
  fetchStatus: FetchStatus;

  socketStatus: SocketStatus;

  setRecentLogs: (recentLogs: [T.GameIndex, T.GameLog][]) => void;
  addLogs: (logs: [T.GameIndex, T.GameLog][]) => void;

  setSocketStatus: (s: SocketStatus) => void;

  setUserToken: (s: string | undefined) => void;

  setFetchStatus: (s: FetchStatus) => void;
  setGameId: (s: T.GameID | undefined) => void;
  setGameName: (s: string | undefined) => void;
  refresh: (g: T.Game) => void;

  // utility functions for fetching state
  getItem: (iid: T.ItemID) => T.Item | undefined;
  getItems: (iids: T.ItemID[]) => T.Item[];
  getScenes: (sceneIds: T.SceneID[]) => T.Scene[];
  getCurrentCombatCreatureID: () => T.CreatureID | undefined;
  getNote: (path: T.FolderPath, name: string | undefined) => T.Note | undefined;
  getFolderNode: (path: T.FolderPath) => T.FolderNode | undefined;
  getFolder: (path: T.FolderPath) => T.Folder | undefined;
  getScene: (sceneId: T.SceneID) => T.Scene | undefined;

  creatureIsInCombat: (creatureId: T.CreatureID) => boolean;
  getSceneCreatures: (scene: T.Scene) => T.Creature[];
  getCreatures: (cids: T.CreatureID[]) => T.Creature[];
  getCreature: (cid: T.CreatureID) => T.Creature | undefined;
  getCombat: () => T.Combat | null;

  getAbility: (abid: T.AbilityID) => T.Ability | undefined;
  getAbilities: (abids: T.AbilityID[]) => T.Ability[];
  getClass: (classid: T.ClassID) => T.Class | undefined;
  getClasses: (classids: T.ClassID[]) => T.Class[];
  getSceneInventory: (scene: T.Scene) => [T.Item, number][];
}

const appSlice: Slice<AppState> = (set, get) => ({
  userToken: getCookie(ID_TOKEN_NAME),
  game: initialGame,
  gameId: undefined,
  gameName: undefined,
  recentLogs: [],
  fetchStatus: "Unfetched",
  socketStatus: "unconnected",

  setSocketStatus: socketStatus => set(() => ({ socketStatus })),

  setUserToken: userToken => set(() => ({ userToken })),
  setFetchStatus: fetchStatus => set(() => ({ fetchStatus })),
  setGameId: gameId => set(() => ({ gameId })),
  setGameName: gameName => set(() => ({ gameName })),
  refresh: game =>
    set(state => {
      const pid = state.playerId;
      if (pid) {
        // we always want to force the focus on the players to whatever scene they're focused on
        const playerScene = game.players.get(pid)?.scene;
        if (playerScene) {
          getState().setGridFocus(playerScene);
        }
      }
      return { game, fetchStatus: "Ready" };
    }),
  setRecentLogs: recentLogs => set(() => ({recentLogs})),
  addLogs: logs => set(state => {
    let recentLogs = state.recentLogs;
    recentLogs = recentLogs.concat(logs);
    // only keep 100 logs around in-memory
    recentLogs = recentLogs.slice(recentLogs.length - 100);
    return {recentLogs};
  }),

  getItem: iid => get().game.items[iid],
  getItems: iids =>
    sortBy(
      filterMap(iids, iid => get().getItem(iid)),
      i => i.name,
    ),

  getScenes: (sceneIds) =>
    sortBy(filterMap(sceneIds, s => get().game.scenes.get(s)), s => s.name),
  getScene: (scene_id) => get().game.scenes.get(scene_id),

  getCurrentCombatCreatureID: () => {
    const combat = get().getCombat();
    if (!combat) return;
    const entry = combat.creatures.data[combat.creatures.cursor];
    if (!entry) throw new Error(`No combat creature at ${combat.creatures.cursor}`);
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
    let cur: T.Folder | undefined = get().game.campaign;
    for (const seg of path) {
      cur = cur.children.get(seg);
      if (!cur) return undefined;
    }
    return cur;
  },

  creatureIsInCombat: creatureId =>
    get().getCombat()?.creatures.data.find(([cid, _]) => cid === creatureId) !== undefined,
  getSceneCreatures: scene => get().getCreatures(scene.creatures.keySeq().toArray()),
  getCreatures: cids =>
    sortBy(filterMap(cids, cid => get().getCreature(cid)), (c: T.Creature) => c.name),
  getCreature: cid => get().game.creatures.get(cid),
  getCombat: () => get().game.current_combat,

  getAbility: abid => get().game.abilities[abid],
  getAbilities: abids => sortBy(filterMap(abids, abid => get().getAbility(abid)), i => i.name),
  getClass: classid => get().game.classes.get(classid),
  getClasses: classids =>
    sortBy(
      filterMap(classids, classid => get().getClass(classid)),
      c => c.name,
    ),

  getSceneInventory: scene => {
    const arr = filterMap(
      scene.inventory.entrySeq().toArray(),
      ([iid, count]) => optMap(get().getItem(iid), (i): [T.Item, number] => [i, count]),
    );
    return sortBy(arr, ([i, _]) => i.name);
  },
});

const initialGame: T.Game = {
  players: Map(),
  current_combat: null,
  creatures: Map(),
  classes: Map(),
  items: {},
  scenes: Map(),
  abilities: {},
  campaign: {
    children: Map(),
    data: { scenes: [], creatures: [], notes: {}, items: [], abilities: [], classes: [] },
  },
  tile_system: "DnD",
  active_scene: null,
};

interface SecondaryFocusState {
  secondaryFocus?: SecondaryFocus;
  setSecondaryFocus: (f: SecondaryFocus) => void;
}

const secondaryFocusSlice: Slice<SecondaryFocusState> = set => ({
  secondaryFocus: undefined,
  setSecondaryFocus: secondaryFocus => set(() => ({ secondaryFocus })),
});


interface GridState {
  grid: GridModel;
  gridFocus?: GridFocus;

  pendingBackgroundScale: [number, number] | undefined;
  pendingBackgroundOffset: [number, number] | undefined;
  targetingPoint: T.Point3 | undefined;
  affectedPoints: T.Point3[] | undefined;
  affectedCreatures: T.CreatureID[];

  setPendingBackgroundScale: (scale: [number, number] | undefined) => void;
  setPendingBackgroundOffset: (offset: [number, number] | undefined) => void;

  // resetGrid is used in New Game
  resetGrid: () => void;
  setGridFocus: (s: T.SceneID | undefined, l?: SceneLayerType) => void;
  activateObjects: (objects: GridObject[], coords: [number, number]) => void;
  activateContextMenu: (pt: T.Point3, coords: [number, number]) => void;
  clearContextMenu: () => void;
  setHighlightColor: (color: T.Color) => void;
  setObjectVisibility: (v: T.Visibility) => void;
  setTerrain: (t: T.Terrain) => void;
  setHighlights: (h: T.Highlights) => void;
  displayMovementOptions: (options: T.Point3[], cid?: T.CreatureID, teleport?: boolean) => void;
  displayPotentialTargets: (
    cid: T.CreatureID,
    ability_id: T.AbilityID,
    options: T.PotentialTargets,
  ) => void;
  clearPotentialTargets: () => void;
  clearMovementOptions: () => void;

  setTargetingPoint: (p: T.Point3 | undefined) => void;
  setAffectedPoints: (ps: T.Point3[] | undefined) => void;
  setAffectedCreatures: (cs: T.CreatureID[]) => void;

  // accessors
  getFocusedScene: () => T.Scene | undefined;
}

export type AllStates = PlayerState & GridState & AppState & ErrorState & SecondaryFocusState;
type Slice<T> = StateCreator<AllStates, [], [], T>;

const gridSlice: Slice<GridState> = (set, get) => ({
  grid: defaultGrid,
  gridFocus: undefined,

  pendingBackgroundScale: undefined,
  pendingBackgroundOffset: undefined,
  targetingPoint: undefined,
  affectedPoints: undefined,
  affectedCreatures: [],

  setPendingBackgroundScale: pendingBackgroundScale => set(() => ({pendingBackgroundScale})),
  setPendingBackgroundOffset: pendingBackgroundOffset => set(() => ({pendingBackgroundOffset})),

  resetGrid: () => set(() => ({ grid: defaultGrid })),
  setGridFocus: (scene_id, t?) =>
    set(() => {
      if (!scene_id) {
        return { gridFocus: undefined };
      }
      const scene = get().game.scenes.get(scene_id);
      let layer: SceneLayer | undefined = undefined;
      switch (t) {
        case "Terrain":
          // When switching to the Terrain layer, create a copy of the terrain data for editing.
          layer = { t, terrain: scene ? scene.terrain : Set() };
          break;
        case "Highlights":
          layer = { t, highlights: scene ? scene.highlights : Map() };
          break;
        case "Volumes":
          layer = { t };
          break;
      }
      return { gridFocus: { scene_id, layer } };
    }),
  activateObjects: (objects, coords) =>
    set(({ grid }) => ({ grid: { ...grid, active_objects: { objects, coords } } })),
  activateContextMenu: (pt, coords) =>
    set(({ grid }) => ({ grid: { ...grid, context_menu: { pt, coords } } })),
  clearContextMenu: () =>
    set(({ grid }) => ({
      grid: {
        ...grid,
        context_menu: undefined,
        active_objects: { ...grid.active_objects, objects: [] },
      },
    })),
  setHighlightColor: highlight_color => set(({ grid }) => ({ grid: { ...grid, highlight_color } })),
  setObjectVisibility: object_visibility =>
    set(({ grid }) => ({ grid: { ...grid, object_visibility } })),
  setTerrain: terrain =>
    set(({ gridFocus }) => {
      // TODO: do we really need to do this conditional?
      if (gridFocus?.layer?.t === "Terrain") {
        return { gridFocus: { ...gridFocus, layer: { ...gridFocus.layer, terrain } } };
      }
      return {};
    }),
  setHighlights: highlights =>
    set(({ gridFocus }) => {
      if (gridFocus?.layer?.t === "Highlights") {
        return { gridFocus: { ...gridFocus, layer: { ...gridFocus.layer, highlights } } };
      }
      return {};
    }),
  displayMovementOptions: (options, cid, teleport) =>
    set(({ grid }) => ({
      grid: { ...grid, movement_options: { cid, options, teleport: !!teleport } },
    })),
  displayPotentialTargets: (cid, ability_id, options) =>
    set(({ grid }) => ({ grid: { ...grid, target_options: { cid, ability_id, options } } })),
  clearPotentialTargets: () =>
    set(({ grid }) => ({ grid: { ...grid, target_options: undefined } })),
  clearMovementOptions: () =>
    set(({ grid }) => ({ grid: { ...grid, movement_options: undefined } })),

  setTargetingPoint: targetingPoint => set(() => ({targetingPoint})),
  setAffectedPoints: affectedPoints => set(() => ({affectedPoints})),
  setAffectedCreatures: affectedCreatures => set(() => ({affectedCreatures})),

  getFocusedScene: () => {
    const state = get();
    if (!state.gridFocus) return;
    return state.game.scenes.get(state.gridFocus.scene_id);
  },
});

const defaultGrid = {
  highlight_color: "#FF0000",
  object_visibility: "AllPlayers" as T.Visibility,
  active_objects: { objects: [], coords: [0, 0] as [number, number] },
};

interface PlayerState {
  playerId: T.PlayerID | undefined;
  setPlayerId: (id: T.PlayerID | undefined) => void;
}
const playerSlice: Slice<PlayerState> = set => ({
  playerId: undefined,
  setPlayerId: playerId => set(() => ({ playerId })),
});

interface ErrorState {
  error: string | undefined;
  setError: (e: string) => void;
  clearError: () => void;
}

const errorSlice: Slice<ErrorState> = set => ({
  error: undefined,
  setError: error => set(() => ({ error })),
  clearError: () => set(() => ({ error: undefined })),
});

export const useState = createWithEqualityFn<AllStates>()(
  (...a) => ({
    ...errorSlice(...a),
    ...playerSlice(...a),
    ...gridSlice(...a),
    ...appSlice(...a),
    ...secondaryFocusSlice(...a),
  }),
  // There may be an argument for *deep* comparison here. The app is 100%
  // replaced on every refresh, and selectors will often return structures deeper than just 1 level.
  shallow,
);

export const getState = useState.getState;

// Just for debugging
if (typeof window !== "undefined") (window as any).getState = getState;

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
  | { t: "SceneHotSpot"; scene_id: T.SceneID; pt: T.Point3 };

export type SceneLayer =
  | { t: "Terrain"; terrain: T.Terrain }
  | { t: "Highlights"; highlights: T.Highlights }
  | { t: "Volumes" }
  | { t: "LinkedScenes" };

export type SceneLayerType =
  | "Terrain"
  | "Highlights"
  | "Volumes"
  | "LinkedScenes";

export interface GridFocus {
  scene_id: T.SceneID;
  layer?: SceneLayer;
}

export type SecondaryFocus =
  | { t: "Note"; path: T.FolderPath; name: string | undefined }
  | { t: "Class", class_id: T.ClassID }
  | { t: "Ability", ability_id: T.AbilityID }
  | { t: "Creature"; creature_id: T.CreatureID }
  | { t: "Item"; item_id: T.ItemID };

export function filterMap<T, R>(coll: Array<T>, f: (t: T) => R | undefined): Array<R> {
  return coll.flatMap(el => {
    const newEl = f(el);
    return newEl ? [newEl] : [];
  });
}

export function filterMapValues<T, R>(
  obj: { [index: string]: T },
  f: (val: T) => R | undefined,
): { [index: string]: R } {
  const result: { [index: string]: R } = {};
  for (const [key, value] of Object.entries(obj)) {
    const new_val = f(value);
    if (new_val !== undefined) result[key] = new_val;
  }
  return result;
}

/// A version of isEqual that requires both arguments to be the same type.
export function isEqual<T>(l: T, r: T): boolean {
  return deepEqual(l, r);
}

export function optMap<T, R>(x: T | undefined, f: (t: T) => R): R | undefined {
  if (x !== undefined) {
    return f(x);
  }
}

// hasAtLeast is from https://stackoverflow.com/a/69370003/4930992

type Indices<L extends number, T extends number[] = []> = T["length"] extends L ? T[number]
  : Indices<L, [T["length"], ...T]>;

type LengthAtLeast<T extends readonly any[], L extends number> = Pick<Required<T>, Indices<L>>;

/** Check if the length of an array is at least some number long.
 *
 * This is a workaround for the fact that typescript doesn't do type-narrowing on manual length
 * checks in the face of noUncheckedIndexAccess.
 */
export function hasAtLeast<T extends readonly any[], L extends number>(
  arr: T,
  len: L,
): arr is T & LengthAtLeast<T, L> {
  return arr.length >= len;
}

// this is a workaround for https://stackoverflow.com/questions/60834196/union-types-typescript-complains-function-lacks-ending-return-statement-and-re
export function assertNever(x: never): never {
  throw new Error("Unexpected object: " + x);
}



export function getFolder(tree: T.Folder, path: T.FolderPath): T.Folder | undefined {
  for (const seg of path) {
    let child = tree.children.get(seg);
    if (!child) return;
    tree = child;
  }
  return tree;
}
