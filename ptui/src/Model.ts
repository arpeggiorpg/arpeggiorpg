import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';
import * as JD from "type-safe-json-decoder";
import { createWithEqualityFn } from "zustand/traditional";

import * as T from './PTTypes';
import type { StateCreator } from "zustand";
import { shallow } from 'zustand/shallow';

export const RPI_URL = import.meta.env.VITE_RPI_URL;

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

export async function decodeFetch<J>(
  url: string, init: RequestInit | undefined,
  decoder: JD.Decoder<J>
): Promise<J> {
  const result = await fetch(url, init);
  const json = await result.json();
  try {
    return decoder.decodeAny(json);
  } catch (e) {
    console.error(e);
    throw { _pt_error: "JSON", original: e };
  }
}

export async function ptfetch<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: JD.Decoder<J>
): Promise<J> {
  try {
    const json = await decodeFetch(url, init, decoder);
    return json;
  } catch (e) {
    const error = extract_error_details(e);
    getState().setError(error);
    throw e;
  }

  function extract_error_details(error: any): string {
    if (error._pt_error) {
      switch (error._pt_error) {
        case "JSON": return `Failed to decode JSON ${error.original}`;
        // RADIX: is "RPI" actually produced anywhere?
        case "RPI": return `Error received from server ${error.message}`;
        default: return `Unknown error ${error.toString()}`;
      }
    } else {
      return `Unknown error ${error.toString()}`;
    }
  }
}

const default_state = {
  grid: {
    highlight_color: "#FF0000",
    object_visibility: { t: "AllPlayers" } as T.Visibility,
    active_objects: { objects: [], coords: [0, 0] as [number, number] },
  },
};

export async function startPoll() {
  // This is kinda dumb, but:
  // - first, we fetch the entire APP at "/"
  // - then, we start long-polling at /poll/{snapidx}/{logidx}
  //
  // Why not just start long-polling at `/poll/0/0`? Because if the server has a freshly loaded
  // game, it will be at index 0/0, and so won't return immediately when you poll 0/0.
  const app = await ptfetch(RPI_URL, undefined, T.decodeApp);
  getState().refresh(app);
  await poll(app);

  async function poll(app: T.App) {
    while (true) {
      const num_snaps = app.snapshots.length;
      const snaps = app.snapshots[num_snaps - 1];
      const num_logs = snaps ? snaps.logs.length : 0;
      const url = `${RPI_URL}/poll/${num_snaps}/${num_logs}`;
      try {
        console.log("gonna fetch");
        app = await ptfetch(url, undefined, T.decodeApp);
        getState().refresh(app);
      } catch (e) {
        console.error("oops got an error", e);
        getState().setFetchStatus("Error");
      }
    }
  }
}

// ACTIONS!
// These should probably be moved to a different module.
// All of these should ONLY be called in "imperative" code, i.e. event handlers,
// never while rendering components.
export async function requestMove(cid: T.CreatureID) {
  const scene = getFocusedScene();
  if (scene) {
    const result = await ptfetch(
      `${RPI_URL}/movement_options/${scene.id}/${cid}`,
      undefined,
      JD.array(T.decodePoint3)
    );
    getState().displayMovementOptions(result, cid);
  }
}

export function moveCreature(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getFocusedScene();
  if (scene) {
    sendCommand({ t: "PathCreature", scene_id: scene.id, creature_id, dest });
  } else {
    throw new Error(`Tried moving when there is no scene`);
  }
}

export function setCreaturePos(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getFocusedScene();
  if (scene) {
    sendCommand({ t: 'SetCreaturePos', scene_id: scene.id, creature_id, dest });
  }
}

export function moveCombatCreature(dest: T.Point3) {
  getState().clearMovementOptions();
  sendCommand({ t: "PathCurrentCombatCreature", dest });
}

export async function requestCombatMovement() {
  const options = await ptfetch(
    RPI_URL + "/combat_movement_options", undefined,
    JD.array(T.decodePoint3));
  getState().displayMovementOptions(options);
}


/* Execute an ability that has already been selected, with a target.
 * This relies on the state being set up ahead of time: we must have a target_options already.
 */
export async function executeCombatAbility(target_id: T.CreatureID) {
  const opts = getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (options.t !== "CreatureIDs") { throw new Error(`Only support CreatureIDs for now`); }
  const target: T.DecidedTarget = { t: "Creature", creature_id: target_id };
  sendCommand({ t: "CombatAct", ability_id, target });
  getState().clearPotentialTargets();
}

export function executeCombatPointTargetedAbility(point: T.Point3) {
  const opts = getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (options.t !== "Points") {
    throw new Error(`This function only works for abilities that use Points`);
  }
  const target: T.DecidedTarget = { t: "Point", point };
  sendCommand({ t: "CombatAct", ability_id, target });
  getState().clearPotentialTargets();
}

export async function loadGame(source: T.ModuleSource, name: string): Promise<void> {
  const url = source === "SavedGame"
    ? `${RPI_URL}/saved_games/user/${name}/load`
    : `${RPI_URL}/saved_games/module/${name}/load`;

  const app = await ptfetch(url, { method: 'POST' }, T.decodeApp);
  resetApp(app);
}

export async function fetchSavedGames(): Promise<[Array<string>, Array<string>]> {
  return ptfetch(
    RPI_URL + '/saved_games',
    undefined,
    JD.tuple(JD.array(JD.string()), JD.array(JD.string()))
  );
}

export async function saveGame(game: string): Promise<undefined> {
  return ptfetch(
    `${RPI_URL}/saved_games/user/${game}`,
    { method: 'POST' },
    JD.succeed(undefined)
  );
}

export function exportModule(path: T.FolderPath, name: string): Promise<undefined> {
  const url = `${RPI_URL}/modules/${name}`;
  const opts = {
    method: 'POST',
    body: JSON.stringify(T.encodeFolderPath(path)),
    headers: { "content-type": "application/json" },
  };
  return ptfetch(url, opts, JD.succeed(undefined));
}


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
        getState().setFocus(playerScene);
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
  // reset is used in New Game
  reset: () => void;
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
  grid: default_state.grid,
  gridFocus: undefined,
  reset: () => set(() => ({ grid: default_state.grid })),
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

export function folderPathToString(path: T.FolderPath): string {
  if (isEqual(path, [])) {
    return "Campaign Root";
  }
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

export function optMap<T, R>(x: T | undefined, f: ((t: T) => R)): R | undefined {
  if (x !== undefined) {
    return f(x);
  }
}

export async function sendCommand(cmd: T.GameCommand) {
  const json = T.encodeGameCommand(cmd);
  console.log("[sendCommand:JSON]", json);
  const result = await ptfetch(
    RPI_URL,
    {
      method: "POST",
      body: JSON.stringify(json),
      headers: { "content-type": "application/json" },
    },
    T.decodeRustResult(T.decodeSendCommandResult, JD.string())
  );
  switch (result.t) {
    case "Ok":
      getState().refreshGame(result.result[0]);
      return;
    case "Err":
      throw { _pt_error: 'RPI', message: result.error };
  }
}

export function sendCommands(cmds: Array<T.GameCommand>) {
  // TODO: I guess we could add an endpoint that handles multiple commands at once
  for (const cmd of cmds) {
    sendCommand(cmd);
  }
}

/// Send a Command and *don't* automatically handle errors, but instead return a future
/// representing the result. This is useful for code which wants to send a command and interpret
/// the resulting gamelogs.
export async function sendCommandWithResult(cmd: T.GameCommand): Promise<T.RustResult<Array<T.GameLog>, string>> {
  const json = T.encodeGameCommand(cmd);
  console.log("[sendCommand:JSON]", json);
  const rpi_result = decodeFetch(
    RPI_URL,
    {
      method: "POST",
      body: JSON.stringify(json),
      headers: { "content-type": "application/json" },
    },
    T.decodeRustResult(
      JD.map(
        ([_, logs]) => logs,
        T.decodeSendCommandResult),
      JD.string())
  );
  return rpi_result;
}


export async function newGame() {
  const app = await ptfetch(`${RPI_URL}/new_game`, { method: "POST" }, T.decodeApp);
  resetApp(app);
}

function resetApp(app: T.App) {
  getState().refresh(app);
  getState().reset();
}

function selectAbility(
  scene_id: T.SceneID, cid: T.CreatureID, ability_id: T.AbilityID): ThunkAction<void> {
  return (dispatch, getState) => {
    const url = `${RPI_URL}/target_options/${scene_id}/${cid}/${ability_id}`;
    ptfetch(dispatch, url, undefined, T.decodePotentialTargets,
      options => dispatch({ type: "DisplayPotentialTargets", cid, ability_id, options }));
  };
}

export function requestCombatAbility(
  cid: T.CreatureID, ability_id: T.AbilityID, ability: T.Ability, scene_id: T.SceneID
): ThunkAction<void> {
  if (ability.action.t === "Creature" && ability.action.target.t === "Actor") {
    return sendCommand({ t: "CombatAct", ability_id, target: { t: "Actor" } });
  } else {
    return selectAbility(scene_id, cid, ability_id);
  }
}

export function fetchAbilityTargets(
  scene_id: T.SceneID, actor_id: T.CreatureID, ability_id: T.AbilityID, point: T.Point3
): Promise<{ points: Array<T.Point3>; creatures: Array<T.CreatureID> }> {
  const uri =
    `${RPI_URL}/preview_volume_targets/${scene_id}/${actor_id}/`
    + `${ability_id}/${point.x}/${point.y}/${point.z}`;
  return ptfetch(
    uri,
    { method: 'POST' },
    JD.map(([creatures, points]) => ({ points, creatures }),
      JD.tuple(JD.array(JD.string()), JD.array(T.decodePoint3))),
  );
}

export function getCreaturePos(scene: T.Scene, creature_id: T.CreatureID): T.Point3 | undefined {
  return optMap(scene.creatures.get(creature_id), ([pos, _]) => pos);
}

export function getCreatureDetails(creature: T.Creature): T.CreatureCreation {
  const { name, class_, portrait_url, note, bio, initiative, size, icon_url } = creature;
  return { name, class_, portrait_url, note, bio, initiative, size, icon_url };
}
