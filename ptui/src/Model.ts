import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';
import * as JD from "type-safe-json-decoder";
import { createWithEqualityFn } from "zustand/traditional";

import * as T from './PTTypes';
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
    useError.getState().set(error);
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
  useApp.getState().refresh(app);
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
        useApp.getState().refresh(app);
      } catch (e) {
        console.error("oops got an error", e);
        useApp.getState().setFetchStatus("Error");
      }
    }
  }
}

function getFocusedScene() {
  const focus = useGrid.getState().focus;
  if (!focus) return;
  return useApp.getState().app.current_game.scenes.get(focus.scene_id);
}

export async function requestMove(cid: T.CreatureID) {
  const scene = getFocusedScene();
  if (scene) {
    const result = await ptfetch(
      `${RPI_URL}/movement_options/${scene.id}/${cid}`,
      undefined,
      JD.array(T.decodePoint3)
    );
    useGrid.getState().displayMovementOptions(result, cid);
  }
}

export function moveCreature(creature_id: T.CreatureID, dest: T.Point3) {
  useGrid.getState().clearMovementOptions();
  const scene = getFocusedScene();
  if (scene) {
    sendCommand({ t: "PathCreature", scene_id: scene.id, creature_id, dest });
  } else {
    throw new Error(`Tried moving when there is no scene`);
  }
}

export function setCreaturePos(creature_id: T.CreatureID, dest: T.Point3) {
  useGrid.getState().clearMovementOptions();
  const scene = getFocusedScene();
  if (scene) {
    sendCommand({ t: 'SetCreaturePos', scene_id: scene.id, creature_id, dest });
  }
}

export function moveCombatCreature(dest: T.Point3) {
  useGrid.getState().clearMovementOptions();
  sendCommand({ t: "PathCurrentCombatCreature", dest });
}

export async function requestCombatMovement() {
  const options = await ptfetch(
    RPI_URL + "/combat_movement_options", undefined,
    JD.array(T.decodePoint3));
  useGrid.getState().displayMovementOptions(options);
}


/* Execute an ability that has already been selected, with a target.
 * This relies on the state being set up ahead of time: we must have a target_options already.
 */
export async function executeCombatAbility(target_id: T.CreatureID) {
  const opts = useGrid.getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (options.t !== "CreatureIDs") { throw new Error(`Only support CreatureIDs for now`); }
  const target: T.DecidedTarget = { t: "Creature", creature_id: target_id };
  sendCommand({ t: "CombatAct", ability_id, target });
  useGrid.getState().clearPotentialTargets();
}

export function executeCombatPointTargetedAbility(point: T.Point3) {
  const opts = useGrid.getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (options.t !== "Points") {
    throw new Error(`This function only works for abilities that use Points`);
  }
  const target: T.DecidedTarget = { t: "Point", point };
  sendCommand({ t: "CombatAct", ability_id, target });
  useGrid.getState().clearPotentialTargets();
}


export class PTUI {

  getItem(iid: T.ItemID): T.Item | undefined {
    return get(this.app.current_game.items, iid);
  }

  getItems(iids: Array<T.ItemID>): Array<T.Item> {
    return LD.sortBy(
      filterMap(iids, iid => this.getItem(iid)),
      i => i.name);
  }

  getAbility(abid: T.AbilityID): T.Ability | undefined {
    return get(this.app.current_game.abilities, abid);
  }
  getAbilities(abids: Array<T.AbilityID>): Array<T.Ability> {
    return LD.sortBy(
      filterMap(abids, abid => this.getAbility(abid)),
      i => i.name);
  }

  getClass(classid: T.ClassID): T.Class | undefined {
    return this.app.current_game.classes.get(classid);
  }
  getClasses(classids: Array<T.ClassID>): Array<T.Class> {
    return LD.sortBy(
      filterMap(classids, classid => this.getClass(classid)),
      c => c.name,
    );
  }

  getCurrentCombatCreature(combat: T.Combat): T.Creature {
    const cid = this.getCurrentCombatCreatureID(combat);
    const creature = this.getCreature(cid);
    if (!creature) { throw new Error(`Current combat creature does not exist: ${cid}`); }
    return creature;
  }

  creatureIsInCombat(combat: T.Combat, creature_id: T.CreatureID): boolean {
    return LD.find(
      combat.creatures.data,
      ([cid, _]) => cid === creature_id) !== undefined;
  }

  getSceneCreatures(scene: T.Scene): Array<T.Creature> {
    return this.getCreatures(scene.creatures.keySeq().toArray());
  }

  getSceneInventory(scene: T.Scene): I.List<[T.Item, number]> {
    const arr = filterMap(scene.inventory.entrySeq().toArray(),
      ([iid, count]) => optMap(this.getItem(iid), (i): [T.Item, number] => [i, count]));
    const list = I.List(arr);
    return list.sortBy(([i, _]) => i.name);
  }
}

export function getCurrentCombatCreatureID(combat: T.Combat): T.CreatureID {
  const entry = idx(combat.creatures.data, combat.creatures.cursor);
  if (!entry) { throw new Error(`No combat creature at ${combat.creatures.cursor}`); }
  return entry[0];
}

export function useNote(path: T.FolderPath, name: string | undefined): T.Note | undefined {
  if (!name) return;
  const fnode = useFolderNode(path);
  if (fnode && fnode.notes.hasOwnProperty(name)) {
    return fnode.notes[name];
  }
}

export function useFolderNode(path: T.FolderPath): T.FolderNode | undefined {
  return useApp(s => {
    let cur: T.Folder | undefined = s.app.current_game.campaign;
    for (const seg of path) {
      cur = cur.children.get(seg);
      if (!cur) { return undefined; }
    }
    return cur.data;
  });

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



export function useScene(scene_id: T.SceneID): T.Scene | undefined {
  return useApp(s => s.app.current_game.scenes.get(scene_id));
}

export function useScenes(scene_ids: Array<T.SceneID>): Array<T.Scene> {
  // TODO: this isn't cacheable without deep equality checks
  return useApp(s => LD.sortBy(
    filterMap(scene_ids, sid => s.app.current_game.scenes.get(sid)),
    s => s.name));
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

type FetchStatus = "Unfetched" | "Ready" | "Error";
interface AppState {
  app: T.App;
  fetchStatus: FetchStatus;
  setFetchStatus: (s: FetchStatus) => void;
  refresh: (app: T.App) => void;
  refreshGame: (game: T.Game) => void;
}

export const useApp = createWithEqualityFn<AppState>()(set => ({
  app: initialApp,
  fetchStatus: "Unfetched",
  setFetchStatus: fetchStatus => set(() => ({ fetchStatus })),
  refresh: app => set(() => {
    const pid = usePlayer.getState().id;
    if (pid) {
      // we always want to force the focus on the players to whatever scene they're focused on
      const playerScene = app.current_game.players.get(pid)?.scene;
      if (playerScene) {
        useGrid.getState().setFocus(playerScene);
      }
    }
    return { app, fetchStatus: "Ready" };
  }),
  refreshGame: game => set(state => {
    state.refresh({ ...state.app, current_game: game });
    return state;
  })
  // TODO: maybe "fetch"?
}),
  // There may be an argument for *deep* comparison here. The app is 100%
  // replaced on every refresh.
  shallow);

interface SecondaryFocusState {
  focus?: SecondaryFocus;
  setFocus: (f: SecondaryFocus) => void;
}

export const useSecondaryFocus = createWithEqualityFn<SecondaryFocusState>()(set => ({
  focus: undefined,
  setFocus: focus => set(() => ({ focus }))
}), shallow);

interface GridState {
  grid: GridModel;
  focus?: GridFocus;
  // reset is used in New Game
  reset: () => void;
  setFocus: (s: T.SceneID, l?: SceneLayerType) => void;
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
}

export const useGrid = createWithEqualityFn<GridState>()(set => ({
  grid: default_state.grid,
  focus: undefined,
  reset: () => set(() => ({ grid: default_state.grid })),
  setFocus: (scene_id: T.SceneID, t?: SceneLayerType) => set(() => {
    const scene = useApp.getState().app.current_game.scenes.get(scene_id);
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
    return { focus: { scene_id, layer } };
  }),
  activateObjects: (objects, coords) => set(({ grid }) => ({ grid: { ...grid, activate_objects: { objects, coords } } })),
  activateContextMenu: (pt, coords) => set(({ grid }) => ({ grid: { ...grid, context_menu: { pt, coords } } })),
  clearContextMenu: () => set(({ grid }) => ({ grid: { ...grid, context_menu: undefined, active_objects: { ...grid.active_objects, objects: [] } } })),
  setHighlightColor: highlight_color => set(({ grid }) => ({ grid: { ...grid, highlight_color } })),
  setObjectVisibility: object_visibility => set(({ grid }) => ({ grid: { ...grid, object_visibility } })),
  setTerrain: terrain => set(({ focus }) => {
    // TODO: do we really need to do this conditional?
    if (focus?.layer?.t === "Terrain") {
      return { focus: { ...focus, layer: { ...focus.layer, terrain } } }
    }
    return {};
  }),
  setHighlights: highlights => set(({ focus }) => {
    if (focus?.layer?.t === "Highlights") {
      return { focus: { ...focus, layer: { ...focus.layer, highlights } } };
    }
    return {}
  }),
  displayMovementOptions: (options, cid, teleport) => set(({ grid }) => ({ grid: { ...grid, movement_options: { cid, options, teleport: !!teleport } } })),
  displayPotentialTargets: (cid, ability_id, options) => set(({ grid }) => ({ grid: { ...grid, target_options: { cid, ability_id, options } } })),
  clearPotentialTargets: () => set(({ grid }) => ({ grid: { ...grid, target_options: undefined } })),
  clearMovementOptions: () => set(({ grid }) => ({ grid: { ...grid, movement_options: undefined } })),
}), shallow);

export function useFocusedScene(): T.Scene | undefined {
  const focus = useGrid(g => g.focus);
  const game = useApp(a => a.app.current_game);
  // oh for Rust's "?" operator
  if (focus) {
    return game.scenes.get(focus.scene_id);
  }
}

interface PlayerState {
  id: T.PlayerID | undefined;
  set: (id: T.PlayerID) => void;
}
export const usePlayer = createWithEqualityFn<PlayerState>()((set) => ({
  id: undefined,
  set: id => set(() => ({id}))
}), Object.is);

interface ErrorState {
  error: string | undefined;
  set: (e: string) => void,
  clear: () => void
}

export const useError = createWithEqualityFn<ErrorState>()(set => ({
  error: undefined,
  set: error => set(() => ({ error })),
  clear: () => set(() => ({ error: undefined }))
}), Object.is);

// Just for debugging
(window as any).ptstate = { useError, usePlayer, useGrid, useSecondaryFocus, useApp };


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

export function creatureIsInCombat(combat: T.Combat, creature_id: T.CreatureID): boolean {
  return LD.find(
    combat.creatures.data,
    ([cid, _]) => cid === creature_id) !== undefined;
}

export function getSceneCreatures(app: T.App, scene: T.Scene) {
  return getCreatures(app, scene.creatures.keySeq().toArray());
}

export function useSceneCreatures<T>(scene: T.Scene, selector: (c: T.Creature[]) => T): T {
  return useApp(s => selector(getSceneCreatures(s.app, scene)));
}

export function getCreatures(app: T.App, cids: Array<T.CreatureID>): Array<T.Creature> {
  return LD.sortBy(filterMap(cids, cid => getCreature(app, cid)), (c: T.Creature) => c.name);
}

export function getCreature(app: T.App, cid: T.CreatureID): T.Creature | undefined {
  return app.current_game.creatures.get(cid);
}

export function useCreature(cid: T.CreatureID) { }

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
      useApp.getState().refreshGame(result.result[0]);
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
  useApp.getState().refresh(app);
  useGrid.getState().reset();
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


type ThunkAction<R> = (dispatch: Dispatch, getState: () => PTUI, extraArgument: undefined) => R;


export interface DispatchProps { dispatch: Dispatch; }
export interface ReduxProps extends DispatchProps { ptui: PTUI; }

export function connectRedux<BaseProps extends {} & object>(
  x: React.ComponentType<BaseProps & ReduxProps>
): React.ComponentType<BaseProps> {
  return x;
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
