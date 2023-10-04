// Actions.

// All of these should ONLY be called in "imperative" code, i.e. event handlers,
// never while rendering components.

import * as Z from "zod";

import * as T from "./PTTypes";
import { getState } from "./Model";
import { getCookie } from "react-use-cookie";

export const RPI_URL = import.meta.env.VITE_RPI_URL;
if (!RPI_URL) { console.error("No VITE_RPI_URL was defined!!!"); }


export async function decodeFetch<J>(
  url: string, init: RequestInit | undefined,
  decoder: (json: object) => J
): Promise<J> {
  const result = await fetch(url, init);
  const json = await result.json();
  try {
    return decoder(json);
  } catch (e) {
    console.error(e);
    throw { _pt_error: "JSON", original: e };
  }
}

export async function ptfetch_<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: (json: object) => J
): Promise<J> {
  if (!init) {
    init = {};
  }
  init.credentials = 'include';
  const ptIdToken = getCookie("pt-id-token");
  if (ptIdToken)
    init.headers = {'authorization': ptIdToken};
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

function ptfetch<J>(url: string, init: RequestInit | undefined, decoder: { parse: (json: object) => J }): Promise<J> {
  return ptfetch_(url, init, decoder.parse);
}

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

export async function requestMove(cid: T.CreatureID) {
  const scene = getState().getFocusedScene();
  if (scene) {
    const result = await ptfetch(
      `${RPI_URL}/movement_options/${scene.id}/${cid}`,
      undefined,
      T.arrayOfPoint3,
    );
    getState().displayMovementOptions(result, cid);
  }
}

export function moveCreature(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getState().getFocusedScene();
  if (scene) {
    sendCommand({ PathCreature: [scene.id, creature_id, dest] });
  } else {
    throw new Error(`Tried moving when there is no scene`);
  }
}

export function setCreaturePos(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getState().getFocusedScene();
  if (scene) {
    sendCommand({ SetCreaturePos: [scene.id, creature_id, dest] });
  }
}

export function moveCombatCreature(dest: T.Point3) {
  getState().clearMovementOptions();
  sendCommand({ PathCurrentCombatCreature: dest });
}

export async function requestCombatMovement() {
  const options = await ptfetch(
    RPI_URL + "/combat_movement_options", undefined,
    T.arrayOfPoint3);
  getState().displayMovementOptions(options);
}


/* Execute an ability that has already been selected, with a target.
 * This relies on the state being set up ahead of time: we must have a target_options already.
 */
export async function executeCombatAbility(target_id: T.CreatureID) {
  const opts = getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (!("CreatureIDs" in options)) { throw new Error(`Only support CreatureIDs for now`); }
  const target: T.DecidedTarget = { Creature: target_id };
  sendCommand({ CombatAct: [ability_id, target] });
  getState().clearPotentialTargets();
}

export function executeCombatPointTargetedAbility(point: T.Point3) {
  const opts = getState().grid.target_options;
  if (!opts) { throw new Error(`Can't execute an ability if we haven't selected it first.`); }
  const { ability_id, options } = opts;
  if (!("Points" in options)) {
    throw new Error(`This function only works for abilities that use Points`);
  }
  const target: T.DecidedTarget = { Point: point };
  sendCommand({ CombatAct: [ability_id, target] });
  getState().clearPotentialTargets();
}

export async function loadGame(source: T.ModuleSource, name: string): Promise<void> {
  const url = source === "SavedGame"
    ? `${RPI_URL}/saved_games/user/${name}/load`
    : `${RPI_URL}/saved_games/module/${name}/load`;

  const app = await ptfetch(url, { method: 'POST' }, T.decodeApp);
  resetApp(app);
}

export async function loadModule(opts: { path: T.FolderPath, name: string, source: T.ModuleSource }): Promise<void> {
  const source = { "Module": "module", "SavedGame": "user" }[opts.source];
  const url = `${RPI_URL}/saved_games/${source}/${opts.name}/load_into?path=${T.encodeFolderPath(opts.path)}`;
  await ptfetch(url, { method: "POST" }, Z.any());
}

export async function fetchSavedGames(): Promise<{ games: Array<string>, modules: Array<string> }> {
  const url = `${RPI_URL}/saved_games`;
  const [modules, games] = await ptfetch(url, {}, Z.tuple([Z.array(Z.string()), Z.array(Z.string())]));
  return { games, modules };
}

export async function saveGame(game: string): Promise<undefined> {
  const url = `${RPI_URL}/saved_games/user/${game}`;
  await ptfetch(url, { method: 'POST' }, Z.any());
}

export async function exportModule(path: T.FolderPath, name: string): Promise<undefined> {
  const url = `${RPI_URL}/modules/${name}`;
  const opts = {
    method: 'POST',
    body: JSON.stringify(T.encodeFolderPath(path)),
    headers: { "content-type": "application/json" },
  };
  await ptfetch(url, opts, Z.any());
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
    T.decodeRustResult(T.decodeSendCommandResult, Z.string())
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
      T.decodeSendCommandResult.transform(([_, logs]) => logs),
      Z.string()
    ).parse
  );
  return rpi_result;
}


export async function newGame() {
  const app = await ptfetch(`${RPI_URL}/new_game`, { method: "POST" }, T.decodeApp);
  resetApp(app);
}

function resetApp(app: T.App) {
  getState().refresh(app);
  getState().resetGrid();
}

async function selectAbility(scene_id: T.SceneID, cid: T.CreatureID, ability_id: T.AbilityID) {
  const url = `${RPI_URL}/target_options/${scene_id}/${cid}/${ability_id}`;
  const options = await ptfetch(url, undefined, T.decodePotentialTargets);
  getState().displayPotentialTargets(cid, ability_id, options);
}

export function requestCombatAbility(
  cid: T.CreatureID, ability_id: T.AbilityID, ability: T.Ability, scene_id: T.SceneID
) {
  if ("Creature" in ability.action && "Actor" in ability.action.Creature) {
    return sendCommand({ CombatAct: [ability_id, "Actor"] });
  } else {
    return selectAbility(scene_id, cid, ability_id);
  }
}

export async function fetchAbilityTargets(
  sceneId: T.SceneID, actorId: T.CreatureID, abilityId: T.AbilityID, point: T.Point3
): Promise<{ points: Array<T.Point3>; creatures: Array<T.CreatureID> }> {
  const url = `${RPI_URL}/preview_volume_targets/${sceneId}/${actorId}/${abilityId}/${point.x}/${point.y}/${point.z}`;
  const result = await ptfetch(url, { method: 'POST' }, Z.tuple([Z.array(Z.string()), Z.array(T.decodePoint3)]));
  return {
    creatures: result[0],
    points: result[1],
  };
}

export async function newGetGame(gameId: string): Promise<T.Game> {
  const url = `${RPI_URL}/g/${gameId}/gm/`;
  return await ptfetch(url, {method: 'GET'}, T.decodeGame);
}
