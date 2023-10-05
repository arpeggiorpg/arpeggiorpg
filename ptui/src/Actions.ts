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
  url = `${RPI_URL}${url}`;
  const result = await fetch(url, init);
  const json = await result.json();
  try {
    return decoder(json);
  } catch (e) {
    console.error(e);
    throw { _pt_error: "JSON", original: e };
  }
}

async function ptfetch_<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: (json: object) => J
): Promise<J> {
  if (!init) {
    init = {};
  }
  const ptIdToken = getCookie("pt-id-token");
  if (ptIdToken)
    init.headers = { ...init.headers, 'x-pt-rpi-auth': ptIdToken };
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

export function ptfetch<J>(url: string, init: RequestInit | undefined, decoder: { parse: (json: object) => J }): Promise<J> {
  return ptfetch_(url, init, decoder.parse);
}

export async function startPoll(mode: "gm" | "player", gameId: string) {
  // This is kinda dumb, but:
  // - first, we fetch the entire Game
  // - then, we start long-polling at /poll/{snapidx}/{logidx}
  //
  // Why not just start long-polling at `/poll/0/0`? Because if the server has a freshly loaded
  // game, it will be at index 0/0, and so won't return immediately when you poll 0/0.

  // TODO: we need a way to cancel startPoll so that if the user navigates away from their current
  // game, we stop polling it.
  const gameUrl = `/g/${gameId}/${mode}/`;
  let result = await ptfetch(gameUrl, undefined, T.decodeGameWithIndex);
  getState().refresh(result.game);
  getState().setGameId(gameId);

  let { index } = result;

  while (true) {
    const url = `${gameUrl}poll/${index.game_idx}/${index.log_idx}`;
    try {
      console.log("gonna fetch");
      let result = await ptfetch(url, {}, T.decodeGameWithIndex);
      index = result.index;
      getState().refresh(result.game);
    } catch (e) {
      console.error("oops got an error", e);
      getState().setFetchStatus("Error");
      await new Promise(res => setTimeout(res, 5000));
    }
  }
}

export async function requestMove(cid: T.CreatureID) {
  const scene = getState().getFocusedScene();
  if (scene) {
    const result = await ptfetch(
      `${gameUrl()}/movement_options/${scene.id}/${cid}`,
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
  const options = await ptfetch(`${gameUrl()}/combat_movement_options`, undefined, T.arrayOfPoint3);
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

// export async function loadModule(opts: { path: T.FolderPath, name: string, source: T.ModuleSource }): Promise<void> {
//   const source = { "Module": "module", "SavedGame": "user" }[opts.source];
//   const url = `/saved_games/${source}/${opts.name}/load_into?path=${T.encodeFolderPath(opts.path)}`;
//   await ptfetch(url, { method: "POST" }, Z.any());
// }

// export async function exportModule(path: T.FolderPath, name: string): Promise<undefined> {
//   const url = `/modules/${name}`;
//   const opts = {
//     method: 'POST',
//     body: JSON.stringify(T.encodeFolderPath(path)),
//     headers: { "content-type": "application/json" },
//   };
//   await ptfetch(url, opts, Z.any());
// }

function gameUrl() {
  let { gameId } = getState();
  let mode = getState().playerId === undefined ? "gm" : "player";
  return `/g/${gameId}/${mode}`;
}

export async function sendCommand(cmd: T.GameCommand) {
  const json = T.encodeGameCommand(cmd);
  console.log("[sendCommand:JSON]", json);

  const result = await ptfetch(
    `${gameUrl()}/execute`,
    {
      method: "POST",
      body: JSON.stringify(json),
      headers: { "content-type": "application/json" },
    },
    T.decodeRustResult(T.decodeChangedGame, Z.string())
  );
  switch (result.t) {
    case "Ok":
      getState().refresh(result.result.game);
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
    "/",
    {
      method: "POST",
      body: JSON.stringify(json),
      headers: { "content-type": "application/json" },
    },
    T.decodeRustResult(
      T.decodeChangedGame.transform(cg => cg.logs),
      Z.string()
    ).parse
  );
  return rpi_result;
}

async function selectAbility(scene_id: T.SceneID, cid: T.CreatureID, ability_id: T.AbilityID) {
  const url = `${gameUrl()}/target_options/${scene_id}/${cid}/${ability_id}`;
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
  const url = `${gameUrl()}/preview_volume_targets/${sceneId}/${actorId}/${abilityId}/${point.x}/${point.y}/${point.z}`;
  const result = await ptfetch(url, { method: 'POST' }, Z.tuple([Z.array(Z.string()), Z.array(T.decodePoint3)]));
  return {
    creatures: result[0],
    points: result[1],
  };
}

export async function createGame(name: string): Promise<T.GameID> {
  const result = await ptfetch('/g/create', {
    method: "POST",
    body: JSON.stringify(name),
    headers: { "content-type": "application/json" },
  }, Z.object({ game_id: Z.string() }));
  return result.game_id;
}
