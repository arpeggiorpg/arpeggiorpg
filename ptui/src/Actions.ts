// Actions.

// All of these should ONLY be called in "imperative" code, i.e. event handlers,
// never while rendering components.

import * as JD from "type-safe-json-decoder";

import * as T from "./PTTypes";
import {getState} from "./Model";


export const RPI_URL = import.meta.env.VITE_RPI_URL;

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
      JD.array(T.decodePoint3)
    );
    getState().displayMovementOptions(result, cid);
  }
}

export function moveCreature(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getState().getFocusedScene();
  if (scene) {
    sendCommand({ t: "PathCreature", scene_id: scene.id, creature_id, dest });
  } else {
    throw new Error(`Tried moving when there is no scene`);
  }
}

export function setCreaturePos(creature_id: T.CreatureID, dest: T.Point3) {
  getState().clearMovementOptions();
  const scene = getState().getFocusedScene();
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
