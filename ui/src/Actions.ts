// Actions.

// All of these should ONLY be called in "imperative" code, i.e. event handlers,
// never while rendering components.

import * as Z from "zod";

import { assertNever, getState } from "./Model";
import * as T from "./PTTypes";
import { sendWSRequest, WEBSOCKETS_ENABLED } from "./wsrpi";

export const RPI_URL = import.meta.env.VITE_RPI_URL;
if (!RPI_URL) console.error("No VITE_RPI_URL was defined!!!");

export async function decodeFetch<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: (json: object) => J,
): Promise<J> {
  url = `${RPI_URL}${url}`;
  const result = await fetch(url, init);
  if (result.status === 401) {
    getState().setUserToken("");
    throw new Http401Error("401 from PT RPI");
  }
  const json = await result.json();
  try {
    return decoder(json);
  } catch (e) {
    console.error(e);
    throw { _pt_error: "JSON", original: e };
  }
}

class Http401Error extends Error {
  name: string;
  constructor(message: string) {
    super(message);
    this.name = "Http401Error";
  }
}

async function ptfetch_<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: (json: object) => J,
): Promise<J> {
  if (!init) {
    init = {};
  }
  const idToken = getState().userToken;
  if (idToken) {
    init.headers = { ...init.headers, "x-arpeggio-auth": idToken };
  }
  try {
    const json = await decodeFetch(url, init, decoder);
    return json;
  } catch (e) {
    if (!isAbortError(e) && !isHttp401Error(e)) {
      const error = extract_error_details(e);
      getState().setError(error);
    }
    throw e;
  }

  function extract_error_details(error: any): string {
    if (error._pt_error) {
      switch (error._pt_error) {
        case "JSON":
          return `Failed to decode JSON ${error.original}`;
        case "RPI":
          return `Error received from server ${error.message}`;
        default:
          return `Unknown error ${error.toString()}`;
      }
    } else {
      return `Unknown error ${error.toString()}`;
    }
  }
}

export function ptfetch<J>(
  url: string,
  init: RequestInit | undefined,
  decoder: { parse: (json: object) => J },
): Promise<J> {
  return ptfetch_(url, init, decoder.parse);
}

export function startPoll(mode: T.Role, gameId: string): () => void {
  let polling = true;

  const controller = new AbortController();
  const signal = controller.signal;

  async function poll() {
    // - first, fetch the entire Game
    // - then, start long-polling at /poll/{snapidx}/{logidx}
    //
    // Why not just start long-polling at `/poll/0/0`? Because if the server has a freshly loaded
    // game, it will be at index 0/0, and so won't return immediately when you poll 0/0.
    const gameUrl = `/g/${gameId}/${mode.toLowerCase}/`;
    let result = await ptfetch(gameUrl, {}, T.decodeGameWithMetadata);
    let state = getState();
    state.refresh(result.game);
    state.setGameId(gameId);
    state.setGameName(result.metadata.name);

    let { index } = result;

    while (polling) {
      const url = `${gameUrl}poll/${index.game_idx}/${index.log_idx}`;
      try {
        console.log("gonna fetch");
        let result = await ptfetch(url, { signal }, T.decodeGameWithMetadata);
        index = result.index;
        getState().refresh(result.game);
      } catch (e) {
        if (isAbortError(e)) {
          console.log("Request was cancelled intentionally");
        } else {
          console.error("oops got an error", e);
          getState().setFetchStatus("Error");
          await new Promise(res => setTimeout(res, 5000));
        }
      }
    }
    console.log("Stopped polling because polling was set to false.");
    state = getState();
    state.setGameId(undefined);
    state.setGameName(undefined);
  }
  poll();
  return () => {
    polling = false;
    console.log("requested cancellation of poll");
    controller.abort();
  };
}

function isAbortError(error: any): boolean {
  return getattr(error, "name") === "AbortError";
}

function isHttp401Error(error: any): boolean {
  return getattr(error, "name") === "Http401Error";
}

function getattr(o: any, name: string): any {
  return o && typeof o === "object" && name in o && o[name];
}

export async function requestMove(creature_id: T.CreatureID) {
  const scene = getState().getFocusedScene();
  if (scene) {
    let scene_id = scene.id;
    const result = await sendRequest(
      { t: "MovementOptions", scene_id, creature_id },
      T.arrayOfPoint3,
    );
    getState().displayMovementOptions(result, creature_id);
  } else {
    console.error("requestMove: No scene!");
  }
}

export function moveCreature(creature_id: T.CreatureID, destination: T.Point3) {
  getState().clearMovementOptions();
  if (getState().playerId) {
    sendPlayerCommand({ t: "PathCreature", creature_id: creature_id, destination });
  } else {
    const scene = getState().getFocusedScene();
    if (scene) {
      sendGMCommand({
        t: "PathCreature",
        scene_id: scene.id,
        creature_id: creature_id,
        destination,
      });
    } else {
      throw new Error(`Tried moving when there is no scene`);
    }
  }
}

export function setCreaturePos(creature_id: T.CreatureID, pos: T.Point3) {
  getState().clearMovementOptions();
  const scene = getState().getFocusedScene();
  if (scene) {
    sendGMCommand({ t: "SetCreaturePos", scene_id: scene.id, creature_id: creature_id, pos });
  }
}

export function moveCombatCreature(destination: T.Point3) {
  getState().clearMovementOptions();
  sendGMCommand({ t: "PathCurrentCombatCreature", destination });
}

export async function requestCombatMovement() {
  const options = await sendRequest({ t: "CombatMovementOptions" }, T.arrayOfPoint3);
  getState().displayMovementOptions(options);
}

/* Execute an ability that has already been selected, with a target.
 * This relies on the state being set up ahead of time: we must have a target_options already.
 */
export async function executeCombatAbility(target_id: T.CreatureID) {
  const opts = getState().grid.target_options;
  if (!opts) throw new Error(`Can't execute an ability if we haven't selected it first.`);
  const { ability_id, options } = opts;
  if (!("CreatureIDs" in options)) throw new Error(`Only support CreatureIDs for now`);
  const target: T.DecidedTarget = { Creature: target_id };
  sendGMCommand({ t: "CombatAct", ability_id, target });
  getState().clearPotentialTargets();
}

export function executeCombatPointTargetedAbility(point: T.Point3) {
  const opts = getState().grid.target_options;
  if (!opts) throw new Error(`Can't execute an ability if we haven't selected it first.`);
  const { ability_id, options } = opts;
  if (!("Points" in options)) {
    throw new Error(`This function only works for abilities that use Points`);
  }
  const target: T.DecidedTarget = { Point: point };
  sendGMCommand({ t: "CombatAct", ability_id, target });
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
  let gameId = getState().gameId;
  if (!gameId) {
    throw new Error("Sorry, I lost the gameId somehow!?");
  }
  // this should probably be explicitly passed to this function?
  let mode = getState().playerId === undefined ? "gm" : "player";
  return `/g/${gameId}/${mode}`;
}

export function sendRequest<T>(request: T.RPIGameRequest, decoder: T.Decoder<T>): Promise<T> {
  if (WEBSOCKETS_ENABLED) {
    return sendWSRequest(request, decoder);
  } else {
    switch (request.t) {
      case "GMGetGame":
        return get("/");
      case "GMCommand":
        return execute();
      case "PlayerCommand":
        return execute();
      case "MovementOptions":
        return get(`/movement_options/${request.scene_id}/${request.creature_id}`);
      case "CombatMovementOptions":
        return get(`/combat_movement_options`);
      case "TargetOptions":
        return get(
          `/target_options/${request.scene_id}/${request.creature_id}/${request.ability_id}`,
        );
      case "PreviewVolumeTargets":
        return get(
          `/preview_volume_targets/${request.scene_id}/${request.creature_id}/${request.ability_id}/${request.point.x}/${request.point.y}/${request.point.z}`,
        );
      default:
        throw new Error("I don't think I care about this any more");
    }
  }

  function get(suffix: string) {
    return ptfetch(`${gameUrl()}${suffix}`, {}, decoder);
  }

  function execute() {
    const encoded = T.encodeRPIGameRequest(request);
    if (!("command" in encoded)) throw new Error("This isn't a command");
    const command = encoded.command;
    const body = JSON.stringify(command);
    return ptfetch(
      `${gameUrl()}/execute`,
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body,
      },
      decoder,
    );
  }
}

export async function sendPlayerCommand(cmd: T.PlayerCommand) {
  console.log("[sendPlayerCommand:JSON]", cmd);
  const json = T.encodePlayerCommand(cmd);
  const result = await sendRequest(
    { t: "PlayerCommand", command: cmd },
    T.decodeRustResult(Z.array(T.decodeGameLog), Z.string()),
  );

  switch (result.t) {
    case "Ok":
      // Theoretically we could do something with the returned GameLogs, but we don't need to
      // because the poll (or websocket) is going to refresh the state of the game instantly anyway.
      return;
    case "Err":
      getState().setError(result.error);
      throw { _pt_error: "RPI", message: result.error };
  }
}

export async function sendGMCommand(cmd: T.GMCommand): Promise<T.GameLog[]> {
  console.log("[sendGMCommand:JSON]", cmd);
  const result = await sendRequest(
    { t: "GMCommand", command: cmd },
    T.decodeRustResult(Z.array(T.decodeGameLog), Z.string()),
  );

  switch (result.t) {
    case "Ok":
      // Theoretically we could do something with the returned GameLogs, but we don't need to
      // because the poll (or websocket) is going to refresh the state of the game instantly anyway.
      return result.result;
    case "Err":
      throw { _pt_error: "RPI", message: result.error };
  }
}

export function sendGMCommands(cmds: Array<T.GMCommand>) {
  // TODO: I guess we could add an endpoint that handles multiple commands at once
  for (const cmd of cmds) {
    sendGMCommand(cmd);
  }
}

/// Send a Command and *don't* automatically handle errors, but instead return a future
/// representing the result. This is useful for code which wants to send a command and interpret
/// the resulting gamelogs.
export async function sendGMCommandWithResult(
  cmd: T.GMCommand,
): Promise<T.RustResult<Array<T.GameLog>, string>> {
  const json = T.encodeGMCommand(cmd);
  console.log("[sendGMCommand:JSON]", json);
  const rpi_result = decodeFetch(
    "/",
    {
      method: "POST",
      body: JSON.stringify(json),
      headers: { "content-type": "application/json" },
    },
    T.decodeRustResult(
      T.decodeChangedGame.transform(cg => cg.logs),
      Z.string(),
    ).parse,
  );
  return rpi_result;
}

async function selectAbility(
  scene_id: T.SceneID,
  creature_id: T.CreatureID,
  ability_id: T.AbilityID,
) {
  let options = await sendRequest(
    { t: "TargetOptions", scene_id, creature_id, ability_id },
    T.decodePotentialTargets,
  );
  getState().displayPotentialTargets(creature_id, ability_id, options);
}

export function requestCombatAbility(
  cid: T.CreatureID,
  ability_id: T.AbilityID,
  ability: T.Ability,
  scene_id: T.SceneID,
) {
  if ("Creature" in ability.action && ability.action.Creature.target === "Actor") {
    return sendGMCommand({ t: "CombatAct", ability_id, target: "Actor" });
  } else {
    return selectAbility(scene_id, cid, ability_id);
  }
}

export async function fetchAbilityTargets(
  scene_id: T.SceneID,
  creature_id: T.CreatureID,
  ability_id: T.AbilityID,
  point: T.Point3,
): Promise<{ points: Array<T.Point3>; creatures: Array<T.CreatureID> }> {
  const result = await sendRequest({
    t: "PreviewVolumeTargets",
    scene_id,
    creature_id,
    ability_id,
    point,
  }, Z.tuple([Z.array(Z.string()), Z.array(T.decodePoint3)]));
  return {
    creatures: result[0],
    points: result[1],
  };
}

export async function createGame(name: string): Promise<T.GameID> {
  const result = await ptfetch("/g/create", {
    method: "POST",
    body: JSON.stringify(name),
    headers: { "content-type": "application/json" },
  }, Z.object({ game_id: Z.string() }));
  return result.game_id;
}

export async function invite(): Promise<T.InvitationID> {
  const gameId = getState().gameId;
  if (!gameId) throw new Error("Must be called in context of a game!");
  return await sendRequest({ t: "GMGenerateInvitation" }, Z.string());
}
