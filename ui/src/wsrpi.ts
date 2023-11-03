import * as Z from "zod";

import * as A from "./Actions";
import { sendRequest } from "./Actions";
import { RPIGameRequest } from "./bindings/bindings";
import * as M from "./Model";
import * as T from "./PTTypes";

export const WEBSOCKETS_ENABLED = typeof import.meta.env.VITE_WEBSOCKET_URL !== "undefined";

// I considered storing these in Zustand, but you will *never* use a selector hook to access them,
// so really, so I think global variables are the best representation.
const _requests: Record<string, (o: any) => void> = {};
let webSocket: WebSocket | undefined;
let timeout: NodeJS.Timeout;
const IDLE_TIMEOUT = 10 * 60;

export function sendWSRequest<T>(request: RPIGameRequest, parser: T.Decoder<T>): Promise<T> {
  let id = crypto.randomUUID();
  let requestWithId = { id, request: T.encodeRPIGameRequest(request) };
  if (!webSocket) {
    throw new Error("Not connected to a websocket!");
  }
  webSocket.send(JSON.stringify(requestWithId));
  return new Promise((res, rej) => {
    _requests[id] = data => {
      console.log("handler invoked with", data);
      if ("error" in data) {
        rej(data.error);
      } else {
        res(parser.parse(data.payload));
      }
    };
  });
}

export function connect(gameId: string, mode: T.Role) {
  console.log("[CONNECT]", gameId);
  M.getState().setGameId(gameId);

  let intentionalStop = false;

  // This connect() function needs to synchronously return a callback that can be used to clean up
  // the connection, so we do all of our async work in this inner function.
  async function theAsyncFunction() {
    // We need to request a websocket token, which allows the server to validate our credentials.
    let { token } = await A.ptfetch(
      `/request-websocket/${gameId}/${mode}`,
      {},
      Z.object({ token: Z.string() }),
    );
    webSocket = new WebSocket(import.meta.env.VITE_WEBSOCKET_URL + `/ws/${gameId}/${token}`);
    if (window) (window as any).arpeggioSocket = webSocket;

    webSocket.addEventListener("open", async (event) => {
      resetTimeout();
      M.getState().setSocketStatus("open");
      console.log("connected to WebSocket. initiating GetGame");
      const { logs, game, metadata } = await sendRequest(
        { t: "GMGetGame" },
        Z.object({ logs: T.decodeGameLogs, game: T.decodeGame, metadata: T.decodeGameMetadata }),
      );
      console.log("got the game.", game);
      M.getState().refresh(game);
      M.getState().setRecentLogs(logs);
      M.getState().setGameName(metadata.name);
    });

    webSocket.addEventListener("message", (event) => {
      resetTimeout();
      console.log("Message from server ", event);
      handleWSEvent(event);
    });
    webSocket.addEventListener("close", event => {
      console.log("WebSocket is closed.");
      M.getState().setSocketStatus(intentionalStop ? "unconnected" : "closed");
    });
  }

  theAsyncFunction();

  return () => {
    console.log("Cleaning up WebSocket");
    const state = M.getState();
    state.setGameName(undefined);
    state.setRecentLogs([]);
    const ws = webSocket;
    webSocket = undefined;
    ws?.close();
    intentionalStop = true;
  };
}

function resetTimeout() {
  clearTimeout(timeout);
  timeout = setTimeout(() => {
    console.log("Idle timeout, shutting down webSocket", webSocket);
    webSocket?.close();
    console.log("what's the state?", webSocket?.readyState);
  }, IDLE_TIMEOUT * 1000);
}

function handleWSEvent(event: MessageEvent<string>) {
  const parsed = JSON.parse(event.data);
  if ("id" in parsed) {
    const id = parsed["id"];
    const handler = _requests[id];
    if (handler) {
      delete _requests[id];
      handler(parsed);
    }
  } else {
    // Handle server-sent events
    if ("t" in parsed && parsed["t"] === "refresh_game") {
      let decoder = Z.object({ game: T.decodeGame, logs: T.decodeGameLogs });
      let { game, logs } = decoder.parse(parsed);
      M.getState().refresh(game);
      M.getState().addLogs(logs);
    } else {
      console.info("Got an unexpected message from the server:", parsed);
    }
  }
}
