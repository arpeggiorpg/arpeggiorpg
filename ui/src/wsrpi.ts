import * as Z from "zod";

import * as A from "./Actions";
import * as M from "./Model";
import * as T from "./PTTypes";
import { RPIGameRequest } from "./bindings/bindings";
import { sendRequest } from "./Actions";

export const WEBSOCKETS_ENABLED = typeof import.meta.env.VITE_WEBSOCKET_URL !== "undefined"

// I considered storing these in Zustand, but you will *never* use a selector hook to access them,
// so really, so I think global variables are the best representation.
const _requests: Record<string, (o: any) => void> = {};
let webSocket: WebSocket | undefined;

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
      res(parser.parse(data.payload));
    }
  });

}


export function connect(gameId: string) {
  console.log("[CONNECT]", gameId);
  M.getState().setGameId(gameId);

  async function theAsyncFunction() {
    // First, we need to request a websocket token.
    let {token} = await A.ptfetch(`/request-websocket/${gameId}`, {}, Z.object({token: Z.string()}));
    webSocket = new WebSocket(import.meta.env.VITE_WEBSOCKET_URL + `/ws/${token}`);

    webSocket.addEventListener("open", async (event) => {
      console.log("connected to WebSocket. initiating GetGame");
      const game = await sendRequest({ t: "GMGetGame" }, T.decodeGame);
      console.log("got the game.", game);
      M.getState().refresh(game);
    });

    webSocket.addEventListener("message", (event) => {
      console.log("Message from server ", event);
      handleWSEvent(event);
    });
  }

  theAsyncFunction();

  return () => {
    console.log("Cleaning up WebSocket");
    const ws = webSocket;
    webSocket = undefined;
    ws?.close();
  }

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
    if ("t" in parsed && parsed["t"] === "refresh_game" && "game" in parsed) {
      const game = T.decodeGame.parse(parsed["game"]);
      M.getState().refresh(game);
    } else {
      console.info("Got an unexpected message from the server:", parsed)
    }
  }
}
