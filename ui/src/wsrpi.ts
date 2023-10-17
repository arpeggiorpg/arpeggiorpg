
import * as M from "./Model";
import * as T from "./PTTypes";

export const WEBSOCKETS_ENABLED = typeof import.meta.env.VITE_WEBSOCKET_URL !== "undefined"

// TODO: BIND
export type WSCommand =
  | { t: "GetGame" }
  // GMCommand's command is a *post-serialization* GMCommand, not a T.GMCommand. This is confusing.
  // :-(
  | { t: "GMCommand", command: string | object}
  ;

// I considered storing these in Zustand, but you will *never* use a selector hook to access them,
// so really, so I think global variables are the best representation.
const _requests: Record<string, (o: any) => void> = {};
let webSocket: WebSocket | undefined;

export function sendWSRequest<T>(command: WSCommand, parser: T.Decoder<T>): Promise<T> {
  let id = crypto.randomUUID();
  let commandWithId = { id, command };
  if (!webSocket) {
    throw new Error("Not connected to a websocket!");
  }
  webSocket.send(JSON.stringify(commandWithId));
  return new Promise((res, rej) => {
    _requests[id] = data => {
      console.log("handler invoked with", data);
      res(parser.parse(data.payload));
    }
  });

}


export function connect(gameId: string) {
  M.getState().setGameId(gameId);
  webSocket = new WebSocket(import.meta.env.VITE_WEBSOCKET_URL + `/game/${gameId}`);

  webSocket.addEventListener("open", async (event) => {
    console.log("connected to WebSocket. initiating GetGame");
    const game = await sendWSRequest({ t: "GetGame" }, T.decodeGame);
    console.log("got the game.", game);
    M.getState().refresh(game);
  });

  webSocket.addEventListener("message", (event) => {
    console.log("Message from server ", event);
    handleWSEvent(event);
  });

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
    // Handle server-sent events like GameUpdate
    console.info("Got an unexpected message from the server:", parsed)
  }
}
