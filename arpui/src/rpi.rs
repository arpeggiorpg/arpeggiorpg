use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::format_err;
use dioxus::prelude::*;
use futures::channel::oneshot::{self, Sender};
use futures_util::{stream::StreamExt, SinkExt, TryStreamExt};
use log::info;
use reqwest_websocket::{Message, RequestBuilderExt, WebSocket};
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use arptypes::multitenant::{self, RPIGameRequest, Role};

pub static AUTH_TOKEN: GlobalSignal<String> = Signal::global(String::new);

pub fn auth_token() -> String {
  wasm_cookies::get("arpeggio-token").unwrap_or(Ok(String::new())).unwrap_or(String::new())
}

fn rpi_url() -> String {
  let window = web_sys::window().expect("global window doesn't exist");
  let document = window.document().expect("document must exist");
  let meta = document
    .query_selector("meta[name='RPI_URL']")
    .expect("meta RPI_URL tag must exist in index.html (1)")
    .expect("meta RPI_URL tag must exist in index.html (2)");
  meta.get_attribute("content").expect("meta RPI_URL tag must have content")
}

pub async fn list_games() -> Result<multitenant::GameList, anyhow::Error> {
  rpi_get("g/list").await
}

async fn connect_coroutine(role: Role, game_id: Uuid) -> anyhow::Result<WebSocket> {
  let response =
    rpi_get::<serde_json::Value>(&format!("request-websocket/{game_id}/{role}")).await?;
  let token = response
    .get("token")
    .ok_or(format_err!("No token found in request-websocket response"))?
    .as_str()
    .ok_or(format_err!("token wasn't a string"))?;
  info!("got websocket token! {token}");

  let ws_url = format!("ws://localhost:8787/ws/{game_id}/{token}");
  let response = reqwest::Client::default().get(ws_url).upgrade().send().await?;
  let websocket = response.into_websocket().await?;
  Ok(websocket)
}

#[component]
pub fn Connector(role: Role, game_id: Uuid, children: Element) -> Element {
  // let connection_count = use_signal(|| 0);
  let mut error = use_signal(|| None);
  let _coro = use_coroutine(|mut rx: UnboundedReceiver<UIRequest>| async move {
    let response_handlers: HashMap<uuid::Uuid, Sender<anyhow::Result<serde_json::Value>>> =
      HashMap::new();
    let response_handlers = Rc::new(RefCell::new(response_handlers));
    let websocket = match connect_coroutine(role, game_id).await {
      Ok(ws) => ws,
      Err(e) => {
        error.set(Some(e.to_string()));
        return;
      }
    };

    let (mut websocket_tx, mut websocket_rx) = websocket.split();
    let receiver_response_handlers = response_handlers.clone();
    spawn_local(async move {
      while let Some(message) = websocket_rx.try_next().await.expect("websocket.try_next") {
        match message {
          Message::Text(text) => {
            info!("WS Text Message: {text}");
            let json: serde_json::Value = serde_json::from_str(&text).expect("parse JSON");
            if let Some(id_val) = json.as_object().expect("json must be an object").get("id") {
              let id: uuid::Uuid = id_val
                .as_str()
                .expect("id must be a string")
                .parse()
                .expect("parse UUID in websocket response");
              // TODO: handle "error" in response
              if let Some(handler) = receiver_response_handlers.borrow_mut().remove(&id) {
                let payload =
                  json.get("payload").expect("any response with an id must also have a payload");
                handler.send(Ok(payload.clone())).expect("couldn't send result to one-shot");
              }
            }
            // TODO: else! Handle server-sent events (like refresh_game)
          }
          Message::Binary(vecu8) => info!("WS Binary Message: {vecu8:?}"),
        }
      }
    });

    // TODO RADIX: figure out what I'm doing here. Should I send the game here? How am I
    // communicating it to the outside world? (well, I guess obviously a GlobalSignal...)

    // I think it would be pretty cool to split out WSConnector as something that is 100% unrelated
    // to Arpeggio to show as an example of how to integrate Dioxus and reqwest-websocket.
    // This would require to parameterize the type of the Coroutine and also parameterize the
    // handler for general non-response events.
    // But I don't really care about that for now. Let's just get this working!

    // let get_game_json = serde_json::json!({
    //   "id": uuid::Uuid::new_v4().to_string(),
    //   "request": {
    //     "t": "GMGetGame"
    //   }
    // });
    // let get_game_json = serde_json::to_string(&get_game_json)?;
    // websocket.send(Message::Text(get_game_json)).await?;

    while let Some(ui_req) = rx.next().await {
      let request_id = uuid::Uuid::new_v4();
      let request = serde_json::json!({
        "id": request_id.to_string(),
        "request": &ui_req.game_request
      });
      let cmd_json =
        serde_json::to_string(&request).expect("must be able to serialize RPIGameRequests");
      if let Some(callback) = ui_req.callback {
        response_handlers.borrow_mut().insert(request_id, callback);
      }
      websocket_tx.send(Message::Text(cmd_json)).await.expect("Couldn't send command to websocket");
    }
  });
  if let Some(error) = error() {
    rsx! { "Coroutine Error! {error}"}
  } else {
    children
  }
}

pub struct UIRequest {
  game_request: RPIGameRequest,
  callback: Option<Sender<anyhow::Result<serde_json::Value>>>,
}

pub fn use_ws() -> Coroutine<UIRequest> { use_coroutine_handle::<UIRequest>() }

pub async fn send_request(
  req: RPIGameRequest, coro: Coroutine<UIRequest>,
) -> anyhow::Result<serde_json::Value> {
  let (sender, receiver) = oneshot::channel::<anyhow::Result<serde_json::Value>>();
  let ui_req = UIRequest { game_request: req, callback: Some(sender) };
  coro.send(ui_req);
  receiver.await?
}

async fn rpi_get<T: serde::de::DeserializeOwned>(path: &str) -> Result<T, anyhow::Error> {
  let rpi_url = rpi_url();
  let url = format!("{rpi_url}/{path}");
  let client = reqwest::Client::new();
  let response = client.get(url).header("x-arpeggio-auth", AUTH_TOKEN()).send().await?;
  response.json().await.map_err(|e| e.into())
}

// export default function Connector(props: { role: T.Role } & React.PropsWithChildren) {
//   const [connectionCount, setConnectionCount] = React.useState(0);

//   React.useEffect(() => {
//     // connect returns a cancellation function, which we return here from the effect so react will
//     // call it when this component gets unmounted.
//     return WS.connect(gameId, props.role);
//   }, [gameId, connectionCount]);

//   const status = M.useState((s) => s.socketStatus);
//   const reconnect = () => setConnectionCount(connectionCount + 1);

//   return (
//     <>
//       {status === "closed"
//         ? (
//           <Modal open={true}>
//             <Modal.Header>Connection is inactive</Modal.Header>
//             <Modal.Content>
//               Connection is inactive (most likely due to idle timeout; I'm just trying to avoid
//               running up a big server bill!)
//             </Modal.Content>
//             <Modal.Actions>
//               <button onClick={reconnect}>Reconnect</button>
//             </Modal.Actions>
//           </Modal>
//         )
//         : null}
//       {props.children}
//     </>
//   );
// }
