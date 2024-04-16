use anyhow::format_err;
use dioxus::prelude::*;
use futures_util::{stream::StreamExt, SinkExt, TryStreamExt};
use log::info;
use reqwest_websocket::{Message, RequestBuilderExt};
use uuid::Uuid;

use arptypes::multitenant::{self, Role};

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

async fn connect_coroutine(role: Role, game_id: Uuid) -> anyhow::Result<()> {
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
  let mut websocket = response.into_websocket().await?;

  let get_game_json = serde_json::json!({
    "id": uuid::Uuid::new_v4().to_string(),
    "request": {
      "t": "GMGetGame"
    }
  });
  let get_game_json = serde_json::to_string(&get_game_json)?;
  websocket.send(Message::Text(get_game_json)).await?;


  // RADIX: send the initial request for game state!
  // const { logs, game, metadata } = await sendRequest(
  //   { t: "GMGetGame" },
  //   Z.object({ logs: T.decodeGameLogs, game: T.decodeGame, metadata: T.decodeGameMetadata }),
  // );


  while let Some(message) = websocket.try_next().await? {
    match message {
      Message::Text(text) => info!("WS Text Message: {text}"),
      Message::Binary(vecu8) => info!("WS Binary Message: {vecu8:?}"),
    }
  }

  Ok(())
}

#[component]
pub fn Connector(role: Role, game_id: Uuid, children: Element) -> Element {
  // let connection_count = use_signal(|| 0);
  let mut error = use_signal(|| None);
  let ws: Coroutine<()> = use_coroutine(|rx| async move {
    match connect_coroutine(role, game_id).await {
      Ok(x) => info!("Succeeded?"),
      Err(e) => error.set(Some(e.to_string())),
    }
  });
  if let Some(error) = error() {
    rsx! { "Coroutine Error! {error}"}
  } else {
    children
  }
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
