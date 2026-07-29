use std::{cell::RefCell, collections::HashMap, rc::Rc};

use dioxus::prelude::*;
use futures::channel::oneshot::{self, Sender};
use futures_util::{SinkExt, TryStreamExt, stream::StreamExt};
use reqwest_websocket::{Message, RequestBuilderExt, WebSocket};
use tracing::info;
use wasm_bindgen_futures::spawn_local;

use crate::{GAME_LOGS, GAME_SOURCE, GameSource};
use arptypes::{
    Game,
    protocol::{GameUpdate, RpcRequest, RpcResponse},
};

pub fn rpi_url() -> String {
    let window = web_sys::window().expect("global window doesn't exist");
    let document = window.document().expect("document must exist");
    let meta = document
        .query_selector("meta[name='RPI_URL']")
        .expect("meta RPI_URL tag must exist in index.html (1)")
        .expect("meta RPI_URL tag must exist in index.html (2)");
    meta.get_attribute("content")
        .expect("meta RPI_URL tag must have content")
}

pub fn websocket_base_url() -> anyhow::Result<String> {
    let base = rpi_url().trim_end_matches('/').to_string();
    if let Some(rest) = base.strip_prefix("https://") {
        return Ok(format!("wss://{rest}"));
    }
    if let Some(rest) = base.strip_prefix("http://") {
        return Ok(format!("ws://{rest}"));
    }
    if base.starts_with("wss://") || base.starts_with("ws://") {
        return Ok(base);
    }
    Err(anyhow::anyhow!("Unsupported RPI_URL scheme: {base}"))
}

type ResponseHandler = Sender<anyhow::Result<serde_json::Value>>;
type ResponseHandlers = HashMap<uuid::Uuid, ResponseHandler>;

#[component]
pub fn Connector(
    websocket_url: String,
    player_id: Option<arptypes::PlayerID>,
    children: Element,
) -> Element {
    // let connection_count = use_signal(|| 0);
    let mut error = use_signal(|| None);
    let _coro = use_coroutine(move |mut rx: UnboundedReceiver<UIRequest>| {
        let player_id = player_id.clone();
        let websocket_url = websocket_url.clone();
        async move {
            let response_handlers: ResponseHandlers = HashMap::new();
            let response_handlers = Rc::new(RefCell::new(response_handlers));
            let websocket = match connect_coroutine(websocket_url.clone()).await {
                Ok(ws) => ws,
                Err(e) => {
                    error.set(Some(e.to_string()));
                    return;
                }
            };

            let (mut websocket_tx, websocket_rx) = websocket.split();
            let receiver_response_handlers = response_handlers.clone();
            spawn_local(async move {
                let result =
                    ws_receiver(websocket_rx, receiver_response_handlers, player_id.clone()).await;
                match result {
                    Ok(r) => info!(?r, "ws_receiver completed"),
                    Err(error) => error!(?error, "ws_receiver error"),
                }
                info!("Websocket is now dead.");
            });

            while let Some(ui_req) = rx.next().await {
                let request_id = uuid::Uuid::new_v4();
                let request = RpcRequest {
                    id: request_id.to_string(),
                    request: ui_req.game_request,
                };
                let cmd_json = serde_json::to_string(&request)
                    .expect("must be able to serialize RPC requests");
                if let Some(callback) = ui_req.callback {
                    response_handlers.borrow_mut().insert(request_id, callback);
                }
                websocket_tx
                    .send(Message::Text(cmd_json))
                    .await
                    .expect("Couldn't send command to websocket");
            }
        }
    });
    if let Some(error) = error() {
        rsx! { "Coroutine Error! {error}"}
    } else {
        children
    }
}

async fn ws_receiver(
    mut websocket_rx: futures::stream::SplitStream<WebSocket>,
    receiver_response_handlers: Rc<RefCell<ResponseHandlers>>,
    player_id: Option<arptypes::PlayerID>,
) -> anyhow::Result<()> {
    while let Some(message) = websocket_rx.try_next().await? {
        match message {
            Message::Text(text) => {
                let json: serde_json::Value = serde_json::from_str(&text)?;
                if json.get("id").is_some() {
                    let response: RpcResponse<serde_json::Value> =
                        serde_json::from_value(json.clone())?;
                    let (id, result) = match response {
                        RpcResponse::Success { id, payload } => (id, Ok(payload)),
                        RpcResponse::Error { id, error } => (id, Err(anyhow::anyhow!(error))),
                    };
                    let id: uuid::Uuid = id.parse()?;
                    if let Some(handler) = receiver_response_handlers.borrow_mut().remove(&id) {
                        if let Err(response) = handler.send(result) {
                            error!(?id, ?response, "Response handler disappeared");
                        }
                    } else {
                        warn!(?id, ?json, "Got result for unexpected ID");
                    }
                } else {
                    let update: GameUpdate = serde_json::from_value(json)?;
                    handle_unsolicited(update, player_id.clone())?;
                }
            }
            Message::Binary(vecu8) => info!(?vecu8, "WS Binary Message"),
        }
    }
    Ok(())
}

fn handle_unsolicited(
    update: GameUpdate,
    player_id: Option<arptypes::PlayerID>,
) -> anyhow::Result<()> {
    match update {
        GameUpdate::RefreshGame { game, logs } => {
            let game = Game::from_serialized_game(*game);
            *GAME_SOURCE.write() = GameSource::GM(game);
            GAME_LOGS.write().extend(logs);
        }
        GameUpdate::RefreshPlayerGame { game, logs } => {
            let player_id = player_id.unwrap_or(arptypes::PlayerID(String::new()));
            *GAME_SOURCE.write() = GameSource::Player {
                player_id,
                game: *game,
            };
            GAME_LOGS.write().extend(logs);
        }
    }
    Ok(())
}

async fn connect_coroutine(websocket_url: String) -> anyhow::Result<WebSocket> {
    let response = reqwest::Client::default()
        .get(websocket_url)
        .upgrade()
        .send()
        .await?;
    let websocket = response.into_websocket().await?;
    Ok(websocket)
}

pub struct UIRequest {
    game_request: serde_json::Value,
    callback: Option<Sender<anyhow::Result<serde_json::Value>>>,
}

pub fn use_ws() -> Coroutine<UIRequest> {
    use_coroutine_handle::<UIRequest>()
}

pub async fn send_request<T: serde::de::DeserializeOwned>(
    req: impl serde::Serialize,
    coro: Coroutine<UIRequest>,
) -> anyhow::Result<T> {
    let (sender, receiver) = oneshot::channel::<anyhow::Result<serde_json::Value>>();
    let ui_req = UIRequest {
        game_request: serde_json::to_value(req)?,
        callback: Some(sender),
    };
    coro.send(ui_req);
    let response = receiver.await??;
    Ok(serde_json::from_value(response)?)
}
