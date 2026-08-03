use std::{cell::RefCell, collections::HashMap, rc::Rc};

use dioxus::prelude::*;
use futures::channel::oneshot::{self, Sender};
use futures_util::{
    SinkExt,
    stream::{self, StreamExt},
};
use reqwest_websocket::{Message, RequestBuilderExt, WebSocket};
use tracing::info;

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
    let _coro = use_coroutine(move |rx: UnboundedReceiver<UIRequest>| {
        let player_id = player_id.clone();
        let websocket_url = websocket_url.clone();
        async move {
            let response_handlers = Rc::new(RefCell::new(ResponseHandlers::new()));
            let websocket = match connect_coroutine(websocket_url).await {
                Ok(ws) => ws,
                Err(e) => {
                    error.set(Some(e.to_string()));
                    return;
                }
            };

            match run_connector(websocket, rx, player_id, response_handlers.clone()).await {
                Ok(()) => {
                    fail_pending_requests(&response_handlers, "WebSocket connector stopped");
                }
                Err(connection_error) => {
                    error!(?connection_error, "WebSocket connector failed");
                    let message = connection_error.to_string();
                    fail_pending_requests(&response_handlers, &message);
                    error.set(Some(message));
                }
            }
        }
    });
    if let Some(error) = error() {
        rsx! { "Coroutine Error! {error}"}
    } else {
        children
    }
}

fn fail_pending_requests(response_handlers: &Rc<RefCell<ResponseHandlers>>, message: &str) {
    let pending = std::mem::take(&mut *response_handlers.borrow_mut());
    for (_, handler) in pending {
        let _ = handler.send(Err(anyhow::anyhow!(message.to_string())));
    }
}

enum ConnectorEvent {
    WebSocketMessage(Result<Message, reqwest_websocket::Error>),
    WebSocketClosed,
    UIRequest(UIRequest),
    UIClosed,
}

async fn run_connector(
    websocket: WebSocket,
    rx: UnboundedReceiver<UIRequest>,
    player_id: Option<arptypes::PlayerID>,
    response_handlers: Rc<RefCell<ResponseHandlers>>,
) -> anyhow::Result<()> {
    let (mut websocket_tx, websocket_rx) = websocket.split();
    let websocket_events = websocket_rx
        .map(ConnectorEvent::WebSocketMessage)
        .chain(stream::iter([ConnectorEvent::WebSocketClosed]));
    let ui_events = rx
        .map(ConnectorEvent::UIRequest)
        .chain(stream::iter([ConnectorEvent::UIClosed]));
    let events = stream::select(websocket_events, ui_events);
    futures::pin_mut!(events);

    while let Some(event) = events.next().await {
        match event {
            ConnectorEvent::WebSocketMessage(Ok(message)) => {
                handle_websocket_message(message, &response_handlers, &player_id)?;
            }
            ConnectorEvent::WebSocketMessage(Err(websocket_error)) => {
                return Err(anyhow::anyhow!(websocket_error));
            }
            ConnectorEvent::WebSocketClosed => {
                return Err(anyhow::anyhow!("WebSocket connection closed"));
            }
            ConnectorEvent::UIRequest(ui_request) => {
                send_ui_request(&mut websocket_tx, &response_handlers, ui_request).await?;
            }
            ConnectorEvent::UIClosed => return Ok(()),
        }
    }

    Ok(())
}

fn handle_websocket_message(
    message: Message,
    response_handlers: &Rc<RefCell<ResponseHandlers>>,
    player_id: &Option<arptypes::PlayerID>,
) -> anyhow::Result<()> {
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
                if let Some(handler) = response_handlers.borrow_mut().remove(&id) {
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
    Ok(())
}

async fn send_ui_request(
    websocket_tx: &mut futures::stream::SplitSink<WebSocket, Message>,
    response_handlers: &Rc<RefCell<ResponseHandlers>>,
    ui_request: UIRequest,
) -> anyhow::Result<()> {
    let request_id = uuid::Uuid::new_v4();
    let request = RpcRequest {
        id: request_id.to_string(),
        request: ui_request.game_request,
    };
    let message = serde_json::to_string(&request)?;
    if let Some(callback) = ui_request.callback {
        response_handlers.borrow_mut().insert(request_id, callback);
    }
    websocket_tx
        .send(Message::Text(message))
        .await
        .map_err(|error| anyhow::anyhow!("WebSocket send failed: {error}"))
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



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn failing_pending_requests_notifies_waiter_and_clears_handler() {
        let response_handlers = Rc::new(RefCell::new(ResponseHandlers::new()));
        let (sender, receiver) = oneshot::channel();
        response_handlers
            .borrow_mut()
            .insert(uuid::Uuid::new_v4(), sender);

        fail_pending_requests(&response_handlers, "connection closed");

        assert!(response_handlers.borrow().is_empty());
        let result = futures::executor::block_on(receiver).expect("callback must be notified");
        assert_eq!(result.unwrap_err().to_string(), "connection closed");
    }
}
