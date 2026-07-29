use std::{cell::RefCell, collections::HashMap, rc::Rc};

use anyhow::format_err;
use dioxus::prelude::*;
use futures::channel::oneshot::{self, Sender};
use futures_util::{SinkExt, TryStreamExt, stream::StreamExt};
use reqwest::StatusCode;
use reqwest_websocket::{Message, RequestBuilderExt, WebSocket};
use tracing::info;
use wasm_bindgen_futures::spawn_local;
use wasm_cookies::CookieOptions;

use crate::{GAME_LOGS, GAME_SOURCE, GameSource};
use arptypes::{
    Game,
    hosted::{CopyToPreprodResult, GameList, InvitationCheck, InvitationID},
    protocol::{GameID, GameUpdate, Role, RpcRequest, RpcResponse},
};

pub static AUTH_TOKEN: GlobalSignal<String> = Signal::global(String::new);

pub fn auth_token() -> String {
    wasm_cookies::get("arpeggio-token")
        .unwrap_or(Ok(String::new()))
        .unwrap_or(String::new())
}

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

pub fn is_preprod() -> bool {
    let Some(document) = web_sys::window().and_then(|window| window.document()) else {
        return false;
    };
    let Ok(Some(meta)) = document.query_selector("meta[name='ARPEGGIO_ENVIRONMENT']") else {
        return false;
    };
    meta.get_attribute("content").as_deref() == Some("preprod")
}

fn websocket_base_url() -> anyhow::Result<String> {
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

#[derive(Clone, Debug, serde::Deserialize, PartialEq)]
pub struct CurrentUser {
    pub is_superuser: bool,
}

pub async fn list_games() -> Result<GameList, anyhow::Error> {
    rpi_get("g/list").await
}

pub async fn current_user() -> Result<CurrentUser, anyhow::Error> {
    rpi_get("me").await
}

pub async fn create_game(name: String) -> Result<GameID, anyhow::Error> {
    #[derive(serde::Deserialize)]
    struct CreateGameResponse {
        game_id: GameID,
    }
    let resp: CreateGameResponse = rpi_post("g/create", &name).await?;
    Ok(resp.game_id)
}

pub async fn copy_game_from_production(
    game_id: &str,
) -> Result<CopyToPreprodResult, anyhow::Error> {
    rpi_post(&format!("superuser/copy-from-production/{game_id}"), &()).await
}

pub async fn delete_preprod_copy(game_id: &str) -> Result<(), anyhow::Error> {
    let _: serde_json::Value =
        rpi_post(&format!("superuser/delete-preprod-copy/{game_id}"), &()).await?;
    Ok(())
}

pub async fn check_invitation(
    game_id: GameID,
    invitation_id: InvitationID,
) -> Result<InvitationCheck, anyhow::Error> {
    rpi_get(&format!("g/invitations/{game_id}/{invitation_id}")).await
}

pub async fn accept_invitation(
    game_id: GameID,
    invitation_id: InvitationID,
    profile_name: String,
) -> Result<(), anyhow::Error> {
    let _resp: serde_json::Value = rpi_post(
        &format!("g/invitations/{game_id}/{invitation_id}/accept"),
        &profile_name,
    )
    .await?;
    Ok(())
}

type ResponseHandler = Sender<anyhow::Result<serde_json::Value>>;
type ResponseHandlers = HashMap<uuid::Uuid, ResponseHandler>;

#[component]
pub fn Connector(
    role: Role,
    game_id: GameID,
    player_id: Option<arptypes::PlayerID>,
    children: Element,
) -> Element {
    // let connection_count = use_signal(|| 0);
    let mut error = use_signal(|| None);
    let _coro = use_coroutine(move |mut rx: UnboundedReceiver<UIRequest>| {
        let player_id = player_id.clone();
        async move {
            let response_handlers: ResponseHandlers = HashMap::new();
            let response_handlers = Rc::new(RefCell::new(response_handlers));
            let websocket = match connect_coroutine(role, game_id).await {
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
            let game = Game::from_serialized_game(game);
            *GAME_SOURCE.write() = GameSource::GM(game);
            GAME_LOGS.write().extend(logs);
        }
        GameUpdate::RefreshPlayerGame { game, logs } => {
            let player_id = player_id.unwrap_or(arptypes::PlayerID(String::new()));
            *GAME_SOURCE.write() = GameSource::Player { player_id, game };
            GAME_LOGS.write().extend(logs);
        }
    }
    Ok(())
}

async fn connect_coroutine(role: Role, game_id: GameID) -> anyhow::Result<WebSocket> {
    let response =
        rpi_get::<serde_json::Value>(&format!("request-websocket/{game_id}/{role}")).await?;
    let token = response
        .get("token")
        .ok_or(format_err!("No token found in request-websocket response"))?
        .as_str()
        .ok_or(format_err!("token wasn't a string"))?;
    info!(token, "got websocket token!");

    let ws_base = websocket_base_url()?;
    let ws_url = format!("{ws_base}/ws/{game_id}/{token}");
    let response = reqwest::Client::default()
        .get(ws_url)
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

fn logout_for_auth_failure() {
    *AUTH_TOKEN.write() = String::new();
    let options: CookieOptions = Default::default();
    wasm_cookies::set("arpeggio-token", "", &options.with_path("/"));
}

async fn ensure_successful_response(
    response: reqwest::Response,
    context: &str,
) -> Result<reqwest::Response, anyhow::Error> {
    let status = response.status();
    if status == StatusCode::UNAUTHORIZED || status == StatusCode::FORBIDDEN {
        let body = response.text().await.unwrap_or_default();
        logout_for_auth_failure();
        return Err(anyhow::anyhow!("{context} failed with {status}: {body}"));
    }
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(anyhow::anyhow!("{context} failed with {status}: {body}"));
    }
    Ok(response)
}

pub(crate) async fn rpi_get<T: serde::de::DeserializeOwned>(
    path: &str,
) -> Result<T, anyhow::Error> {
    let rpi_url = rpi_url();
    let url = format!("{rpi_url}/{path}");
    let client = reqwest::Client::new();
    let response = client
        .get(url)
        .header("x-arpeggio-auth", AUTH_TOKEN())
        .send()
        .await?;
    let response = ensure_successful_response(response, "GET request").await?;
    response.json().await.map_err(|e| e.into())
}

async fn rpi_post<B: serde::Serialize, T: serde::de::DeserializeOwned>(
    path: &str,
    body: &B,
) -> Result<T, anyhow::Error> {
    let rpi_url = rpi_url();
    let url = format!("{rpi_url}/{path}");
    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .header("x-arpeggio-auth", AUTH_TOKEN())
        .json(body)
        .send()
        .await?;
    let response = ensure_successful_response(response, "POST request").await?;
    response.json().await.map_err(|e| e.into())
}
