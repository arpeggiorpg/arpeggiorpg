mod actor;
mod images;
mod storage;

use std::{path::PathBuf, sync::Arc};

use actor::{GameActor, GameChanged};
use arpeggio::session::{SessionUser, gm_refresh, player_refresh};
use arptypes::{
    PlayerID,
    protocol::{GameRequest, RpcRequest, RpcResponse},
};
use axum::{
    Json, Router,
    body::Bytes,
    extract::{
        DefaultBodyLimit, Path, State,
        ws::{Message, WebSocket, WebSocketUpgrade},
    },
    http::{
        HeaderMap, HeaderValue, Method, StatusCode,
        header::{CONTENT_TYPE, ORIGIN},
    },
    response::{IntoResponse, Response},
    routing::{get, put},
};
use images::ImageStore;
use serde_json::{Value, json};
use storage::NativeStorage;
use tower_http::{
    cors::{Any, CorsLayer},
    services::{ServeDir, ServeFile},
    trace::TraceLayer,
};
use tracing::{error, info, warn};

const MAX_IMAGE_BYTES: usize = 20 * 1024 * 1024;

#[derive(Clone, Debug)]
pub struct ServerConfig {
    pub data_dir: PathBuf,
    pub public_url: String,
    pub allowed_origins: Vec<String>,
    pub ui_dir: Option<PathBuf>,
}

#[derive(Clone)]
struct AppState {
    actor: GameActor,
    images: ImageStore,
    allowed_origins: Arc<[String]>,
}

pub async fn app(config: ServerConfig) -> anyhow::Result<Router> {
    let storage = NativeStorage::open(&config.data_dir).await?;
    let images = ImageStore::new(&config.data_dir, config.public_url).await?;
    let actor = GameActor::spawn(storage, images.clone());
    let state = AppState {
        actor,
        images,
        allowed_origins: config.allowed_origins.clone().into(),
    };

    let cors = cors_layer(&config.allowed_origins)?;
    let mut router = Router::new()
        .route("/health", get(health))
        .route("/ws/GM", get(gm_websocket))
        .route("/ws/Player/{player_name}", get(player_websocket))
        .route(
            "/api/images/{image_id}",
            put(upload_image).layer(DefaultBodyLimit::max(MAX_IMAGE_BYTES)),
        )
        .route("/images/{image_id}", get(get_image))
        .with_state(state)
        .layer(cors)
        .layer(TraceLayer::new_for_http());

    if let Some(ui_dir) = config.ui_dir {
        let index = ui_dir.join("index.html");
        router = router.fallback_service(ServeDir::new(ui_dir).fallback(ServeFile::new(index)));
    }

    Ok(router)
}

fn cors_layer(origins: &[String]) -> anyhow::Result<CorsLayer> {
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::PUT, Method::OPTIONS])
        .allow_headers([CONTENT_TYPE]);
    if origins.iter().any(|origin| origin == "*") {
        Ok(cors.allow_origin(Any))
    } else {
        let origins: Vec<HeaderValue> = origins
            .iter()
            .map(|origin| origin.parse())
            .collect::<Result<_, _>>()?;
        Ok(cors.allow_origin(origins))
    }
}

async fn health() -> Json<Value> {
    Json(json!({ "status": "ok" }))
}

async fn gm_websocket(
    State(state): State<AppState>,
    headers: HeaderMap,
    websocket: WebSocketUpgrade,
) -> Response {
    upgrade_websocket(state, headers, websocket, SessionUser::gm()).await
}

async fn player_websocket(
    State(state): State<AppState>,
    Path(player_name): Path<String>,
    headers: HeaderMap,
    websocket: WebSocketUpgrade,
) -> Response {
    upgrade_websocket(
        state,
        headers,
        websocket,
        SessionUser::player(PlayerID(player_name)),
    )
    .await
}

async fn upgrade_websocket(
    state: AppState,
    headers: HeaderMap,
    websocket: WebSocketUpgrade,
    user: SessionUser,
) -> Response {
    if !origin_allowed(&state.allowed_origins, &headers) {
        return (StatusCode::FORBIDDEN, "origin is not allowed").into_response();
    }
    websocket
        .on_upgrade(move |socket| connection(socket, state.actor, user))
        .into_response()
}

fn origin_allowed(allowed_origins: &[String], headers: &HeaderMap) -> bool {
    let Some(origin) = headers.get(ORIGIN).and_then(|origin| origin.to_str().ok()) else {
        return true;
    };
    allowed_origins
        .iter()
        .any(|allowed| allowed == "*" || allowed == origin)
}

async fn connection(mut socket: WebSocket, actor: GameActor, user: SessionUser) {
    let mut changes = actor.subscribe();
    loop {
        tokio::select! {
            message = socket.recv() => {
                let Some(message) = message else {
                    break;
                };
                match message {
                    Ok(Message::Text(text)) => {
                        if let Err(error) = handle_socket_request(&mut socket, &actor, &user, &text).await {
                            error!(?error, "failed to handle websocket request");
                            break;
                        }
                    }
                    Ok(Message::Ping(bytes)) => {
                        if socket.send(Message::Pong(bytes)).await.is_err() {
                            break;
                        }
                    }
                    Ok(Message::Close(_)) | Err(_) => break,
                    Ok(Message::Binary(_) | Message::Pong(_)) => {}
                }
            }
            change = changes.recv() => {
                match change {
                    Ok(change) => {
                        if let Some(update) = update_for_user(&user, &change)
                            && send_json(&mut socket, &update).await.is_err()
                        {
                            break;
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        warn!(skipped, "websocket lagged behind game refreshes");
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                }
            }
        }
    }
    info!(role = ?user.role, player_id = ?user.player_id, "websocket disconnected");
}

async fn handle_socket_request(
    socket: &mut WebSocket,
    actor: &GameActor,
    user: &SessionUser,
    text: &str,
) -> anyhow::Result<()> {
    let request = match serde_json::from_str::<RpcRequest<GameRequest>>(text) {
        Ok(request) => request,
        Err(error) => {
            if let Ok(untyped) = serde_json::from_str::<RpcRequest<Value>>(text) {
                send_json(
                    socket,
                    &RpcResponse::<Value>::Error {
                        id: untyped.id,
                        error: format!("invalid game request: {error}"),
                    },
                )
                .await?;
            } else {
                send_json(
                    socket,
                    &json!({ "error": format!("invalid RPC request: {error}") }),
                )
                .await?;
            }
            return Ok(());
        }
    };

    let response = match actor.request(user.clone(), request.request).await {
        Ok(payload) => RpcResponse::Success {
            id: request.id,
            payload,
        },
        Err(error) => RpcResponse::Error {
            id: request.id,
            error: error.to_string(),
        },
    };
    send_json(socket, &response).await
}

fn update_for_user(
    user: &SessionUser,
    change: &GameChanged,
) -> Option<arptypes::protocol::GameUpdate> {
    match user.role {
        arptypes::protocol::Role::GM => gm_refresh(&change.game, &change.logs).ok(),
        arptypes::protocol::Role::Player => user
            .player_id
            .as_ref()
            .and_then(|player_id| player_refresh(&change.game, player_id, &change.logs).ok()),
    }
}

async fn send_json(socket: &mut WebSocket, value: &impl serde::Serialize) -> anyhow::Result<()> {
    socket
        .send(Message::Text(serde_json::to_string(value)?.into()))
        .await?;
    Ok(())
}

async fn upload_image(
    State(state): State<AppState>,
    Path(image_id): Path<String>,
    headers: HeaderMap,
    body: Bytes,
) -> Response {
    let content_type = headers
        .get(CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .unwrap_or("application/octet-stream");
    match state.images.write(&image_id, &body, content_type).await {
        Ok(image) => Json(json!({ "image_url": image.public_url })).into_response(),
        Err(error) => {
            warn!(?error, "image upload failed");
            (StatusCode::BAD_REQUEST, error.to_string()).into_response()
        }
    }
}

async fn get_image(State(state): State<AppState>, Path(image_id): Path<String>) -> Response {
    match state.images.read(&image_id).await {
        Ok((bytes, content_type)) => {
            let mut response = bytes.into_response();
            if let Ok(content_type) = HeaderValue::from_str(&content_type) {
                response.headers_mut().insert(CONTENT_TYPE, content_type);
            }
            response
        }
        Err(_) => StatusCode::NOT_FOUND.into_response(),
    }
}

#[cfg(test)]
mod tests {
    use arptypes::{
        GMCommand, GameLog,
        protocol::{GameAndMetadata, GameUpdate, ImageType, PlayerGameAndMetadata},
    };
    use axum_test::TestServer;

    use super::*;

    static HTTP_TEST_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

    async fn test_app() -> (tempfile::TempDir, Router) {
        let directory = tempfile::tempdir().unwrap();
        let router = app(ServerConfig {
            data_dir: directory.path().to_path_buf(),
            public_url: "http://server.test".to_string(),
            allowed_origins: vec!["http://ui.test".to_string()],
            ui_dir: None,
        })
        .await
        .unwrap();
        (directory, router)
    }

    #[tokio::test]
    async fn websocket_routes_support_one_gm_and_two_players() {
        let _http_test_guard = HTTP_TEST_LOCK.lock().await;
        let (_directory, router) = test_app().await;
        let server = TestServer::builder()
            .http_transport()
            .build(router)
            .unwrap();
        let mut gm = server.get_websocket("/ws/GM").await.into_websocket().await;
        let mut alice = server
            .get_websocket("/ws/Player/Alice")
            .await
            .into_websocket()
            .await;
        let mut bob = server
            .get_websocket("/ws/Player/Bob")
            .await
            .into_websocket()
            .await;

        register_player(&mut gm, "register-alice", "Alice").await;
        let _: GameUpdate = gm.receive_json().await;
        let _: GameUpdate = alice.receive_json().await;

        register_player(&mut gm, "register-bob", "Bob").await;
        let _: GameUpdate = gm.receive_json().await;
        let _: GameUpdate = alice.receive_json().await;
        let _: GameUpdate = bob.receive_json().await;

        gm.send_json(&RpcRequest {
            id: "get-gm".to_string(),
            request: GameRequest::GMGetGame,
        })
        .await;
        let response: RpcResponse<Value> = gm.receive_json().await;
        let gm_game: GameAndMetadata = serde_json::from_value(success(response)).unwrap();
        assert_eq!(gm_game.game.players.len(), 2);

        for (socket, id) in [(&mut alice, "get-alice"), (&mut bob, "get-bob")] {
            socket
                .send_json(&RpcRequest {
                    id: id.to_string(),
                    request: GameRequest::PlayerGetGame,
                })
                .await;
            let response: RpcResponse<Value> = socket.receive_json().await;
            let _: PlayerGameAndMetadata = serde_json::from_value(success(response)).unwrap();
        }
    }

    async fn register_player(
        socket: &mut axum_test::TestWebSocket,
        request_id: &str,
        player_name: &str,
    ) {
        socket
            .send_json(&RpcRequest {
                id: request_id.to_string(),
                request: GameRequest::GMCommand {
                    command: Box::new(GMCommand::RegisterPlayer {
                        id: PlayerID(player_name.to_string()),
                    }),
                },
            })
            .await;
        let response: RpcResponse<Value> = socket.receive_json().await;
        let result: Result<Vec<GameLog>, String> =
            serde_json::from_value(success(response)).unwrap();
        assert!(result.is_ok());
    }

    fn success(response: RpcResponse<Value>) -> Value {
        match response {
            RpcResponse::Success { payload, .. } => payload,
            RpcResponse::Error { error, .. } => panic!("unexpected RPC error: {error}"),
        }
    }

    #[tokio::test]
    async fn configured_origins_are_enforced_for_browser_websockets() {
        let _http_test_guard = HTTP_TEST_LOCK.lock().await;
        let (_directory, router) = test_app().await;
        let server = TestServer::builder()
            .http_transport()
            .build(router)
            .unwrap();
        let response = server
            .get_websocket("/ws/GM")
            .add_header(ORIGIN, HeaderValue::from_static("http://other.test"))
            .await;
        response.assert_status_forbidden();
    }

    #[tokio::test]
    async fn image_uploads_are_written_and_served_from_the_data_directory() {
        let _http_test_guard = HTTP_TEST_LOCK.lock().await;
        let (directory, router) = test_app().await;
        let server = TestServer::builder()
            .http_transport()
            .build(router)
            .unwrap();
        let mut gm = server.get_websocket("/ws/GM").await.into_websocket().await;
        gm.send_json(&RpcRequest {
            id: "request-image".to_string(),
            request: GameRequest::RequestUploadImage {
                purpose: ImageType::CreatureIcon,
            },
        })
        .await;
        let response: RpcResponse<Value> = gm.receive_json().await;
        let payload = success(response);
        let upload_url = reqwest::Url::parse(payload["upload_url"].as_str().unwrap()).unwrap();
        let final_url = reqwest::Url::parse(payload["final_url"].as_str().unwrap()).unwrap();
        let image_bytes = Bytes::from_static(b"not-a-real-png-but-stored-verbatim");

        let upload = server
            .put(upload_url.path())
            .content_type("image/png")
            .bytes(image_bytes.clone())
            .await;
        upload.assert_status_ok();
        let download = server.get(final_url.path()).await;
        download.assert_status_ok();
        assert_eq!(download.as_bytes(), &image_bytes);
        assert_eq!(download.header(CONTENT_TYPE).to_str().unwrap(), "image/png");
        assert!(directory.path().join("images").is_dir());
    }

    #[tokio::test]
    async fn optional_ui_directory_supports_direct_spa_routes() {
        let directory = tempfile::tempdir().unwrap();
        let ui_directory = directory.path().join("ui");
        tokio::fs::create_dir_all(&ui_directory).await.unwrap();
        tokio::fs::write(
            ui_directory.join("index.html"),
            "<html><body>Standalone Arpeggio</body></html>",
        )
        .await
        .unwrap();
        let router = app(ServerConfig {
            data_dir: directory.path().join("data"),
            public_url: "http://server.test".to_string(),
            allowed_origins: vec!["http://ui.test".to_string()],
            ui_dir: Some(ui_directory),
        })
        .await
        .unwrap();
        let server = TestServer::new(router).unwrap();

        let response = server.get("/Player/Alice").await;
        response.assert_status_ok();
        response.assert_text_contains("Standalone Arpeggio");
    }
}
