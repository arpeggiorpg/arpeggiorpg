use google_signin;
use serde_json::json;
use worker::{console_error, console_log, event, Context, Cors, Env, Request, Response, Result};

use crate::{rust_error, storage};
use mtarp::types::{GameID, GameList, GameMetadata, GameProfile, Role, UserID};



/// The main cloudflare Worker for Arpeggio. Handles routes for listing &
/// creating games, etc, and forwarding websockets to the Durable Object.
#[event(fetch, respond_with_errors)]
async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
  console_log!("[worker] Start {} {:?}", req.path(), req.headers());

  if req.path().starts_with("/ws/") {
    // WebSocket requests can't go through the entire HTTP rigmarole
    return forward_websocket(req, env).await;
  }

  let cors = Cors::new().with_origins(vec!["*"]).with_allowed_headers(vec!["*"]);
  return http_routes(req, env).await.and_then(|r| r.with_cors(&cors));
}

async fn http_routes(req: Request, env: Env) -> Result<Response> {
  let path = req.path();
  let id_token = req.headers().get("x-arpeggio-auth")?;
  if id_token.is_none() {
    console_error!("No arpeggio auth header!");
    return Response::from_json(&json!({"error": "need x-arpeggio-auth"}));
  }

  let id_token = id_token.unwrap();
  let client_id = env.var("GOOGLE_CLIENT_ID")?.to_string();

  let validation_result = validate_google_token(&id_token, client_id).await;
  if let Err(e) = validation_result {
    console_log!("token is invalid; returning 401 {e:?}");
    return Response::error("Invalid token", 401);
  }
  let user_id = validation_result.unwrap();

  match path.split("/").collect::<Vec<_>>()[1..] {
    ["request-websocket", game_id, role] => {
      request_websocket(req, env, game_id, user_id, role).await
    }
    ["g", "create"] => create_game(req, env, user_id).await,
    ["g", "list"] => list_games(req, env, user_id).await,
    _ => Response::error(format!("No route matched {path:?}"), 404),
  }
}

/// Generate a token that grants the bearer to connect to a Game with a WebSocket.
///
/// This works around the fact that browsers don't send any authentication headers with WebSocket
/// requests, by requiring this regular HTTP request that can be authenticated normally to generate
/// a temporary token that is then used to create the WebSocket connection.
async fn request_websocket(
  req: Request, env: Env, game_id: &str, user_id: UserID, role: &str,
) -> Result<Response> {
  let game_id: GameID = game_id.parse().map_err(rust_error)?;
  let role: Role = role.parse().map_err(rust_error)?;
  let has_game = storage::check_game_access(&env, user_id, game_id, role).await?;
  if has_game {
    let namespace = env.durable_object("ARPEGGIOGAME")?;
    let stub = namespace.id_from_name(&game_id.to_string())?.get_stub()?;
    stub.fetch_with_request(req).await
  } else {
    Response::error("You don't have access to this game", 401)
  }
}

async fn forward_websocket(req: Request, env: Env) -> Result<Response> {
  // This is a WebSocket request. CORS & authentication are meaningless here. We use a token
  // system where the client sends a regular HTTP request to /request-websocket/{game_id} to do
  // authn & authz, which returns a temporary token that is then passed here, in the request that
  // creates the websocket.

  // NOTE: It's possible for a bad actor to spam requests to this endpoint to cause allocation of
  // infinite Durable Objects since there isn't any authz happening here for the user having access
  // to the game *before* we send the request on to the Durable Object. This is because browsers
  // can't send any sort of headers with WebSocket connections (so we can't check x-arpeggio-auth),
  // and also because of the way I've implemented the websocket token system (where the Durable
  // Object itself owns those tokens).
  //
  // Theoretically I could use JWTs for the tokens, which would allow this Worker code to verify
  // that the user has access to the game before passing it on, but for now I don't think the
  // ability to create empty DOs is a problem.

  let path = req.path();
  if let Some(game_id) = path.splitn(4, "/").nth(2) {
    console_log!("[worker] GAME {game_id:?}");
    let namespace = env.durable_object("ARPEGGIOGAME")?;
    // We should probably make GameIDs actually be the Durable Object ID and use
    // namespace.id_from_string? This requires us to allocate the IDs with namespace.unique_id()
    // and would mean the type of GameID would have to change from wrapping UUIDs to instead
    // wrapping u256s (or more likely, [u8; 32]. or, more likely, String :P).
    let stub = namespace.id_from_name(&game_id.to_string())?.get_stub()?;
    stub.fetch_with_request(req).await
  } else {
    Response::error("Bad path", 404)
  }
}

/// Create a game
async fn create_game(_req: Request, env: Env, user_id: UserID) -> Result<Response> {
  let game_id = GameID::gen();
  storage::create_game(&env, game_id, user_id).await?;
  let json = json!({"game_id": game_id});
  Response::from_json(&json)
}

/// List games
async fn list_games(_req: Request, env: Env, user_id: UserID) -> Result<Response> {
  let game_infos = storage::list_games_with_names(&env, user_id).await?;

  let list = GameList {
    games: game_infos
      .into_iter()
      .map(|r| {
        (
          GameProfile {
            user_id: r.user_id,
            game_id: r.game_id,
            profile_name: r.profile_name,
            role: r.role,
          },
          GameMetadata { name: r.name },
        )
      })
      .collect::<Vec<_>>(),
  };
  return Response::from_json(&list);
}

async fn validate_google_token(id_token: &str, client_id: String) -> anyhow::Result<UserID> {
  let mut certs = google_signin::CachedCerts::new();
  // let mut certs = self.cached_certs.lock().await;
  certs.refresh_if_needed().await?;
  let mut client = google_signin::Client::new();
  client.audiences.push(client_id);
  let claims = client.verify(id_token, &certs).await?;
  // let expiry = UNIX_EPOCH + Duration::from_secs(id_info.exp);
  // let time_until_expiry = expiry.duration_since(std::time::SystemTime::now());
  let custom = claims.custom;
  console_log!(
    "validate-token: email={:?} name={:?} sub={:?} expires={:?} ",
    custom.email,
    custom.name,
    custom.sub,
    claims.expiration,
    // time_until_expiry
  );
  Ok(UserID(format!("google_{}", custom.sub)))
}
