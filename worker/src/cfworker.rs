use arpeggio::types::PlayerID;

use serde::Deserialize;
use serde_json::json;
use tracing::{error, info};
use worker::{event, Context, Cors, Env, Method, Request, Response, Result};

use crate::{rust_error, storage};
use arptypes::multitenant::{GameID, GameList, GameMetadata, GameProfile, Role, UserID};

/// The main cloudflare Worker for Arpeggio. Handles routes for listing &
/// creating games, etc, and forwarding websockets to the Durable Object.
#[event(fetch, respond_with_errors)]
#[tracing::instrument(name = "worker", skip(req, env, _ctx), fields(method = ?req.method(), path = req.path()))]
async fn main(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    info!(event = "start", path = ?req.path());

    if req.path().starts_with("/ws/") {
        // WebSocket requests can't go through the entire HTTP rigmarole
        return forward_websocket(req, env).await;
    }

    let cors = Cors::new()
        .with_origins(vec!["*"])
        .with_allowed_headers(vec!["*"]);
    // TODO: If http_routes returns an Error, we are not applying cors headers.
    http_routes(req, env).await.and_then(|r| r.with_cors(&cors))
}

async fn http_routes(req: Request, env: Env) -> Result<Response> {
    let path = req.path();
    let parts = &path.split('/').collect::<Vec<_>>()[1..];
    if parts == ["oauth", "redirect"] {
        return oauth_redirect(req, env).await;
    }

    let id_token = req.headers().get("x-arpeggio-auth")?;
    if id_token.is_none() {
        // RADIX TODO: so I'm pretty confused... apparently OPTIONS request is hitting this. It's
        // working fine with me returning this error, probably because I'm using from_json here and not
        // Response::error... but it makes me think that I should be handling OPTIONS explicitly by just
        // returning an empty response with the CORS options headers.
        error!(event = "missing-auth", method=?req.method());
        return Response::from_json(&json!({"error": "need x-arpeggio-auth"}));
    }

    let id_token = id_token.unwrap();
    let client_id = env.var("GOOGLE_CLIENT_ID")?.to_string();

    let validation_result = validate_google_token(&id_token, client_id).await;
    if let Err(e) = validation_result {
        info!(event = "invalid-token", ?e);
        return Response::error("Invalid token", 401);
    }
    let user_id = validation_result.unwrap();

    match parts {
        ["superuser", rest @ ..] => {
            if !storage::check_superuser(&env, user_id).await? {
                return Response::error("You ain't super", 401);
            }
            superuser_routes(req, env, rest).await
        }
        ["request-websocket", game_id, role] => {
            request_websocket(req, env, game_id, user_id, role).await
        }
        ["g", "create"] => create_game(req, env, user_id).await,
        ["g", "list"] => list_games(req, env, user_id).await,
        ["g", "invitations", game_id, _invitation_id] if req.method() == Method::Get => {
            let game_id: GameID = game_id.parse().map_err(rust_error)?;
            forward_to_do(req, env, game_id).await
        }
        ["g", "invitations", game_id, invitation_id, "accept"] if req.method() == Method::Post => {
            accept_invitation(req, env, user_id, game_id, invitation_id).await
        }
        _ => Response::error(format!("No route matched {path:?}"), 404),
    }
}

async fn superuser_routes(req: Request, env: Env, path: &[&str]) -> Result<Response> {
    match path {
        ["games"] => superuser_games(env).await,
        ["dump", game_id] => {
            let game_id: GameID = game_id.parse().map_err(rust_error)?;
            forward_to_do(req, env, game_id).await
        }
        ["destroy", game_id] => {
            let game_id: GameID = game_id.parse().map_err(rust_error)?;
            forward_to_do(req, env, game_id).await
        }
        _ => Response::error(format!("No route matched {path:?}"), 404),
    }
}

async fn superuser_games(env: Env) -> Result<Response> {
    let games = storage::list_all_games(&env).await?;
    Response::from_json(&json!({"games": games}))
}

async fn accept_invitation(
    mut req: Request,
    env: Env,
    user_id: UserID,
    game_id: &str,
    invitation_id: &str,
) -> Result<Response> {
    // first, check that the invitation is valid
    let stub = durable_object(&env, game_id)?;
    let mut check_response = stub
        .fetch_with_str(&format!(
            "https://fake-host/g/invitations/{game_id}/{invitation_id}"
        ))
        .await?;
    let check_response: bool = check_response.json().await?;

    if check_response {
        // cool! let's create a profile. The profile name is passed in the request body.
        let profile_name: String = req.json().await?;
        let game_id: GameID = game_id.parse().map_err(rust_error)?;
        storage::create_profile(&env, game_id, user_id, PlayerID(profile_name), Role::Player)
            .await?;
    }

    Response::from_json(&json!({"cool": true}))
}

/// Generate a token that grants the bearer to connect to a Game with a WebSocket.
///
/// This works around the fact that browsers don't send any authentication headers with WebSocket
/// requests, by requiring this regular HTTP request that can be authenticated normally to generate
/// a temporary token that is then used to create the WebSocket connection.
async fn request_websocket(
    _req: Request,
    env: Env,
    game_id: &str,
    user_id: UserID,
    role: &str,
) -> Result<Response> {
    let game_id: GameID = game_id.parse().map_err(rust_error)?;
    let role: Role = role.parse().map_err(rust_error)?;
    let profile = storage::check_game_access(&env, user_id, game_id, role).await?;
    if let Some(profile) = profile {
        let stub = durable_object(&env, &game_id.to_string())?;
        let player_id = profile.profile_name;
        let mut url = worker::Url::parse("https://fake-host")?;
        // Url::set_path does percent-encoding, so we should be safe to put arbitrary player IDs here.
        url.set_path(&format!(
            "request-websocket/{}/{role}/{}",
            game_id.to_string(),
            player_id.0
        ));
        stub.fetch_with_str(url.as_str()).await
    } else {
        Response::error("You don't have access to this game", 401)
    }
}

fn durable_object(env: &Env, game_id: &str) -> Result<worker::Stub> {
    // We should probably make GameIDs actually be the Durable Object ID and use
    // namespace.id_from_string? This requires us to allocate the IDs with namespace.unique_id()
    // and would mean the type of GameID would have to change from wrapping UUIDs to instead
    // wrapping u256s (or more likely, [u8; 32]. or, more likely, String :P).
    let namespace = env.durable_object("ARPEGGIOGAME")?;
    namespace.id_from_name(game_id)?.get_stub()
}

/// Forward a simple request to the ArpeggioGame durable object
async fn forward_to_do(req: Request, env: Env, game_id: GameID) -> Result<Response> {
    let stub = durable_object(&env, &game_id.to_string())?;
    stub.fetch_with_request(req).await
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
    if let Some(game_id) = path.split('/').nth(2) {
        info!(event = "forward-websocket", ?game_id);
        let stub = durable_object(&env, game_id)?;
        stub.fetch_with_request(req).await
    } else {
        Response::error("Bad path", 404)
    }
}

/// Create a game
async fn create_game(mut req: Request, env: Env, user_id: UserID) -> Result<Response> {
    let game_id = GameID::gen();
    let name: String = req.json().await?;

    storage::create_game(&env, game_id, user_id, name).await?;
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
    Response::from_json(&list)
}

#[derive(Debug, Deserialize)]
struct GoogleTokenResponse {
    id_token: Option<String>,
    scope: Option<String>,
    // token_type: Option<String>,
    // expires_in: Option<i64>,
    // access_token: Option<String>,
    // refresh_token: Option<String>,
}

async fn oauth_redirect(req: Request, env: Env) -> Result<Response> {
    let mut url = req.url().map_err(rust_error)?;
    let code = url.query_pairs().find_map(|(key, value)| {
        if key == "code" {
            Some(value.into_owned())
        } else {
            None
        }
    });

    if code.is_none() {
        error!(event = "oauth-missing-code", query = ?url.query());
        return Response::error("Missing authorization code", 400);
    }

    let code = code.unwrap();
    let state = url.query_pairs().find_map(|(key, value)| {
        if key == "state" {
            Some(value.into_owned())
        } else {
            None
        }
    });

    // remove query parameters to match the original redirect URI sent to Google
    url.set_query(None);
    let redirect_uri = url.to_string();

    let client_id = env.var("GOOGLE_CLIENT_ID")?.to_string();
    let client_secret = env
        .var("GOOGLE_CLIENT_SECRET")
        .ok()
        .map(|value| value.to_string());

    let mut form: Vec<(String, String)> = vec![
        ("code".to_string(), code),
        ("client_id".to_string(), client_id),
        ("grant_type".to_string(), "authorization_code".to_string()),
        ("redirect_uri".to_string(), redirect_uri),
    ];

    if let Some(secret) = client_secret {
        form.push(("client_secret".to_string(), secret));
    }

    info!(
        event = "oauth-exchange-start",
        state = state.as_deref().unwrap_or_default()
    );

    let client = reqwest::Client::new();
    let response = client
        .post("https://oauth2.googleapis.com/token")
        .form(&form)
        .send()
        .await
        .map_err(rust_error)?;

    let status = response.status();
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        error!(
          event = "oauth-exchange-failed",
          %status,
          body = body.as_str()
        );
        return Response::error("Failed to exchange authorization code", status.as_u16());
    }

    let token_response: GoogleTokenResponse = response.json().await.map_err(rust_error)?;
    let Some(id_token) = token_response.id_token else {
        error!(event = "oauth-missing-id-token", response = ?token_response);
        return Response::error("No id_token in response", 502);
    };
    info!(id_token = id_token.as_str(), scope = ?token_response.scope, "Retrieved ID token");
    let client_id = env.var("GOOGLE_CLIENT_ID")?.to_string();
    let user_id = validate_google_token(&id_token, client_id)
        .await
        .map_err(rust_error)?;
    info!(?user_id, "ID Token validated, got user ID!");

    let frontend_base = env.var("FRONTEND_URL")?.to_string();
    let mut frontend_url = worker::Url::parse(&frontend_base).map_err(rust_error)?;
    frontend_url.set_path("/auth-success");
    {
        let mut pairs = frontend_url.query_pairs_mut();
        pairs.clear();
        pairs.append_pair("id_token", &id_token);
    }
    info!(event = "oauth-redirect", redirect = frontend_url.as_str());

    Response::redirect(frontend_url)
}

async fn validate_google_token(id_token: &str, client_id: String) -> anyhow::Result<UserID> {
    let client = google_oauth::Client::new(client_id);
    let payload = client
        .validate_id_token(id_token.to_string())
        .await
        .map_err(|s| anyhow::anyhow!(s))?;
    Ok(UserID(format!("google_{}", payload.sub)))
}
