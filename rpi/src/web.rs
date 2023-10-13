use std::sync::Arc;

use anyhow::anyhow;
use axum::{
  body,
  extract::{Path, State},
  http::Request,
  middleware::{from_fn, from_fn_with_state, Next},
  response::{IntoResponse, Response},
  routing::{get, post},
  Extension, Json,
};
use http::StatusCode;
use tower_http::{cors::CorsLayer, trace::TraceLayer};

use arpeggio::types::{
  AbilityID, CreatureID, GMCommand, Game, PlayerCommand, PlayerID, Point3, PotentialTargets,
  RPIGame, SceneID,
};

use mtarp::{
  actor::{AuthenticatedService, GMService, PlayerService},
  types::{GameID, GameIndex, GameList, GameMetadata, GameProfile, InvitationID},
};

use crate::{authn::AuthenticatableService, waiters::PingService};

pub fn router(authn: AuthenticatableService) -> axum::Router {
  let waiters = PingService::new();
  let state = Arc::new(AxumState { waiters, authn });

  let gm_routes = axum::Router::new()
    .route("/", get(gm_get_game))
    .route("/poll/:game_idx/:log_idx", get(gm_poll_game))
    .route("/execute", post(perform_gm_command))
    .route("/movement_options/:scene_id/:cid", get(movement_options))
    .route("/combat_movement_options", get(combat_movement_options))
    .route("/target_options/:scene_id/:cid/:abid", get(target_options))
    .route(
      "/preview_volume_targets/:scene_id/:service_id/:ability_id/:x/:y/:z",
      get(preview_volume_targets),
    )
    .route("/invitations", get(list_invitations).post(invite))
    .route_layer(from_fn(authorize_gm));

  let player_routes = axum::Router::new()
    // TODO: These should not return entire RPIGames, but rather RPIPlayerGames, which:
    // - don't have access to the campaign
    // - maybe only include top-level objects (creatures, etc) that are in the current scene
    .route("/", get(player_get_game))
    .route("/poll/:game_idx/:log_idx", get(player_poll_game))
    .route("/execute", post(perform_player_command))
    .route("/movement_options/:scene_id/:creature_id", get(player_movement_options))
    .route_layer(from_fn(authorize_player));

  let auth_routes = axum::Router::new()
    .route("/list", get(list_games))
    .route("/create", post(create_game))
    .route("/invitations/:game_id/:invitation_id", get(check_invitation))
    .route("/invitations/:game_id/:invitation_id/accept", post(accept_invitation))
    .nest("/:game_id/gm/", gm_routes)
    .nest("/:game_id/player/", player_routes)
    .route_layer(from_fn_with_state(state.clone(), authenticate));

  let cors = CorsLayer::permissive();
  let trace = TraceLayer::new_for_http();

  axum::Router::new().nest("/g", auth_routes).with_state(state.clone()).layer(cors).layer(trace)
}

struct AxumState {
  authn: AuthenticatableService,
  waiters: PingService,
}

#[derive(serde::Deserialize)]
struct GameIDPath {
  game_id: GameID,
}

#[derive(serde::Deserialize)]
struct InvitationPath {
  game_id: GameID,
  invitation_id: InvitationID,
}

async fn authenticate<B>(
  State(state): State<Arc<AxumState>>, mut request: Request<B>, next: Next<B>,
) -> Result<Response, WebError> {
  let header =
    request.headers().get("x-pt-rpi-auth").ok_or(anyhow!("Need a x-pt-rpi-auth header"))?;
  let id_token = header.to_str()?;
  // TODO: we should specifically handle the case where the token is valid but expired and return a
  // special response so the client can refresh the token.
  let authenticated_result = state.authn.authenticate(id_token.to_string()).await;
  match authenticated_result {
    Ok(authenticated) => {
      request.extensions_mut().insert(Arc::new(authenticated));
      Ok(next.run(request).await)
    }
    Err(_) => {
      let body = body::boxed("auth failed".to_string());
      let response = Response::builder().status(401).body(body)?;
      Ok(response)
    }
  }
}

async fn authorize_gm<B>(
  Path(GameIDPath { game_id }): Path<GameIDPath>,
  Extension(authenticated): Extension<Arc<AuthenticatedService>>, mut request: Request<B>,
  next: Next<B>,
) -> Result<Response, WebError> {
  // TODO: return a 404 when the game doesn't exist.
  let gm = authenticated.gm(&game_id).await?;
  request.extensions_mut().insert(Arc::new(gm));
  Ok(next.run(request).await)
}

async fn authorize_player<B>(
  Path(GameIDPath { game_id }): Path<GameIDPath>,
  Extension(authenticated): Extension<Arc<AuthenticatedService>>, mut request: Request<B>,
  next: Next<B>,
) -> Result<Response, WebError> {
  // TODO: return a 404 when the game doesn't exist.
  let player = authenticated.player(&game_id).await?;
  request.extensions_mut().insert(Arc::new(player));
  Ok(next.run(request).await)
}

async fn list_games(
  Extension(service): Extension<Arc<AuthenticatedService>>,
) -> WebResult<Json<GameList>> {
  let games = service.list_games().await?;
  Ok(Json(games))
}

async fn invite(Extension(service): Extension<Arc<GMService>>) -> WebResult<Json<InvitationID>> {
  Ok(Json(service.invite().await?))
}

async fn list_invitations(
  Extension(service): Extension<Arc<GMService>>,
) -> WebResult<Json<Vec<InvitationID>>> {
  Ok(Json(service.list_invitations().await?))
}

async fn check_invitation(
  Extension(service): Extension<Arc<AuthenticatedService>>,
  Path(InvitationPath { game_id, invitation_id }): Path<InvitationPath>,
) -> WebResult<Json<bool>> {
  let bool = service.check_invitation(&game_id, &invitation_id).await?;
  Ok(Json(bool))
}

async fn accept_invitation(
  Extension(service): Extension<Arc<AuthenticatedService>>, State(state): State<Arc<AxumState>>,
  Path(InvitationPath { game_id, invitation_id }): Path<InvitationPath>,
  Json(profile_name): Json<String>,
) -> WebResult<Json<GameProfile>> {
  let player_id = PlayerID(profile_name);
  let thing = service.accept_invitation(&game_id, &invitation_id, player_id).await?;
  state.waiters.ping(&game_id).await?;
  Ok(Json(thing))
}

async fn create_game(
  Extension(service): Extension<Arc<AuthenticatedService>>, Json(name): Json<String>,
) -> WebResult<Json<serde_json::Value>> {
  let game_id = service.new_game(name.to_string()).await?;
  let json = serde_json::json!({"game_id": game_id});
  Ok(Json(json))
}

async fn gm_get_game(
  Extension(service): Extension<Arc<GMService>>,
) -> WebResult<Json<serde_json::Value>> {
  let (game, index, metadata) = service.get_game().await?;
  Ok(Json(_get_game(game, index, metadata).await?))
}

async fn player_get_game(
  Extension(service): Extension<Arc<PlayerService>>,
) -> WebResult<Json<serde_json::Value>> {
  // TODO: this needs to return just a subset of the game! (only the currently focused scene, and
  // creatures etc available in that scene)
  let (game, index, metadata) = service.get_game().await?;
  Ok(Json(_get_game(game, index, metadata).await?))
}

async fn gm_poll_game(
  Extension(service): Extension<Arc<GMService>>, State(state): State<Arc<AxumState>>,
  Path((game_id, game_idx, log_idx)): Path<(GameID, usize, usize)>,
) -> WebResult<Json<serde_json::Value>> {
  state.waiters.poll_game(game_id, GameIndex { game_idx, log_idx }).await?;
  let (game, index, metadata) = service.get_game().await?;
  Ok(Json(_get_game(game, index, metadata).await?))
}

async fn player_poll_game(
  Extension(service): Extension<Arc<PlayerService>>, State(state): State<Arc<AxumState>>,
  Path((game_id, game_idx, log_idx)): Path<(GameID, usize, usize)>,
) -> WebResult<Json<serde_json::Value>> {
  // Ok, this is dumb because this function is identical to gm_poll_game except for using
  // PlayerService I could switch to using a trait or something, where both GMService and
  // PlayerService implement poll_game, BUT, I'm not sure if the interface is going to remain the
  // same, so I'll duplicate it for now.
  state.waiters.poll_game(game_id, GameIndex { game_idx, log_idx }).await?;
  let (game, index, metadata) = service.get_game().await?;
  Ok(Json(_get_game(game, index, metadata).await?))
}

async fn _get_game(
  game: &Game, index: GameIndex, metadata: GameMetadata,
) -> WebResult<serde_json::Value> {
  // Unfortunately, I don't yet encode this type as a real Rust type, because RPIGame takes a
  // reference to the Game, and I can't derive TS on structs that have parameterized lifetimes (I
  // think?)
  let game = RPIGame(game);
  Ok(serde_json::json!({
    "game": game,
    "index": index,
    "metadata": metadata
  }))
}

async fn perform_gm_command(
  Extension(service): Extension<Arc<GMService>>, State(state): State<Arc<AxumState>>,
  Json(command): Json<GMCommand>,
) -> WebResult<Json<std::result::Result<serde_json::Value, String>>> {
  // Note that this function actually serializes the result from calling
  // perform_gm_command into the JSON response -- there are no "?" operators here!
  let changed_game = service.perform_command(command).await;
  let changed_game = changed_game
    .map(|cg| serde_json::json!({"game": RPIGame(&cg.game), "logs": cg.logs}))
    .map_err(|e| format!("{e:?}"));
  state.waiters.ping(&service.game_id).await?;
  Ok(Json(changed_game))
}

async fn perform_player_command(
  Extension(service): Extension<Arc<PlayerService>>, State(state): State<Arc<AxumState>>,
  Json(command): Json<PlayerCommand>,
) -> WebResult<Json<std::result::Result<serde_json::Value, String>>> {
  // Note that this function actually serializes the Result from calling perform_player_command into
  // the JSON response, so that it will encode both the Ok and the Err -- there are no "?" operators
  // here!
  let changed_game = service.perform_command(command).await;
  let changed_game = changed_game
    .map(|cg| serde_json::json!({"game": RPIGame(&cg.game), "logs": cg.logs}))
    .map_err(|e| format!("{e:?}"));
  state.waiters.ping(&service.game_id).await?;
  Ok(Json(changed_game))
}

async fn movement_options(
  Extension(service): Extension<Arc<GMService>>,
  Path((_game_id, scene_id, creature_id)): Path<(String, SceneID, CreatureID)>,
) -> WebResult<Json<Vec<Point3>>> {
  Ok(Json(service.movement_options(scene_id, creature_id).await?))
}

async fn combat_movement_options(
  Extension(service): Extension<Arc<GMService>>,
) -> WebResult<Json<Vec<Point3>>> {
  Ok(Json(service.combat_movement_options().await?))
}

async fn player_movement_options(
  Extension(service): Extension<Arc<PlayerService>>,
  Path((_game_id, scene_id, creature_id)): Path<(String, SceneID, CreatureID)>,
) -> WebResult<Json<Vec<Point3>>> {
  Ok(Json(service.movement_options(scene_id, creature_id).await?))
}

async fn target_options(
  Extension(service): Extension<Arc<GMService>>,
  Path((_game_id, scene_id, creature_id, ability_id)): Path<(
    String,
    SceneID,
    CreatureID,
    AbilityID,
  )>,
) -> WebResult<Json<PotentialTargets>> {
  Ok(Json(service.target_options(scene_id, creature_id, ability_id).await?))
}

async fn preview_volume_targets(
  Extension(service): Extension<Arc<GMService>>,
  Path((_game_id, scene_id, creature_id, ability_id, x, y, z)): Path<(
    String,
    SceneID,
    CreatureID,
    AbilityID,
    i64,
    i64,
    i64,
  )>,
) -> WebResult<Json<(Vec<CreatureID>, Vec<Point3>)>> {
  let point = Point3::new(x, y, z);
  let targets = service.preview_volume_targets(scene_id, creature_id, ability_id, point).await?;
  Ok(Json(targets))
}

// #[derive(serde::Deserialize)]
// struct LoadIntoFolderPath {

//   path: String
// }

// async fn load_into_folder(service: web::Data<AuthenticatableService>, route: web::Path<(String, String)>, query: web::Query<LoadIntoFolderPath>) -> impl Responder {
//   let source_string = route.0.as_ref();
//   let source = match source_string {
//     "saved_game" => ModuleSource::SavedGame,
//     "module" => ModuleSource::Module,
//     _ => return string_json_response(format!("{{'error': 'bad source {source_string}'}}"))
//   };
//   let name = route.1.clone();
//   println!("Trying to parse {}: {:?}", &query.path, query.path.parse::<foldertree::FolderPath>());
//   let path: foldertree::FolderPath = query.path.parse::<foldertree::FolderPath>()?;
//   println!("Loading {source:?} {name} at {path}");
//   string_json_response(service.load_into_folder(source, name, path).await?)
// }

// async fn load_module_as_game(
//   service: web::Data<AuthenticatableService>, path: web::Path<String>,
// ) -> impl Responder {
//   string_json_response(service.load_saved_game(&path.into_inner(), ModuleSource::Module).await?)
// }

// async fn save_module(
//   service: web::Data<AuthenticatableService>, path: web::Path<String>,
//   folder_path: web::Json<::foldertree::FolderPath>,
// ) -> impl Responder {
//   string_json_response(service.save_module(path.into_inner(), folder_path.into_inner()).await?)
// }

// Make our own error that wraps `anyhow::Error`.
struct WebError(anyhow::Error);

type WebResult<T> = std::result::Result<T, WebError>;

// Tell axum how to convert `WebError` into a response.
// We could theoretically downcast the error to return different status codes.
impl IntoResponse for WebError {
  fn into_response(self) -> Response {
    println!("{:?}", self.0);
    (StatusCode::INTERNAL_SERVER_ERROR, format!("Something went wrong: {:?}", self.0))
      .into_response()
  }
}

// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
// `Result<_, WebError>`. That way you don't need to do that manually.
impl<E> From<E> for WebError
where
  E: Into<anyhow::Error>,
{
  fn from(err: E) -> Self { Self(err.into()) }
}
