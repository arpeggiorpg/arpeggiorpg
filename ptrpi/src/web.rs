use std::sync::Arc;

use anyhow::{anyhow, Context};
use axum::{
  extract::{Path, State},
  http::Request,
  middleware::{from_fn, from_fn_with_state, Next},
  response::{IntoResponse, Response},
  routing::{get, post},
  Extension, Json, body,
};
use http::StatusCode;
use log::error;
use tower_http::{cors::CorsLayer, trace::TraceLayer};

use pandt::types::{
  AbilityID, CreatureID, GameCommand, Point3, PotentialTargets, RPIGame, SceneID,
};

use crate::{
  actor::{AuthenticatableService, AuthenticatedService, GameService},
  types::{GameID, GameIndex, GameList},
};

pub fn router(service: Arc<AuthenticatableService>) -> axum::Router {
  let game_routes = axum::Router::new()
    .route("/", get(get_game))
    .route("/poll/:game_idx/:log_idx", get(poll_game))
    .route("/execute", post(execute))
    .route("/movement_options/:scene_id/:cid", get(movement_options))
    .route("/combat_movement_options", get(combat_movement_options))
    .route("/target_options/:scene_id/:cid/:abid", get(target_options))
    .route(
      "/preview_volume_targets/:scene_id/:service_id/:ability_id/:x/:y/:z",
      get(preview_volume_targets),
    )
    .route_layer(from_fn(authorize_game));

  let auth_routes = axum::Router::new()
    .route("/list", get(list_games))
    .route("/create", post(create_game))
    .nest("/:game_id/gm/", game_routes)
    .route_layer(from_fn_with_state(service.clone(), authenticate));

  let cors = CorsLayer::permissive();
  let trace = TraceLayer::new_for_http();
  return axum::Router::new().nest("/g", auth_routes).with_state(service).layer(cors).layer(trace);
}

#[derive(serde::Deserialize)]
struct GameIDPath {
  game_id: GameID,
}

async fn authenticate<B>(
  State(service): State<Arc<AuthenticatableService>>, mut request: Request<B>, next: Next<B>,
) -> Result<Response, WebError> {
  let header =
    request.headers().get("x-pt-rpi-auth").ok_or(anyhow!("Need a x-pt-rpi-auth header"))?;
  let id_token = header.to_str()?;
  // TODO: we should specifically handle the case where the token is valid but expired and return a
  // special response so the client can refresh the token.
  let authenticated_result = service.authenticate(id_token.to_string()).await;
  match authenticated_result {
    Ok(authenticated) => {
      request.extensions_mut().insert(Arc::new(authenticated));
      Ok(next.run(request).await.into())
    }
    Err(_) => {
      let body = body::boxed("auth failed".to_string());
      let response = Response::builder().status(401).body(body)?;
      Ok(response)
    }
  }

}

async fn authorize_game<B>(
  Path(GameIDPath { game_id }): Path<GameIDPath>,
  Extension(authenticated): Extension<Arc<AuthenticatedService>>, mut request: Request<B>,
  next: Next<B>,
) -> Result<Response, WebError> {
  // TODO: return a 404 when the game doesn't exist.
  let gm = authenticated.gm(&game_id).await?;
  request.extensions_mut().insert(Arc::new(gm));
  Ok(next.run(request).await.into())
}

async fn list_games(
  Extension(service): Extension<Arc<AuthenticatedService>>,
) -> WebResult<Json<GameList>> {
  let games = service.list_games().await?;
  Ok(Json(games))
}

async fn create_game(
  Extension(service): Extension<Arc<AuthenticatedService>>, Json(name): Json<String>,
) -> WebResult<Json<serde_json::Value>> {
  let game_id = service.new_game(name.to_string()).await?;
  let json = serde_json::json!({"game_id": game_id});
  Ok(Json(json))
}

async fn get_game(
  Extension(service): Extension<Arc<GameService>>,
  Path(GameIDPath { game_id }): Path<GameIDPath>
) -> WebResult<Json<serde_json::Value>> {
  Ok(Json(_get_game(service, game_id).await?))
}

async fn poll_game(
  Extension(service): Extension<Arc<GameService>>,
  Path((game_id, game_idx, log_idx)): Path<(GameID, usize, usize)>,
) -> WebResult<Json<serde_json::Value>> {
  service.poll_game(GameIndex { game_idx, log_idx }).await?;
  Ok(Json(_get_game(service, game_id).await?))
}

async fn _get_game(service: Arc<GameService>, game_id: GameID) -> WebResult<serde_json::Value> {
  let game = RPIGame(&service.game);
  Ok(serde_json::json!({
    "game": game,
    "index": service.game_index,
    "metadata": service.storage.get_game_metadata(&game_id).await?
  }))
}

async fn execute(
  Extension(service): Extension<Arc<GameService>>, Json(command): Json<GameCommand>,
) -> WebResult<Json<std::result::Result<serde_json::Value, String>>> {
  // Note that this function actually serializes the result from calling
  // perform_command into the JSON response -- there are no "?" operators here!
  let changed_game = service.perform_command(command).await;
  let changed_game = changed_game
    .map(|cg| serde_json::json!({"game": RPIGame(&cg.game), "logs": cg.logs}))
    .map_err(|e| format!("{e:?}"));
  Ok(Json(changed_game))
}

async fn movement_options(
  Extension(service): Extension<Arc<GameService>>,
  Path((_game_id, scene_id, creature_id)): Path<(String, SceneID, CreatureID)>,
) -> WebResult<Json<Vec<Point3>>> {
  Ok(Json(service.movement_options(scene_id, creature_id).await?))
}

async fn combat_movement_options(
  Extension(service): Extension<Arc<GameService>>,
) -> WebResult<Json<Vec<Point3>>> {
  Ok(Json(service.combat_movement_options().await?))
}

async fn target_options(
  Extension(service): Extension<Arc<GameService>>,
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
  Extension(service): Extension<Arc<GameService>>,
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
    (StatusCode::INTERNAL_SERVER_ERROR, format!("Something went wrong: {}", self.0)).into_response()
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
