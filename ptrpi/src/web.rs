use std::{sync::Arc, collections::HashMap};

use actix_web::{web, HttpMessage, HttpResponse, Responder, get, post, dev::{Service, ServiceRequest, ServiceResponse}, body::MessageBody, ResponseError, http::header};
use actix_web_lab::middleware::{Next, from_fn};
use anyhow::{anyhow, Context};
use http::StatusCode;
use log::error;

use pandt::types::{AbilityID, CreatureID, GameCommand, ModuleSource, Point3, SceneID};

use crate::{actor::{AuthenticatableService, GameService, AuthenticatedService}, types::{GameID}};

pub fn router(service:AuthenticatableService, config: &mut web::ServiceConfig) {
  config
    .app_data(web::Data::new(service))
    .service(
      web::scope("g").wrap(from_fn(add_authenticated_to_req))
      .service(list_games)
      .service(create_game)
      .service(
        web::scope("{game_id}").wrap(from_fn(add_game_to_req))
        .service(
          web::scope("gm").wrap_fn(|req, srv| {srv.call(req)})
          .service(get_game)
        )
        .service(web::scope("player").wrap_fn(|req, srv| {srv.call(req)}))
      )
    )
    // .route("games/{game_id}", web::get().to(get_game))
    // .route("poll/{snapshot_len}/{log_len}", web::get().to(poll_app))
    // .route("movement_options/{scene_id}/{cid}", web::get().to(movement_options))
    // .route("combat_movement_options", web::get().to(combat_movement_options))
    // .route("target_options/{scene_id}/{cid}/{abid}", web::get().to(target_options))
    // .route("preview_volume_targets/{scene_id}/{service_id}/{ability_id}/{x}/{y}/{z}",
    //        web::post().to(preview_volume_targets))
    // .route("saved_games/{source}/{name}/load_into", web::post().to(load_into_folder))
    ;
}

async fn add_authenticated_to_req(service: web::Data<AuthenticatableService>, request: ServiceRequest, next: Next<impl MessageBody>) -> Result<ServiceResponse<impl MessageBody>, actix_web::Error> {

  async fn bettererror(service: Arc<AuthenticatableService>, request: &ServiceRequest) -> anyhow::Result<()> {
    let header = request.headers().get("Authorization").ok_or(anyhow!("Need an Authorization header"))?;
    let id_token = header.to_str()?;
    let authenticated = service.authenticate(id_token.to_string()).await?;
    request.extensions_mut().insert(Arc::new(authenticated));
    Ok(())
  }
  bettererror(service.into_inner(), &request).await.map_err(MyError::AnyhowError)?;
  Ok(next.call(request).await?)
}

async fn add_game_to_req(path: web::Path<String>, authenticated: web::ReqData<Arc<AuthenticatedService>>, request: ServiceRequest, next: Next<impl MessageBody>) -> Result<ServiceResponse<impl MessageBody>, actix_web::Error> {

  async fn bettererror(request: &ServiceRequest, authenticated: Arc<AuthenticatedService>, game_id: String) -> anyhow::Result<()> {
    let game_id = game_id.parse().context("Parsing game_id as UUID")?;
    let gm = authenticated.gm(&game_id).await?;
    request.extensions_mut().insert(Arc::new(gm));
    Ok(())
  }
  bettererror(&request, authenticated.into_inner(), path.into_inner()).await.map_err(MyError::AnyhowError)?;
  Ok(next.call(request).await?)
}

#[get("/list")]
async fn list_games(service: web::ReqData<Arc<AuthenticatedService>>) -> impl Responder {
  let games = service.list_games().await?;
  let s = serde_json::to_string(&games)?;
  string_json_response(s)
}

#[post("/create")]
async fn create_game(service: web::ReqData<Arc<AuthenticatedService>>) -> impl Responder {
  let game_id = service.new_game().await?;
  let json = serde_json::json!({"game_id": game_id});
  string_json_response(serde_json::to_string(&json)?)
}


#[get("/")]
async fn get_game(service: web::ReqData<Arc<GameService>>) -> impl Responder {
  let response = serde_json::json!({
    "game": service.game,
    "index": service.game_index,
  });
  string_json_response(serde_json::to_string(&response)?)
}

// async fn poll_app(service: web::Data<AuthenticatableService>, path: web::Path<(usize, usize)>) -> impl Responder {
//   string_json_response(service.poll_app(path.0, path.1).await?)
// }

// async fn post_command(
//   service: web::Data<AuthenticatableService>, command: web::Json<GameCommand>,
// ) -> impl Responder {
//   string_json_response(service.perform_command(command.into_inner()).await?)
// }

// async fn movement_options(
//   service: web::Data<AuthenticatableService>, path: web::Path<(SceneID, CreatureID)>,
// ) -> impl Responder {
//   string_json_response(service.movement_options(path.0, path.1).await?)
// }

// async fn combat_movement_options(service: web::Data<AuthenticatableService>) -> impl Responder {
//   string_json_response(service.combat_movement_options().await?)
// }

// async fn target_options(
//   service: web::Data<AuthenticatableService>, path: web::Path<(SceneID, CreatureID, AbilityID)>,
// ) -> impl Responder {
//   string_json_response(service.target_options(path.0, path.1, path.2).await?)
// }

// async fn preview_volume_targets(
//   service: web::Data<AuthenticatableService>, path: web::Path<(SceneID, CreatureID, AbilityID, i64, i64, i64)>,
// ) -> impl Responder {
//   let point = Point3::new(path.3, path.4, path.5);
//   let targets = service.preview_volume_targets(path.0, path.1, path.2, point).await?;
//   string_json_response(serde_json::to_string(&targets)?)
// }

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

fn string_json_response(body: String) -> Result<HttpResponse, MyError> {
  Ok(HttpResponse::Ok().content_type("application/json").body(body))
}



#[derive(thiserror::Error, Debug)]
pub enum MyError {
    #[error("an unspecified internal error occurred: {0}")]
    AnyhowError(#[from] anyhow::Error),
    #[error("actix_web::Error: {0}")]
    ActixWebError(#[from] actix_web::Error),
    #[error("serde_json::Error: {0}")]
    SerdeJsonError(#[from] serde_json::Error)
}

impl ResponseError for MyError {

  fn status_code(&self) -> StatusCode {
    match &self {
        Self::ActixWebError(e) => e.as_response_error().status_code(),
        _ => StatusCode::INTERNAL_SERVER_ERROR,
    }
  }

  fn error_response(&self) -> HttpResponse {
    match &self {
      Self::ActixWebError(e) => e.error_response(),
      Self::AnyhowError(e) => HttpResponse::build(self.status_code()).append_header(header::ContentType::plaintext()).body(format!("{:?}", e)),
      Self::SerdeJsonError(e) => HttpResponse::build(self.status_code()).append_header(header::ContentType::plaintext()).body(format!("{:?}", e)),
    }
  }

}
