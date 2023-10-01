use actix_web::{web, HttpResponse, Responder};

use pandt::types::{AbilityID, CreatureID, GameCommand, ModuleSource, SceneID};

use crate::actor::AppActor;

pub fn router(actor: AppActor, config: &mut web::ServiceConfig) {
  config
    .app_data(web::Data::new(actor))
    .service(web::resource("/").route(web::get().to(get_app)).route(web::post().to(post_command)))
    .route("poll/{snapshot_len}/{log_len}", web::get().to(poll_app))
    .service(
      web::resource("movement_options/{scene_id}/{cid}").route(web::get().to(movement_options)),
    )
    .service(web::resource("combat_movement_options").route(web::get().to(combat_movement_options)))
    .service(
      web::resource("target_options/{scene_id}/{cid}/{abid}").route(web::get().to(target_options)),
    )
    .service(web::resource("saved_games").route(web::get().to(list_saved_games)))
    .service(
      web::resource("saved_games/module/{name}/load").route(web::post().to(load_module_as_game)),
    )
    .service(web::resource("saved_games/user/{name}/load").route(web::post().to(load_saved_game)))
    .service(web::resource("saved_games/user/{name}").route(web::post().to(save_game)))
    .service(web::resource("saved_games/{source}/{name}/load_into").route(web::post().to(load_into_folder)))
    .service(web::resource("modules/{name}").route(web::post().to(save_module)))
    .service(web::resource("new_game").route(web::post().to(new_game)));
}

async fn get_app(actor: web::Data<AppActor>) -> impl Responder {
  string_json_response(actor.get_app().await?)
}

async fn poll_app(actor: web::Data<AppActor>, path: web::Path<(usize, usize)>) -> impl Responder {
  string_json_response(actor.poll_app(path.0, path.1).await?)
}

async fn post_command(
  actor: web::Data<AppActor>, command: web::Json<GameCommand>,
) -> impl Responder {
  string_json_response(actor.perform_command(command.into_inner()).await?)
}

async fn movement_options(
  actor: web::Data<AppActor>, path: web::Path<(SceneID, CreatureID)>,
) -> impl Responder {
  string_json_response(actor.movement_options(path.0, path.1).await?)
}

async fn combat_movement_options(actor: web::Data<AppActor>) -> impl Responder {
  string_json_response(actor.combat_movement_options().await?)
}

async fn target_options(
  actor: web::Data<AppActor>, path: web::Path<(SceneID, CreatureID, AbilityID)>,
) -> impl Responder {
  string_json_response(actor.target_options(path.0, path.1, path.2).await?)
}

async fn list_saved_games(actor: web::Data<AppActor>) -> Result<web::Json<(Vec<String>, Vec<String>)>, Box<dyn ::std::error::Error>> {
  Ok(web::Json(actor.list_saved_games().await?))
}

async fn load_saved_game(actor: web::Data<AppActor>, path: web::Path<String>) -> impl Responder {
  string_json_response(actor.load_saved_game(&path.into_inner(), ModuleSource::SavedGame).await?)
}


#[derive(serde::Deserialize)]
struct LoadIntoFolderPath {

  path: String
}

async fn load_into_folder(actor: web::Data<AppActor>, route: web::Path<(String, String)>, query: web::Query<LoadIntoFolderPath>) -> impl Responder {
  let source_string = route.0.as_ref();
  let source = match source_string {
    "saved_game" => ModuleSource::SavedGame,
    "module" => ModuleSource::Module,
    _ => return string_json_response(format!("{{'error': 'bad source {source_string}'}}"))
  };
  let name = route.1.clone();
  println!("Trying to parse {}: {:?}", &query.path, query.path.parse::<foldertree::FolderPath>());
  let path: foldertree::FolderPath = query.path.parse::<foldertree::FolderPath>()?;
  println!("Loading {source:?} {name} at {path}");
  string_json_response(actor.load_into_folder(source, name, path).await?)
}

async fn load_module_as_game(
  actor: web::Data<AppActor>, path: web::Path<String>,
) -> impl Responder {
  string_json_response(actor.load_saved_game(&path.into_inner(), ModuleSource::Module).await?)
}

async fn save_game(actor: web::Data<AppActor>, path: web::Path<String>) -> impl Responder {
  string_json_response(actor.save_game(path.into_inner()).await?)
}

async fn save_module(
  actor: web::Data<AppActor>, path: web::Path<String>,
  folder_path: web::Json<::foldertree::FolderPath>,
) -> impl Responder {
  string_json_response(actor.save_module(path.into_inner(), folder_path.into_inner()).await?)
}

async fn new_game(actor: web::Data<AppActor>) -> impl Responder {
  string_json_response(actor.new_game().await?)
}

fn string_json_response(body: String) -> Result<HttpResponse, Box<dyn ::std::error::Error>> {
  Ok(HttpResponse::Ok().content_type("application/json").body(body))
}
