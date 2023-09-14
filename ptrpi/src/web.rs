use std::fs;
use std::path::Path;

use actix_web::{web, HttpResponse, Responder};
use anyhow::Error;
use log::error;

use pandt::types::{AbilityID, CreatureID, GameCommand, ModuleSource, Point3, SceneID};

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
    .service(
      web::resource("preview_volume_targets/{scene_id}/{actor_id}/{ability_id}/{x}/{y}/{z}")
        .route(web::post().to(preview_volume_targets)),
    )
    .service(web::resource("saved_games").route(web::get().to(list_saved_games)))
    .service(
      web::resource("saved_games/module/{name}/load").route(web::post().to(load_module_as_game)),
    )
    .service(web::resource("saved_games/user/{name}/load").route(web::post().to(load_saved_game)))
    .service(web::resource("saved_games/user/{name}").route(web::post().to(save_game)))
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

async fn preview_volume_targets(
  actor: web::Data<AppActor>, path: web::Path<(SceneID, CreatureID, AbilityID, i64, i64, i64)>,
) -> impl Responder {
  let point = Point3::new(path.3, path.4, path.5);
  string_json_response(actor.preview_volume_targets(path.0, path.1, path.2, point).await?)
}

async fn list_saved_games(
  actor: web::Data<AppActor>,
) -> Result<web::Json<(Vec<String>, Vec<String>)>, Box<dyn ::std::error::Error>> {
  // This does not require access to the app, so we don't dispatch to the actor.

  fn list_dir_into_strings(path: &Path) -> Result<Vec<String>, Error> {
    let mut result = vec![];
    for mpath in fs::read_dir(path)? {
      let path = mpath?;
      if path.file_type()?.is_file() {
        match path.file_name().into_string() {
          Ok(s) => result.push(s),
          Err(x) => error!("Couldn't parse filename as unicode: {:?}", x),
        }
      }
    }
    Ok(result)
  }

  let modules = match actor.module_path {
    Some(ref path) => list_dir_into_strings(path.as_ref())?,
    None => vec![],
  };
  let result = (modules, list_dir_into_strings(&actor.saved_game_path)?);
  Ok(web::Json(result))
}

async fn load_saved_game(actor: web::Data<AppActor>, path: web::Path<String>) -> impl Responder {
  string_json_response(actor.load_saved_game(path.into_inner(), ModuleSource::SavedGame).await?)
}

async fn load_module_as_game(
  actor: web::Data<AppActor>, path: web::Path<String>,
) -> impl Responder {
  string_json_response(actor.load_saved_game(path.into_inner(), ModuleSource::Module).await?)
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
