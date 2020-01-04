use std::fs;
use std::path::Path;

use http::status::StatusCode;
use tide::{IntoResponse, Request, Response, Route};
// use failure::Error;
// use futures::Future;
// use http::{header, Method};

// use pandt::types::{CreatureID, GameCommand, ModuleSource, Point3, SceneID};

use crate::actor::PT;

pub(crate) fn router<'a>(router: &mut Route<'a, PT>) -> () { router.at("/").get(get_app); }

fn r500<S: IntoResponse>(result: Result<S, failure::Error>) -> Response {
  match result {
    Ok(s) => s.into_response(),
    Err(e) => {
      error!("Error: {}", e);
      Response::new(500).body_string(format!("{}", e))
    }
  }
}

async fn get_app(req: Request<PT>) -> impl IntoResponse {
  let state = req.state();
  r500(state.get_app().await)
}

// pub fn router(pt: PT) -> Application<PT> {
//   let mut corsm = cors::Cors::build();
//   corsm.send_wildcard().allowed_header(header::CONTENT_TYPE);
//   let corsm = corsm.finish().unwrap();

//   Application::with_state(pt)
//     .middleware(corsm)
//     .resource("/", |r| {
//       r.method(Method::GET).f(get_app);
//       r.method(Method::POST).f(post_app);
//     })
//     .resource("/poll/{snapshot_len}/{log_len}", |r| r.route().f(poll_app))
//     .resource("/movement_options/{scene_id}/{cid}", |r| r.route().f(movement_options))
//     .resource("/combat_movement_options", |r| r.route().f(combat_movement_options))
//     .resource("/target_options/{scene_id}/{cid}/{abid}", |r| r.route().f(target_options))
//     .resource("/preview_volume_targets/{scene_id}/{actor_id}/{ability_id}/{x}/{y}/{z}", |r| {
//       r.f(preview_volume_targets)
//     })
//     .resource("/saved_games", |r| r.f(list_saved_games))
//     .resource("/saved_games/module/{name}/load", |r| r.method(Method::POST).f(load_module_as_game))
//     .resource("/saved_games/user/{name}/load", |r| r.method(Method::POST).f(load_saved_game))
//     .resource("/saved_games/user/{name}", |r| r.method(Method::POST).f(save_game))
//     .resource("/modules/{name}", |r| r.method(Method::POST).f(save_module))
//     .resource("/new_game", |r| r.method(Method::POST).f(new_game))
// }

// /// If the client is polling with a non-current app "version", then immediately return the current
// /// App. Otherwise, wait 30 seconds for any new changes.
// fn poll_app(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let snapshot_len: usize = try_fut!(get_arg(&req, "snapshot_len"));
//   let log_len: usize = try_fut!(get_arg(&req, "log_len"));
//   invoke_actor_string_result(&req.state().app_address, actor::PollApp { snapshot_len, log_len })
// }

// fn post_app(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let app_address = req.state().app_address.clone();
//   let f = req.json().from_err().and_then(move |command: GameCommand| -> AsyncRPIResponse {
//     info!("[perform_command] {:?}", command);
//     invoke_actor_string_result(&app_address, actor::PerformCommand(command))
//   });
//   Box::new(f)
// }

// fn invoke_actor_string_result<M>(address: &::AppAddress, msg: M) -> AsyncRPIResponse
// where
//   actor::AppActor: actix::Handler<M>,
//   M: actix::Message<Result = Result<String, Error>> + Send + 'static,
// {
//   let fut = address.send(msg).from_err().and_then(|s| s).and_then(string_json_response);
//   // I should not need to box this result, but it is too hard to write the type of the return
//   // value
//   Box::new(fut)
// }

// fn movement_options(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let creature_id: CreatureID = try_fut!(parse_arg(&req, "cid"));
//   let scene_id: SceneID = try_fut!(parse_arg(&req, "scene_id"));
//   invoke_actor_string_result(
//     &req.state().app_address,
//     actor::MovementOptions { creature_id, scene_id },
//   )
// }

// fn combat_movement_options(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   invoke_actor_string_result(&req.state().app_address, actor::CombatMovementOptions)
// }

// fn target_options(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let scene_id = try_fut!(parse_arg(&req, "scene_id"));
//   let creature_id = try_fut!(parse_arg(&req, "cid"));
//   let ability_id = try_fut!(parse_arg(&req, "abid"));
//   invoke_actor_string_result(
//     &req.state().app_address,
//     actor::TargetOptions { scene_id, creature_id, ability_id },
//   )
// }

// fn preview_volume_targets(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let scene_id = try_fut!(parse_arg(&req, "scene_id"));
//   let actor_id = try_fut!(parse_arg(&req, "actor_id"));
//   let ability_id = try_fut!(parse_arg(&req, "ability_id"));
//   let x = try_fut!(get_arg(&req, "x"));
//   let y = try_fut!(get_arg(&req, "y"));
//   let z = try_fut!(get_arg(&req, "z"));
//   let point = Point3::new(x, y, z);

//   invoke_actor_string_result(
//     &req.state().app_address,
//     actor::PreviewVolumeTargets { scene_id, actor_id, ability_id, point },
//   )
// }

// fn list_saved_games(req: HttpRequest<PT>) -> Result<Json<(Vec<String>, Vec<String>)>, Error> {
//   // This does not require access to the app, so we don't dispatch to the actor.
//   fn list_dir_into_strings(path: &Path) -> Result<Vec<String>, Error> {
//     let mut result = vec![];
//     for mpath in fs::read_dir(path)? {
//       let path = mpath?;
//       if path.file_type()?.is_file() {
//         match path.file_name().into_string() {
//           Ok(s) => result.push(s),
//           Err(x) => error!("Couldn't parse filename as unicode: {:?}", x),
//         }
//       }
//     }
//     return Ok(result);
//   }
//   let modules = match req.state().module_path {
//     Some(ref path) => list_dir_into_strings(path)?,
//     None => vec![],
//   };
//   let result = (modules, list_dir_into_strings(&req.state().saved_game_path)?);
//   Ok(Json(result))
// }

// fn load_saved_game(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let name: String = try_fut!(get_arg(&req, "name"));
//   invoke_actor_string_result(
//     &req.state().app_address,
//     actor::LoadSavedGame { name, source: ModuleSource::SavedGame },
//   )
// }

// fn load_module_as_game(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let name: String = try_fut!(get_arg(&req, "name"));
//   invoke_actor_string_result(
//     &req.state().app_address,
//     actor::LoadSavedGame { name, source: ModuleSource::Module },
//   )
// }

// fn save_game(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let name: String = try_fut!(get_arg(&req, "name"));
//   invoke_actor_string_result(&req.state().app_address, actor::SaveGame(name))
// }

// fn save_module(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   let name: String = try_fut!(get_arg(&req, "name"));
//   let app_address = req.state().app_address.clone();
//   let f = req.json().from_err().and_then(move |path| -> AsyncRPIResponse {
//     invoke_actor_string_result(&app_address, actor::SaveModule { name, path })
//   });
//   Box::new(f)
// }

// fn new_game(req: HttpRequest<PT>) -> AsyncRPIResponse {
//   invoke_actor_string_result(&req.state().app_address, actor::NewGame)
// }

// fn string_json_response(body: String) -> Result<HttpResponse, Error> {
//   Ok(HttpResponse::Ok().content_type("application/json").body(body)?)
// }

// fn get_arg<T>(req: &HttpRequest<PT>, key: &str) -> Result<T, Error>
// where
//   T: FromParam,
// {
//   Ok(req.match_info().query::<T>(key)?)
// }

// fn parse_arg<T>(req: &HttpRequest<PT>, key: &str) -> Result<T, Error>
// where
//   T: ::std::str::FromStr,
//   Error: From<<T as ::std::str::FromStr>::Err>, // I dunno man
// {
//   let s = req.match_info().query::<String>(key);
//   Ok(s.map_err(|e| format_err!("Failed to parse an argument: {:?} ({:?})", key, e))?.parse()?)
// }
