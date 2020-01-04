#[macro_use]
extern crate log;

use std::env;
use std::fs;
use std::path::PathBuf;

use structopt::StructOpt;

use pandt::game::load_app_from_path;
use pandt::types::{App, ModuleSource};

mod actor;
mod web;

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
  if env::var("PANDT_LOG").is_err() {
    env::set_var("PANDT_LOG", "info");
  }
  let env = env_logger::Env::new().filter("PANDT_LOG").write_style("PANDT_LOG_STYLE");
  env_logger::init_from_env(env);

  info!("Starting up the P&T Remote Programming Interface HTTP server!");

  let opts = Opts::from_args();
  let saved_game_path =
    fs::canonicalize(opts.saved_game_path).expect("Couldn't canonicalize game dir");
  info!("Saved game directory is {}", saved_game_path.to_str().unwrap());
  let module_path =
    opts.module_path.map(|p| fs::canonicalize(p).expect("Couldn't canonicalize module dir"));

  let app = match opts.load_game {
    Some(initial_file) => {
      load_app_from_path(&saved_game_path, None, ModuleSource::SavedGame, &initial_file)
        .expect("Couldn't load app from file")
    }
    None => App::new(Default::default()),
  };

  let pt = actor::PT::new(saved_game_path, module_path, app);
  let mut tide_app = tide::with_state(pt);
  tide_app.at("/").nest(web::router);
  tide_app.listen("127.0.0.1:1337").await?;
  Ok(())
}

#[derive(Debug, StructOpt)]
#[structopt(name = "ptrpi")]
/// The P&T Remote Programming Interface HTTP server
struct Opts {
  /// The directory where saved games should be stored
  #[structopt(long = "saved-games", parse(from_os_str))]
  saved_game_path: PathBuf,

  /// The directory where read-only modules should be loaded from
  #[structopt(long = "modules", parse(from_os_str))]
  module_path: Option<PathBuf>,

  #[structopt(long = "load-game")]
  load_game: Option<String>,
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use pandt::types::ModuleSource;

  #[test]
  fn load_samplegame_yaml() {
    ::load_app_from_path(
      Path::new("sample_games"),
      None,
      ModuleSource::SavedGame,
      "samplegame.yaml",
    )
    .unwrap();
  }
}
