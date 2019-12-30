#[macro_use]
extern crate log;
use async_std::prelude::*;

// mod actor;
// mod web;

use std::env;
// use std::fs;
use std::path::PathBuf;

use structopt::StructOpt;

use pandt::game::load_app_from_path;
use pandt::types::{App, ModuleSource};


// #[derive(Clone)]
// pub struct PT {
//   saved_game_path: PathBuf,
//   module_path: Option<PathBuf>,
//   app_address: AppAddress,
// }

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
  if env::var("PANDT_LOG").is_err() {
    env::set_var("PANDT_LOG", "info");
  }
  let env = env_logger::Env::new().filter("PANDT_LOG").write_style("PANDT_LOG_STYLE");
  env_logger::init_from_env(env);

  info!("Starting up the P&T Remote Programming Interface HTTP server!");

  let mut app = tide::new();
  app.at("/").get(|_| async move { "Hello, world!" });
  app.listen("127.0.0.1:1337").await?;
  Ok(())
  // let opts = Opts::from_args();
  // let saved_game_path =
  //   fs::canonicalize(opts.saved_game_path).expect("Couldn't canonicalize game dir");
  // let module_path =
  //   opts.module_path.map(|p| fs::canonicalize(p).expect("Couldn't canonicalize module dir"));

  // let app = match opts.load_game {
  //   Some(initial_file) => {
  //     load_app_from_path(&saved_game_path, None, ModuleSource::SavedGame, &initial_file)
  //       .expect("Couldn't load app from file")
  //   }
  //   None => App::new(Default::default()),
  // };

  // let pt = PT { saved_game_path, module_path, app_address };
}

#[derive(StructOpt)]
#[structopt(name = "basic")]
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
    ).unwrap();
  }
}
