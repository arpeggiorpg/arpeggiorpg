#![feature(proc_macro)]
//! Phone and Tablet.

extern crate serde_yaml;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate odds;

extern crate nonempty;

use std::fs::File;
use std::io::Read;

pub mod app;
pub mod types;

fn load_game() -> serde_yaml::Result<app::ActorApp> {
    let mut gamefile = File::open("game.yaml").expect("Couldn't find game.yaml");
    let mut data = "".to_owned();
    let _ = gamefile.read_to_string(&mut data);
    serde_yaml::from_str(&data)
}

fn aapp(app: app::ActorApp) -> app::App<types::Able> {
    match app {
        app::ActorApp::Able(a) => a,
        _ => panic!(),
    }
}

fn main() {
    match load_game() {
        Ok(app) => {
            println!("{:?}", app);
            let app = aapp(app);
            let app = aapp(app.act(types::AbilityID("punch".to_string()), vec![1]).unwrap());
            println!("Result of choosing ability: {:?}", app);
            let app = aapp(app.act(types::AbilityID("punch".to_string()), vec![1]).unwrap());
            println!("Result of choosing ability: {:?}", app);
            let app = aapp(app.act(types::AbilityID("layonhands".to_string()), vec![0]).unwrap());
            println!("Result of choosing ability: {:?}", app);
            let app = aapp(app.act(types::AbilityID("layonhands".to_string()), vec![1]).unwrap());
            println!("Result of choosing ability: {:?}", app);
            println!("YAML: App");
            println!("{}", serde_yaml::to_string(&app).unwrap());
        }
        Err(e) => println!("Sorry, error loading game file: {}", e),
    }
}
