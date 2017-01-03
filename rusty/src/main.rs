#![feature(proc_macro)]
extern crate serde_yaml;
#[macro_use]
extern crate serde_derive;
extern crate serde;

extern crate nonempty;

use std::fs::File;
use std::io::Read;

mod app;
mod types;

fn load_game() -> serde_yaml::Result<app::App> {
    let mut gamefile = File::open("game.yaml").expect("Couldn't find game.yaml");
    let mut data = "".to_owned();
    let _ = gamefile.read_to_string(&mut data);
    serde_yaml::from_str(&data)
}

fn main() {
    match load_game() {
        Ok(mut app) => {
            println!("{:?}", app);

            let r = app.act(types::AbilityID("punch".to_string()), vec![1]);
            println!("Result of choosing ability: {:?}", r);
            let r = app.act(types::AbilityID("punch".to_string()), vec![1]);
            println!("Result of choosing ability: {:?}", r);
            let r = app.act(types::AbilityID("layonhands".to_string()), vec![0]);
            println!("Result of choosing ability: {:?}", r);
            let r = app.act(types::AbilityID("layonhands".to_string()), vec![1]);
            println!("Result of choosing ability: {:?}", r);
            println!("YAML: App");
            println!("{}", serde_yaml::to_string(&app).unwrap());
        }
        Err(e) => println!("Sorry, error loading game file: {}", e),
    }
}
