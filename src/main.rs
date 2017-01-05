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

fn load_game() -> serde_yaml::Result<app::AppVari> {
    let mut gamefile = File::open("game.yaml").expect("Couldn't find game.yaml");
    let mut data = "".to_owned();
    let _ = gamefile.read_to_string(&mut data);
    serde_yaml::from_str(&data)
}

fn main() {
    match load_game() {
        Ok(app) => {
            println!("{:?}", app);
            println!("{}", serde_yaml::to_string(&app).unwrap());
        }
        Err(e) => println!("Sorry, error loading game file: {}", e),
    }
}
