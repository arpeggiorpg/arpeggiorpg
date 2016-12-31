#![feature(proc_macro)]
extern crate pandt;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use std::fs::File;
use std::io::Read;

mod types;

fn main() {
    let game1: types::Game = {
        let mut gamefile = File::open("game.json").unwrap();
        let mut data = "".to_owned();
        let _ = gamefile.read_to_string(&mut data);
        serde_json::from_str(&data).unwrap()
    };
    println!("{:?}", game1);
}
