extern crate pandt;
extern crate serde_json;

use std::fs::File;
use std::io::Read;

fn main() {
    let game1: pandt::Game = {
        let mut gamefile = File::open("game.json").unwrap();
        let mut data = "".to_owned();
        let _ = gamefile.read_to_string(&mut data);
        serde_json::from_str(&data).unwrap()
    };
    println!("{:?}", game1);
}
