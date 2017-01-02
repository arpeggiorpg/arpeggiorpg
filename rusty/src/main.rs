#![feature(proc_macro)]
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate serde;

use std::fs::File;
use std::io::Read;

mod app;
mod types;
mod nonempty;

fn load_json() -> serde_json::error::Result<app::App> {
    let mut gamefile = File::open("game.json").unwrap();
    let mut data = "".to_owned();
    let _ = gamefile.read_to_string(&mut data);
    serde_json::from_str(&data)
}

fn main() {
    match load_json() {
        Ok(mut app) => {
            println!("{:?}", app);

            let r = app.act("Punch".to_string(), vec![1]);
            println!("Result of choosing ability: {:?}", r);
            let r = app.act("Punch".to_string(), vec![1]);
            println!("Result of choosing ability: {:?}", r);
            println!("Current json: {}",
                     serde_json::to_string_pretty(&app).unwrap());
        }
        Err(e) => println!("Sorry, error loading json: {}", e),
    }
}
