#![feature(test)]
//! Phone and Tablet.
#[macro_use]
extern crate derive_more;
extern crate serde_yaml;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate odds;
extern crate string_wrapper;
extern crate pathfinding;
extern crate num_traits;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
extern crate serde_json;

#[cfg(test)]
extern crate test;

extern crate rand;

extern crate nonempty;

pub mod game;
pub mod app;
pub mod combat;
pub mod creature;
pub mod grid;
pub mod types;
