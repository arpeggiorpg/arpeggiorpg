#![recursion_limit = "256"]
#![feature(conservative_impl_trait)]
#![feature(test)]
//! Phone and Tablet.

extern crate bresenham;
#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate error_chain;
extern crate nalgebra;
extern crate ncollide;
extern crate nonempty;
extern crate num_traits;
extern crate odds;
extern crate pathfinding;
extern crate rand;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[cfg(test)]
#[macro_use]
extern crate serde_json;
extern crate serde_yaml;
extern crate string_wrapper;
#[cfg(test)]
extern crate test;
extern crate uuid;

pub mod app;
pub mod combat;
pub mod creature;
pub mod foldertree;
pub mod game;
pub mod grid;
pub mod indexed;
pub mod types;
