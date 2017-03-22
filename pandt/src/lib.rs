#![recursion_limit="256"]
#![feature(conservative_impl_trait)]
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
extern crate uuid;
#[macro_use]
extern crate error_chain;

#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
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
pub mod indexed;
pub mod foldertree;
