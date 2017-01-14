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

#[cfg(test)]
extern crate test;

extern crate nonempty;

pub mod game;
pub mod app;
pub mod combat;
pub mod creature;
pub mod grid;
pub mod types;
