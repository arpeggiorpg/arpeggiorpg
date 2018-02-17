#![recursion_limit = "256"]

//! Phone and Tablet.

extern crate bresenham;
#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate indexed;
#[macro_use]
extern crate log;
extern crate nalgebra;
extern crate ncollide;
extern crate nonempty;
extern crate num;
extern crate num_traits;
extern crate odds;
extern crate rand;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
#[macro_use]
extern crate uom;
extern crate uuid;

#[cfg(test)]
#[macro_use]
extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate serde_json;

pub mod app;
pub mod combat;
pub mod creature;
pub mod foldertree;
pub mod game;
pub mod grid;
pub mod scene;
pub mod types;
