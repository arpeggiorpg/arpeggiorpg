#![recursion_limit = "256"]
#![feature(vec_remove_item, conservative_impl_trait)]
#![cfg_attr(test, feature(test))]

//! Phone and Tablet.

extern crate bresenham;
#[macro_use]
extern crate derive_more;
#[macro_use]
extern crate error_chain;
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate nalgebra;
extern crate ncollide;
extern crate nonempty;
extern crate num_traits;
extern crate odds;
extern crate rand;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
extern crate uuid;

#[cfg(test)]
#[macro_use]
extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate serde_json;
#[cfg(test)]
extern crate test;

pub mod app;
pub mod combat;
pub mod creature;
pub mod foldertree;
pub mod game;
pub mod grid;
pub mod indexed;
pub mod scene;
pub mod types;
