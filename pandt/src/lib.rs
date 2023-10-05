#![recursion_limit = "256"]

//! Phone and Tablet.

// I don't know why I still need to use old-style extern crate for uom.
// In types.rs we use "ISQ!", but replacing this `extern crate` with
// `use uom::ISQ` does not work.
#[macro_use]
extern crate uom;

pub mod combat;
pub mod creature;
pub mod game;
pub mod grid;
pub mod scene;
pub mod types;
