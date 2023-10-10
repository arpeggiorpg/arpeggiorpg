#[macro_use]
extern crate uom;
use uom::si::length::{centimeter, meter};

mod i64units {
  ISQ!(uom::si, i64, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

// use units;

fn i64cm(v: i64) -> i64units::Length { i64units::Length::new::<centimeter>(v) }

fn main() {
  println!("101 cm in meters: {:?}", i64cm(101).get::<meter>());
  println!("99cm in meters: {:?}", i64cm(99).get::<meter>());
  println!("-101 cm in meters: {:?}", i64cm(-101).get::<meter>());
  println!("-99cm in meters: {:?}", i64cm(-99).get::<meter>());
}
