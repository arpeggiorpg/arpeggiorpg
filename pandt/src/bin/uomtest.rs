#[macro_use] extern crate uom;
use uom::si::length::{centimeter, meter};
use uom::si;

mod u64units {
  ISQ!(uom::si, u64, (centimeter, gram, second, ampere, kelvin, mole, candela));
}
mod i64units {
  ISQ!(uom::si, i64, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

// use units;

fn main() {
  let onecm: u64units::Length = u64units::Length::new::<centimeter>(1);
  let m: u64units::Length = u64units::Length::new::<meter>(1);
  //let r = cm + m;
  println!("int 1m: {:?}", m);
  println!("int 1cm: {:?}", onecm);
  //println!("added: {:?}", r);
  
  let fcm: si::f32::Length = si::f32::Length::new::<centimeter>(1.0);
  let fm: si::f32::Length = si::f32::Length::new::<meter>(1.0);
  println!("float 1m: {:?}", fm);
  println!("float 1cm: {:?}", fcm);
}
