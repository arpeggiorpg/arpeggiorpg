use derive_more::{Add, Div, Mul, Sub};
use num::Saturating;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use uom::si::length::{centimeter, meter};

pub type Color = String;

pub mod u32units {
  ISQ!(uom::si, u32, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

pub fn u32cm(v: u32) -> u32units::Length { u32units::Length::new::<centimeter>(v) }
pub fn u32meter<T: Into<u32>>(v: T) -> u32units::Length { u32units::Length::new::<meter>(v.into()) }

pub mod i64units {
  ISQ!(uom::si, i64, (centimeter, gram, second, ampere, kelvin, mole, candela));
}

pub fn i64cm<T: Into<i64>>(v: T) -> i64units::Length {
  i64units::Length::new::<centimeter>(v.into())
}
pub fn i64meter<T: Into<i64>>(v: T) -> i64units::Length { i64units::Length::new::<meter>(v.into()) }

pub fn up_length(v: u32units::Length) -> i64units::Length { i64cm(v.get::<centimeter>()) }

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize, TS)]
pub struct PlayerID(pub String);

#[derive(
  Add,
  Sub,
  Mul,
  Div,
  Clone,
  Copy,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Debug,
  Hash,
  Serialize,
  Deserialize,
  TS,
)]
pub struct HP(pub u8);
impl Saturating for HP {
  fn saturating_add(self, other: Self) -> Self { HP(self.0.saturating_add(other.0)) }
  fn saturating_sub(self, other: Self) -> Self { HP(self.0.saturating_sub(other.0)) }
}

#[derive(
  Add,
  Sub,
  Mul,
  Div,
  Clone,
  Copy,
  Eq,
  PartialEq,
  Ord,
  PartialOrd,
  Debug,
  Hash,
  Serialize,
  Deserialize,
  TS,
)]
pub struct Energy(pub u8);
impl Saturating for Energy {
  fn saturating_add(self, other: Self) -> Self { Energy(self.0.saturating_add(other.0)) }
  fn saturating_sub(self, other: Self) -> Self { Energy(self.0.saturating_sub(other.0)) }
}
