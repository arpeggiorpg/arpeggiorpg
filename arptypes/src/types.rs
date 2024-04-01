use derive_more::{Add, Div, Mul, Sub};
use indexed::DeriveKey;
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

#[derive(Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct AttrID(pub String);

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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize, TS)]
pub enum SkillLevel {
  // The way to read these are:
  // "A {variant} person has a 75% chance of doing this."
  // If you're unskilled and you are doing a Skilled difficulty challenge: 50%?
  // Trivial to Expert: 10%?
  Inept,
  Unskilled,
  Skilled,
  Expert,
  Supernatural,
}

impl SkillLevel {
  pub fn to_ord(&self) -> i8 {
    match *self {
      SkillLevel::Inept => -1,
      SkillLevel::Unskilled => 0,
      SkillLevel::Skilled => 1,
      SkillLevel::Expert => 2,
      SkillLevel::Supernatural => 3,
    }
  }

  pub fn difficulty(&self, difficulty_level: SkillLevel) -> u8 {
    100
      - match difficulty_level.to_ord() - self.to_ord() {
        -4 => 100,
        -3 => 99,
        -2 => 95,
        -1 => 85,
        0 => 75,
        1 => 50,
        2 => 10,
        3 => 1,
        4 => 0,
        diff => panic!("[SkillLevel::difficulty] Two skill levels were too far apart: {:?}", diff),
      }
  }
}

/// Serializes as either "Interminate" or {"Rounds": 0}
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, TS)]
pub enum Duration {
  Interminate,
  Rounds(u8),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize, TS)]
pub struct SceneCreation {
  pub name: String,
  pub background_image_url: String,
  pub background_image_offset: Option<(i32, i32)>,
  pub background_image_scale: (f64, f64),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, TS, Default)]
pub enum TileSystem {
  /// Square grid with diagonal movement costing 1.41
  #[default]
  Realistic,
  /// Square grid with diagonal movement costing 1
  DnD,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, TS)]
pub struct Note {
  pub name: String,
  pub content: String,
}

impl DeriveKey for Note {
  type KeyType = String;
  fn derive_key(&self) -> String { self.name.clone() }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize, TS)]
pub enum Visibility {
  GMOnly,
  AllPlayers,
}
