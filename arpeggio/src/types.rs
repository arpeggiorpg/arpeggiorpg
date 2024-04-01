//! Simple types, with pure operations.

// Just disable large_enum_variant lints for now, since I'm not really that interested in fixing
// that for a while
#![cfg_attr(feature = "cargo-clippy", allow(clippy::large_enum_variant))]

use std::collections::HashMap;

use rand::Rng;
use serde::{
  ser::{Error as SerError, SerializeStruct},
  Serialize, Serializer,
};
use ts_rs::TS;

use indexed::IndexedHashMap;

pub use arptypes::types::*;

use crate::game::GameExt;

pub trait DiceExt {
  fn expr(n: u8, d: u8) -> Dice;

  fn flat(value: i8) -> Dice;

  fn plus(&self, d: Dice) -> Dice;

  /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
  /// final result.
  fn roll(&self) -> (Vec<i16>, i32);
}

impl DiceExt for Dice {
  fn expr(n: u8, d: u8) -> Dice { Dice::Expr { num: n, size: d } }

  fn flat(value: i8) -> Dice { Dice::Flat { value } }

  fn plus(&self, d: Dice) -> Dice { Dice::Plus(Box::new(self.clone()), Box::new(d)) }

  /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
  /// final result.
  fn roll(&self) -> (Vec<i16>, i32) {
    match *self {
      Dice::Expr { num, size } => {
        let mut intermediate = vec![];
        let mut result = 0i32;
        let mut rng = rand::thread_rng();

        for _ in 0..num {
          let val = rng.gen_range(1..i32::from(size) + 1);
          result += val;
          intermediate.push(val as i16);
        }
        (intermediate, result)
      }
      Dice::Flat { value } => (vec![i16::from(value)], i32::from(value)),
      Dice::Plus(ref l, ref r) => {
        let (mut intermediate, left_result) = l.roll();
        let (right_intermediate, right_result) = r.roll();
        intermediate.extend(right_intermediate);
        (intermediate, left_result + right_result)
      }
      Dice::BestOf(count, ref dice) => {
        if count == 0 {
          panic!("Sorry, can't roll best of 0.")
        }
        let (mut best_rolls, mut best_result) = dice.roll();
        for _ in 1..count {
          let (rolls, result) = dice.roll();
          if result > best_result {
            best_rolls = rolls;
            best_result = result;
          }
        }
        (best_rolls, best_result)
      }
    }
  }
}

pub type CollisionWorld = ::ncollide3d::world::CollisionWorld<f64, CollisionData>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum CollisionData {
  Creature(CreatureID),
  ConditionVolume(ConditionID),
  // BlockedTerrain ????
}

#[derive(Clone, Debug, PartialEq)]
pub struct DynamicCombat<'game> {
  pub scene: &'game Scene,
  pub combat: &'game Combat,
  pub game: &'game Game,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DynamicCreature<'creature, 'game: 'creature> {
  pub creature: &'creature Creature,
  pub game: &'game Game,
  pub class: &'game Class,
}

/// A newtype wrapper over Game that has a special Serialize implementation, which includes extra
/// data dynamically as a convenience for the client.
pub struct RPIGame<'a>(pub &'a Game);

impl<'a> Serialize for RPIGame<'a> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut str = serializer.serialize_struct("Game", 11)?;
    let game = self.0;

    str.serialize_field("current_combat", &game.current_combat)?;
    str.serialize_field("abilities", &game.abilities)?;
    str.serialize_field(
      "creatures",
      &game
        .creatures()
        .map_err(|e| S::Error::custom(format!("Oh no! Couldn't serialize creatures!? {:?}", e)))?,
    )?;
    str.serialize_field("classes", &game.classes)?;
    str.serialize_field("tile_system", &game.tile_system)?;
    str.serialize_field("scenes", &game.scenes)?;
    str.serialize_field("campaign", &game.campaign)?;
    str.serialize_field("items", &game.items)?;
    str.serialize_field("players", &game.players)?;
    str.serialize_field("active_scene", &game.active_scene)?;
    str.end()
  }
}

impl<'creature, 'game: 'creature> Serialize for DynamicCreature<'creature, 'game> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let screature = SerializedCreature {
      id: self.creature.id,
      name: self.creature.name.clone(),
      max_energy: self.creature.max_energy,
      cur_energy: self.creature.cur_energy,
      class: self.creature.class,
      max_health: self.creature.max_health,
      cur_health: self.creature.cur_health,
      note: self.creature.note.clone(),
      bio: self.creature.bio.clone(),
      portrait_url: self.creature.portrait_url.clone(),
      icon_url: self.creature.icon_url.clone(),
      attributes: self.creature.attributes.clone(),
      initiative: self.creature.initiative.clone(),
      size: self.creature.size,
      inventory: self.creature.inventory.clone(),
      conditions: self.creature.conditions.clone(),
      // overriden fields:
      speed: self.speed(),
      abilities: self.ability_statuses(),

      // synthesized fields:
      own_conditions: self.own_conditions().clone(),
      volume_conditions: self.volume_conditions(),
      can_act: self.can_act(),
      can_move: self.can_move(),
    };
    SerializedCreature::serialize(&screature, serializer)
  }
}

/// A Serde Serializer helper
// This could probably store references instead of owned objects for
// some more efficiency.
#[derive(Serialize, TS)]
#[ts(rename = "DynamicCreature")]
// I don't really want to make this pub, but I have to since it's used in src/bin/gents.rs
pub struct SerializedCreature {
  pub id: CreatureID,
  pub name: String,
  pub max_energy: Energy,
  pub cur_energy: Energy,
  pub class: ClassID,
  pub max_health: HP,
  pub cur_health: HP,
  pub note: String,
  #[serde(default)]
  pub bio: String,
  pub portrait_url: String,
  #[serde(default)]
  pub icon_url: String,
  #[ts(type = "CreatureAttributes")]
  pub attributes: HashMap<AttrID, SkillLevel>,
  pub initiative: Dice,
  pub size: AABB,
  #[serde(default)]
  #[ts(type = "CreatureInventory")]
  pub inventory: Inventory,
  #[ts(type = "CreatureConditions")]
  pub conditions: HashMap<ConditionID, AppliedCondition>,

  // overridden field
  #[ts(type = "Record<AbilityID, AbilityStatus>")]
  pub abilities: IndexedHashMap<AbilityStatus>,
  #[ts(type = "number")]
  pub speed: u32units::Length,

  // synthesized fields
  #[ts(type = "CreatureConditions")]
  pub own_conditions: HashMap<ConditionID, AppliedCondition>,
  #[ts(type = "CreatureConditions")]
  pub volume_conditions: HashMap<ConditionID, AppliedCondition>,
  pub can_act: bool,
  pub can_move: bool,
}

#[cfg(test)]
pub mod test {
  use crate::{creature::CreatureExt, grid::test::*, types::*};
  use maplit::hashmap;
  use std::{collections::HashSet, iter::FromIterator};
  use uuid::Uuid;

  use serde_json;
  use serde_yaml;
  pub fn uuid_0() -> Uuid { "00000000-0000-0000-0000-000000000000".parse().unwrap() }
  pub fn uuid_1() -> Uuid { "00000000-0000-0000-0000-000000000001".parse().unwrap() }
  pub fn uuid_2() -> Uuid { "00000000-0000-0000-0000-000000000002".parse().unwrap() }
  pub fn uuid_3() -> Uuid { "00000000-0000-0000-0000-000000000003".parse().unwrap() }
  pub fn uuid_4() -> Uuid { "00000000-0000-0000-0000-000000000004".parse().unwrap() }
  pub fn uuid_5() -> Uuid { "00000000-0000-0000-0000-000000000005".parse().unwrap() }
  pub fn cid_cleric() -> CreatureID { CreatureID(uuid_0()) }
  pub fn cid_ranger() -> CreatureID { CreatureID(uuid_1()) }
  pub fn cid_rogue() -> CreatureID { CreatureID(uuid_2()) }

  pub fn t_creature(name: &str, class: ClassID, init: i8) -> Creature {
    Creature::create(&CreatureCreation {
      name: name.to_string(),
      note: "".to_string(),
      bio: "".to_string(),
      class,
      portrait_url: "".to_string(),
      icon_url: "".to_string(),
      initiative: Dice::flat(init),
      size: AABB { x: u32cm(100), y: u32cm(100), z: u32cm(100) },
    })
  }

  pub fn t_rogue(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, classid_rogue(), 20) }
  }

  pub fn t_ranger(name: &str) -> Creature {
    Creature { id: cid_ranger(), ..t_creature(name, classid_ranger(), 10) }
  }

  pub fn t_cleric(name: &str) -> Creature {
    Creature { id: cid_rogue(), ..t_creature(name, classid_cleric(), 0) }
  }

  pub fn t_scene_id() -> SceneID { SceneID(uuid_3()) }

  pub fn t_scene() -> Scene {
    Scene {
      id: t_scene_id(),
      name: "Test Scene".to_string(),
      background_image_url: "".to_string(),
      background_image_offset: None,
      background_image_scale: (1., 1.),
      terrain: huge_box(),
      highlights: HashMap::new(),
      annotations: HashMap::new(),

      scene_hotspots: HashMap::new(),
      related_scenes: HashSet::new(),

      attribute_checks: HashMap::new(),
      creatures: hashmap! {
        cid_rogue() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
        cid_cleric() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
        cid_ranger() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
      },
      inventory: HashMap::new(),
      volume_conditions: HashMap::new(),
      focused_creatures: vec![],
    }
  }

  pub fn app_cond(c: Condition, r: Duration) -> AppliedCondition {
    AppliedCondition { condition: c, remaining: r }
  }

  pub fn classid_rogue() -> ClassID { ClassID(uuid_0()) }
  pub fn classid_cleric() -> ClassID { ClassID(uuid_1()) }
  pub fn classid_ranger() -> ClassID { ClassID(uuid_2()) }

  pub fn abid_punch() -> AbilityID { AbilityID(uuid_0()) }
  pub fn abid_shoot() -> AbilityID { AbilityID(uuid_1()) }
  pub fn abid_heal() -> AbilityID { AbilityID(uuid_2()) }
  pub fn abid_fireball() -> AbilityID { AbilityID(uuid_3()) }
  pub fn abid_piercing_shot() -> AbilityID { AbilityID(uuid_4()) }
  pub fn abid_thorn_patch() -> AbilityID { AbilityID(uuid_5()) }

  pub fn t_punch() -> Ability {
    Ability {
      id: abid_punch(),
      name: "Punch".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Melee,
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_shoot() -> Ability {
    Ability {
      id: abid_shoot(),
      name: "Shoot".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Range(u32cm(500)),
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_heal() -> Ability {
    Ability {
      id: abid_heal(),
      name: "Heal".to_string(),
      cost: Energy(0),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::Range(u32cm(500)),
        effect: CreatureEffect::Heal(Dice::flat(3)),
      },
    }
  }

  pub fn t_fireball() -> Ability {
    Ability {
      id: abid_fireball(),
      name: "Fireball".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::AllCreaturesInVolumeInRange {
          volume: Volume::Sphere(u32cm(1000)),
          range: u32cm(2000),
        },
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_piercing_shot() -> Ability {
    Ability {
      id: abid_piercing_shot(),
      name: "Piercing Shot".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::Creature {
        target: CreatureTarget::LineFromActor { distance: u32cm(1000) },
        effect: CreatureEffect::Damage(Dice::flat(3)),
      },
    }
  }

  pub fn t_thorn_patch() -> Ability {
    Ability {
      id: abid_thorn_patch(),
      name: "Thorn Patch".to_string(),
      cost: Energy(8),
      usable_ooc: true,
      action: Action::SceneVolume {
        target: SceneTarget::RangedVolume {
          volume: Volume::Sphere(u32cm(200)),
          range: u32cm(1000),
        },
        effect: SceneEffect::CreateVolumeCondition {
          duration: Duration::Interminate,
          condition: Condition::RecurringEffect(Box::new(CreatureEffect::Damage(Dice::flat(3)))),
        },
      },
    }
  }

  pub fn t_abilities() -> IndexedHashMap<Ability> {
    IndexedHashMap::from_iter(vec![
      t_punch(),
      t_shoot(),
      t_heal(),
      t_fireball(),
      t_piercing_shot(),
      t_thorn_patch(),
    ])
  }

  #[test]
  fn serde_ids() {
    let id = abid_heal();
    let serialized = serde_yaml::to_string(&id).unwrap();
    assert_eq!(serialized, "---\n00000000-0000-0000-0000-000000000002\n");
    let deserialized = serde_yaml::from_str::<AbilityID>(&serialized).unwrap();
    assert_eq!(deserialized, id);
  }

  #[test]
  fn serde_condition_duration() {
    let cd = Duration::Interminate;
    assert_eq!(serde_json::to_string(&cd).unwrap(), "\"Interminate\"");
    let cd = Duration::Rounds(3);
    assert_eq!(serde_json::to_string(&cd).unwrap(), "{\"Rounds\":3}");
  }

  #[test]
  fn dice_plus() {
    let d = Dice::flat(1).plus(Dice::flat(1));
    assert_eq!(d.roll(), (vec![1, 1], 2));
  }

  #[test]
  fn dice_negative() {
    let d = Dice::flat(1).plus(Dice::flat(-5));
    assert_eq!(d.roll(), (vec![1, -5], -4));
  }

  #[test]
  fn serialize_hashmap_point3() {
    let p = Point3::new(0, 0, 0);
    let hm = hashmap! {p => 5};
    assert_eq!(serde_json::to_string(&hm).unwrap(), "{\"0/0/0\":5}");
  }
}
