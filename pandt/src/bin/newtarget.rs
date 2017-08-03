extern crate pandt;
use pandt::types::*;


/// Creatures have abilities that they can perform (in-combat or out).
/// Abilities trigger *Action*s. Actions cause effects on things (usually creatures, with
/// CreatureEffect, sometimes other things).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ability {
  pub name: String,
  pub cost: Energy,
  pub usable_ooc: bool,
  pub action: Action,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
  Creature { effect: CreatureEffect, target: CreatureTarget },
  SceneVolume { effect: SceneEffect, target: SceneTarget },
  Multi(Vec<(String, Action)>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CreatureTarget {
  Melee,
  Range(Distance),
  Actor,
  LineFromActor(Distance),
  LineFromActorToCreature(Distance),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SceneTarget {
  RangedSceneVolume { volume: Volume, range: Distance },
  CasterCenteredSceneVolume(Volume),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CreatureEffect {
  Damage(usize),
  Heal(usize),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SceneEffect {
  CreateVolumeCondition { duration: ConditionDuration, condition: Condition },
  DestroyTerrain,
}

fn main() {
  let action =
    Action::Creature { effect: CreatureEffect::Damage(3), target: CreatureTarget::Melee };
  let drain_life = Action::Multi(vec![
    (
      "foe".to_string(),
      Action::Creature { effect: CreatureEffect::Damage(3), target: CreatureTarget::Melee },
    ),
    (
      "friend".to_string(),
      Action::Creature { effect: CreatureEffect::Heal(3), target: CreatureTarget::Actor },
    ),
  ]);
  let thorn_patch = Action::SceneVolume {
    effect: SceneEffect::CreateVolumeCondition {
      duration: ConditionDuration::Duration(3),
      condition: Condition::RecurringEffect(Box::new(Effect::Damage(Dice::flat(3)))),
    },
    target: SceneTarget::RangedSceneVolume {
      range: Distance(100),
      volume: Volume::Sphere(Distance(3)),
    },
  };
  println!("oh");
}
