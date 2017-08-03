use std::collections::HashMap;

#[macro_use] extern crate maplit;
extern crate pandt;
use pandt::types::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ability {
  pub name: String,
  pub cost: Energy,
  pub usable_ooc: bool,
  pub action: Action,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
  Melee(CreatureEffect),
  Range(Distance, CreatureEffect),
  Actor(CreatureEffect),
  LineFromActor { distance: Distance, effect: CreatureEffect },
  LineFromActorToCreature{ distance: Distance, effect: CreatureEffect },
  RangedSceneVolume { volume: Volume, range: Distance, effect: SceneEffect },
  CasterCenteredSceneVolume{volume: Volume, effect: SceneEffect},
  Multi(HashMap<String, Action>)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CreatureEffect {
  Damage(usize),
  Heal(usize),
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SceneEffect {
  CreateVolumeCondition(ConditionDuration, Condition),
}


// super shallow version

pub struct ShallowAction {
  targets: Vec<(String, PointOrCreature)>,
  effects: HashMap<String, Box<EffectHandler>>
}

pub enum PointOrCreature {
  Point, Creature,
}


pub trait EffectHandler {
  fn handle(&self, targets: Vec<(String, PointOrCreature)>, scene: &Scene) -> Scene;
}


fn main() {
  let action = Action::Melee(CreatureEffect::Damage(3));
  let drain_life = Action::Multi(hashmap!{
    "foe".to_string() => Action::Melee(CreatureEffect::Damage(3)),
    "friend".to_string() => Action::Actor(CreatureEffect::Heal(3))});
  let thorn_patch = Action::RangedSceneVolume{
    range: Distance(100),
    volume: Volume::Sphere(Distance(3)),
    effect: SceneEffect::CreateVolumeCondition(
      ConditionDuration::Duration(3),
      Condition::RecurringEffect(Box::new(Effect::Damage(Dice::flat(3)))))
      };
  println!("oh");
}


