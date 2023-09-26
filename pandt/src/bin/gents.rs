use std::fs::File;
use std::io::prelude::*;
use ts_rs::TS;

use pandt::types as T;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let mut file = File::create("bindings/bindings.ts")?;
  let decls = vec![
    <T::AABB as TS>::decl(),
    <T::Ability as TS>::decl(),
    <T::AbilityID as TS>::decl(),
    <T::AbilityStatus as TS>::decl(),
    <T::Action as TS>::decl(),
    <T::AppliedCondition as TS>::decl(),
    <T::AttributeCheck as TS>::decl(),
    <T::AttrID as TS>::decl(),
    <T::Class as TS>::decl(),
    <T::ClassID as TS>::decl(),
    <T::Combat as TS>::decl(),
    <T::CombatLog as TS>::decl(),
    <T::Condition as TS>::decl(),
    <T::ConditionID as TS>::decl(),
    <T::CreatureCreation as TS>::decl(),
    <T::CreatureEffect as TS>::decl(),
    <T::CreatureID as TS>::decl(),
    <T::CreatureLog as TS>::decl(),
    <T::CreatureTarget as TS>::decl(),
    <T::DecidedTarget as TS>::decl(),
    <T::Dice as TS>::decl(),
    <T::Duration as TS>::decl(),
    <T::Energy as TS>::decl(),
    <foldertree::FolderTree<T::Folder> as TS>::decl(),
    <T::Folder as TS>::decl(),
    <T::FolderItemID as TS>::decl(),
    <T::Game as TS>::decl(),
    <T::HP as TS>::decl(),
    <T::InventoryOwner as TS>::decl(),
    <T::Item as TS>::decl(),
    <T::ItemID as TS>::decl(),
    <T::ModuleSource as TS>::decl(),
    <T::Note as TS>::decl(),
    <T::Player as TS>::decl(),
    <T::PlayerID as TS>::decl(),
    <T::PotentialTargets as TS>::decl(),
    <T::Scene as TS>::decl(),
    <T::SceneCreation as TS>::decl(),
    <T::SceneEffect as TS>::decl(),
    <T::SceneID as TS>::decl(),
    <T::SceneTarget as TS>::decl(),
    <T::SkillLevel as TS>::decl(),
    <T::TileSystem as TS>::decl(),
    <T::Visibility as TS>::decl(),
    <T::Volume as TS>::decl(),
    <T::VolumeCondition as TS>::decl(),
  ];
  file.write_all(b"\
  import type { Map } from 'immutable';
  import type {
     Point3, Highlights, NonEmpty, Annotations, SceneHotspots, RelatedScenes, SceneAttributeChecks, SceneCreatures, SceneInventory, SceneVolumeConditions, Terrain, SceneFocusedCreatures, GameAbilities, GameCreatures, GameClasses, GameScenes, GameItems, GamePlayers
  } from '../PTTypes';

  ")?;
  for decl in decls.iter() {
    file.write_all(b"export ")?;
    file.write_all(decl.as_bytes())?;
    file.write_all(b"\n\n")?;
  }
  return Ok(());
}
