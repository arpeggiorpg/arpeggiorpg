use std::collections::HashMap;

use types::*;
use grid::{make_world, query_world};

impl Scene {
  pub fn create(creation: SceneCreation) -> Scene {
    Scene {
      id: SceneID::gen(),
      name: creation.name,
      background_image_url: creation.background_image_url.clone(),
      background_image_offset: creation.background_image_offset,
      background_image_scale: creation.background_image_scale,
      terrain: vec![],
      highlights: HashMap::new(),
      annotations: HashMap::new(),
      creatures: HashMap::new(),
      attribute_checks: HashMap::new(),
      inventory: HashMap::new(),
      volume_conditions: HashMap::new(),
      focused_creatures: vec![],
    }
  }
  pub fn get_pos(&self, creature_id: CreatureID) -> Result<Point3, GameError> {
    self
      .creatures
      .get(&creature_id)
      .map(|x| x.0)
      .ok_or_else(|| GameError::CreatureNotFound(creature_id.to_string()).into())
  }
  pub fn set_pos(&self, cid: CreatureID, pt: Point3) -> Result<Scene, GameError> {
    let mut new = self.clone();
    {
      let data = new
        .creatures
        .get_mut(&cid)
        .ok_or_else(|| GameError::CreatureNotFound(cid.to_string()))?;
      data.0 = pt;
    }
    Ok(new)
  }
  pub fn add_volume_condition(
    &self, condition_id: ConditionID, point: Point3, volume: Volume, condition: Condition,
    duration: Duration,
  ) -> Scene {
    let mut new = self.clone();
    new.volume_conditions.insert(
      condition_id,
      VolumeCondition {
        point,
        volume,
        condition,
        remaining: duration,
      },
    );
    new
  }

  /// Figure out which volume conditions apply to the given creature.
  pub fn creature_volume_conditions(
    &self, game: &Game, creature: &Creature
  ) -> Result<Vec<(ConditionID, &VolumeCondition)>, GameError> {
    let condition_ids = query_world(&self.get_world(game)?, |cdata1, cdata2| {
      if let (CollisionData::Creature(cid), CollisionData::ConditionVolume(cond_id)) =
        (cdata1, cdata2)
      {
        if cid == creature.id {
          Some(cond_id)
        } else {
          None
        }
      } else {
        None
      }
    });
    let mut results = vec![];
    for cond_id in condition_ids {
      let val = self
        .volume_conditions
        .get(&cond_id)
        .expect("Mapping over IDs that should only appear in this collection");
      results.push((cond_id, val));
    }
    Ok(results)
  }

  /// Return a set of points of open terrain which  intersect a volume.
  /// Largely used for previewing the area that will be affected by a volume-affecting ability.
  pub fn open_terrain_in_volume(
    &self, game: &Game, pt: Point3, volume: Volume
  ) -> Result<Terrain, GameError> {
    let all_open = self.terrain.iter().map(|pt| (*pt, *pt)).collect();
    Ok(game.tile_system.items_within_volume(volume, pt, &all_open))
  }

  pub fn creatures_in_volume(&self, ts: TileSystem, pt: Point3, volume: Volume) -> Vec<CreatureID> {
    let creature_locations = self
      .creatures
      .iter()
      .map(|(cid, &(pt, _))| (*cid, pt))
      .collect();
    ts.items_within_volume(volume, pt, &creature_locations)
  }

  pub fn get_world(&self, game: &Game) -> Result<CollisionWorld, GameError> {
    let creatures = self
      .creatures
      .iter()
      .filter_map(|(creature_id, &(pos, _))| {
        game
          .get_creature(*creature_id)
          .map(|dc| (dc.creature, pos))
          .ok()
      });
    let vcs = self.volume_conditions.iter().map(|(c, vc)| (*c, vc));
    Ok(make_world(creatures, vcs))
  }
}

#[cfg(test)]
mod test {
  use types::*;
  use types::test::*;
  use game::test::*;

  #[test]
  fn creature_volume_conditions() {
    let game = t_game();
    let mut scene = t_scene();
    let cond_id = ConditionID::gen();
    let volume_cond = VolumeCondition {
      point: Point3::new(0, 0, 0),
      volume: Volume::Sphere(Distance(300)),
      remaining: Duration::Interminate,
      condition: Condition::Dead,
    };
    scene.volume_conditions.insert(cond_id, volume_cond.clone());
    let rogue = t_rogue("rogue");
    let conds = scene
      .creature_volume_conditions(&game, &rogue)
      .expect("Couldn't get conds");
    assert_eq!(conds, vec![(cond_id, &volume_cond)]);
  }
}
