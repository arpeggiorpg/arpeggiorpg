use std::collections::HashMap;

use types::*;

impl Scene {
  pub fn create(creation: SceneCreation) -> Scene {
    Scene {
      id: SceneID::new(),
      name: creation.name,
      background_image_url: creation.background_image_url.clone(),
      map: creation.map,
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
      .ok_or_else(|| GameErrorEnum::CreatureNotFound(creature_id.to_string()).into())
  }
  pub fn set_pos(&self, cid: CreatureID, pt: Point3) -> Result<Scene, GameError> {
    let mut new = self.clone();
    {
      let data = new
        .creatures
        .get_mut(&cid)
        .ok_or_else(|| GameErrorEnum::CreatureNotFound(cid.to_string()))?;
      data.0 = pt;
    }
    Ok(new)
  }
  pub fn add_volume_condition(
    &self, condition_id: ConditionID, point: Point3, volume: Volume, condition: Condition,
    duration: Duration,
  ) -> Scene {
    let mut new = self.clone();
    new
      .volume_conditions
      .insert(condition_id, VolumeCondition { point, volume, condition, remaining: duration });
    new
  }

  pub fn creature_volume_conditions(
    &self, ts: TileSystem, creature: &Creature
  ) -> Result<Vec<(ConditionID, &VolumeCondition)>, GameError> {
    let creature_pos = self.get_pos(creature.id)?;
    let volumes_with_data: Vec<(Point3, Volume, ConditionID)> = self
      .volume_conditions
      .iter()
      .map(|(cond_id, vol_cond)| (vol_cond.point, vol_cond.volume, *cond_id))
      .collect();
    let condition_ids =
      ts.intersecting_volumes(creature_pos, Volume::AABB(creature.size), &volumes_with_data);
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

  /// Return a set of points which are intersect a volume. Largely used for previewing the area
  /// that will be affected by a volume-affecting ability.
  pub fn open_terrain_in_volume(
    &self, game: &Game, pt: Point3, volume: Volume
  ) -> Result<Vec<Point3>, GameError> {
    let terrain = game.get_map(self.map)?.terrain.iter();
    let all_open = terrain.map(|pt| (*pt, *pt)).collect();
    Ok(game.tile_system.items_within_volume(volume, pt, &all_open))
  }

  pub fn creatures_in_volume(&self, ts: TileSystem, pt: Point3, volume: Volume) -> Vec<CreatureID> {
    let creature_locations = self.creatures.iter().map(|(cid, &(pt, _))| (*cid, pt)).collect();
    ts.items_within_volume(volume, pt, &creature_locations)
  }
}
