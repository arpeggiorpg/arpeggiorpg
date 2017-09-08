use std::collections::HashSet;
use std::iter::FromIterator;
use bresenham;
use odds::vec::VecExt;

use nalgebra as na;
use nalgebra::{Isometry3, Vector3};
use ncollide::shape;
use ncollide::shape::Cuboid;
use ncollide::query::PointQuery;
use ncollide::world;

use types::{CollisionData, CollisionWorld, ConditionID, Creature, Distance, Map, Point3,
            TileSystem, VectorCM, Volume, VolumeCondition};

// I got curious about how to implement this in integer math.
// the maximum distance on a grid of i16 positions (−32768 to 32767) is....?
// √((x₂ - x₁)² + (y₂ - y₁)² + (z₂ - z₁)²)
// √((32767 - −32768)² + (32767 - −32768)² + (32767 - −32768)²)
// √(65535² + 65535² + 65535²)
// √(4,294,836,225 + 4,294,836,225 + 4,294,836,225) (each close to the limit of 32-bit integers).
// √(12,884,901,888) // NOTE! This number requires a (signed-ok) 64-bit integer to store!
// 113511.68172483394 -- as an integer, requires a (signed-ok) 32.
// so we need a i32/u32 for the result, and we need to use a i64/u64 for the calculation.

fn na_iso(pt: Point3) -> Isometry3<f32> {
  Isometry3::new(na_vector(pt), na::zero())
}

fn na_point(pt: Point3) -> na::Point3<f32> {
  na::Point3::new(f32::from(pt.0), f32::from(pt.1), f32::from(pt.2))
}

fn na_vector(pt: Point3) -> Vector3<f32> {
  Vector3::new(f32::from(pt.0), f32::from(pt.1), f32::from(pt.2))
}

fn na_vector_to_vector_cm(v: Vector3<f32>) -> VectorCM {
  ((v[0] * 100.0) as i32, (v[1] * 100.0) as i32, (v[2] * 100.0) as i32)
}

pub fn line_through_point(origin: Point3, clicked: Point3, length: Distance) -> Volume {
  let offset = point3_difference(clicked, origin);
  let mut navec = na_vector(offset);
  navec.normalize_mut();
  let new_vec = navec * length.to_meters();
  Volume::Line { vector: na_vector_to_vector_cm(new_vec) }
}

/// Get the vector difference between two points, i.e., the offset of pt2 from pt1.
/// This returns a plain old Point3, but it'd be nicer if it were a `Point3Difference`...
pub fn point3_difference(pt1: Point3, pt2: Point3) -> Point3 {
  (pt1.0 - pt2.0, pt1.1 - pt2.1, pt1.2 - pt2.2)
}

pub fn point3_add_vec(pt: Point3, diff: VectorCM) -> Point3 {
  (
    ((i32::from(pt.0) * 100 + diff.0) / 100) as i16,
    ((i32::from(pt.1) * 100 + diff.1) / 100) as i16,
    ((i32::from(pt.2) * 100 + diff.2) / 100) as i16,
  )
}

impl TileSystem {
  /// Get the distance between two points, considering the system being used.
  /// In DnD, an angular distance is "equivalent" to a horizontal/vertical distance.
  /// i.e., The distance from 0,0 to 1,1 is sqrt(2) in Realistic and 1.0 in DnD.
  pub fn point3_distance(&self, pos1: Point3, pos2: Point3) -> Distance {
    match *self {
      TileSystem::Realistic => {
        let meaningless = Cuboid::new(Vector3::new(0.0, 0.0, 0.0));
        let ncpos1 = na_iso(pos1);
        let ncpos2 = na_point(pos2);
        let distance = meaningless.distance_to_point(&ncpos1, &ncpos2, false);
        Distance::from_meters(distance)
      }
      TileSystem::DnD => {
        // I'd use cmp::max but it's not usable on floats
        let xdiff = (pos1.0 - pos2.0).abs() as u32;
        let ydiff = (pos1.1 - pos2.1).abs() as u32;
        Distance(if xdiff > ydiff { xdiff * 100 } else { ydiff * 100 })
      }
    }
  }

  /// Check whether two points are within some distance of each other.
  pub fn points_within_distance(&self, c1: Point3, c2: Point3, d: Distance) -> bool {
    self.point3_distance(c1, c2) <= d
  }


  /// Garbage Function
  pub fn items_within_volume<I: Clone + Eq + Hash>(
    &self, volume: Volume, pt: Point3, items: &HashMap<I, Point3>
  ) -> Vec<I> {
    // TODO: unimplemented! this doesn't support non-1x1 items
    // TODO: this function is really dumb, and instead should probably work on a HashSet of Point3s,
    // or maybe a HashMap<Point3, I>. And it should make use of points_in_volume.
    let mut results = vec![];
    match volume {
      Volume::Sphere(radius) => for (item, item_pos) in items {
        if self.point3_distance(pt, *item_pos) <= radius {
          results.push(item.clone());
        }
      },
      Volume::AABB(_) => unimplemented!("unimplemented: items_within_volume for AABB"),
      Volume::Line { vector } => {
        let dest = point3_add_vec(pt, vector);
        let line_pts: HashSet<Point3> = HashSet::from_iter(
          bresenham::Bresenham::new(
            (pt.0 as isize, pt.1 as isize),
            (dest.0 as isize, dest.1 as isize),
          ).map(|(x, y)| (x as i16, y as i16, 0)),
        );
        for (item, item_pos) in items {
          if line_pts.contains(item_pos) {
            results.push(item.clone());
          }
        }
      }
      Volume::VerticalCylinder { .. } => unimplemented!("items_within_volume for VerticalCylinder"),
    }
    results
  }

  pub fn open_points_in_range(&self, start: Point3, terrain: &Map, range: Distance) -> Vec<Point3> {
    let meters = (range.0 / 100) as i16;
    let mut open = vec![];
    for x in start.0 - meters..start.0 + meters + 1 {
      for y in start.1 - meters..start.1 + meters + 1 {
        let end_point = (x, y, 0);
        if !terrain.is_open(&end_point) {
          continue;
        }
        open.push(end_point);
      }
    }
    open
  }

  /// Get the set of points which can be pathed to from some point.
  pub fn get_all_accessible(
    &self, start: Point3, terrain: &Map, volume: Volume, speed: Distance
  ) -> Vec<Point3> {
    let points_to_check = self.open_points_in_range(start, terrain, speed);
    // println!("Number of points to check: {:?}", points_to_check.len());
    let mut success_fns: Vec<Box<Fn(&Point3) -> bool>> = vec![];
    for pt in points_to_check {
      if pt != start {
        success_fns.push(Box::new(move |n: &Point3| *n == pt));
      }
    }
    let mut final_points = vec![];
    for (path, cost) in astar_multi(
      &start,
      |n| self.point3_neighbors(terrain, volume, *n),
      |n| self.point3_distance(start, *n).0,
      speed.0,
      success_fns,
    ) {
      if Distance(cost) <= speed {
        // FIXME: we should NOT be checking cost here, instead astar_multi should support
        // max distance.
        final_points.push(*path.last().unwrap())
      }
    }
    final_points
  }

  /// Find a path from some start point to some destination point. If one can be found, a Vec of
  /// points on the way to the destination is returned, along with the total length of that path.
  pub fn find_path(
    &self, start: Point3, speed: Distance, terrain: &Map, volume: Volume, destination: Point3
  ) -> Option<(Vec<Point3>, Distance)> {
    let success = Box::new(move |n: &Point3| *n == destination);
    let result: Vec<(Vec<Point3>, u32)> = astar_multi(
      &start,
      |n| self.point3_neighbors(terrain, volume, *n),
      |n| self.point3_distance(start, *n).0,
      speed.0,
      vec![success],
    );
    if let Some((path, cost)) = result.into_iter().next() {
      Some((path, Distance(cost)))
    } else {
      None
    }
  }

  /// Determine which points a volume occupies.
  /// The way a volume fits at a point is specific to the volume type.
  /// AABB: top-left
  /// Sphere: center
  /// Line: origin
  /// VerticalCylinder: center of base
  // TODO FIXME XXX: Implement for more volume types.
  // TODO: Should this use ncollide?
  fn points_in_volume(&self, volume: Volume, pt: Point3) -> Vec<Point3> {
    match volume {
      Volume::Sphere(..) => {
        unimplemented!("unimplemented: points_in_volume for Sphere");
      }
      Volume::AABB(aabb) => (pt.0..(pt.0 + i16::from(aabb.x)))
        .flat_map(|x| {
          (pt.1..(pt.1 + i16::from(aabb.y)))
            .flat_map(move |y| (pt.2..(pt.2 + i16::from(aabb.z))).map(move |z| (x, y, z)))
        })
        .collect(),
      Volume::Line { .. } => unimplemented!("points_in_volume for Line"),
      Volume::VerticalCylinder { .. } => {
        unimplemented!("unimplemented: points_in_volume for VerticalCylinder")
      }
    }
  }

  /// Determine whether a volume will not collide *with terrain* if it is placed at a point.
  /// Note that this doesn't consider other creatures or other map objects.
  fn volume_fits_at_point(&self, volume: Volume, map: &Map, pt: Point3) -> bool {
    for pt in self.points_in_volume(volume, pt) {
      if !map.terrain.contains(&pt) {
        return false;
      }
    }
    true
  }

  /// Find neighbors of the given point that the given volume can fit in, given the terrain.
  fn point3_neighbors(&self, terrain: &Map, volume: Volume, pt: Point3) -> Vec<(Point3, u32)> {
    let mut results = vec![];
    for x in -1..2 {
      for y in -1..2 {
        if (x, y) == (0, 0) {
          continue;
        }
        let neighbor = (pt.0 + x, pt.1 + y, pt.2);
        if terrain.is_open(&neighbor) && self.volume_fits_at_point(volume, terrain, neighbor) {
          let is_angle = x.abs() == y.abs();
          let cost = if is_angle {
            match *self {
              TileSystem::Realistic => 141,
              TileSystem::DnD => 100,
            }
          } else {
            match *self {
              TileSystem::Realistic => 100,
              // ok, this is ridiculous, but:
              // since D&D movement makes diagonals cost the same as cardinals, the pathfinder
              // will arbitrarily choose to move diagonally when a normal person would move in
              // a straight line. By ever-so-slightly reducing the cost of straight lines here,
              // we get it to prefer to move straight.
              TileSystem::DnD => 99,
            }
          };
          // don't allow diagonal movement around corners
          if is_angle && !terrain.is_open(&(neighbor.0, pt.1, pt.2)) ||
            !terrain.is_open(&(pt.0, neighbor.1, pt.2))
          {
            continue;
          }
          results.push((neighbor, cost));
        }
      }
    }
    results
  }
}

/// Make a CollisionWorld given some creatures and volume conditions.
/// The factoring is a little sad; this is the only function in grid.rs that knows about these P&T
/// types. Maybe I should just move this to collision.rs or something.
pub fn make_world<'c, 'vc, CI, VCI>(creatures: CI, volume_conditions: VCI) -> CollisionWorld
where
  CI: Iterator<Item = (&'c Creature, Point3)>,
  VCI: Iterator<Item = (ConditionID, &'vc VolumeCondition)>,
{
  let mut world = world::CollisionWorld3::new(0.0, true);
  let mut creature_group = world::CollisionGroups::new();
  creature_group.set_membership(&[1]);
  creature_group.set_whitelist(&[2]);
  creature_group.set_blacklist(&[1]);

  let mut condition_group = world::CollisionGroups::new();
  condition_group.set_membership(&[2]);
  condition_group.set_whitelist(&[1]);
  condition_group.set_blacklist(&[2]);

  let query = world::GeometricQueryType::Contacts(0.0);

  let mut idx = 0;

  for (creature, pos) in creatures {
    let volume = Volume::AABB(creature.size);
    world.deferred_add(
      idx,
      na_iso(pos),
      volume_to_na_shape(volume),
      creature_group,
      query,
      CollisionData::Creature(creature.id),
    );
    idx += 1;
  }

  for (condition_id, volume_condition) in volume_conditions {
    world.deferred_add(
      idx,
      na_iso(volume_condition.point),
      volume_to_na_shape(volume_condition.volume),
      condition_group,
      query,
      CollisionData::ConditionVolume(condition_id),
    );
    idx += 1;
  }

  world.update();
  world
}

pub fn query_world<F, R>(world: &CollisionWorld, f: F) -> Vec<R>
where
  F: Fn(CollisionData, CollisionData) -> Option<R>,
{
  let mut results = vec![];
  for (obj1, obj2, _) in world.contact_pairs() {
    if let Some(r) = f(obj1.data, obj2.data) {
      results.push(r);
    }
    if let Some(r) = f(obj2.data, obj1.data) {
      results.push(r);
    }
  }
  results
}

fn volume_to_na_shape(volume: Volume) -> shape::ShapeHandle3<f32> {
  match volume {
    Volume::Sphere(r) => shape::ShapeHandle3::new(shape::Ball::new(r.0 as f32 / 100.0)),
    Volume::AABB(aabb) => shape::ShapeHandle3::new(shape::Cuboid::new(Vector3::new(
      (f32::from(aabb.x) / 100.0) / 2.0,
      (f32::from(aabb.y) / 100.0) / 2.0,
      (f32::from(aabb.z) / 100.0) / 2.0,
    ))),
    Volume::Line { .. } => unimplemented!("volume_to_na_shape for Line"),
    Volume::VerticalCylinder { .. } => unimplemented!("volume_to_na_shape for VerticalCylinder"),
  }
}

// FOLLOWING COPIED FROM PATHFINDING CRATE
// ***************************************
use num_traits::Zero;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::cmp::Ordering;

struct InvCmpHolder<K, P> {
  key: K,
  payload: P,
}

impl<K: PartialEq, P> PartialEq for InvCmpHolder<K, P> {
  fn eq(&self, other: &InvCmpHolder<K, P>) -> bool {
    self.key.eq(&other.key)
  }
}

impl<K: PartialEq, P> Eq for InvCmpHolder<K, P> {}

impl<K: PartialOrd, P> PartialOrd for InvCmpHolder<K, P> {
  fn partial_cmp(&self, other: &InvCmpHolder<K, P>) -> Option<Ordering> {
    other.key.partial_cmp(&self.key)
  }
}

impl<K: Ord, P> Ord for InvCmpHolder<K, P> {
  fn cmp(&self, other: &InvCmpHolder<K, P>) -> Ordering {
    other.key.cmp(&self.key)
  }
}

/// Find a path through the given DAG.
/// parents: a `HashMap` containing a DAG of nodes, decorated with the cost of those connections.
/// start: The node to start at, but actually *this is the *END* node of the A* search!!!!
fn reverse_path<N: Eq + Hash>(mut parents: HashMap<N, N>, start: N) -> Vec<N> {
  let mut path = vec![start];
  while let Some(parent) = parents.remove(path.last().unwrap()) {
    path.push(parent);
  }
  path.into_iter().rev().collect()
}

pub fn astar_multi<N, C, FN, IN, FH>(
  start: &N, neighbours: FN, heuristic: FH, max_cost: C, mut successes: Vec<Box<Fn(&N) -> bool>>
) -> Vec<(Vec<N>, C)>
where
  N: Eq + Hash + Clone,
  C: Zero + Ord + Copy + PartialEq + PartialOrd, // maybe relax these so floats can be used?
  FN: Fn(&N) -> IN,
  IN: IntoIterator<Item = (N, C)>,
  FH: Fn(&N) -> C,
{
  let mut to_see = BinaryHeap::new();
  to_see.push(InvCmpHolder { key: heuristic(start), payload: (Zero::zero(), start.clone()) });
  let mut parents: HashMap<N, (N, C)> = HashMap::new();
  let mut found_nodes = vec![];
  while let Some(InvCmpHolder { payload: (cost, node), .. }) = to_see.pop() {
    successes.retain_mut(|ref mut success_fn| {
      let was_successful = success_fn(&node);
      if was_successful {
        found_nodes.push((node.clone(), cost));
      }
      !was_successful
    });
    if successes.is_empty() {
      break;
    }
    // We may have inserted a node several time into the binary heap if we found
    // a better way to access it. Ensure that we are currently dealing with the
    // best path and discard the others.
    if let Some(&(_, c)) = parents.get(&node) {
      if cost > c {
        continue;
      }
    }
    for (neighbour, move_cost) in neighbours(&node) {
      let old_cost = parents.get(&neighbour).map(|&(_, c)| c);
      let new_cost = cost + move_cost;
      if neighbour != *start && old_cost.map_or(true, |c| new_cost < c) && new_cost <= max_cost {
        parents.insert(neighbour.clone(), (node.clone(), new_cost));
        let new_predicted_cost = new_cost + heuristic(&neighbour);
        to_see.push(InvCmpHolder { key: new_predicted_cost, payload: (new_cost, neighbour) });
      }
    }
  }

  let mut results = vec![];
  let parents: HashMap<N, N> = parents.into_iter().map(|(n, (p, _))| (n, p)).collect();
  for (found_node, cost) in found_nodes {
    results.push((reverse_path(parents.clone(), found_node), cost));
  }
  results
}

// PRECEDING COPIED FROM PATHFINDING CRATE
// ***************************************

#[cfg(test)]
pub mod test {
  use grid::*;
  use types::test::*;
  use types::*;
  use std::iter::FromIterator;

  /// A map containing a single open block of terrain at 0,0,0
  fn box_map() -> Map {
    Map::new("box".to_string(), vec![(0, 0, 0)])
  }

  /// A map shaped like a dumbbell, with two 2x3 rooms connected by a 1x1 passage
  fn dumbbell_map() -> Map {
    Map::new(
      "dumbbell".to_string(),
      vec![
        (0, 0, 0),
        (1, 0, 0),
        (3, 0, 0),
        (4, 0, 0),
        (0, 1, 0),
        (1, 1, 0),
        (2, 1, 0),
        (3, 1, 0),
        (4, 1, 0),
        (0, 2, 0),
        (1, 2, 0),
        (3, 2, 0),
        (4, 2, 0),
      ],
    )
  }

  /// A map containing a large 40-meter square of open terrain.
  pub fn huge_box() -> Map {
    let mut map = vec![];
    for x in -20..20 {
      for y in -20..20 {
        map.push((x, y, 0));
      }
    }
    Map {
      id: t_map_id(),
      name: "huge box".to_string(),
      terrain: map,
      specials: vec![],
      background_image_url: "".to_string(),
      background_image_offset: (0, 0),
      background_image_scale: (0, 0),
    }
  }

  #[test]
  fn test_biggest_distance() {
    let pos1 = (i16::min_value(), i16::min_value(), i16::min_value());
    let pos2 = (i16::max_value(), i16::max_value(), i16::max_value());
    println!("pos1: {:?};\npos2: {:?}", pos1, pos2);

    let pos1p = (pos1.0 as f64, pos1.1 as f64, pos1.2 as f64);
    let pos2p = (pos2.0 as f64, pos2.1 as f64, pos2.2 as f64);
    let test_distance = ((pos1p.0 - pos2p.0).powi(2) + (pos1p.1 - pos2p.1).powi(2) +
      (pos1p.2 - pos2p.2).powi(2))
      .sqrt();
    println!("My calculated distance: {:?};", test_distance);
    assert_eq!(
      (TileSystem::Realistic.point3_distance(pos1, pos2)),
      Distance::from_meters(test_distance as f32)
    );
  }

  #[test]
  fn test_simple_distance() {
    // Points are in meters, so the distance between 0 and 1 should be 100 centimeters
    let pos1 = (0, 0, 0);
    let pos2 = (1, 0, 0);
    assert_eq!(TileSystem::Realistic.point3_distance(pos1, pos2), Distance(100));
  }

  #[test]
  fn test_diagonal_distance() {
    let pos1 = (0, 0, 0);
    let pos2 = (1, 1, 0);
    assert_eq!(
      TileSystem::Realistic.point3_distance(pos1, pos2),
      Distance::from_meters(2.0f32.sqrt())
    );
  }

  #[test]
  fn test_neighbors() {
    let terrain = huge_box();
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let mut pts = TileSystem::Realistic.point3_neighbors(&terrain, size, (0, 0, 0));
    pts.sort();
    let mut expected = vec![
      ((-1, 0, 0), 100),
      ((1, 0, 0), 100),
      ((0, -1, 0), 100),
      ((0, 1, 0), 100),
      ((-1, -1, 0), 141),
      ((1, 1, 0), 141),
      ((-1, 1, 0), 141),
      ((1, -1, 0), 141),
    ];
    expected.sort();
    assert_eq!(pts, expected)
  }

  /// a diagonal neighbor is not considered accessible if it "goes around" a blocked corner
  #[test]
  fn test_neighbors_around_corners() {
    let terrain = Map::new("single".to_string(), vec![(1, 0, 0)]);
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let pts: Vec<Point3> = TileSystem::Realistic
      .point3_neighbors(&terrain, size, (0, 0, 0))
      .iter()
      .map(|&(p, _)| p)
      .collect();
    assert!(!pts.contains(&(1, 1, 0)));
    assert!(!pts.contains(&(1, -1, 0)));
  }

  #[test]
  fn pathfinding_astar_multi() {
    let start = (0, 0, 0);
    let success = Box::new(|n: &Point3| *n == (2, 2, 0));
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let paths_and_costs = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| TileSystem::Realistic.point3_distance(start, *n).0,
      u32::max_value(),
      vec![success],
    );
    let ex_path = vec![(0, 0, 0), (1, 1, 0), (2, 2, 0)];
    assert_eq!(paths_and_costs, [(ex_path, 282)]);
  }

  #[test]
  fn astar_multi_max_cost() {
    let start = (0, 0, 0);
    let success = Box::new(|n: &Point3| *n == (5, 0, 0));
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let result = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| TileSystem::Realistic.point3_distance(start, *n).0,
      499,
      vec![success],
    );
    assert_eq!(result, vec![]);
  }

  #[test]
  fn astar_multi_eq_max_cost() {
    let start = (0, 0, 0);
    let success = Box::new(|n: &Point3| *n == (5, 0, 0));
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let result = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| TileSystem::Realistic.point3_distance(start, *n).0,
      500,
      vec![success],
    );
    assert_eq!(
      result,
      vec![
        (
          vec![
            (0, 0, 0),
            (1, 0, 0),
            (2, 0, 0),
            (3, 0, 0),
            (4, 0, 0),
            (5, 0, 0),
          ],
          500,
        ),
      ]
    );
  }


  #[test]
  fn pathfinding_astar_multi_2() {
    let start = (0, 0, 0);
    let successes: Vec<Box<Fn(&Point3) -> bool>> = vec![
      Box::new(|n: &Point3| *n == (1, 1, 0)),
      Box::new(|n: &Point3| *n == (-1, -1, 0)),
    ];
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let paths_and_costs = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| TileSystem::Realistic.point3_distance(start, *n).0,
      u32::max_value(),
      successes,
    );
    let ex_path_positive = vec![(0, 0, 0), (1, 1, 0)];
    let ex_path_negative = vec![(0, 0, 0), (-1, -1, 0)];
    assert_eq!(paths_and_costs, [(ex_path_positive, 141), (ex_path_negative, 141)]);
  }

  #[test]
  fn test_accessible_nowhere_to_go() {
    let terrain = box_map();
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    assert_eq!(
      TileSystem::Realistic.get_all_accessible((0, 0, 0), &terrain, size, Distance(1000)),
      vec![]
    );
  }

  #[test]
  fn test_accessible_small_limit() {
    // a speed of 100 means you can only move on the axes
    let terrain = huge_box();
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let mut pts =
      TileSystem::Realistic.get_all_accessible((0, 0, 0), &terrain, size, Distance(100));
    pts.sort();
    let mut expected = vec![(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)];
    expected.sort();
    assert_eq!(pts, expected)
  }

  #[test]
  fn test_accessible_less_small_limit() {
    // a speed of 141 means you can also move diagonally, but only once
    let terrain = huge_box();
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let mut pts =
      TileSystem::Realistic.get_all_accessible((0, 0, 0), &terrain, size, Distance(141));
    pts.sort();
    let mut expected = vec![
      (-1, 0, 0),
      (1, 0, 0),
      (0, -1, 0),
      (0, 1, 0),
      (-1, -1, 0),
      (1, 1, 0),
      (-1, 1, 0),
      (1, -1, 0),
    ];
    expected.sort();
    assert_eq!(pts, expected)
  }

  #[test]
  fn test_accessible_average_speed() {
    let terrain = huge_box();
    let size = Volume::AABB(AABB { x: 1, y: 1, z: 1 });
    let pts = TileSystem::Realistic.get_all_accessible((0, 0, 0), &terrain, size, Distance(1000));
    // NOTE: The reason this isn't 314 (pie are square of radius=100) is that we only allow
    // 8 degrees of movement, which leaves certain positions within a circle impossible to
    // reach even if you can technically move the radius of the circle in one turn.
    assert_eq!(pts.len(), 284);
  }

  extern crate test;
  use self::test::Bencher;
  #[bench]
  fn accessible_average_speed_bench(bencher: &mut Bencher) {
    bencher.iter(|| { test_accessible_average_speed(); });
  }

  #[test]
  fn items_within_volume() {
    let ts = TileSystem::Realistic;
    let vol = Volume::Sphere(Distance(500));
    let vol_pt = (4, 4, 0);
    let item_1 = ("Elron", (-1, 0, 0));
    let item_2 = ("Kurok To", (1, 1, 0));
    let item_3 = ("Silmarillion", (0, 0, 0));
    let items = HashMap::from_iter(vec![item_1.clone(), item_2.clone(), item_3.clone()]);
    let results = ts.items_within_volume(vol, vol_pt, &items);
    println!("{:?}", results);
    for result in results.iter() {
      let result_pos = items.get(result).expect("Got result that wasn't in input");
      let dist = ts.point3_distance(*result_pos, vol_pt);
      println!("Checking distance between {:?} and {:?}: {:?}", result_pos, vol_pt, dist);
      assert!(dist <= Distance(500));
    }
    for (item, item_pos) in items.iter() {
      if !results.contains(item) {
        assert!(ts.point3_distance(*item_pos, vol_pt) > Distance(500))
      }
    }
  }

  #[test]
  fn points_in_volume() {
    let ts = TileSystem::Realistic;
    let vol = Volume::AABB(AABB { x: 2, y: 2, z: 1 });
    let vol_pt = (1, 1, 0);
    let results = ts.points_in_volume(vol, vol_pt);
    assert_eq!(results, vec![(1, 1, 0), (1, 2, 0), (2, 1, 0), (2, 2, 0)]);
  }

  #[test]
  fn line_through_point_simple() {
    let line = line_through_point((0, 0, 0), (1, 0, 0), Distance(200));
    match line {
      Volume::Line { vector } => assert_eq!(vector, (200, 0, 0)),
      _ => panic!("Expected Line"),
    }
  }

  #[test]
  fn line_through_point_accuracy() {
    let line = line_through_point((0, 0, 0), (2, 1, 0), Distance(1000));
    match line {
      Volume::Line { vector } => assert_eq!(vector, (894, 447, 0)),
      _ => panic!("Expected Line"),
    }
  }

  #[test]
  fn large_creature_cannot_fit_through_small_opening() {
    let ts = TileSystem::Realistic;
    let dumbbell = dumbbell_map();
    let big_guy = Volume::AABB(AABB { x: 2, y: 2, z: 1 });
    let path = ts.find_path((0, 0, 0), Distance(1000), &dumbbell, big_guy, (3, 0, 0));
    assert_eq!(path, None);
  }

  #[test]
  fn large_creature_can_fit_through_large_opening() {
    let ts = TileSystem::Realistic;
    let mut dumbbell = dumbbell_map();
    dumbbell.terrain.push((2, 2, 0));
    let big_guy = Volume::AABB(AABB { x: 2, y: 2, z: 1 });
    let path = ts.find_path((0, 0, 0), Distance(1000), &dumbbell, big_guy, (3, 0, 0));
    assert_eq!(
      path,
      Some((vec![(0, 0, 0), (1, 1, 0), (2, 1, 0), (3, 1, 0), (3, 0, 0)], Distance(441)))
    );
  }
}
