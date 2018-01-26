use std::cmp;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::time;
use bresenham;
use odds::vec::VecExt;

use nalgebra as na;
use nalgebra::{Isometry3, Vector3};
use ncollide::shape;
use ncollide::shape::Cuboid;
use ncollide::query::PointQuery;
use ncollide::world;
use num_traits::Signed;
use uom::si::length::{centimeter, meter};

use types::{CollisionData, CollisionWorld, ConditionID, Creature, Point3, Terrain, TileSystem,
            Volume, VolumeCondition, i64cm, u32cm, u32units};

// unimplemented!: "burst"-style AoE effects, and "wrap-around-corner" AoE effects.
// This needs to be implemented for both Spheres and Circles (or VerticalCylinder?)
//
// Grenades should be "burst"-style spheres. These basically "cast" the effect outward from the
// center, and are blocked by any solid terrain. This could be implemented by raycasting from
// the origin to every point within a particular radius of the origin.
//
// Abilities like D&D's "fireball" and P&T's "thorn patch" should be "wrap-around-corner" AoE
// effects: Fireball is a sphere and Thorn Patch is a Circle or VerticalCylinder{height=1}.
//
// These "crawl" out from the origin point and can go anywhere within the radius that has a valid.
// path. This could be implemented exactly the same way we implement potential walk-targets.
// However, for 3d shapes this might get expensive...

// I got curious about how to implement this in integer math.
// the maximum distance on a grid of i16 positions (−32768 to 32767) is....?
// √((x₂ - x₁)² + (y₂ - y₁)² + (z₂ - z₁)²)
// √((32767 - −32768)² + (32767 - −32768)² + (32767 - −32768)²)
// √(65535² + 65535² + 65535²)
// √(4,294,836,225 + 4,294,836,225 + 4,294,836,225) (each close to the limit of 32-bit integers).
// √(12,884,901,888) // NOTE! This number requires a (signed-ok) 64-bit integer to store!
// 113511.68172483394 -- as an integer, requires a (signed-ok) 32.
// so we need a i32/u32 for the result, and we need to use a i64/u64 for the calculation.

fn na_iso(pt: Point3) -> Isometry3<f64> { Isometry3::new(na_vector(pt), na::zero()) }

fn na_point(pt: Point3) -> na::Point3<f64> {
  // this is a potential representation error: max i64 does not fit in f64.
  na::Point3::new(
    pt.x.get(centimeter) as f64,
    pt.y.get(centimeter) as f64,
    pt.z.get(centimeter) as f64,
  )
}

fn na_vector(pt: Point3) -> Vector3<f64> {
  // this is a potential representation error: max i64 does not fit in f64.
  Vector3::new(
    pt.x.get(centimeter) as f64,
    pt.y.get(centimeter) as f64,
    pt.z.get(centimeter) as f64,
  )
}

fn na_vector_to_point3(v: Vector3<f64>) -> Point3 {
  Point3::new(
    (v[0] * 100.0) as i64,
    (v[1] * 100.0) as i64,
    (v[2] * 100.0) as i64,
  )
}

pub fn line_through_point(origin: Point3, clicked: Point3, length: u32units::Length) -> Volume {
  let offset = point3_difference(clicked, origin);
  let mut navec = na_vector(offset);
  navec.normalize_mut();
  let new_vec = navec * length.get(meter) as f64;
  Volume::Line {
    vector: na_vector_to_point3(new_vec),
  }
}

/// Get the vector difference between two points, i.e., the offset of pt2 from pt1.
/// This returns a plain old Point3, but it'd be nicer if it we re a `Point3Difference`...
pub fn point3_difference(pt1: Point3, pt2: Point3) -> Point3 {
  Point3::from_quantities(pt1.x - pt2.x, pt1.y - pt2.y, pt1.z - pt2.z)
}

pub fn point3_add_vec(pt: Point3, diff: Point3) -> Point3 {
  Point3::new(
    // TODO RADIX: actually treat Point3 as centimeters!
    (pt.x.get(centimeter) * 100 + diff.x.get(centimeter)) / 100,
    (pt.y.get(centimeter) * 100 + diff.y.get(centimeter)) / 100,
    (pt.z.get(centimeter) * 100 + diff.z.get(centimeter)) / 100,
  )
}

fn is_open(terrain: &Terrain, pt: Point3) -> bool { terrain.contains(&pt) }

impl TileSystem {
  /// Get the distance between two points, considering the system being used.
  /// In DnD, an angular distance is "equivalent" to a horizontal/vertical distance.
  /// i.e., The distance from 0,0 to 1,1 is sqrt(2) in Realistic and 1.0 in DnD.
  pub fn point3_distance(&self, pos1: Point3, pos2: Point3) -> u32units::Length {
    match *self {
      TileSystem::Realistic => {
        let meaningless = Cuboid::new(Vector3::new(0.0, 0.0, 0.0));
        let ncpos1 = na_iso(pos1);
        let ncpos2 = na_point(pos2);
        let distance = meaningless.distance_to_point(&ncpos1, &ncpos2, false);
        u32units::Length::new::<centimeter>(distance as u32)
      }
      TileSystem::DnD => {
        let xdiff = (pos1.x - pos2.x).abs();
        let ydiff = (pos1.y - pos2.y).abs();
        u32cm((cmp::max(xdiff, ydiff)).get(centimeter) as u32)
      }
    }
  }

  /// Check whether two points are within some distance of each other.
  pub fn points_within_distance(&self, c1: Point3, c2: Point3, d: u32units::Length) -> bool {
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
            (pt.x.get(centimeter) as isize, pt.y.get(centimeter) as isize),
            (
              dest.x.get(centimeter) as isize,
              dest.y.get(centimeter) as isize,
            ),
          ).map(|(x, y)| Point3::new(x as i64, y as i64, 0)),
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

  pub fn open_points_in_range(
    &self, start: Point3, terrain: &Terrain, range: u32units::Length
  ) -> Vec<Point3> {
    let cm: u32 = range.get(centimeter);
    let meters = (cm / 100) as i64;
    let mut open = vec![];
    for x in start.x.get(centimeter) - meters..start.x.get(centimeter) + meters + 1 {
      for y in start.y.get(centimeter) - meters..start.y.get(centimeter) + meters + 1 {
        let end_point = Point3::new(x, y, 0);
        if !is_open(terrain, end_point) {
          continue;
        }
        open.push(end_point);
      }
    }
    open
  }

  /// Get the set of points which can be pathed to from some point.
  pub fn get_all_accessible(
    &self, start: Point3, terrain: &Terrain, volume: Volume, speed: u32units::Length
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
    let start_time = time::Instant::now();
    let path_result = astar_multi(
      &start,
      |n| self.point3_neighbors(terrain, volume, *n),
      |n| self.point3_distance(start, *n).get(centimeter),
      speed.get(centimeter),
      success_fns,
    );
    let end_time = time::Instant::now();
    println!("astar_multi time: {:?}", end_time - start_time);

    for (path, cost) in path_result {
      if u32cm(cost) <= speed {
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
    &self, start: Point3, speed: u32units::Length, terrain: &Terrain, volume: Volume,
    destination: Point3,
  ) -> Option<(Vec<Point3>, u32units::Length)> {
    let success = Box::new(move |n: &Point3| *n == destination);
    let result: Vec<(Vec<Point3>, u32)> = astar_multi(
      &start,
      |n| self.point3_neighbors(terrain, volume, *n),
      |n| self.point3_distance(start, *n).get(centimeter),
      speed.get(centimeter),
      vec![success],
    );
    if let Some((path, cost)) = result.into_iter().next() {
      Some((path, u32cm(cost)))
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
      // sadly uom doesn't implement Step for Quantity
      Volume::AABB(aabb) => {
        let max = aabb.get_max(pt);
        (pt.x.get(centimeter)..(max.x.get(centimeter))).flat_map(|x| {
          (pt.y.get(centimeter)..(max.y.get(centimeter))).flat_map(move |y| {
            (pt.z.get(centimeter)..(max.z.get(centimeter)))
              .map(move |z| Point3::new(x, y, z))
          })
        })
        .collect()
      }
      Volume::Line { .. } => unimplemented!("points_in_volume for Line"),
      Volume::VerticalCylinder { .. } => {
        unimplemented!("unimplemented: points_in_volume for VerticalCylinder")
      }
    }
  }

  /// Determine whether a volume will not collide *with terrain* if it is placed at a point.
  /// Note that this doesn't consider other creatures or other map objects.
  fn volume_fits_at_point(&self, volume: Volume, terrain: &Terrain, pt: Point3) -> bool {
    for pt in self.points_in_volume(volume, pt) {
      if !terrain.contains(&pt) {
        return false;
      }
    }
    true
  }

  /// Find neighbors of the given point that the given volume can fit in, given the terrain.
  fn point3_neighbors(&self, terrain: &Terrain, volume: Volume, pt: Point3) -> Vec<(Point3, u32)> {
    let mut results = vec![];
    for x in -1..2 {
      for y in -1..2 {
        if (x, y) == (0, 0) {
          continue;
        }
        let neighbor = Point3::from_quantities(pt.x + i64cm(x), pt.y + i64cm(y), pt.z);
        if is_open(terrain, neighbor) && self.volume_fits_at_point(volume, terrain, neighbor) {
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
          if is_angle && !is_open(terrain, Point3::from_quantities(neighbor.x, pt.y, pt.z))
            || !is_open(terrain, Point3::from_quantities(pt.x, neighbor.y, pt.z))
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

/// Make a `CollisionWorld` given some creatures and volume conditions.
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

fn volume_to_na_shape(volume: Volume) -> shape::ShapeHandle3<f64> {
  match volume {
    Volume::Sphere(r) => {
      shape::ShapeHandle3::new(shape::Ball::new(r.get(centimeter) as f64))
    }
    Volume::AABB(aabb) => shape::ShapeHandle3::new(shape::Cuboid::new(Vector3::new(
      (f64::from(aabb.x.get(centimeter))) / 2.0,
      (f64::from(aabb.y.get(centimeter))) / 2.0,
      (f64::from(aabb.z.get(centimeter))) / 2.0,
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
  fn eq(&self, other: &InvCmpHolder<K, P>) -> bool { self.key.eq(&other.key) }
}

impl<K: PartialEq, P> Eq for InvCmpHolder<K, P> {}

impl<K: PartialOrd, P> PartialOrd for InvCmpHolder<K, P> {
  fn partial_cmp(&self, other: &InvCmpHolder<K, P>) -> Option<Ordering> {
    other.key.partial_cmp(&self.key)
  }
}

impl<K: Ord, P> Ord for InvCmpHolder<K, P> {
  fn cmp(&self, other: &InvCmpHolder<K, P>) -> Ordering { other.key.cmp(&self.key) }
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
  to_see.push(InvCmpHolder {
    key: heuristic(start),
    payload: (Zero::zero(), start.clone()),
  });
  let mut parents: HashMap<N, (N, C)> = HashMap::new();
  let mut found_nodes = vec![];
  while let Some(InvCmpHolder {
    payload: (cost, node),
    ..
  }) = to_see.pop()
  {
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
        to_see.push(InvCmpHolder {
          key: new_predicted_cost,
          payload: (new_cost, neighbour),
        });
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
  use types::*;

  /// A map containing a single open block of terrain at 0,0,0
  fn box_map() -> Terrain { vec![Point3::new(0, 0, 0)] }

  /// A map shaped like a dumbbell, with two 2x3 rooms connected by a 1x1 passage
  fn dumbbell_map() -> Terrain {
    vec![
      Point3::new(0, 0, 0),
      Point3::new(100, 0, 0),
      Point3::new(300, 0, 0),
      Point3::new(400, 0, 0),
      Point3::new(0, 100, 0),
      Point3::new(100, 100, 0),
      Point3::new(200, 100, 0),
      Point3::new(300, 100, 0),
      Point3::new(400, 100, 0),
      Point3::new(0, 200, 0),
      Point3::new(100, 200, 0),
      Point3::new(300, 200, 0),
      Point3::new(400, 200, 0),
    ]
  }

  /// A map containing a large 40-meter square of open terrain.
  pub fn huge_box() -> Terrain {
    let mut map = vec![];
    for x in -20..20 {
      for y in -20..20 {
        map.push(Point3::new(x * 100, y * 100, 0));
      }
    }
    map
  }

  fn medium_size() -> AABB {
    AABB { x: u32cm(100), y: u32cm(100), z: u32cm(100)}
  }

  fn large_size() -> AABB {
    AABB { x: u32cm(200), y: u32cm(200), z: u32cm(100)}
  }

  #[test]
  fn test_simple_distance() {
    let pos1 = Point3::new(0, 0, 0);
    let pos2 = Point3::new(100, 0, 0);
    assert_eq!(
      TileSystem::Realistic.point3_distance(pos1, pos2),
      u32cm(100)
    );
  }

  #[test]
  fn test_diagonal_distance() {
    let pos1 = Point3::new(0, 0, 0);
    let pos2 = Point3::new(100, 100, 0);
    assert_eq!(
      TileSystem::Realistic.point3_distance(pos1, pos2),
      u32cm(141)
    );
  }

  #[test]
  fn test_neighbors() {
    let terrain = huge_box();
    let size = Volume::AABB(medium_size());
    let mut pts = TileSystem::Realistic.point3_neighbors(&terrain, size, Point3::new(0, 0, 0));
    pts.sort();
    let mut expected = vec![
      (Point3::new(-100, 0, 0), 100),
      (Point3::new(100, 0, 0), 100),
      (Point3::new(0, -100, 0), 100),
      (Point3::new(0, 100, 0), 100),
      (Point3::new(-100, -100, 0), 141),
      (Point3::new(100, 100, 0), 141),
      (Point3::new(-100, 100, 0), 141),
      (Point3::new(100, -100, 0), 141),
    ];
    expected.sort();
    assert_eq!(pts, expected)
  }

  /// a diagonal neighbor is not considered accessible if it "goes around" a blocked corner
  #[test]
  fn test_neighbors_around_corners() {
    let terrain = vec![Point3::new(100, 0, 0)];
    let size = Volume::AABB(medium_size());
    let pts: Vec<Point3> = TileSystem::Realistic
      .point3_neighbors(&terrain, size, Point3::new(0, 0, 0))
      .iter()
      .map(|&(p, _)| p)
      .collect();
    assert!(!pts.contains(&Point3::new(100, 100, 0)));
    assert!(!pts.contains(&Point3::new(100, -100, 0)));
  }

  #[test]
  fn pathfinding_astar_multi() {
    let start = Point3::new(0, 0, 0);
    let success = Box::new(|n: &Point3| *n == Point3::new(200, 200, 0));
    let size = Volume::AABB(medium_size());
    let paths_and_costs = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| {
        TileSystem::Realistic
          .point3_distance(start, *n)
          .get(centimeter)
      },
      u32::max_value(),
      vec![success],
    );
    let ex_path = vec![
      Point3::new(0, 0, 0),
      Point3::new(100, 100, 0),
      Point3::new(200, 200, 0),
    ];
    assert_eq!(paths_and_costs, [(ex_path, 282)]);
  }

  #[test]
  fn astar_multi_max_cost() {
    let start = Point3::new(0, 0, 0);
    let success = Box::new(|n: &Point3| *n == Point3::new(500, 0, 0));
    let size = Volume::AABB(medium_size());
    let result = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| {
        TileSystem::Realistic
          .point3_distance(start, *n)
          .get(centimeter)
      },
      499,
      vec![success],
    );
    assert_eq!(result, vec![]);
  }

  #[test]
  fn astar_multi_eq_max_cost() {
    let start = Point3::new(0, 0, 0);
    let success = Box::new(|n: &Point3| *n == Point3::new(5, 0, 0));
    let size = Volume::AABB(medium_size());
    let result = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| {
        TileSystem::Realistic
          .point3_distance(start, *n)
          .get(centimeter)
      },
      500,
      vec![success],
    );
    assert_eq!(
      result,
      vec![
        (
          vec![
            Point3::new(0, 0, 0),
            Point3::new(100, 0, 0),
            Point3::new(200, 0, 0),
            Point3::new(300, 0, 0),
            Point3::new(400, 0, 0),
            Point3::new(500, 0, 0),
          ],
          500,
        ),
      ]
    );
  }

  #[test]
  fn pathfinding_astar_multi_2() {
    let start = Point3::new(0, 0, 0);
    let successes: Vec<Box<Fn(&Point3) -> bool>> = vec![
      Box::new(|n: &Point3| *n == Point3::new(100, 100, 0)),
      Box::new(|n: &Point3| *n == Point3::new(-100, -100, 0)),
    ];
    let size = Volume::AABB(medium_size());
    let paths_and_costs = astar_multi(
      &start,
      |n| TileSystem::Realistic.point3_neighbors(&huge_box(), size, *n),
      |n| {
        TileSystem::Realistic
          .point3_distance(start, *n)
          .get(centimeter)
      },
      u32::max_value(),
      successes,
    );
    let ex_path_positive = vec![Point3::new(0, 0, 0), Point3::new(100, 100, 0)];
    let ex_path_negative = vec![Point3::new(0, 0, 0), Point3::new(-100, -100, 0)];
    assert_eq!(
      paths_and_costs,
      [(ex_path_positive, 141), (ex_path_negative, 141)]
    );
  }

  #[test]
  fn test_accessible_nowhere_to_go() {
    let terrain = box_map();
    let size = Volume::AABB(medium_size());
    assert_eq!(
      TileSystem::Realistic.get_all_accessible(Point3::new(0, 0, 0), &terrain, size, u32cm(1000)),
      vec![]
    );
  }

  #[test]
  fn test_accessible_small_limit() {
    // a speed of 100 means you can only move on the axes
    let terrain = huge_box();
    let size = Volume::AABB(medium_size());
    let mut pts =
      TileSystem::Realistic.get_all_accessible(Point3::new(0, 0, 0), &terrain, size, u32cm(100));
    pts.sort();
    let mut expected = vec![
      Point3::new(-100, 0, 0),
      Point3::new(100, 0, 0),
      Point3::new(0, -100, 0),
      Point3::new(0, 100, 0),
    ];
    expected.sort();
    assert_eq!(pts, expected)
  }

  #[test]
  fn test_accessible_less_small_limit() {
    // a speed of 141 means you can also move diagonally, but only once
    let terrain = huge_box();
    let size = Volume::AABB(medium_size());
    let mut pts =
      TileSystem::Realistic.get_all_accessible(Point3::new(0, 0, 0), &terrain, size, u32cm(141));
    pts.sort();
    let mut expected = vec![
      Point3::new(-100, 0, 0),
      Point3::new(100, 0, 0),
      Point3::new(0, -100, 0),
      Point3::new(0, 100, 0),
      Point3::new(-100, -100, 0),
      Point3::new(100, 100, 0),
      Point3::new(-100, 100, 0),
      Point3::new(100, -100, 0),
    ];
    expected.sort();
    assert_eq!(pts, expected)
  }

  #[test]
  fn test_accessible_average_speed() {
    let terrain = huge_box();
    let size = Volume::AABB(medium_size());
    let pts =
      TileSystem::Realistic.get_all_accessible(Point3::new(0, 0, 0), &terrain, size, u32cm(1000));
    // NOTE: The reason this isn't 314 (pie are square of radius=100) is that we only allow
    // 8 degrees of movement, which leaves certain positions within a circle impossible to
    // reach even if you can technically move the radius of the circle in one turn.
    assert_eq!(pts.len(), 284);
  }

  #[test]
  fn items_within_volume() {
    let ts = TileSystem::Realistic;
    let vol = Volume::Sphere(u32cm(500));
    let vol_pt = Point3::new(400, 400, 0);
    let items = hashmap!{
      "Elron" => Point3::new(-100, 0, 0),
      "Kurok To" => Point3::new(100, 100, 0),
      "Silmarillion" => Point3::new(0, 0, 0),
    };
    let results = ts.items_within_volume(vol, vol_pt, &items);
    println!("{:?}", results);
    for result in results.iter() {
      let result_pos = items.get(result).expect("Got result that wasn't in input");
      let dist = ts.point3_distance(*result_pos, vol_pt);
      println!(
        "Checking distance between {:?} and {:?}: {:?}",
        result_pos, vol_pt, dist
      );
      assert!(dist <= u32cm(500));
    }
    for (item, item_pos) in items.iter() {
      if !results.contains(item) {
        assert!(ts.point3_distance(*item_pos, vol_pt) > u32cm(500))
      }
    }
  }

  #[test]
  fn points_in_volume() {
    let ts = TileSystem::Realistic;
    let vol = Volume::AABB(large_size());
    let vol_pt = Point3::new(100, 100, 0);
    let results = ts.points_in_volume(vol, vol_pt);
    assert_eq!(
      results,
      vec![
        Point3::new(100, 100, 0),
        Point3::new(100, 200, 0),
        Point3::new(200, 100, 0),
        Point3::new(200, 200, 0),
      ]
    );
  }

  #[test]
  fn line_through_point_simple() {
    let line = line_through_point(Point3::new(0, 0, 0), Point3::new(100, 0, 0), u32cm(200));
    match line {
      Volume::Line { vector } => assert_eq!(vector, Point3::new(20_000, 0, 0)),
      _ => panic!("Expected Line"),
    }
  }

  #[test]
  fn line_through_point_accuracy() {
    let line = line_through_point(Point3::new(0, 0, 0), Point3::new(200, 100, 0), u32cm(1000));
    match line {
      Volume::Line { vector } => assert_eq!(vector, Point3::new(89_400, 44_700, 0)),
      _ => panic!("Expected Line"),
    }
  }

  #[test]
  fn large_creature_cannot_fit_through_small_opening() {
    let ts = TileSystem::Realistic;
    let dumbbell = dumbbell_map();
    let big_guy = Volume::AABB(large_size());
    let path = ts.find_path(
      Point3::new(0, 0, 0),
      u32cm(1000),
      &dumbbell,
      big_guy,
      Point3::new(300, 0, 0),
    );
    assert_eq!(path, None);
  }

  #[test]
  fn large_creature_can_fit_through_large_opening() {
    let ts = TileSystem::Realistic;
    let mut dumbbell = dumbbell_map();
    dumbbell.push(Point3::new(200, 200, 0));
    let big_guy = Volume::AABB(large_size());
    let path = ts.find_path(
      Point3::new(0, 0, 0),
      u32cm(1000),
      &dumbbell,
      big_guy,
      Point3::new(300, 0, 0),
    );
    assert_eq!(
      path,
      Some((
        vec![
          Point3::new(0, 0, 0),
          Point3::new(100, 100, 0),
          Point3::new(200, 100, 0),
          Point3::new(300, 100, 0),
          Point3::new(300, 0, 0),
        ],
        u32cm(441)
      ))
    );
  }
}
