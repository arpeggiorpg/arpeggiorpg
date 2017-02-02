extern crate nalgebra as na;
extern crate ncollide as nc;
use odds::vec::VecExt;

use self::na::{Isometry3, Vector3};
use self::nc::shape::Cuboid;
use self::nc::query::PointQuery;

use types::*;
use creature::*;

// I got curious about how to implement this in integer math.
// the maximum distance on a grid of i16 positions (−32768 to 32767) is....?
// √((x₂ - x₁)² + (y₂ - y₁)² + (z₂ - z₁)²)
// √((32767 - −32768)² + (32767 - −32768)² + (32767 - −32768)²)
// √(65535² + 65535² + 65535²)
// √(4,294,836,225 + 4,294,836,225 + 4,294,836,225) (each close to the limit of 32-bit integers)
// √(12,884,901,888) // NOTE! This number requires a (signed-ok) 64-bit integer to store!
// 113511.68172483394 -- as an integer, requires a (signed-ok) 32.
// so we need a i32/u32 for the result, and we need to use a i64/u64 for the calculation.



pub fn point3_distance(pos1: Point3, pos2: Point3) -> Distance {
    let meaningless = Cuboid::new(Vector3::new(0.0, 0.0, 0.0));
    let ncpos1 = Isometry3::new(Vector3::new(pos1.0 as f32, pos1.1 as f32, pos1.2 as f32),
                                na::zero());
    let ncpos2 = na::Point3::new(pos2.0 as f32, pos2.1 as f32, pos2.2 as f32);
    let distance = meaningless.distance_to_point(&ncpos1, &ncpos2, false);
    Distance::new(distance)
}

pub fn creature_within_distance(c1: &Creature, c2: &Creature, d: Distance) -> bool {
    point3_distance(c1.pos(), c2.pos()) <= d
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
/// parents: a HashMap containing a DAG of nodes, decorated with the cost of those connections.
/// start: The node to start at, but actually *this is the *END* node of the A* search!!!!
fn reverse_path<N: Eq + Hash>(mut parents: HashMap<N, N>, start: N) -> Vec<N> {
    let mut path = vec![start];
    while let Some(parent) = parents.remove(path.last().unwrap()) {
        path.push(parent);
    }
    path.into_iter().rev().collect()
}

pub fn astar<N, C, FN, IN, FH, FS>(start: &N,
                                   neighbours: FN,
                                   heuristic: FH,
                                   success: FS)
                                   -> Option<(Vec<N>, C)>
    where N: Eq + Hash + Clone,
          C: Zero + Ord + Copy,
          FN: Fn(&N) -> IN,
          IN: IntoIterator<Item = (N, C)>,
          FH: Fn(&N) -> C,
          FS: Fn(&N) -> bool
{
    let mut to_see = BinaryHeap::new();
    to_see.push(InvCmpHolder {
        key: heuristic(start),
        payload: (Zero::zero(), start.clone()),
    });
    let mut parents: HashMap<N, (N, C)> = HashMap::new();
    while let Some(InvCmpHolder { payload: (cost, node), .. }) = to_see.pop() {
        if success(&node) {
            let parents = parents.into_iter().map(|(n, (p, _))| (n, p)).collect();
            return Some((reverse_path(parents, node), cost));
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
            if neighbour != *start && old_cost.map_or(true, |c| new_cost < c) {
                parents.insert(neighbour.clone(), (node.clone(), new_cost));
                let new_predicted_cost = new_cost + heuristic(&neighbour);
                to_see.push(InvCmpHolder {
                    key: new_predicted_cost,
                    payload: (new_cost, neighbour),
                });
            }
        }
    }
    None
}

pub fn astar_multi<N, C, FN, IN, FH>(start: &N,
                                     neighbours: FN,
                                     heuristic: FH,
                                     max_cost: C,
                                     mut successes: Vec<Box<Fn(&N) -> bool>>)
                                     -> Vec<(Vec<N>, C)>
    where N: Eq + Hash + Clone,
          C: Zero + Ord + Copy + PartialEq + PartialOrd, // maybe relax these so floats can be used?
          FN: Fn(&N) -> IN,
          IN: IntoIterator<Item = (N, C)>,
          FH: Fn(&N) -> C
{
    let mut to_see = BinaryHeap::new();
    to_see.push(InvCmpHolder {
        key: heuristic(start),
        payload: (Zero::zero(), start.clone()),
    });
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
        if successes.len() == 0 {
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
            if neighbour != *start && old_cost.map_or(true, |c| new_cost < c) &&
               new_cost <= max_cost {
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

fn point3_neighbors(terrain: &Map, pt: Point3) -> Vec<(Point3, u32)> {
    let mut results = vec![];
    for x in -1..2 {
        for y in -1..2 {
            if (x, y) == (0, 0) {
                continue;
            }
            let neighbor = (pt.0 + x, pt.1 + y, pt.2);
            if terrain.contains(&neighbor) {
                let is_angle = x.abs() == y.abs();
                let cost = if is_angle { 141 } else { 100 };
                // don't allow diagonal movement around corners
                if is_angle && !terrain.contains(&(neighbor.0, pt.1, pt.2)) ||
                   !terrain.contains(&(pt.0, neighbor.1, pt.2)) {
                    continue;
                }
                results.push((neighbor, cost));
            }
        }
    }
    results
}

pub fn get_all_accessible(start: Point3, terrain: &Map, speed: Distance) -> Vec<Point3> {
    let meters = (speed.0 / 100) as i16;
    let mut points_to_check = vec![];
    for x in start.0 - meters..start.0 + meters + 1 {
        for y in start.1 - meters..start.1 + meters + 1 {
            let end_point = (x, y, 0);
            if end_point == start {
                continue;
            }
            points_to_check.push(end_point);
        }
    }
    // println!("Number of points to check: {:?}", points_to_check.len());
    let mut success_fns: Vec<Box<Fn(&Point3) -> bool>> = vec![];
    for pt in points_to_check {
        success_fns.push(Box::new(move |n: &Point3| *n == pt.clone()));
    }
    let mut final_points = vec![];
    for (path, cost) in astar_multi(&start,
                                    |n| point3_neighbors(terrain, *n),
                                    |n| point3_distance(start, *n).0,
                                    speed.0,
                                    success_fns) {
        if Distance(cost) <= speed {
            // FIXME: we should NOT be checking cost here, instead astar_multi should support
            // max distance.
            final_points.push(path.last().unwrap().clone())
        }
    }
    final_points
}

pub fn find_path(start: Point3,
                 speed: Distance,
                 terrain: &Map,
                 destination: Point3)
                 -> Option<(Vec<Point3>, Distance)> {
    let success = Box::new(move |n: &Point3| *n == destination);
    let result: Vec<(Vec<Point3>, u32)> = astar_multi(&start,
                                                      |n| point3_neighbors(terrain, *n),
                                                      |n| point3_distance(start, *n).0,
                                                      speed.0,
                                                      vec![success]);
    if let Some((path, cost)) = result.into_iter().next() {
        Some((path, Distance(cost)))
    } else {
        None
    }
}


#[cfg(test)]
pub mod test {
    use grid::*;

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
        assert_eq!((point3_distance(pos1, pos2)),
                   Distance::new(test_distance as f32));
    }

    #[test]
    fn test_simple_distance() {
        // Points are in meters, so the distance between 0 and 1 should be 100 centimeters
        let pos1 = (0, 0, 0);
        let pos2 = (1, 0, 0);
        assert_eq!(point3_distance(pos1, pos2), Distance(100));
    }

    #[test]
    fn test_diagonal_distance() {
        let pos1 = (0, 0, 0);
        let pos2 = (1, 1, 0);
        assert_eq!(point3_distance(pos1, pos2), Distance::new(2.0f32.sqrt()));
    }

    #[test]
    fn test_neighbors() {
        let terrain = huge_box();
        let mut pts = point3_neighbors(&terrain, (0, 0, 0));
        pts.sort();
        let mut expected = vec![((-1, 0, 0), 100),
                                ((1, 0, 0), 100),
                                ((0, -1, 0), 100),
                                ((0, 1, 0), 100),
                                ((-1, -1, 0), 141),
                                ((1, 1, 0), 141),
                                ((-1, 1, 0), 141),
                                ((1, -1, 0), 141)];
        expected.sort();
        assert_eq!(pts, expected)
    }

    /// a diagonal neighbor is not considered accessible if it "goes around" a blocked corner
    #[test]
    fn test_neighbors_around_corners() {
        let terrain = vec![(1, 0, 0)];
        let pts: Vec<Point3> =
            point3_neighbors(&terrain, (0, 0, 0)).iter().map(|&(p, _)| p).collect();
        assert!(!pts.contains(&(1, 1, 0)));
        assert!(!pts.contains(&(1, -1, 0)));
    }


    #[test]
    fn pathfinding_astar() {
        let start = (0, 0, 0);
        let (path, cost) = astar(&start,
                                 |n| point3_neighbors(&huge_box(), *n),
                                 |n| point3_distance(start, *n).0,
                                 |n| *n == (2, 2, 0))
            .unwrap();
        assert_eq!(cost, 282);
        assert_eq!(path, vec![(0, 0, 0), (1, 1, 0), (2, 2, 0)]);
    }

    #[test]
    fn pathfinding_astar_multi() {
        let start = (0, 0, 0);
        let success = Box::new(|n: &Point3| *n == (2, 2, 0));
        let paths_and_costs = astar_multi(&start,
                                          |n| point3_neighbors(&huge_box(), *n),
                                          |n| point3_distance(start, *n).0,
                                          u32::max_value(),
                                          vec![success]);
        let ex_path = vec![(0, 0, 0), (1, 1, 0), (2, 2, 0)];
        assert_eq!(paths_and_costs, [(ex_path, 282)]);
    }

    #[test]
    fn astar_multi_max_cost() {
        let start = (0, 0, 0);
        let success = Box::new(|n: &Point3| *n == (5, 0, 0));
        let result = astar_multi(&start,
                                 |n| point3_neighbors(&huge_box(), *n),
                                 |n| point3_distance(start, *n).0,
                                 499,
                                 vec![success]);
        assert_eq!(result, vec![]);
    }

    #[test]
    fn astar_multi_eq_max_cost() {
        let start = (0, 0, 0);
        let success = Box::new(|n: &Point3| *n == (5, 0, 0));
        let result = astar_multi(&start,
                                 |n| point3_neighbors(&huge_box(), *n),
                                 |n| point3_distance(start, *n).0,
                                 500,
                                 vec![success]);
        assert_eq!(result,
                   vec![(vec![(0, 0, 0), (1, 0, 0), (2, 0, 0), (3, 0, 0), (4, 0, 0), (5, 0, 0)],
                         500)]);
    }


    #[test]
    fn pathfinding_astar_multi_2() {
        let start = (0, 0, 0);
        let successes: Vec<Box<Fn(&Point3) -> bool>> = vec![Box::new(|n: &Point3| *n == (1, 1, 0)),
                                                            Box::new(|n: &Point3| *n == (-1, -1, 0))];
        let paths_and_costs = astar_multi(&start,
                                          |n| point3_neighbors(&huge_box(), *n),
                                          |n| point3_distance(start, *n).0,
                                          u32::max_value(),
                                          successes);
        let ex_path_positive = vec![(0, 0, 0), (1, 1, 0)];
        let ex_path_negative = vec![(0, 0, 0), (-1, -1, 0)];
        assert_eq!(paths_and_costs,
                   [(ex_path_positive, 141), (ex_path_negative, 141)]);
    }

    /// A map containing a "room" that only has one free space in the middle.
    fn box_map() -> Map {
        vec![(0, 0, 0)]
    }

    pub fn huge_box() -> Map {
        let mut map = vec![];
        for x in -20..20 {
            for y in -20..20 {
                map.push((x, y, 0));
            }
        }
        map
    }

    // #[test]
    // fn path_inaccessible() {
    //     let terrain = box_map();
    //     let mut gs = GridState {
    //         start: (3, 3, 0),
    //         end: (0, 0, 0),
    //         max_distance: 1000,
    //         terrain: &terrain,
    //     };
    //     assert_eq!(astar(&mut gs), None);
    // }

    #[test]
    fn test_accessible_nowhere_to_go() {
        let terrain = box_map();
        assert_eq!(get_all_accessible((0, 0, 0), &terrain, Distance(1000)),
                   vec![]);
    }

    #[test]
    fn test_accessible_small_limit() {
        // a speed of 100 means you can only move on the axes
        let terrain = huge_box();
        let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(100));
        pts.sort();
        let mut expected = vec![(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)];
        expected.sort();
        assert_eq!(pts, expected)
    }

    #[test]
    fn test_accessible_less_small_limit() {
        // a speed of 141 means you can also move diagonally, but only once
        let terrain = huge_box();
        let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(141));
        pts.sort();
        let mut expected = vec![(-1, 0, 0),
                                (1, 0, 0),
                                (0, -1, 0),
                                (0, 1, 0),
                                (-1, -1, 0),
                                (1, 1, 0),
                                (-1, 1, 0),
                                (1, -1, 0)];
        expected.sort();
        assert_eq!(pts, expected)
    }

    #[test]
    fn test_accessible_average_speed() {
        let terrain = huge_box();
        let pts = get_all_accessible((0, 0, 0), &terrain, Distance(1000));
        // NOTE: The reason this isn't 314 (pie are square of radius=100) is that we only allow
        // 8 degrees of movement, which leaves certain positions within a circle impossible to
        // reach even if you can technically move the radius of the circle in one turn.
        assert_eq!(pts.len(), 284);
    }

    extern crate test;
    use self::test::Bencher;
    #[bench]
    fn accessible_average_speed_bench(bencher: &mut Bencher) {
        bencher.iter(|| {
            test_accessible_average_speed();
        });
    }
}
