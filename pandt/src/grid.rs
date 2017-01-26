extern crate nalgebra as na;
extern crate ncollide as nc;

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

    // PATHFINDING MESSING ABOUT

    use std::collections::VecDeque;
    use std::vec::IntoIter;
    use std::collections::HashSet;


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

    // pub fn astar_multi<N, C, FN, IN, FH, FS>(start: &N,
    //                                          neighbours: FN,
    //                                          heuristic: FH,
    //                                          mut successes: Vec<FS>)
    //                                          -> Vec<(Vec<N>, C)>
    //     where N: Eq + Hash + Clone,
    //           C: Zero + Ord + Copy,
    //           FN: Fn(&N) -> IN,
    //           IN: IntoIterator<Item = (N, C)>,
    //           FH: Fn(&N) -> C,
    //           FS: Fn(&N) -> bool
    // {
    //     let mut to_see = BinaryHeap::new();
    //     to_see.push(InvCmpHolder {
    //         key: heuristic(start),
    //         payload: (Zero::zero(), start.clone()),
    //     });
    //     let mut parents: HashMap<N, (N, C)> = HashMap::new();
    //     let mut results = vec![];
    //     while let Some(InvCmpHolder { payload: (cost, node), .. }) = to_see.pop() {
    //         for success_idx in 0..successes.len() {
    //             let was_successful = {
    //                 let success = &successes[success_idx];
    //                 if success(&node) {
    //                     let parents = parents.into_iter().map(|(n, (p, _))| (n, p)).collect();
    //                     results.push((reverse_path(parents, node.clone()), cost));
    //                     true
    //                 } else {
    //                     false
    //                 }
    //             };
    //             if was_successful {
    //                 successes.remove(success_idx);
    //                 continue;
    //             }
    //         }
    //         // We may have inserted a node several time into the binary heap if we found
    //         // a better way to access it. Ensure that we are currently dealing with the
    //         // best path and discard the others.
    //         if let Some(&(_, c)) = parents.get(&node) {
    //             if cost > c {
    //                 continue;
    //             }
    //         }
    //         for (neighbour, move_cost) in neighbours(&node) {
    //             let old_cost = parents.get(&neighbour).map(|&(_, c)| c);
    //             let new_cost = cost + move_cost;
    //             if neighbour != *start && old_cost.map_or(true, |c| new_cost < c) {
    //                 parents.insert(neighbour.clone(), (node.clone(), new_cost));
    //                 let new_predicted_cost = new_cost + heuristic(&neighbour);
    //                 to_see.push(InvCmpHolder {
    //                     key: new_predicted_cost,
    //                     payload: (new_cost, neighbour),
    //                 });
    //             }
    //         }
    //     }
    //     results
    // }

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
                if !terrain.contains(&neighbor) {
                    let cost = if x.abs() == y.abs() { 141 } else { 100 };
                    results.push((neighbor, cost));
                }
            }
        }
        results
    }

    #[test]
    fn test_neighbors() {
        let terrain = vec![];
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


    #[test]
    fn pathfinding_astar() {
        let start = (0, 0, 0);
        let (path, cost) = astar(&start,
                                 |n| point3_neighbors(&vec![], *n),
                                 |n| point3_distance(start, *n).0,
                                 |n| *n == (2, 2, 0))
            .unwrap();
        assert_eq!(cost, 282);
        assert_eq!(path, vec![(0, 0, 0), (1, 1, 0), (2, 2, 0)]);
    }

    /// A map containing a "room" that only has one free space in the middle.
    fn box_map() -> Map {
        vec![// left wall
             (-1, -1, 0),
             (-1, 0, 0),
             (-1, 1, 0),
             // top wall
             (0, -1, 0),
             // right wall
             (1, -1, 0),
             (1, 0, 0),
             (1, 1, 0),
             // bottom wall
             (0, 1, 0)]
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

    // #[test]
    // fn test_accessible_nowhere_to_go() {
    //     let terrain = box_map();
    //     assert_eq!(get_all_accessible((0, 0, 0), &terrain, Distance(9000)),
    //                vec![]);
    // }

    // #[test]
    // fn test_accessible_small_limit() {
    //     // a speed of 100 means you can only move on the axes
    //     let terrain = vec![];
    //     let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(100));
    //     pts.sort();
    //     let mut expected = vec![(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)];
    //     expected.sort();
    //     assert_eq!(pts, expected)
    // }

    // #[test]
    // fn test_accessible_less_small_limit() {
    //     // a speed of 141 means you can also move diagonally, but only once
    //     let terrain = vec![];
    //     let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(141));
    //     pts.sort();
    //     let mut expected = vec![(-1, 0, 0),
    //                             (1, 0, 0),
    //                             (0, -1, 0),
    //                             (0, 1, 0),
    //                             (-1, -1, 0),
    //                             (1, 1, 0),
    //                             (-1, 1, 0),
    //                             (1, -1, 0)];
    //     expected.sort();
    //     assert_eq!(pts, expected)
    // }

    // #[test]
    // #[bench]
    // fn test_accessible_average_speed() {
    //     let terrain = vec![];
    //     let pts = get_all_accessible((0, 0, 0), &terrain, Distance(1000));
    //     assert_eq!(pts.len(), 316);
    // }

}
