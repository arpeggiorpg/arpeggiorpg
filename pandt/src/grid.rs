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




    // ASTAR STUFF


    use astar::{astar, SearchProblem, TwoDSearchProblem, ReusableSearchProblem};
    use std::collections::VecDeque;
    use std::vec::IntoIter;

    struct GridState<'a> {
        start: Point3,
        end: Point3,
        max_distance: u32,
        terrain: &'a Map,
    }

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

    impl<'a> SearchProblem for GridState<'a> {
        type Node = Point3;
        type Cost = u32;
        type Iter = IntoIter<(Self::Node, Self::Cost)>;

        fn start(&self) -> Self::Node {
            self.start
        }
        fn is_end(&self, other: &Self::Node) -> bool {
            other == &self.end
        }
        fn heuristic(&self, node: &Self::Node) -> Self::Cost {
            point3_distance(self.start, node.clone()).0
        }
        fn neighbors(&mut self, here: &Self::Node) -> Self::Iter {
            let mut vec = vec![];
            for i in -1..1 + 1 {
                for k in -1..1 + 1 {
                    if !(i == 0 && k == 0) {
                        let newpt = (here.0 + i, here.1 + k, 0);
                        if self.heuristic(&newpt) <= self.max_distance {
                            let cost = point3_distance(here.clone(), newpt).0;
                            if !self.terrain.contains(&newpt) {
                                vec.push((newpt, cost));
                            }
                        }
                    }
                }
            }
            vec.into_iter()
        }
    }

    fn path(start: Point3, end: Point3) -> Option<VecDeque<Point3>> {
        let terrain = vec![];
        let mut gs = GridState {
            start: start,
            end: end,
            max_distance: u32::max_value(),
            terrain: &terrain,
        };
        astar(&mut gs)
    }

    #[test]
    fn path_inaccessible() {
        let terrain = box_map();
        let mut gs = GridState {
            start: (3, 3, 0),
            end: (0, 0, 0),
            max_distance: 1000,
            terrain: &terrain,
        };
        assert_eq!(astar(&mut gs), None);
    }

    fn get_all_accessible(start: Point3, terrain: &Map, speed: Distance) -> Vec<Point3> {
        let meters = (speed.0 / 100) as i16;
        let mut results = vec![];
        for x in start.0 - meters..start.0 + meters + 1 {
            for y in start.1 - meters..start.1 + meters + 1 {
                if x == 0 && y == 0 {
                    continue;
                }
                let end_point = (x, y, 0);
                let mut gs = GridState {
                    start: start,
                    end: end_point,
                    max_distance: speed.0,
                    terrain: terrain,
                };
                if let Some(_) = astar(&mut gs) {
                    results.push(end_point);
                }
            }
        }
        results
    }

    #[test]
    fn test_accessible_nowhere_to_go() {
        let terrain = box_map();
        assert_eq!(get_all_accessible((0, 0, 0), &terrain, Distance(9000)),
                   vec![]);
    }

    #[test]
    fn test_accessible_small_limit() {
        // a speed of 100 means you can only move on the axes
        let terrain = vec![];
        let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(100));
        pts.sort();
        let mut expected = vec![(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0)];
        expected.sort();
        assert_eq!(pts, expected)
    }

    #[test]
    fn test_accessible_less_small_limit() {
        // a speed of 141 means you can also move diagonally, but only once
        let terrain = vec![];
        let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(141));
        pts.sort();
        let mut expected = vec![(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (-1,-1,0), (1,1,0), (-1,1,0), (1,-1,0)];
        expected.sort();
        assert_eq!(pts, expected)
    }

    #[test]
    fn test_accessible_average_speed() {
        let terrain = vec![];
        let mut pts = get_all_accessible((0, 0, 0), &terrain, Distance(1000));
        assert_eq!(pts.len(), 316);
    }


    #[test]
    fn test_iter() {
        let mut gs = GridState {
            start: (0, 0, 0),
            end: (0, 0, 0),
            max_distance: u32::max_value(),
            terrain: &vec![],
        };
        assert_eq!(gs.neighbors(&(0, 0, 0)).collect::<Vec<_>>(),
                   vec![((-1, -1, 0), 141),
                        ((-1, 0, 0), 100),
                        ((-1, 1, 0), 141),
                        ((0, -1, 0), 100),
                        ((0, 1, 0), 100),
                        ((1, -1, 0), 141),
                        ((1, 0, 0), 100),
                        ((1, 1, 0), 141)])
    }

    #[test]
    fn test_start_end() {
        let p = path((0, 0, 0), (0, 0, 0)).unwrap();
        assert_eq!(p, vec![(0, 0, 0)].into_iter().collect());
    }

    #[test]
    fn test_next() {
        let p = path((0, 0, 0), (0, 1, 0)).unwrap();
        assert_eq!(p, vec![(0, 0, 0), (0, 1, 0)].into_iter().collect());
    }

    #[test]
    fn test_few() {
        let p = path((0, 0, 0), (0, 4, 0)).unwrap();
        assert_eq!(p,
                   vec![(0, 0, 0), (0, 1, 0), (0, 2, 0), (0, 3, 0), (0, 4, 0)]
                       .into_iter()
                       .collect());
    }

    struct Maze {
        xmax: i32,
        ymax: i32,
    }

    impl TwoDSearchProblem for Maze {
        fn get(&mut self, x: i32, y: i32) -> Option<u32> {
            // Imagine a simple maze, that looks something like this:
            // .
            // ........
            // .
            // ........
            // .
            // ........
            // where . is passable, and everywhere else is impassible.
            //
            if x < 0 || x > self.xmax || y < 0 || y > self.ymax {
                None
            } else if y % 4 == 0 && x > 0 {
                None
            } else if (y + 2) % 4 == 0 && x < self.xmax {
                None
            } else {
                Some(0)
            }
        }
    }

    #[test]
    fn test_maze() {
        let mut maze = Maze { xmax: 7, ymax: 5 };
        // If this test fails, try printing out the maze using this code:
        // println!("");
        // for y in 0 .. maze.ymax+1 {
        // for x in 0 .. maze.xmax+1 {
        // match maze.get(x, y) {
        // Some(_) => print!("."),
        // None => print!(" "),
        // }
        // }
        // println!("");
        // }
        //
        let p = astar(&mut maze.search((0, 0), (0, 4))).unwrap();
        assert_eq!(p,
                   vec![(0, 0), (0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1),
                        (7, 2), (7, 3), (6, 3), (5, 3), (4, 3), (3, 3), (2, 3), (1, 3), (0, 3),
                        (0, 4)]
                       .into_iter()
                       .collect());
    }

    #[test]
    fn test_maze_reusable() {
        let mut maze = Maze { xmax: 7, ymax: 5 };
        let p = astar(&mut maze.search((0, 0), (0, 4))).unwrap();
        let p2 = astar(&mut maze.search((0, 0), (0, 4))).unwrap();
        assert_eq!(p, p2);
    }

    #[test]
    fn test_maze_reverse() {
        let mut maze = Maze { xmax: 7, ymax: 5 };
        let p = astar(&mut maze.search((0, 0), (0, 4))).unwrap();
        let p2 = astar(&mut maze.search((0, 4), (0, 0))).unwrap();
        assert_eq!(p, p2.into_iter().rev().collect());
    }

}
