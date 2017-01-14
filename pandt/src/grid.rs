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
    fn test_diagonal_distance() {
        let pos1 = (0, 0, 0);
        let pos2 = (1, 1, 0);
        assert_eq!(point3_distance(pos1, pos2), Distance::new(2.0f32.sqrt()));
    }
}
