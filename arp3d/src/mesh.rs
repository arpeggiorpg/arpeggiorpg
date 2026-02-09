use std::collections::HashSet;

use bytemuck::{Pod, Zeroable};
use glam::Vec3;

use crate::{Creature3d, Scene3d};
use crate::camera::{SceneBounds, scene_bounds_for_camera};

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub(crate) struct Vertex {
    pub(crate) position: [f32; 3],
    pub(crate) color: [f32; 3],
}

impl Vertex {
    pub(crate) const ATTRIBS: [wgpu::VertexAttribute; 2] =
        wgpu::vertex_attr_array![0 => Float32x3, 1 => Float32x3];

    pub(crate) fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &Self::ATTRIBS,
        }
    }
}

#[derive(Clone, Copy)]
struct CreatureModelDims {
    footprint_x: f32,
    footprint_z: f32,
    center_x: f32,
    center_z: f32,
    base_y: f32,
    leg_height: f32,
    torso_height: f32,
    head_height: f32,
    arm_height: f32,
    leg_width: f32,
    leg_depth: f32,
    leg_gap: f32,
    torso_width: f32,
    torso_depth: f32,
    arm_width: f32,
    arm_depth: f32,
    arm_offset: f32,
    head_width: f32,
    head_depth: f32,
}

pub(crate) fn build_scene_mesh(
    scene: &Scene3d,
    highlighted_terrain: Option<usize>,
    highlighted_creature: Option<usize>,
    movement_option_tiles: &[usize],
) -> (Vec<Vertex>, Vec<u32>, Option<SceneBounds>) {
    let mut vertices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 24);
    let mut indices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 36);
    let movement_option_tiles: HashSet<usize> = movement_option_tiles.iter().copied().collect();

    for (idx, tile) in scene.terrain.iter().enumerate() {
        let min = Vec3::new(tile.x, tile.y, tile.z);
        let max = min + Vec3::ONE;
        let top = if Some(idx) == highlighted_terrain {
            [0.92, 0.90, 0.44]
        } else if movement_option_tiles.contains(&idx) {
            [0.34, 0.90, 0.94]
        } else {
            [0.36, 0.73, 0.31]
        };
        append_cube(
            &mut vertices,
            &mut indices,
            min,
            max,
            top,
            [0.62, 0.48, 0.30],
            [0.50, 0.38, 0.24],
            [0.22, 0.18, 0.14],
        );
    }

    for (idx, creature) in scene.creatures.iter().enumerate() {
        let _ = append_creature_model(
            &mut vertices,
            &mut indices,
            *creature,
            Some(idx) == highlighted_creature,
        );
    }

    let bounds = scene_bounds_for_camera(scene);
    (vertices, indices, bounds)
}

fn append_cube(
    vertices: &mut Vec<Vertex>,
    indices: &mut Vec<u32>,
    min: Vec3,
    max: Vec3,
    top: [f32; 3],
    side_light: [f32; 3],
    side_dark: [f32; 3],
    bottom: [f32; 3],
) {
    let x0 = min.x;
    let y0 = min.y;
    let z0 = min.z;
    let x1 = max.x;
    let y1 = max.y;
    let z1 = max.z;

    push_quad(
        vertices,
        indices,
        Vec3::new(x0, y1, z0),
        Vec3::new(x1, y1, z0),
        Vec3::new(x1, y1, z1),
        Vec3::new(x0, y1, z1),
        top,
    );
    push_quad(
        vertices,
        indices,
        Vec3::new(x0, y0, z0),
        Vec3::new(x1, y0, z0),
        Vec3::new(x1, y0, z1),
        Vec3::new(x0, y0, z1),
        bottom,
    );
    push_quad(
        vertices,
        indices,
        Vec3::new(x1, y0, z0),
        Vec3::new(x1, y0, z1),
        Vec3::new(x1, y1, z1),
        Vec3::new(x1, y1, z0),
        side_light,
    );
    push_quad(
        vertices,
        indices,
        Vec3::new(x0, y0, z0),
        Vec3::new(x0, y0, z1),
        Vec3::new(x0, y1, z1),
        Vec3::new(x0, y1, z0),
        side_dark,
    );
    push_quad(
        vertices,
        indices,
        Vec3::new(x0, y0, z1),
        Vec3::new(x0, y1, z1),
        Vec3::new(x1, y1, z1),
        Vec3::new(x1, y0, z1),
        side_light,
    );
    push_quad(
        vertices,
        indices,
        Vec3::new(x0, y0, z0),
        Vec3::new(x0, y1, z0),
        Vec3::new(x1, y1, z0),
        Vec3::new(x1, y0, z0),
        side_dark,
    );
}

fn push_quad(
    vertices: &mut Vec<Vertex>,
    indices: &mut Vec<u32>,
    p0: Vec3,
    p1: Vec3,
    p2: Vec3,
    p3: Vec3,
    color: [f32; 3],
) {
    let base = vertices.len() as u32;
    vertices.push(Vertex {
        position: p0.to_array(),
        color,
    });
    vertices.push(Vertex {
        position: p1.to_array(),
        color,
    });
    vertices.push(Vertex {
        position: p2.to_array(),
        color,
    });
    vertices.push(Vertex {
        position: p3.to_array(),
        color,
    });

    indices.extend_from_slice(&[base, base + 3, base + 1, base + 1, base + 3, base + 2]);
}

fn append_creature_model(
    vertices: &mut Vec<Vertex>,
    indices: &mut Vec<u32>,
    creature: Creature3d,
    highlighted: bool,
) -> (Vec3, Vec3) {
    let dims = creature_model_dims(creature);

    let mut bounds_min = Vec3::splat(f32::INFINITY);
    let mut bounds_max = Vec3::splat(f32::NEG_INFINITY);

    let mut push_part = |min: Vec3,
                         max: Vec3,
                         top: [f32; 3],
                         side_light: [f32; 3],
                         side_dark: [f32; 3],
                         bottom: [f32; 3]| {
        append_cube(vertices, indices, min, max, top, side_light, side_dark, bottom);
        bounds_min = bounds_min.min(min);
        bounds_max = bounds_max.max(max);
    };

    let (leg_top, leg_side_light, leg_side_dark, leg_bottom) = if highlighted {
        (
            [0.38, 0.48, 0.98],
            [0.44, 0.54, 1.0],
            [0.30, 0.38, 0.92],
            [0.24, 0.30, 0.72],
        )
    } else {
        (
            [0.22, 0.33, 0.80],
            [0.27, 0.38, 0.85],
            [0.18, 0.27, 0.70],
            [0.12, 0.18, 0.45],
        )
    };
    let (torso_top, torso_side_light, torso_side_dark, torso_bottom) = if highlighted {
        (
            [0.28, 0.72, 0.95],
            [0.34, 0.78, 0.98],
            [0.22, 0.60, 0.85],
            [0.14, 0.46, 0.68],
        )
    } else {
        (
            [0.18, 0.58, 0.82],
            [0.22, 0.64, 0.88],
            [0.14, 0.46, 0.68],
            [0.10, 0.32, 0.48],
        )
    };
    let (arm_top, arm_side_light, arm_side_dark, arm_bottom) = if highlighted {
        (
            [0.96, 0.78, 0.64],
            [0.99, 0.82, 0.70],
            [0.90, 0.70, 0.56],
            [0.76, 0.59, 0.48],
        )
    } else {
        (
            [0.85, 0.67, 0.54],
            [0.90, 0.72, 0.58],
            [0.76, 0.57, 0.45],
            [0.62, 0.47, 0.38],
        )
    };
    let (head_top, head_side_light, head_side_dark, head_bottom) = if highlighted {
        (
            [1.0, 0.86, 0.72],
            [1.0, 0.90, 0.78],
            [0.94, 0.77, 0.63],
            [0.80, 0.64, 0.52],
        )
    } else {
        (
            [0.91, 0.74, 0.60],
            [0.96, 0.78, 0.64],
            [0.81, 0.63, 0.50],
            [0.66, 0.51, 0.40],
        )
    };

    // Legs
    let leg_left_center_x = dims.center_x - (dims.leg_width * 0.5 + dims.leg_gap * 0.5);
    let leg_right_center_x = dims.center_x + (dims.leg_width * 0.5 + dims.leg_gap * 0.5);
    for leg_center_x in [leg_left_center_x, leg_right_center_x] {
        push_part(
            Vec3::new(
                leg_center_x - dims.leg_width * 0.5,
                dims.base_y,
                dims.center_z - dims.leg_depth * 0.5,
            ),
            Vec3::new(
                leg_center_x + dims.leg_width * 0.5,
                dims.base_y + dims.leg_height,
                dims.center_z + dims.leg_depth * 0.5,
            ),
            leg_top,
            leg_side_light,
            leg_side_dark,
            leg_bottom,
        );
    }

    let torso_min_y = dims.base_y + dims.leg_height;
    let torso_max_y = torso_min_y + dims.torso_height;
    push_part(
        Vec3::new(
            dims.center_x - dims.torso_width * 0.5,
            torso_min_y,
            dims.center_z - dims.torso_depth * 0.5,
        ),
        Vec3::new(
            dims.center_x + dims.torso_width * 0.5,
            torso_max_y,
            dims.center_z + dims.torso_depth * 0.5,
        ),
        torso_top,
        torso_side_light,
        torso_side_dark,
        torso_bottom,
    );

    let arm_min_y = torso_min_y + dims.torso_height * 0.08;
    let arm_max_y = arm_min_y + dims.arm_height;
    for arm_center_x in [
        dims.center_x - dims.arm_offset,
        dims.center_x + dims.arm_offset,
    ] {
        push_part(
            Vec3::new(
                arm_center_x - dims.arm_width * 0.5,
                arm_min_y,
                dims.center_z - dims.arm_depth * 0.5,
            ),
            Vec3::new(
                arm_center_x + dims.arm_width * 0.5,
                arm_max_y,
                dims.center_z + dims.arm_depth * 0.5,
            ),
            arm_top,
            arm_side_light,
            arm_side_dark,
            arm_bottom,
        );
    }

    let head_min_y = torso_max_y;
    let head_max_y = head_min_y + dims.head_height;
    push_part(
        Vec3::new(
            dims.center_x - dims.head_width * 0.5,
            head_min_y,
            dims.center_z - dims.head_depth * 0.5,
        ),
        Vec3::new(
            dims.center_x + dims.head_width * 0.5,
            head_max_y,
            dims.center_z + dims.head_depth * 0.5,
        ),
        head_top,
        head_side_light,
        head_side_dark,
        head_bottom,
    );

    if creature.controlled {
        let cursor_center = Vec3::new(dims.center_x, head_max_y + 0.30, dims.center_z);
        let cursor_radius = ((dims.footprint_x.max(dims.footprint_z)) * 0.22).clamp(0.12, 0.22);
        let cursor_half_height = 0.24;
        append_control_cursor(
            vertices,
            indices,
            cursor_center,
            cursor_radius,
            cursor_half_height,
            highlighted,
        );
    }

    (bounds_min, bounds_max)
}

fn append_control_cursor(
    vertices: &mut Vec<Vertex>,
    indices: &mut Vec<u32>,
    center: Vec3,
    radius: f32,
    half_height: f32,
    highlighted: bool,
) {
    let top = center + Vec3::new(0.0, half_height, 0.0);
    let bottom = center - Vec3::new(0.0, half_height, 0.0);
    let north = center + Vec3::new(0.0, 0.0, -radius);
    let south = center + Vec3::new(0.0, 0.0, radius);
    let east = center + Vec3::new(radius, 0.0, 0.0);
    let west = center + Vec3::new(-radius, 0.0, 0.0);

    let (top_a, top_b, top_c, top_d, bottom_a, bottom_b, bottom_c, bottom_d) = if highlighted {
        (
            [0.82, 0.98, 1.0],
            [0.70, 0.93, 1.0],
            [0.58, 0.87, 1.0],
            [0.66, 0.90, 1.0],
            [0.30, 0.52, 0.80],
            [0.25, 0.44, 0.71],
            [0.20, 0.36, 0.61],
            [0.24, 0.40, 0.67],
        )
    } else {
        (
            [0.72, 0.94, 1.0],
            [0.60, 0.88, 0.98],
            [0.50, 0.80, 0.94],
            [0.56, 0.84, 0.96],
            [0.24, 0.44, 0.72],
            [0.20, 0.37, 0.62],
            [0.16, 0.31, 0.53],
            [0.19, 0.34, 0.58],
        )
    };

    // Two-sided triangles keep the faceted marker visible from any camera direction.
    push_triangle_double_sided(vertices, indices, top, north, east, top_a);
    push_triangle_double_sided(vertices, indices, top, east, south, top_b);
    push_triangle_double_sided(vertices, indices, top, south, west, top_c);
    push_triangle_double_sided(vertices, indices, top, west, north, top_d);

    push_triangle_double_sided(vertices, indices, bottom, east, north, bottom_a);
    push_triangle_double_sided(vertices, indices, bottom, south, east, bottom_b);
    push_triangle_double_sided(vertices, indices, bottom, west, south, bottom_c);
    push_triangle_double_sided(vertices, indices, bottom, north, west, bottom_d);
}

fn push_triangle_double_sided(
    vertices: &mut Vec<Vertex>,
    indices: &mut Vec<u32>,
    p0: Vec3,
    p1: Vec3,
    p2: Vec3,
    color: [f32; 3],
) {
    let base = vertices.len() as u32;
    vertices.push(Vertex {
        position: p0.to_array(),
        color,
    });
    vertices.push(Vertex {
        position: p1.to_array(),
        color,
    });
    vertices.push(Vertex {
        position: p2.to_array(),
        color,
    });
    indices.extend_from_slice(&[base, base + 1, base + 2, base, base + 2, base + 1]);
}

pub(crate) fn creature_model_bounds(creature: Creature3d) -> (Vec3, Vec3) {
    let dims = creature_model_dims(creature);
    let half_depth = (dims.leg_depth
        .max(dims.torso_depth)
        .max(dims.arm_depth)
        .max(dims.head_depth))
        * 0.5;

    let min = Vec3::new(
        dims.center_x - (dims.arm_offset + dims.arm_width * 0.5),
        dims.base_y,
        dims.center_z - half_depth,
    );
    let max = Vec3::new(
        dims.center_x + (dims.arm_offset + dims.arm_width * 0.5),
        dims.base_y + dims.leg_height + dims.torso_height + dims.head_height,
        dims.center_z + half_depth,
    );
    (min, max)
}

fn creature_model_dims(creature: Creature3d) -> CreatureModelDims {
    let footprint_x = creature.size_x.max(0.55);
    let footprint_z = creature.size_z.max(0.55);
    let height = creature.size_y.max(1.3);

    let center_x = creature.x + footprint_x * 0.5;
    let center_z = creature.z + footprint_z * 0.5;
    let base_y = creature.y;

    let leg_height = height * 0.42;
    let torso_height = height * 0.36;
    let head_height = height * 0.22;
    let arm_height = torso_height * 0.88;

    let leg_width = (footprint_x * 0.22).clamp(0.14, 0.28);
    let leg_depth = (footprint_z * 0.22).clamp(0.14, 0.28);
    let leg_gap = leg_width * 0.40;

    let torso_width = (footprint_x * 0.52).clamp(0.24, 0.44);
    let torso_depth = (footprint_z * 0.34).clamp(0.18, 0.32);

    let arm_width = (leg_width * 0.90).clamp(0.12, 0.22);
    let arm_depth = (leg_depth * 0.90).clamp(0.12, 0.22);
    let arm_offset = torso_width * 0.5 + arm_width * 0.5 + 0.02;

    let head_width = (torso_width * 0.95).clamp(0.22, 0.40);
    let head_depth = (torso_depth * 0.95).clamp(0.18, 0.30);

    CreatureModelDims {
        footprint_x,
        footprint_z,
        center_x,
        center_z,
        base_y,
        leg_height,
        torso_height,
        head_height,
        arm_height,
        leg_width,
        leg_depth,
        leg_gap,
        torso_width,
        torso_depth,
        arm_width,
        arm_depth,
        arm_offset,
        head_width,
        head_depth,
    }
}
