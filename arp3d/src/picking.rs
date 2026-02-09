use glam::{Vec3, Vec4};

use crate::{
    PickedObject, Scene3d,
    camera::{scene_bounds_for_camera, scene_mvp},
    mesh::creature_model_bounds,
};

pub fn pick_terrain_tile(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
) -> Option<usize> {
    if scene.terrain.is_empty() {
        return None;
    }
    let (ray_origin, ray_dir) = cursor_ray(scene, viewport_width, viewport_height, cursor_x, cursor_y)?;
    pick_terrain_tile_with_ray(scene, ray_origin, ray_dir).map(|(_, idx)| idx)
}

pub fn pick_creature(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
) -> Option<usize> {
    if scene.creatures.is_empty() {
        return None;
    }

    let (ray_origin, ray_dir) = cursor_ray(scene, viewport_width, viewport_height, cursor_x, cursor_y)?;
    pick_creature_with_ray(scene, ray_origin, ray_dir).map(|(_, idx)| idx)
}

pub fn pick_scene_object(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
) -> Option<PickedObject> {
    let (ray_origin, ray_dir) = cursor_ray(scene, viewport_width, viewport_height, cursor_x, cursor_y)?;
    let terrain_hit = pick_terrain_tile_with_ray(scene, ray_origin, ray_dir);
    let creature_hit = pick_creature_with_ray(scene, ray_origin, ray_dir);

    match (terrain_hit, creature_hit) {
        (Some((terrain_t, terrain_idx)), Some((creature_t, creature_idx))) => {
            if terrain_t <= creature_t {
                Some(PickedObject::Terrain(terrain_idx))
            } else {
                Some(PickedObject::Creature(creature_idx))
            }
        }
        (Some((_, terrain_idx)), None) => Some(PickedObject::Terrain(terrain_idx)),
        (None, Some((_, creature_idx))) => Some(PickedObject::Creature(creature_idx)),
        (None, None) => None,
    }
}

fn cursor_ray(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
) -> Option<(Vec3, Vec3)> {
    if viewport_width == 0 || viewport_height == 0 {
        return None;
    }
    if cursor_x < 0.0
        || cursor_y < 0.0
        || cursor_x > viewport_width as f32
        || cursor_y > viewport_height as f32
    {
        return None;
    }

    let bounds = scene_bounds_for_camera(scene)?;
    let view_proj = scene_mvp(viewport_width, viewport_height, Some(bounds));
    let inv_view_proj = view_proj.inverse();

    let ndc_x = (cursor_x / viewport_width as f32) * 2.0 - 1.0;
    let ndc_y = 1.0 - (cursor_y / viewport_height as f32) * 2.0;

    let near4 = inv_view_proj * Vec4::new(ndc_x, ndc_y, 0.0, 1.0);
    let far4 = inv_view_proj * Vec4::new(ndc_x, ndc_y, 1.0, 1.0);
    if near4.w.abs() < 1e-6 || far4.w.abs() < 1e-6 {
        return None;
    }

    let near = near4.truncate() / near4.w;
    let far = far4.truncate() / far4.w;
    let ray = far - near;
    if ray.length_squared() <= f32::EPSILON {
        return None;
    }
    Some((near, ray.normalize()))
}

fn pick_terrain_tile_with_ray(
    scene: &Scene3d,
    ray_origin: Vec3,
    ray_dir: Vec3,
) -> Option<(f32, usize)> {
    let mut best: Option<(f32, usize)> = None;
    const EPS: f32 = 1e-4;
    for (idx, tile) in scene.terrain.iter().enumerate() {
        if ray_dir.y.abs() < EPS {
            continue;
        }
        let top_y = tile.y + 1.0;
        let t = (top_y - ray_origin.y) / ray_dir.y;
        if t <= 0.0 {
            continue;
        }
        let hit = ray_origin + ray_dir * t;
        if hit.x >= tile.x - EPS
            && hit.x <= tile.x + 1.0 + EPS
            && hit.z >= tile.z - EPS
            && hit.z <= tile.z + 1.0 + EPS
        {
            match best {
                Some((best_t, _)) if t >= best_t => {}
                _ => best = Some((t, idx)),
            }
        }
    }
    best
}

fn pick_creature_with_ray(scene: &Scene3d, ray_origin: Vec3, ray_dir: Vec3) -> Option<(f32, usize)> {
    let mut best: Option<(f32, usize)> = None;
    for (idx, creature) in scene.creatures.iter().enumerate() {
        let (min, max) = creature_model_bounds(*creature);
        let Some(t) = intersect_ray_aabb(ray_origin, ray_dir, min, max) else {
            continue;
        };
        match best {
            Some((best_t, _)) if t >= best_t => {}
            _ => best = Some((t, idx)),
        }
    }
    best
}

fn intersect_ray_aabb(origin: Vec3, dir: Vec3, min: Vec3, max: Vec3) -> Option<f32> {
    let mut tmin = f32::NEG_INFINITY;
    let mut tmax = f32::INFINITY;
    const EPS: f32 = 1e-6;

    for axis in 0..3 {
        let o = origin[axis];
        let d = dir[axis];
        let mn = min[axis];
        let mx = max[axis];
        if d.abs() < EPS {
            if o < mn || o > mx {
                return None;
            }
            continue;
        }
        let inv_d = 1.0 / d;
        let mut t1 = (mn - o) * inv_d;
        let mut t2 = (mx - o) * inv_d;
        if t1 > t2 {
            std::mem::swap(&mut t1, &mut t2);
        }
        tmin = tmin.max(t1);
        tmax = tmax.min(t2);
        if tmax < tmin {
            return None;
        }
    }

    if tmax < 0.0 {
        None
    } else {
        Some(tmin.max(0.0))
    }
}
