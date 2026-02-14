use glam::{Vec3, Vec4};

use crate::{
    PickedObject, Scene3d,
    camera::{scene_bounds_for_camera, scene_mvp},
    mesh::creature_model_bounds,
};

#[derive(Clone, Copy)]
struct Ray {
    origin: Vec3,
    dir: Vec3,
}

#[derive(Clone, Copy)]
struct RayHit {
    distance: f32,
    index: usize,
}

#[derive(Clone, Copy, Default)]
struct SceneRaycastHits {
    terrain: Option<RayHit>,
    creature: Option<RayHit>,
}

pub fn pick_terrain_tile(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
    camera_zoom: f32,
) -> Option<usize> {
    raycast_scene(
        scene,
        viewport_width,
        viewport_height,
        cursor_x,
        cursor_y,
        camera_zoom,
    )
        .and_then(|hits| hits.terrain.map(|hit| hit.index))
}

pub fn pick_creature(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
    camera_zoom: f32,
) -> Option<usize> {
    raycast_scene(
        scene,
        viewport_width,
        viewport_height,
        cursor_x,
        cursor_y,
        camera_zoom,
    )
        .and_then(|hits| hits.creature.map(|hit| hit.index))
}

pub fn pick_scene_object(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
    camera_zoom: f32,
) -> Option<PickedObject> {
    raycast_scene(
        scene,
        viewport_width,
        viewport_height,
        cursor_x,
        cursor_y,
        camera_zoom,
    )
        .and_then(nearest_object)
}

fn raycast_scene(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
    camera_zoom: f32,
) -> Option<SceneRaycastHits> {
    let ray = cursor_ray(
        scene,
        viewport_width,
        viewport_height,
        cursor_x,
        cursor_y,
        camera_zoom,
    )?;
    Some(raycast_scene_with_ray(scene, ray))
}

fn raycast_scene_with_ray(scene: &Scene3d, ray: Ray) -> SceneRaycastHits {
    SceneRaycastHits {
        terrain: pick_terrain_tile_with_ray(scene, ray),
        creature: pick_creature_with_ray(scene, ray),
    }
}

fn nearest_object(hits: SceneRaycastHits) -> Option<PickedObject> {
    match (hits.terrain, hits.creature) {
        (Some(terrain_hit), Some(creature_hit)) => {
            if terrain_hit.distance <= creature_hit.distance {
                Some(PickedObject::Terrain(terrain_hit.index))
            } else {
                Some(PickedObject::Creature(creature_hit.index))
            }
        }
        (Some(terrain_hit), None) => Some(PickedObject::Terrain(terrain_hit.index)),
        (None, Some(creature_hit)) => Some(PickedObject::Creature(creature_hit.index)),
        (None, None) => None,
    }
}

fn cursor_ray(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
    camera_zoom: f32,
) -> Option<Ray> {
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
    let view_proj = scene_mvp(viewport_width, viewport_height, Some(bounds), camera_zoom);
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

    Some(Ray {
        origin: near,
        dir: ray.normalize(),
    })
}

fn pick_terrain_tile_with_ray(scene: &Scene3d, ray: Ray) -> Option<RayHit> {
    let mut best: Option<RayHit> = None;
    const EPS: f32 = 1e-4;

    for (idx, tile) in scene.terrain.iter().enumerate() {
        if ray.dir.y.abs() < EPS {
            continue;
        }
        let top_y = tile.y + 1.0;
        let t = (top_y - ray.origin.y) / ray.dir.y;
        if t <= 0.0 {
            continue;
        }
        let hit = ray.origin + ray.dir * t;
        if hit.x >= tile.x - EPS
            && hit.x <= tile.x + 1.0 + EPS
            && hit.z >= tile.z - EPS
            && hit.z <= tile.z + 1.0 + EPS
        {
            update_best_hit(&mut best, t, idx);
        }
    }

    best
}

fn pick_creature_with_ray(scene: &Scene3d, ray: Ray) -> Option<RayHit> {
    let mut best: Option<RayHit> = None;

    for (idx, creature) in scene.creatures.iter().enumerate() {
        let (min, max) = creature_model_bounds(*creature);
        let Some(t) = intersect_ray_aabb(ray.origin, ray.dir, min, max) else {
            continue;
        };
        update_best_hit(&mut best, t, idx);
    }

    best
}

fn update_best_hit(best: &mut Option<RayHit>, distance: f32, index: usize) {
    match best {
        Some(existing) if distance >= existing.distance => {}
        _ => {
            *best = Some(RayHit { distance, index });
        }
    }
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
