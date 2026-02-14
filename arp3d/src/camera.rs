use glam::{Mat4, Vec3};

use crate::Scene3d;

use crate::mesh::creature_model_bounds;

#[derive(Clone, Copy)]
pub(crate) struct SceneBounds {
    pub(crate) min: Vec3,
    pub(crate) max: Vec3,
}

pub(crate) fn scene_bounds_for_camera(scene: &Scene3d) -> Option<SceneBounds> {
    let mut bounds: Option<SceneBounds> = None;
    for tile in &scene.terrain {
        let min = Vec3::new(tile.x, tile.y, tile.z);
        let max = min + Vec3::ONE;
        bounds = merge_bounds(bounds, min, max);
    }
    for creature in &scene.creatures {
        let (min, max) = creature_model_bounds(*creature);
        bounds = merge_bounds(bounds, min, max);
    }
    bounds
}

pub(crate) fn scene_mvp(
    width: u32,
    height: u32,
    bounds: Option<SceneBounds>,
    camera_zoom: f32,
) -> Mat4 {
    let vfov = std::f32::consts::FRAC_PI_4;
    let aspect = width.max(1) as f32 / height.max(1) as f32;

    if let Some(bounds) = bounds {
        let center = (bounds.min + bounds.max) * 0.5;
        let extent = bounds.max - bounds.min;
        let radius = (extent.length() * 0.5).max(1.0);

        let hfov = 2.0 * ((vfov * 0.5).tan() * aspect).atan();
        let limiting_fov = vfov.min(hfov).max(0.25);
        let distance = ((radius / (limiting_fov * 0.5).tan()) * 1.35) / camera_zoom.max(0.05);

        let eye_dir = Vec3::new(1.0, 1.25, 1.0).normalize();
        let eye = center + eye_dir * distance;
        let view = Mat4::look_at_rh(eye, center, Vec3::Y);
        let near = (distance - radius * 2.2).max(0.1);
        let far = distance + radius * 3.0 + 50.0;
        let proj = Mat4::perspective_rh(vfov, aspect, near, far);
        proj * view
    } else {
        let view = Mat4::look_at_rh(Vec3::new(2.2, 2.2, 2.2), Vec3::ZERO, Vec3::Y);
        let proj = Mat4::perspective_rh(vfov, aspect, 0.1, 100.0);
        proj * view
    }
}

fn merge_bounds(existing: Option<SceneBounds>, min: Vec3, max: Vec3) -> Option<SceneBounds> {
    Some(match existing {
        Some(current) => SceneBounds {
            min: current.min.min(min),
            max: current.max.max(max),
        },
        None => SceneBounds { min, max },
    })
}
