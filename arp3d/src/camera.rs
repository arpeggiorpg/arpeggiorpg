use glam::{Mat4, Vec3};

use crate::{Scene3d, SceneViewParams};

use crate::mesh::{SceneModelLibrary, creature_model_bounds};

#[derive(Clone, Copy)]
pub(crate) struct SceneBounds {
    pub(crate) min: Vec3,
    pub(crate) max: Vec3,
}

pub(crate) fn scene_bounds_for_camera(
    scene: &Scene3d,
    models: &SceneModelLibrary,
) -> Option<SceneBounds> {
    let mut bounds: Option<SceneBounds> = None;
    for tile in &scene.terrain {
        let min = Vec3::new(tile.x, tile.y, tile.z);
        let max = min + Vec3::ONE;
        bounds = merge_bounds(bounds, min, max);
    }
    for creature in &scene.creatures {
        let (min, max) = creature_model_bounds(models, *creature);
        bounds = merge_bounds(bounds, min, max);
    }
    bounds
}

pub(crate) fn scene_mvp(
    width: u32,
    height: u32,
    bounds: Option<SceneBounds>,
    view: SceneViewParams,
) -> Mat4 {
    let vfov = std::f32::consts::FRAC_PI_4;
    let aspect = width.max(1) as f32 / height.max(1) as f32;

    if let Some(bounds) = bounds {
        let center = (bounds.min + bounds.max) * 0.5 + Vec3::new(view.pan_x, 0.0, view.pan_z);
        let distance = camera_distance(bounds, aspect, view.camera_zoom);

        let eye_dir = orbit_eye_direction(view.camera_yaw);
        let eye = center + eye_dir * distance;
        let view = Mat4::look_at_rh(eye, center, Vec3::Y);
        let extent = bounds.max - bounds.min;
        let radius = (extent.length() * 0.5).max(1.0);
        let near = (distance - radius * 2.2).max(0.1);
        let far = distance + radius * 3.0 + 50.0;
        let proj = Mat4::perspective_rh(vfov, aspect, near, far);
        proj * view
    } else {
        let target = Vec3::new(view.pan_x, 0.0, view.pan_z);
        let eye = target + orbit_eye_direction(view.camera_yaw) * Vec3::new(2.2, 2.2, 2.2).length();
        let view_matrix = Mat4::look_at_rh(eye, target, Vec3::Y);
        let proj = Mat4::perspective_rh(vfov, aspect, 0.1, 100.0);
        proj * view_matrix
    }
}

pub(crate) fn drag_pan_delta(
    models: &SceneModelLibrary,
    scene: &Scene3d,
    view: SceneViewParams,
    delta_x: f32,
    delta_y: f32,
) -> (f32, f32) {
    let vfov = std::f32::consts::FRAC_PI_4;
    let aspect = view.viewport_width.max(1) as f32 / view.viewport_height.max(1) as f32;
    let hfov = 2.0 * ((vfov * 0.5).tan() * aspect).atan();
    let distance = scene_bounds_for_camera(scene, models)
        .map(|bounds| camera_distance(bounds, aspect, view.camera_zoom))
        .unwrap_or_else(|| Vec3::new(2.2, 2.2, 2.2).length() / view.camera_zoom.max(0.05));

    let units_per_px_x = (2.0 * distance * (hfov * 0.5).tan()) / view.viewport_width.max(1) as f32;
    let units_per_px_y = (2.0 * distance * (vfov * 0.5).tan()) / view.viewport_height.max(1) as f32;

    let forward = orbit_forward_direction(view.camera_yaw);
    let right = Vec3::new(-forward.z, 0.0, forward.x);
    let pan = right * (-delta_x * units_per_px_x) + forward * (delta_y * units_per_px_y);
    (pan.x, pan.z)
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

fn camera_distance(bounds: SceneBounds, aspect: f32, camera_zoom: f32) -> f32 {
    let vfov = std::f32::consts::FRAC_PI_4;
    let extent = bounds.max - bounds.min;
    let radius = (extent.length() * 0.5).max(1.0);
    let hfov = 2.0 * ((vfov * 0.5).tan() * aspect).atan();
    let limiting_fov = vfov.min(hfov).max(0.25);
    ((radius / (limiting_fov * 0.5).tan()) * 1.35) / camera_zoom.max(0.05)
}

fn orbit_eye_direction(camera_yaw: f32) -> Vec3 {
    // Preserve prior default pitch while allowing horizontal orbit around Y.
    Vec3::new(
        camera_yaw.cos() * std::f32::consts::SQRT_2,
        1.25,
        camera_yaw.sin() * std::f32::consts::SQRT_2,
    )
    .normalize()
}

fn orbit_forward_direction(camera_yaw: f32) -> Vec3 {
    Vec3::new(-camera_yaw.cos(), 0.0, -camera_yaw.sin()).normalize()
}
