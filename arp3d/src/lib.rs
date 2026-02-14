mod camera;
mod mesh;
mod picking;
mod renderer;

pub use renderer::render_scene_on_surface;
pub use picking::{pick_creature, pick_scene_object, pick_terrain_tile};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Scene3d {
    pub terrain: Vec<TerrainTile3d>,
    pub creatures: Vec<Creature3d>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TerrainTile3d {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Creature3d {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub size_x: f32,
    pub size_y: f32,
    pub size_z: f32,
    pub controlled: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PickedObject {
    Terrain(usize),
    Creature(usize),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SceneViewParams {
    pub viewport_width: u32,
    pub viewport_height: u32,
    pub camera_zoom: f32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SceneCursor {
    pub x: f32,
    pub y: f32,
}
