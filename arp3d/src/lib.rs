pub mod wgpu;

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
