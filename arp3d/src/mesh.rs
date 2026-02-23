use std::collections::HashSet;

use anyhow::{Context, bail};
use bytemuck::{Pod, Zeroable};
use glam::Vec3;

use crate::camera::{SceneBounds, scene_bounds_for_camera};
use crate::{Creature3d, Scene3d};

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
struct SourceVertex {
    position: Vec3,
    color: [f32; 3],
}

struct SourceMesh {
    vertices: Vec<SourceVertex>,
    indices: Vec<u32>,
    bounds_min: Vec3,
    bounds_max: Vec3,
}

pub struct SceneModelLibrary {
    terrain: SourceMesh,
    creature: SourceMesh,
    cursor: SourceMesh,
}

impl SceneModelLibrary {
    pub fn from_glb_bytes(
        terrain_cube_glb: &[u8],
        creature_glb: &[u8],
        control_cursor_glb: &[u8],
    ) -> anyhow::Result<Self> {
        Ok(Self {
            terrain: load_mesh_from_glb(terrain_cube_glb, "terrain_cube")?,
            creature: load_mesh_from_glb(creature_glb, "creature")?,
            cursor: load_mesh_from_glb(control_cursor_glb, "control_cursor")?,
        })
    }
}

pub(crate) fn build_scene_mesh(
    models: &SceneModelLibrary,
    scene: &Scene3d,
    highlighted_terrain: Option<usize>,
    highlighted_creature: Option<usize>,
    movement_option_tiles: &[usize],
) -> (Vec<Vertex>, Vec<u32>, Option<SceneBounds>) {
    let mut vertices = Vec::with_capacity(
        scene.terrain.len() * models.terrain.vertices.len()
            + scene.creatures.len() * models.creature.vertices.len(),
    );
    let mut indices = Vec::with_capacity(
        scene.terrain.len() * models.terrain.indices.len()
            + scene.creatures.len() * models.creature.indices.len(),
    );
    let movement_option_tiles: HashSet<usize> = movement_option_tiles.iter().copied().collect();

    for (idx, tile) in scene.terrain.iter().enumerate() {
        let tint = if Some(idx) == highlighted_terrain {
            [0.92, 0.90, 0.44]
        } else if movement_option_tiles.contains(&idx) {
            [0.34, 0.90, 0.94]
        } else {
            [0.36, 0.73, 0.31]
        };
        append_mesh_instance(
            &mut vertices,
            &mut indices,
            &models.terrain,
            Vec3::ONE,
            Vec3::new(tile.x, tile.y, tile.z),
            tint,
        );
    }

    for (idx, creature) in scene.creatures.iter().enumerate() {
        let highlighted = Some(idx) == highlighted_creature;
        let creature_tint = if highlighted {
            [0.74, 0.88, 1.20]
        } else {
            [1.0, 1.0, 1.0]
        };

        let translation = creature_world_translation(*creature);
        let (creature_min, creature_max) = creature_model_bounds(models, *creature);
        append_mesh_instance(
            &mut vertices,
            &mut indices,
            &models.creature,
            Vec3::ONE,
            translation,
            creature_tint,
        );

        if creature.controlled {
            let cursor_center = Vec3::new(
                translation.x,
                translation.y + creature_model_height(models),
                translation.z,
            );
            let creature_width = creature_max.x - creature_min.x;
            let creature_depth = creature_max.z - creature_min.z;
            let cursor_radius = (creature_width.max(creature_depth) * 0.22).clamp(0.12, 0.22);
            let cursor_half_height = 0.24;
            let cursor_tint = if highlighted {
                [1.12, 1.20, 1.24]
            } else {
                [1.0, 1.0, 1.0]
            };
            append_mesh_instance(
                &mut vertices,
                &mut indices,
                &models.cursor,
                Vec3::new(cursor_radius, cursor_half_height, cursor_radius),
                cursor_center,
                cursor_tint,
            );
        }
    }

    let bounds = scene_bounds_for_camera(scene, models);
    (vertices, indices, bounds)
}

fn append_mesh_instance(
    out_vertices: &mut Vec<Vertex>,
    out_indices: &mut Vec<u32>,
    mesh: &SourceMesh,
    scale: Vec3,
    translation: Vec3,
    tint: [f32; 3],
) {
    let base = out_vertices.len() as u32;

    for vertex in &mesh.vertices {
        let position = vertex.position * scale + translation;
        out_vertices.push(Vertex {
            position: position.to_array(),
            color: tinted_color(vertex.color, tint),
        });
    }

    out_indices.extend(mesh.indices.iter().map(|idx| base + *idx));
}

fn tinted_color(base: [f32; 3], tint: [f32; 3]) -> [f32; 3] {
    [
        (base[0] * tint[0]).clamp(0.0, 1.0),
        (base[1] * tint[1]).clamp(0.0, 1.0),
        (base[2] * tint[2]).clamp(0.0, 1.0),
    ]
}

pub(crate) fn creature_model_bounds(
    models: &SceneModelLibrary,
    creature: Creature3d,
) -> (Vec3, Vec3) {
    let translation = creature_world_translation(creature);

    (
        models.creature.bounds_min + translation,
        models.creature.bounds_max + translation,
    )
}

fn creature_world_translation(creature: Creature3d) -> Vec3 {
    // Creature origin is rendered at the center of the terrain tile (top surface in Y).
    Vec3::new(creature.x + 0.5, creature.y, creature.z + 0.5)
}

fn creature_model_height(models: &SceneModelLibrary) -> f32 {
    models.creature.bounds_max.y - models.creature.bounds_min.y
}

fn load_mesh_from_glb(glb_bytes: &[u8], name: &str) -> anyhow::Result<SourceMesh> {
    let gltf = gltf::Gltf::from_slice(glb_bytes).with_context(|| format!("parsing {name}.glb"))?;
    let blob = gltf
        .blob
        .as_deref()
        .with_context(|| format!("{name}.glb is missing BIN chunk"))?;

    let mesh = gltf
        .meshes()
        .find(|mesh| mesh.name() == Some(name))
        .with_context(|| format!("missing mesh named '{name}'"))?;
    let primitive = mesh
        .primitives()
        .next()
        .with_context(|| format!("mesh '{name}' has no primitive"))?;

    let reader = primitive.reader(|buffer| {
        if buffer.index() == 0 {
            Some(blob)
        } else {
            None
        }
    });

    let positions: Vec<Vec3> = reader
        .read_positions()
        .with_context(|| format!("mesh '{name}' has no POSITION"))?
        .map(Vec3::from_array)
        .collect();

    if positions.is_empty() {
        bail!("mesh '{name}' has no vertices");
    }

    let colors: Vec<[f32; 3]> = match reader.read_colors(0) {
        Some(read_colors) => read_colors.into_rgb_f32().collect(),
        None => vec![[1.0, 1.0, 1.0]; positions.len()],
    };

    if colors.len() != positions.len() {
        bail!(
            "mesh '{name}' has {} colors but {} positions",
            colors.len(),
            positions.len()
        );
    }

    let indices: Vec<u32> = match reader.read_indices() {
        Some(read_indices) => read_indices.into_u32().collect(),
        None => (0..positions.len() as u32).collect(),
    };

    if indices.is_empty() {
        bail!("mesh '{name}' has no indices");
    }

    let mut bounds_min = Vec3::splat(f32::INFINITY);
    let mut bounds_max = Vec3::splat(f32::NEG_INFINITY);
    for position in &positions {
        bounds_min = bounds_min.min(*position);
        bounds_max = bounds_max.max(*position);
    }

    let vertices = positions
        .into_iter()
        .zip(colors)
        .map(|(position, color)| SourceVertex { position, color })
        .collect();

    Ok(SourceMesh {
        vertices,
        indices,
        bounds_min,
        bounds_max,
    })
}
