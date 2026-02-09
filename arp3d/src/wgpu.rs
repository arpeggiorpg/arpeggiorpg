use bytemuck::{Pod, Zeroable};
use glam::{Mat4, Vec3, Vec4};
use wgpu::util::DeviceExt;

use crate::Scene3d;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PickedObject {
    Terrain(usize),
    Creature(usize),
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct Vertex {
    position: [f32; 3],
    color: [f32; 3],
}

impl Vertex {
    const ATTRIBS: [wgpu::VertexAttribute; 2] =
        wgpu::vertex_attr_array![0 => Float32x3, 1 => Float32x3];

    fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &Self::ATTRIBS,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
struct Uniforms {
    mvp: [[f32; 4]; 4],
}

#[derive(Clone, Copy)]
struct SceneBounds {
    min: Vec3,
    max: Vec3,
}

pub struct SceneRenderer {
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    index_count: u32,
    _uniform_buffer: wgpu::Buffer,
    bind_group: wgpu::BindGroup,
    depth_view: wgpu::TextureView,
}

impl SceneRenderer {
    pub fn new(
        device: &wgpu::Device,
        config: &wgpu::SurfaceConfiguration,
        scene: &Scene3d,
        highlighted_terrain: Option<usize>,
        highlighted_creature: Option<usize>,
        movement_option_tiles: &[usize],
    ) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Arp3D Scene Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SOURCE.into()),
        });

        let (mut vertices, mut indices, bounds) = build_scene_mesh(
            scene,
            highlighted_terrain,
            highlighted_creature,
            movement_option_tiles,
        );
        if vertices.is_empty() {
            vertices.push(Vertex {
                position: [0.0, 0.0, 0.0],
                color: [0.0, 0.0, 0.0],
            });
        }
        if indices.is_empty() {
            indices.push(0);
        }

        let index_count = if scene.terrain.is_empty() && scene.creatures.is_empty() {
            0
        } else {
            indices.len() as u32
        };

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Arp3D Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Arp3D Index Buffer"),
            contents: bytemuck::cast_slice(&indices),
            usage: wgpu::BufferUsages::INDEX,
        });

        let uniforms = Uniforms {
            mvp: scene_mvp(config.width, config.height, bounds).to_cols_array_2d(),
        };
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Arp3D Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM,
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Arp3D Bind Group Layout"),
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Arp3D Bind Group"),
            layout: &bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Arp3D Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let depth_texture = create_depth_texture(device, config);
        let depth_view = depth_texture.create_view(&wgpu::TextureViewDescriptor::default());

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Arp3D Render Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[Vertex::desc()],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: config.format,
                    blend: Some(wgpu::BlendState::REPLACE),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: Some(wgpu::Face::Back),
                polygon_mode: wgpu::PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false,
            },
            depth_stencil: Some(wgpu::DepthStencilState {
                format: wgpu::TextureFormat::Depth24Plus,
                depth_write_enabled: true,
                depth_compare: wgpu::CompareFunction::Less,
                stencil: wgpu::StencilState::default(),
                bias: wgpu::DepthBiasState::default(),
            }),
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        Self {
            pipeline,
            vertex_buffer,
            index_buffer,
            index_count,
            _uniform_buffer: uniform_buffer,
            bind_group,
            depth_view,
        }
    }

    pub fn render(&self, encoder: &mut wgpu::CommandEncoder, target_view: &wgpu::TextureView) {
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("Arp3D Render Pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: target_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color {
                        r: 0.08,
                        g: 0.09,
                        b: 0.12,
                        a: 1.0,
                    }),
                    store: wgpu::StoreOp::Store,
                },
                depth_slice: None,
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: &self.depth_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: wgpu::StoreOp::Store,
                }),
                stencil_ops: None,
            }),
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        render_pass.set_pipeline(&self.pipeline);
        render_pass.set_bind_group(0, &self.bind_group, &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint32);
        if self.index_count > 0 {
            render_pass.draw_indexed(0..self.index_count, 0, 0..1);
        }
    }
}

pub async fn render_scene_on_surface(
    instance: &wgpu::Instance,
    surface: &wgpu::Surface<'_>,
    client_width: u32,
    client_height: u32,
    scene: &Scene3d,
    highlighted_terrain: Option<usize>,
    highlighted_creature: Option<usize>,
    movement_option_tiles: &[usize],
) -> anyhow::Result<(u32, u32)> {
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(surface),
            force_fallback_adapter: false,
        })
        .await?;

    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor {
            label: Some("Arp3D Device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_webgl2_defaults(),
            memory_hints: Default::default(),
            trace: Default::default(),
        })
        .await?;

    let max_texture_size = device.limits().max_texture_dimension_2d;
    let width = client_width.min(max_texture_size).max(1);
    let height = client_height.min(max_texture_size).max(1);

    let surface_caps = surface.get_capabilities(&adapter);
    let surface_format = surface_caps
        .formats
        .iter()
        .copied()
        .find(|f| f.is_srgb())
        .unwrap_or(surface_caps.formats[0]);

    let config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: surface_format,
        width,
        height,
        present_mode: wgpu::PresentMode::AutoVsync,
        alpha_mode: surface_caps.alpha_modes[0],
        view_formats: vec![],
        desired_maximum_frame_latency: 2,
    };
    surface.configure(&device, &config);

    let renderer = SceneRenderer::new(
        &device,
        &config,
        scene,
        highlighted_terrain,
        highlighted_creature,
        movement_option_tiles,
    );
    let output = match surface.get_current_texture() {
        Ok(output) => output,
        Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
            surface.configure(&device, &config);
            surface.get_current_texture()?
        }
        Err(err) => return Err(anyhow::anyhow!("surface error: {err:?}")),
    };

    let view = output
        .texture
        .create_view(&wgpu::TextureViewDescriptor::default());
    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
        label: Some("Arp3D Render Encoder"),
    });

    renderer.render(&mut encoder, &view);
    queue.submit(std::iter::once(encoder.finish()));
    output.present();

    Ok((width, height))
}

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

    let (ray_origin, ray_dir) =
        cursor_ray(scene, viewport_width, viewport_height, cursor_x, cursor_y)?;
    pick_creature_with_ray(scene, ray_origin, ray_dir).map(|(_, idx)| idx)
}

pub fn pick_scene_object(
    scene: &Scene3d,
    viewport_width: u32,
    viewport_height: u32,
    cursor_x: f32,
    cursor_y: f32,
) -> Option<PickedObject> {
    let (ray_origin, ray_dir) =
        cursor_ray(scene, viewport_width, viewport_height, cursor_x, cursor_y)?;
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

fn build_scene_mesh(
    scene: &Scene3d,
    highlighted_terrain: Option<usize>,
    highlighted_creature: Option<usize>,
    movement_option_tiles: &[usize],
) -> (Vec<Vertex>, Vec<u32>, Option<SceneBounds>) {
    let mut vertices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 24);
    let mut indices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 36);
    let movement_option_tiles: std::collections::HashSet<usize> =
        movement_option_tiles.iter().copied().collect();

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

fn pick_terrain_tile_with_ray(scene: &Scene3d, ray_origin: Vec3, ray_dir: Vec3) -> Option<(f32, usize)> {
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

fn scene_bounds_for_camera(scene: &Scene3d) -> Option<SceneBounds> {
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

fn merge_bounds(existing: Option<SceneBounds>, min: Vec3, max: Vec3) -> Option<SceneBounds> {
    Some(match existing {
        Some(current) => SceneBounds {
            min: current.min.min(min),
            max: current.max.max(max),
        },
        None => SceneBounds { min, max },
    })
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
    creature: crate::Creature3d,
    highlighted: bool,
) -> (Vec3, Vec3) {
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
        ([0.38, 0.48, 0.98], [0.44, 0.54, 1.0], [0.30, 0.38, 0.92], [0.24, 0.30, 0.72])
    } else {
        ([0.22, 0.33, 0.80], [0.27, 0.38, 0.85], [0.18, 0.27, 0.70], [0.12, 0.18, 0.45])
    };
    let (torso_top, torso_side_light, torso_side_dark, torso_bottom) = if highlighted {
        ([0.28, 0.72, 0.95], [0.34, 0.78, 0.98], [0.22, 0.60, 0.85], [0.14, 0.46, 0.68])
    } else {
        ([0.18, 0.58, 0.82], [0.22, 0.64, 0.88], [0.14, 0.46, 0.68], [0.10, 0.32, 0.48])
    };
    let (arm_top, arm_side_light, arm_side_dark, arm_bottom) = if highlighted {
        ([0.96, 0.78, 0.64], [0.99, 0.82, 0.70], [0.90, 0.70, 0.56], [0.76, 0.59, 0.48])
    } else {
        ([0.85, 0.67, 0.54], [0.90, 0.72, 0.58], [0.76, 0.57, 0.45], [0.62, 0.47, 0.38])
    };
    let (head_top, head_side_light, head_side_dark, head_bottom) = if highlighted {
        ([1.0, 0.86, 0.72], [1.0, 0.90, 0.78], [0.94, 0.77, 0.63], [0.80, 0.64, 0.52])
    } else {
        ([0.91, 0.74, 0.60], [0.96, 0.78, 0.64], [0.81, 0.63, 0.50], [0.66, 0.51, 0.40])
    };

    // Legs
    let leg_left_center_x = center_x - (leg_width * 0.5 + leg_gap * 0.5);
    let leg_right_center_x = center_x + (leg_width * 0.5 + leg_gap * 0.5);
    for leg_center_x in [leg_left_center_x, leg_right_center_x] {
        push_part(
            Vec3::new(
                leg_center_x - leg_width * 0.5,
                base_y,
                center_z - leg_depth * 0.5,
            ),
            Vec3::new(
                leg_center_x + leg_width * 0.5,
                base_y + leg_height,
                center_z + leg_depth * 0.5,
            ),
            leg_top,
            leg_side_light,
            leg_side_dark,
            leg_bottom,
        );
    }

    let torso_min_y = base_y + leg_height;
    let torso_max_y = torso_min_y + torso_height;
    push_part(
        Vec3::new(
            center_x - torso_width * 0.5,
            torso_min_y,
            center_z - torso_depth * 0.5,
        ),
        Vec3::new(
            center_x + torso_width * 0.5,
            torso_max_y,
            center_z + torso_depth * 0.5,
        ),
        torso_top,
        torso_side_light,
        torso_side_dark,
        torso_bottom,
    );

    let arm_min_y = torso_min_y + torso_height * 0.08;
    let arm_max_y = arm_min_y + arm_height;
    for arm_center_x in [center_x - arm_offset, center_x + arm_offset] {
        push_part(
            Vec3::new(
                arm_center_x - arm_width * 0.5,
                arm_min_y,
                center_z - arm_depth * 0.5,
            ),
            Vec3::new(
                arm_center_x + arm_width * 0.5,
                arm_max_y,
                center_z + arm_depth * 0.5,
            ),
            arm_top,
            arm_side_light,
            arm_side_dark,
            arm_bottom,
        );
    }

    let head_min_y = torso_max_y;
    let head_max_y = head_min_y + head_height;
    push_part(
        Vec3::new(
            center_x - head_width * 0.5,
            head_min_y,
            center_z - head_depth * 0.5,
        ),
        Vec3::new(
            center_x + head_width * 0.5,
            head_max_y,
            center_z + head_depth * 0.5,
        ),
        head_top,
        head_side_light,
        head_side_dark,
        head_bottom,
    );

    if creature.controlled {
        let cursor_center = Vec3::new(center_x, head_max_y + 0.30, center_z);
        let cursor_radius = ((footprint_x.max(footprint_z)) * 0.22).clamp(0.12, 0.22);
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

fn creature_model_bounds(creature: crate::Creature3d) -> (Vec3, Vec3) {
    let footprint_x = creature.size_x.max(0.55);
    let footprint_z = creature.size_z.max(0.55);
    let height = creature.size_y.max(1.3);

    let center_x = creature.x + footprint_x * 0.5;
    let center_z = creature.z + footprint_z * 0.5;
    let base_y = creature.y;

    let leg_height = height * 0.42;
    let torso_height = height * 0.36;
    let head_height = height * 0.22;

    let leg_depth = (footprint_z * 0.22).clamp(0.14, 0.28);
    let torso_width = (footprint_x * 0.52).clamp(0.24, 0.44);
    let torso_depth = (footprint_z * 0.34).clamp(0.18, 0.32);

    let leg_width = (footprint_x * 0.22).clamp(0.14, 0.28);
    let arm_width = (leg_width * 0.90).clamp(0.12, 0.22);
    let arm_depth = (leg_depth * 0.90).clamp(0.12, 0.22);
    let arm_offset = torso_width * 0.5 + arm_width * 0.5 + 0.02;

    let head_depth = (torso_depth * 0.95).clamp(0.18, 0.30);
    let half_depth = (leg_depth.max(torso_depth).max(arm_depth).max(head_depth)) * 0.5;

    let min = Vec3::new(
        center_x - (arm_offset + arm_width * 0.5),
        base_y,
        center_z - half_depth,
    );
    let max = Vec3::new(
        center_x + (arm_offset + arm_width * 0.5),
        base_y + leg_height + torso_height + head_height,
        center_z + half_depth,
    );
    (min, max)
}

fn scene_mvp(width: u32, height: u32, bounds: Option<SceneBounds>) -> Mat4 {
    let vfov = std::f32::consts::FRAC_PI_4;
    let aspect = width.max(1) as f32 / height.max(1) as f32;

    if let Some(bounds) = bounds {
        let center = (bounds.min + bounds.max) * 0.5;
        let extent = bounds.max - bounds.min;
        let radius = (extent.length() * 0.5).max(1.0);

        let hfov = 2.0 * ((vfov * 0.5).tan() * aspect).atan();
        let limiting_fov = vfov.min(hfov).max(0.25);
        let distance = (radius / (limiting_fov * 0.5).tan()) * 1.35;

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

fn create_depth_texture(
    device: &wgpu::Device,
    config: &wgpu::SurfaceConfiguration,
) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Arp3D Depth Texture"),
        size: wgpu::Extent3d {
            width: config.width,
            height: config.height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Depth24Plus,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        view_formats: &[],
    })
}

const SHADER_SOURCE: &str = r#"
struct Uniforms {
    mvp: mat4x4<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) color: vec3<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec3<f32>,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = uniforms.mvp * vec4<f32>(in.position, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4<f32>(in.color, 1.0);
}
"#;
