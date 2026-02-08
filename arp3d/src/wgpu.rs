use bytemuck::{Pod, Zeroable};
use glam::{Mat4, Vec3};
use wgpu::util::DeviceExt;

use crate::Scene3d;

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
    pub fn new(device: &wgpu::Device, config: &wgpu::SurfaceConfiguration, scene: &Scene3d) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Arp3D Scene Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SOURCE.into()),
        });

        let (mut vertices, mut indices, bounds) = build_scene_mesh(scene);
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

fn build_scene_mesh(scene: &Scene3d) -> (Vec<Vertex>, Vec<u32>, Option<SceneBounds>) {
    let mut vertices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 24);
    let mut indices = Vec::with_capacity((scene.terrain.len() + scene.creatures.len()) * 36);
    let mut bounds: Option<SceneBounds> = None;

    for tile in &scene.terrain {
        let min = Vec3::new(tile.x, tile.y, tile.z);
        let max = min + Vec3::ONE;
        append_cube(
            &mut vertices,
            &mut indices,
            min,
            max,
            [0.36, 0.73, 0.31],
            [0.62, 0.48, 0.30],
            [0.50, 0.38, 0.24],
            [0.22, 0.18, 0.14],
        );
        bounds = merge_bounds(bounds, min, max);
    }

    for creature in &scene.creatures {
        let min = Vec3::new(creature.x, creature.y, creature.z);
        let size = Vec3::new(
            creature.size_x.max(0.1),
            creature.size_y.max(0.1),
            creature.size_z.max(0.1),
        );
        let max = min + size;
        append_cube(
            &mut vertices,
            &mut indices,
            min,
            max,
            [0.80, 0.20, 0.20],
            [0.70, 0.25, 0.25],
            [0.60, 0.16, 0.16],
            [0.38, 0.11, 0.11],
        );
        bounds = merge_bounds(bounds, min, max);
    }

    (vertices, indices, bounds)
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
