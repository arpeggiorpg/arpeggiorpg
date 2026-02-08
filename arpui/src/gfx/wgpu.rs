use bytemuck::{Pod, Zeroable};
use glam::{Mat4, Vec3};
use wgpu::util::DeviceExt;

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

const VERTICES: [Vertex; 24] = [
    Vertex {
        position: [-0.5, 0.5, -0.5],
        color: [0.95, 0.45, 0.45],
    },
    Vertex {
        position: [0.5, 0.5, -0.5],
        color: [0.95, 0.45, 0.45],
    },
    Vertex {
        position: [0.5, 0.5, 0.5],
        color: [0.95, 0.45, 0.45],
    },
    Vertex {
        position: [-0.5, 0.5, 0.5],
        color: [0.95, 0.45, 0.45],
    },
    Vertex {
        position: [-0.5, -0.5, -0.5],
        color: [0.25, 0.25, 0.35],
    },
    Vertex {
        position: [0.5, -0.5, -0.5],
        color: [0.25, 0.25, 0.35],
    },
    Vertex {
        position: [0.5, -0.5, 0.5],
        color: [0.25, 0.25, 0.35],
    },
    Vertex {
        position: [-0.5, -0.5, 0.5],
        color: [0.25, 0.25, 0.35],
    },
    Vertex {
        position: [0.5, -0.5, -0.5],
        color: [0.45, 0.85, 0.55],
    },
    Vertex {
        position: [0.5, -0.5, 0.5],
        color: [0.45, 0.85, 0.55],
    },
    Vertex {
        position: [0.5, 0.5, 0.5],
        color: [0.45, 0.85, 0.55],
    },
    Vertex {
        position: [0.5, 0.5, -0.5],
        color: [0.45, 0.85, 0.55],
    },
    Vertex {
        position: [-0.5, -0.5, -0.5],
        color: [0.45, 0.65, 0.95],
    },
    Vertex {
        position: [-0.5, -0.5, 0.5],
        color: [0.45, 0.65, 0.95],
    },
    Vertex {
        position: [-0.5, 0.5, 0.5],
        color: [0.45, 0.65, 0.95],
    },
    Vertex {
        position: [-0.5, 0.5, -0.5],
        color: [0.45, 0.65, 0.95],
    },
    Vertex {
        position: [-0.5, -0.5, 0.5],
        color: [0.95, 0.85, 0.35],
    },
    Vertex {
        position: [-0.5, 0.5, 0.5],
        color: [0.95, 0.85, 0.35],
    },
    Vertex {
        position: [0.5, 0.5, 0.5],
        color: [0.95, 0.85, 0.35],
    },
    Vertex {
        position: [0.5, -0.5, 0.5],
        color: [0.95, 0.85, 0.35],
    },
    Vertex {
        position: [-0.5, -0.5, -0.5],
        color: [0.75, 0.45, 0.9],
    },
    Vertex {
        position: [-0.5, 0.5, -0.5],
        color: [0.75, 0.45, 0.9],
    },
    Vertex {
        position: [0.5, 0.5, -0.5],
        color: [0.75, 0.45, 0.9],
    },
    Vertex {
        position: [0.5, -0.5, -0.5],
        color: [0.75, 0.45, 0.9],
    },
];

const INDICES: [u16; 36] = [
    0, 3, 1, 1, 3, 2, 4, 5, 7, 5, 6, 7, 8, 11, 9, 9, 11, 10, 12, 13, 15, 13, 14, 15, 16, 19, 17,
    17, 19, 18, 20, 21, 23, 21, 22, 23,
];

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

pub struct CubeRenderer {
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    index_count: u32,
    bind_group: wgpu::BindGroup,
    depth_view: wgpu::TextureView,
}

impl CubeRenderer {
    pub fn new(device: &wgpu::Device, config: &wgpu::SurfaceConfiguration) -> Self {
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("GM Prototype Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SOURCE.into()),
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GM Prototype Vertex Buffer"),
            contents: bytemuck::cast_slice(&VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        });
        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GM Prototype Index Buffer"),
            contents: bytemuck::cast_slice(&INDICES),
            usage: wgpu::BufferUsages::INDEX,
        });

        let uniforms = Uniforms {
            mvp: cube_mvp(config.width, config.height).to_cols_array_2d(),
        };
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("GM Prototype Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM,
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("GM Prototype Bind Group Layout"),
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
            label: Some("GM Prototype Bind Group"),
            layout: &bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("GM Prototype Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let depth_texture = create_depth_texture(device, config);
        let depth_view = depth_texture.create_view(&wgpu::TextureViewDescriptor::default());

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("GM Prototype Render Pipeline"),
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
            index_count: INDICES.len() as u32,
            bind_group,
            depth_view,
        }
    }

    pub fn render(&self, encoder: &mut wgpu::CommandEncoder, target_view: &wgpu::TextureView) {
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("GM Prototype Render Pass"),
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
        render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
        render_pass.draw_indexed(0..self.index_count, 0, 0..1);
    }
}

fn cube_mvp(width: u32, height: u32) -> Mat4 {
    let model = Mat4::from_rotation_y(0.65) * Mat4::from_rotation_x(-0.35);
    let view = Mat4::look_at_rh(Vec3::new(1.9, 1.8, 2.2), Vec3::ZERO, Vec3::Y);
    let aspect = width as f32 / height.max(1) as f32;
    let proj = Mat4::perspective_rh(std::f32::consts::FRAC_PI_4, aspect, 0.1, 100.0);
    proj * view * model
}

fn create_depth_texture(
    device: &wgpu::Device,
    config: &wgpu::SurfaceConfiguration,
) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("GM Prototype Depth Texture"),
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
