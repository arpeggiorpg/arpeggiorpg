use anyhow::Context;
use arptypes::Scene;
use arp3d::{Creature3d, Scene3d, TerrainTile3d};
use dioxus::prelude::*;
use tracing::error;
use wasm_bindgen::JsCast;

#[component]
pub fn GMWgpuScenePrototype(scene: Scene) -> Element {
    let canvas_id = format!("gm-wgpu-canvas-{}", scene.id);
    let canvas_id_for_render = canvas_id.clone();
    let scene3d = to_scene3d(&scene);

    let _startup = use_resource(move || {
        let canvas_id = canvas_id_for_render.clone();
        let scene3d = scene3d.clone();
        async move {
            if let Err(err) = render_scene_once(canvas_id, scene3d).await {
                error!(?err, "Failed to render GM wgpu scene prototype");
            }
        }
    });

    rsx! {
        canvas {
            id: "{canvas_id}",
            class: "block h-full w-full",
            style: "display: block; width: 100%; height: 100%; background: #0c0f1a;",
        }
    }
}

async fn render_scene_once(canvas_id: String, scene3d: Scene3d) -> anyhow::Result<()> {
    let window = web_sys::window().context("window missing")?;
    let document = window.document().context("document missing")?;
    let canvas = document
        .get_element_by_id(&canvas_id)
        .with_context(|| format!("canvas not found: {canvas_id}"))?
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| anyhow::anyhow!("element is not a canvas: {canvas_id}"))?;

    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::GL,
        ..Default::default()
    });
    let surface = instance.create_surface(wgpu::SurfaceTarget::Canvas(canvas.clone()))?;

    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        })
        .await?;

    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor {
            label: Some("GM Scene Prototype Device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_webgl2_defaults(),
            memory_hints: Default::default(),
            trace: Default::default(),
        })
        .await?;

    let max_texture_size = device.limits().max_texture_dimension_2d;
    let width = (canvas.client_width() as u32).min(max_texture_size).max(1);
    let height = (canvas.client_height() as u32).min(max_texture_size).max(1);
    canvas.set_width(width);
    canvas.set_height(height);

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

    let renderer = arp3d::wgpu::SceneRenderer::new(&device, &config, &scene3d);
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
        label: Some("GM Prototype Render Encoder"),
    });

    renderer.render(&mut encoder, &view);
    queue.submit(std::iter::once(encoder.finish()));
    output.present();
    Ok(())
}

fn to_scene3d(scene: &Scene) -> Scene3d {
    let terrain = scene
        .terrain
        .iter()
        .map(|point| TerrainTile3d {
            x: cm_to_world(point.x_cm()),
            y: cm_to_world(point.z_cm()),
            z: cm_to_world(point.y_cm()),
        })
        .collect();

    let creatures = scene
        .creatures
        .iter()
        .map(|(_id, (position, _visibility))| Creature3d {
            x: cm_to_world(position.x_cm()),
            y: cm_to_world(position.z_cm()),
            z: cm_to_world(position.y_cm()),
            size_x: 1.0,
            size_y: 1.0,
            size_z: 1.0,
        })
        .collect();

    Scene3d { terrain, creatures }
}

fn cm_to_world(cm: i64) -> f32 {
    cm as f32 / 100.0
}
