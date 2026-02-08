use anyhow::Context;
use arptypes::SceneID;
use dioxus::prelude::*;
use tracing::error;
use wasm_bindgen::JsCast;

use crate::gfx::wgpu::CubeRenderer;

#[component]
pub fn GMWgpuScenePrototype(scene_id: SceneID) -> Element {
    let canvas_id = format!("gm-wgpu-canvas-{scene_id}");
    let canvas_id_for_render = canvas_id.clone();

    let _startup = use_resource(move || {
        let canvas_id = canvas_id_for_render.clone();
        async move {
            if let Err(err) = render_scene_once(canvas_id).await {
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

async fn render_scene_once(canvas_id: String) -> anyhow::Result<()> {
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

    let renderer = CubeRenderer::new(&device, &config);
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
