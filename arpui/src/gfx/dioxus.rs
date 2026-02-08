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
    let client_width = canvas.client_width() as u32;
    let client_height = canvas.client_height() as u32;
    let (width, height) =
        arp3d::wgpu::render_scene_on_surface(&instance, &surface, client_width, client_height, &scene3d)
            .await?;
    canvas.set_width(width);
    canvas.set_height(height);
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
            size_x: 0.82,
            size_y: 1.86,
            size_z: 0.82,
        })
        .collect();

    Scene3d { terrain, creatures }
}

fn cm_to_world(cm: i64) -> f32 {
    cm as f32 / 100.0
}
