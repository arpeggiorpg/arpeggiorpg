use anyhow::Context;
use arptypes::Scene;
use arp3d::{Creature3d, Scene3d, TerrainTile3d};
use dioxus::prelude::*;
use tracing::error;
use wasm_bindgen::JsCast;

#[derive(Clone, Copy, PartialEq, Eq)]
enum HoveredSceneObject {
    Terrain(usize),
    Creature(usize),
}

#[derive(Clone, Copy, PartialEq)]
struct CreatureMenuState {
    creature_index: usize,
    client_x: f32,
    client_y: f32,
}

#[component]
pub fn GMWgpuScenePrototype(scene: Scene) -> Element {
    let canvas_id = format!("gm-wgpu-canvas-{}", scene.id);
    let canvas_id_for_render = canvas_id.clone();
    let canvas_id_for_events = canvas_id.clone();
    let scene3d = to_scene3d(&scene);
    let scene3d_for_events = scene3d.clone();
    let mut hovered_object = use_signal(|| None::<HoveredSceneObject>);
    let mut creature_menu = use_signal(|| None::<CreatureMenuState>);

    let _startup = use_resource(move || {
        let canvas_id = canvas_id_for_render.clone();
        let scene3d = scene3d.clone();
        let hovered_object = hovered_object();
        async move {
            if let Err(err) = render_scene_once(canvas_id, scene3d, hovered_object).await {
                error!(?err, "Failed to render GM wgpu scene prototype");
            }
        }
    });

    let canvas_id_for_move = canvas_id_for_events.clone();
    let canvas_id_for_click = canvas_id_for_events.clone();
    let scene3d_for_move = scene3d_for_events.clone();
    let scene3d_for_click = scene3d_for_events.clone();

    rsx! {
        div {
            class: "relative h-full w-full",
            canvas {
                id: "{canvas_id}",
                class: "block h-full w-full",
                style: "display: block; width: 100%; height: 100%; background: #0c0f1a;",
                onmousemove: move |evt: Event<MouseData>| {
                    let client_x = evt.data().client_coordinates().x as f32;
                    let client_y = evt.data().client_coordinates().y as f32;
                    let Some(canvas) = find_canvas(&canvas_id_for_move) else {
                        return;
                    };
                    let new_hovered = pick_object_for_pointer(&scene3d_for_move, &canvas, client_x, client_y);
                    if hovered_object() != new_hovered {
                        hovered_object.set(new_hovered);
                    }
                },
                onclick: move |evt: Event<MouseData>| {
                    let client_x = evt.data().client_coordinates().x as f32;
                    let client_y = evt.data().client_coordinates().y as f32;
                    let Some(canvas) = find_canvas(&canvas_id_for_click) else {
                        creature_menu.set(None);
                        return;
                    };
                    let picked = pick_object_for_pointer(&scene3d_for_click, &canvas, client_x, client_y);
                    if let Some(HoveredSceneObject::Creature(creature_index)) = picked {
                        creature_menu.set(Some(CreatureMenuState {
                            creature_index,
                            client_x,
                            client_y,
                        }));
                    } else {
                        creature_menu.set(None);
                    }
                },
                onmouseleave: move |_| {
                    if hovered_object().is_some() {
                        hovered_object.set(None);
                    }
                },
            }

            if let Some(menu) = creature_menu() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| creature_menu.set(None),
                }
                div {
                    class: "fixed z-50 min-w-44 rounded-md border border-gray-200 bg-white/95 p-2 shadow-lg backdrop-blur-sm",
                    style: "left: {menu.client_x}px; top: {menu.client_y}px; transform: translate(8px, 8px);",
                    onclick: move |evt: Event<MouseData>| evt.stop_propagation(),
                    div {
                        class: "px-2 py-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                        "Creature"
                    }
                    div {
                        class: "px-2 py-1 text-sm text-gray-800",
                        "Menu placeholder (creature #{menu.creature_index})"
                    }
                }
            }
        }
    }
}

async fn render_scene_once(
    canvas_id: String,
    scene3d: Scene3d,
    hovered_object: Option<HoveredSceneObject>,
) -> anyhow::Result<()> {
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
    let (hovered_tile, hovered_creature) = match hovered_object {
        Some(HoveredSceneObject::Terrain(idx)) => (Some(idx), None),
        Some(HoveredSceneObject::Creature(idx)) => (None, Some(idx)),
        None => (None, None),
    };
    let (width, height) = arp3d::wgpu::render_scene_on_surface(
        &instance,
        &surface,
        client_width,
        client_height,
        &scene3d,
        hovered_tile,
        hovered_creature,
    )
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

fn find_canvas(canvas_id: &str) -> Option<web_sys::HtmlCanvasElement> {
    let window = web_sys::window()?;
    let document = window.document()?;
    let canvas_element = document.get_element_by_id(canvas_id)?;
    canvas_element.dyn_into::<web_sys::HtmlCanvasElement>().ok()
}

fn pick_object_for_pointer(
    scene3d: &Scene3d,
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
) -> Option<HoveredSceneObject> {
    let rect = canvas.get_bounding_client_rect();
    let x = client_x - rect.left() as f32;
    let y = client_y - rect.top() as f32;
    let client_width = canvas.client_width().max(1) as u32;
    let client_height = canvas.client_height().max(1) as u32;

    arp3d::wgpu::pick_scene_object(scene3d, client_width, client_height, x, y).map(|picked| {
        match picked {
            arp3d::wgpu::PickedObject::Terrain(idx) => HoveredSceneObject::Terrain(idx),
            arp3d::wgpu::PickedObject::Creature(idx) => HoveredSceneObject::Creature(idx),
        }
    })
}
