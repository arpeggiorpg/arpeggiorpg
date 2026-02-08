use anyhow::Context;
use arptypes::{CreatureID, GMCommand, GameLog, Point3, Scene, multitenant::RPIGameRequest};
use arp3d::{Creature3d, Scene3d, TerrainTile3d};
use dioxus::prelude::*;
use tracing::error;
use wasm_bindgen::JsCast;

use crate::{
    GAME_SOURCE, GameSource,
    rpi::{send_request, use_ws},
};

const CANVAS_ID: &str = "gm-wgpu-canvas";

#[derive(Clone, Copy, PartialEq, Eq)]
enum HoveredSceneObject {
    Terrain(usize),
    Creature(usize),
}

#[derive(Clone, PartialEq)]
struct CreatureMenuState {
    creature_id: CreatureID,
    creature_name: String,
    client_x: f32,
    client_y: f32,
}

#[derive(Clone, PartialEq)]
struct MovementModeState {
    creature_id: CreatureID,
    creature_name: String,
}

#[derive(Clone)]
struct SceneCreatureRef {
    id: CreatureID,
    name: String,
}

#[component]
pub fn GMWgpuScenePrototype(scene: Scene) -> Element {
    let game_source = GAME_SOURCE();
    let (scene3d, scene_creatures) = to_scene3d(&scene, &game_source);
    let scene3d_for_events = scene3d.clone();
    let scene_creatures_for_events = scene_creatures.clone();
    let mut hovered_object = use_signal(|| None::<HoveredSceneObject>);
    let mut creature_menu = use_signal(|| None::<CreatureMenuState>);
    let mut movement_mode = use_signal(|| None::<MovementModeState>);
    let ws = use_ws();
    let scene_id = scene.id;

    let _startup = use_resource(move || {
        let scene3d = scene3d.clone();
        let hovered_object = hovered_object();
        async move {
            if let Err(err) = render_scene_once(scene3d, hovered_object).await {
                error!(?err, "Failed to render GM wgpu scene prototype");
            }
        }
    });

    let scene3d_for_move = scene3d_for_events.clone();
    let scene3d_for_click = scene3d_for_events.clone();
    let scene_creatures_for_click = scene_creatures_for_events.clone();

    rsx! {
        div {
            class: "relative h-full w-full",
            canvas {
                id: "{CANVAS_ID}",
                class: "block h-full w-full",
                style: "display: block; width: 100%; height: 100%; background: #0c0f1a;",
                onmousemove: move |evt: Event<MouseData>| {
                    let client_x = evt.data().client_coordinates().x as f32;
                    let client_y = evt.data().client_coordinates().y as f32;
                    let Some(canvas) = find_canvas() else {
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
                    let mode = movement_mode();
                    let scene3d_for_click = scene3d_for_click.clone();
                    let scene_creatures_for_click = scene_creatures_for_click.clone();
                    let mut maybe_request: Option<RPIGameRequest> = None;

                    if let Some(canvas) = find_canvas() {
                        if let Some(mode) = mode {
                            if let Some(tile_idx) =
                                pick_terrain_for_pointer(&scene3d_for_click, &canvas, client_x, client_y)
                            {
                                if let Some(tile) = scene3d_for_click.terrain.get(tile_idx).copied() {
                                    let destination = terrain_tile_to_point(tile);
                                    movement_mode.set(None);
                                    creature_menu.set(None);
                                    maybe_request = Some(RPIGameRequest::GMCommand {
                                        command: Box::new(GMCommand::SetCreaturePos {
                                            scene_id,
                                            creature_id: mode.creature_id,
                                            pos: destination,
                                        }),
                                    });
                                }
                            }
                        } else {
                            let picked =
                                pick_object_for_pointer(&scene3d_for_click, &canvas, client_x, client_y);
                            if let Some(HoveredSceneObject::Creature(creature_index)) = picked {
                                if let Some(creature_ref) =
                                    scene_creatures_for_click.get(creature_index).cloned()
                                {
                                    creature_menu.set(Some(CreatureMenuState {
                                        creature_id: creature_ref.id,
                                        creature_name: creature_ref.name,
                                        client_x,
                                        client_y,
                                    }));
                                } else {
                                    creature_menu.set(None);
                                }
                            } else {
                                creature_menu.set(None);
                            }
                        }
                    } else {
                        creature_menu.set(None);
                    }

                    async move {
                        if let Some(request) = maybe_request {
                            match send_request::<Result<Vec<GameLog>, String>>(request, ws).await {
                                Ok(Ok(_logs)) => {}
                                Ok(Err(err_msg)) => {
                                    error!(?err_msg, "SetCreaturePos rejected");
                                }
                                Err(err) => {
                                    error!(?err, "SetCreaturePos request failed");
                                }
                            }
                        }
                    }
                },
                onmouseleave: move |_| {
                    if hovered_object().is_some() {
                        hovered_object.set(None);
                    }
                },
            }

            if let Some(mode) = movement_mode() {
                div {
                    class: "absolute top-3 left-1/2 z-40 -translate-x-1/2 rounded-lg border border-blue-200 bg-blue-50/95 px-3 py-2 shadow-sm backdrop-blur-sm flex items-center gap-3",
                    onclick: move |evt: Event<MouseData>| evt.stop_propagation(),
                    span {
                        class: "text-sm text-blue-900",
                        "Pick a destination to move {mode.creature_name}"
                    }
                    button {
                        r#type: "button",
                        class: "rounded border border-blue-300 bg-white px-2 py-1 text-xs font-medium text-blue-800 hover:bg-blue-100",
                        onclick: move |_| movement_mode.set(None),
                        "Cancel"
                    }
                }
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
                        "{menu.creature_name}"
                    }
                    button {
                        r#type: "button",
                        class: "w-full rounded px-2 py-1.5 text-left text-sm text-gray-800 hover:bg-gray-100",
                        onclick: move |_| {
                            movement_mode.set(Some(MovementModeState {
                                creature_id: menu.creature_id,
                                creature_name: menu.creature_name.clone(),
                            }));
                            creature_menu.set(None);
                        },
                        "Teleport Creature"
                    }
                }
            }
        }
    }
}

async fn render_scene_once(
    scene3d: Scene3d,
    hovered_object: Option<HoveredSceneObject>,
) -> anyhow::Result<()> {
    let window = web_sys::window().context("window missing")?;
    let document = window.document().context("document missing")?;
    let canvas = document
        .get_element_by_id(CANVAS_ID)
        .with_context(|| format!("canvas not found: {CANVAS_ID}"))?
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| anyhow::anyhow!("element is not a canvas: {CANVAS_ID}"))?;

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

fn to_scene3d(scene: &Scene, game_source: &GameSource) -> (Scene3d, Vec<SceneCreatureRef>) {
    let terrain = scene
        .terrain
        .iter()
        .map(|point| TerrainTile3d {
            x: cm_to_world(point.x_cm()),
            y: cm_to_world(point.z_cm()),
            z: cm_to_world(point.y_cm()),
        })
        .collect();

    let mut ordered_scene_creatures: Vec<(CreatureID, Point3)> = scene
        .creatures
        .iter()
        .map(|(id, (position, _visibility))| (*id, *position))
        .collect();
    ordered_scene_creatures.sort_by_key(|(id, _)| *id);

    let creatures = ordered_scene_creatures
        .iter()
        .map(|(_id, position)| Creature3d {
            x: cm_to_world(position.x_cm()),
            y: cm_to_world(position.z_cm()),
            z: cm_to_world(position.y_cm()),
            size_x: 0.82,
            size_y: 1.86,
            size_z: 0.82,
        })
        .collect();

    let creature_refs = ordered_scene_creatures
        .into_iter()
        .map(|(id, _)| SceneCreatureRef {
            id,
            name: creature_name(game_source, id),
        })
        .collect();

    (Scene3d { terrain, creatures }, creature_refs)
}

fn cm_to_world(cm: i64) -> f32 {
    cm as f32 / 100.0
}

fn find_canvas() -> Option<web_sys::HtmlCanvasElement> {
    let window = web_sys::window()?;
    let document = window.document()?;
    let canvas_element = document.get_element_by_id(CANVAS_ID)?;
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

fn pick_terrain_for_pointer(
    scene3d: &Scene3d,
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
) -> Option<usize> {
    let rect = canvas.get_bounding_client_rect();
    let x = client_x - rect.left() as f32;
    let y = client_y - rect.top() as f32;
    let client_width = canvas.client_width().max(1) as u32;
    let client_height = canvas.client_height().max(1) as u32;
    arp3d::wgpu::pick_terrain_tile(scene3d, client_width, client_height, x, y)
}

fn terrain_tile_to_point(tile: TerrainTile3d) -> Point3 {
    Point3::new(world_to_cm(tile.x), world_to_cm(tile.z), world_to_cm(tile.y))
}

fn world_to_cm(world: f32) -> i64 {
    (world * 100.0).round() as i64
}

fn creature_name(game_source: &GameSource, creature_id: CreatureID) -> String {
    match game_source {
        GameSource::GM(game) => game
            .creatures
            .get(&creature_id)
            .map(|c| c.name.clone())
            .unwrap_or_else(|| creature_id.to_string()),
        GameSource::Player { game, .. } => game
            .creatures
            .get(&creature_id)
            .map(|c| c.name.clone())
            .unwrap_or_else(|| creature_id.to_string()),
    }
}
