use anyhow::Context;
use arp3d::{Creature3d, Scene3d, TerrainTile3d};
use arptypes::{
    CreatureID, GameLog, Point3, Scene, SceneID, multitenant::RPIGameRequest,
};
use dioxus::prelude::*;
use std::collections::HashSet;
use tracing::error;
use wasm_bindgen::JsCast;

use crate::{
    GAME_SOURCE, GameSource,
    grid::{CreatureMenuAction, MOVEMENT_OPTIONS},
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
    actions: Vec<CreatureMenuAction>,
}

#[derive(Clone, PartialEq)]
struct MovementModeState {
    creature_id: CreatureID,
    creature_name: String,
    action: CreatureMenuAction,
    movement_options: Vec<Point3>,
}

#[derive(Clone)]
struct SceneCreatureRef {
    id: CreatureID,
    name: String,
}

enum ClickResolution {
    KeepState,
    CloseMenu,
    OpenMenu(CreatureMenuState),
    QueueRequest(RPIGameRequest),
}

#[component]
pub fn Scene3dView(
    scene: Scene,
    #[props(default)] get_creature_actions: Option<Callback<CreatureID, Vec<CreatureMenuAction>>>,
) -> Element {
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
        let gm_mode = movement_mode();
        let movement_options = movement_options_for_render(gm_mode, MOVEMENT_OPTIONS());
        async move {
            if let Err(err) = render_scene_once(scene3d, hovered_object, movement_options).await {
                error!(?err, "Failed to render wgpu scene prototype");
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
                    let gm_mode = movement_mode();
                    let player_mode = MOVEMENT_OPTIONS();
                    let scene3d_for_click = scene3d_for_click.clone();
                    let scene_creatures_for_click = scene_creatures_for_click.clone();
                    let mut maybe_request: Option<RPIGameRequest> = None;

                    if let Some(canvas) = find_canvas() {
                        match resolve_canvas_click(
                            &scene3d_for_click,
                            &scene_creatures_for_click,
                            &canvas,
                            client_x,
                            client_y,
                            gm_mode.clone(),
                            player_mode.clone(),
                            scene_id,
                            get_creature_actions.clone(),
                        ) {
                            ClickResolution::KeepState => {}
                            ClickResolution::CloseMenu => creature_menu.set(None),
                            ClickResolution::OpenMenu(menu) => creature_menu.set(Some(menu)),
                            ClickResolution::QueueRequest(request) => {
                                if gm_mode.is_some() {
                                    movement_mode.set(None);
                                }
                                if player_mode.is_some() {
                                    *MOVEMENT_OPTIONS.write() = None;
                                }
                                creature_menu.set(None);
                                maybe_request = Some(request);
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
                                    error!(?err_msg, "Creature action request rejected");
                                }
                                Err(err) => {
                                    error!(?err, "Creature action request failed");
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
                MovementModeOverlay {
                    mode,
                    on_cancel: move |_| movement_mode.set(None),
                }
            }

            if let Some((creature_id, _)) = MOVEMENT_OPTIONS() {
                PlayerMovementModeOverlay {
                    creature_name: creature_name(&game_source, creature_id),
                    on_cancel: move |_| {
                        *MOVEMENT_OPTIONS.write() = None;
                    }
                }
            }

            if let Some(menu) = creature_menu() {
                CreatureMenuOverlay {
                    menu,
                    scene_id,
                    on_close: move |_| creature_menu.set(None),
                    on_set_movement_mode: move |mode| {
                        movement_mode.set(Some(mode));
                        creature_menu.set(None);
                    },
                    on_set_player_walk_mode: move |(creature_id, options)| {
                        *MOVEMENT_OPTIONS.write() = Some((creature_id, options));
                        creature_menu.set(None);
                    }
                }
            }
        }
    }
}

fn movement_options_for_render(
    gm_mode: Option<MovementModeState>,
    player_mode: Option<(CreatureID, Vec<Point3>)>,
) -> Vec<Point3> {
    if let Some(mode) = gm_mode {
        if mode.action.requires_movement_options() {
            return mode.movement_options;
        }
    }
    player_mode.map(|(_, options)| options).unwrap_or_default()
}

#[component]
fn MovementModeOverlay(mode: MovementModeState, on_cancel: EventHandler<()>) -> Element {
    rsx! {
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
                onclick: move |_| on_cancel.call(()),
                "Cancel"
            }
        }
    }
}

#[component]
fn CreatureMenuOverlay(
    menu: CreatureMenuState,
    scene_id: SceneID,
    on_close: EventHandler<()>,
    on_set_movement_mode: EventHandler<MovementModeState>,
    on_set_player_walk_mode: EventHandler<(CreatureID, Vec<Point3>)>,
) -> Element {
    let ws = use_ws();
    let creature_id = menu.creature_id;
    let creature_name = menu.creature_name.clone();
    let creature_name_for_action = creature_name.clone();
    let actions = menu.actions.clone();
    let mut execute_action = use_action(move |action: CreatureMenuAction| {
        let creature_name = creature_name_for_action.clone();
        async move {
            if action.requires_movement_options() {
                let movement_options = send_request::<Vec<Point3>>(
                    RPIGameRequest::MovementOptions {
                        scene_id,
                        creature_id,
                    },
                    ws,
                )
                .await?;
                match action {
                    CreatureMenuAction::PlayerWalk => {
                        on_set_player_walk_mode.call((creature_id, movement_options));
                    }
                    CreatureMenuAction::GMWalk => {
                        on_set_movement_mode.call(MovementModeState {
                            creature_id,
                            creature_name,
                            action,
                            movement_options,
                        });
                    }
                    CreatureMenuAction::Teleport => {
                        return Err(anyhow::anyhow!(
                            "Teleport should not require movement options."
                        ));
                    }
                }
                return Ok::<(), anyhow::Error>(());
            }

            match action {
                CreatureMenuAction::Teleport => {
                    on_set_movement_mode.call(MovementModeState {
                        creature_id,
                        creature_name,
                        action,
                        movement_options: vec![],
                    });
                }
                CreatureMenuAction::PlayerWalk | CreatureMenuAction::GMWalk => {
                    return Err(anyhow::anyhow!("Walk should require movement options."));
                }
            }
            Ok::<(), anyhow::Error>(())
        }
    });

    rsx! {
        div {
            class: "fixed inset-0 z-40",
            onclick: move |_| on_close.call(()),
        }
        div {
            class: "fixed z-50 min-w-44 rounded-md border border-gray-200 bg-white/95 p-2 shadow-lg backdrop-blur-sm",
            style: "left: {menu.client_x}px; top: {menu.client_y}px; transform: translate(8px, 8px);",
            onclick: move |evt: Event<MouseData>| evt.stop_propagation(),
            div {
                class: "px-2 py-1 text-xs font-semibold uppercase tracking-wide text-gray-500",
                "{creature_name}"
            }
            if execute_action.pending() {
                div {
                    class: "px-2 py-1 text-xs text-gray-500",
                    "Acting..."
                }
            }
            if let Some(Err(err)) = execute_action.value() {
                div {
                    class: "px-2 py-1 text-xs text-red-600",
                    "{err}"
                }
            }
            for action in actions {
                button {
                    r#type: "button",
                    class: "w-full rounded px-2 py-1.5 text-left text-sm text-gray-800 hover:bg-gray-100",
                    disabled: execute_action.pending(),
                    onclick: move |_| execute_action.call(action.clone()),
                    "{action.name()}"
                }
            }
        }
    }
}

#[component]
fn PlayerMovementModeOverlay(creature_name: String, on_cancel: EventHandler<()>) -> Element {
    rsx! {
        div {
            class: "absolute top-3 left-1/2 z-40 -translate-x-1/2 rounded-lg border border-cyan-200 bg-cyan-50/95 px-3 py-2 shadow-sm backdrop-blur-sm flex items-center gap-3",
            onclick: move |evt: Event<MouseData>| evt.stop_propagation(),
            span {
                class: "text-sm text-cyan-900",
                "Pick a destination to move {creature_name}"
            }
            button {
                r#type: "button",
                class: "rounded border border-cyan-300 bg-white px-2 py-1 text-xs font-medium text-cyan-800 hover:bg-cyan-100",
                onclick: move |_| on_cancel.call(()),
                "Cancel"
            }
        }
    }
}

async fn render_scene_once(
    scene3d: Scene3d,
    hovered_object: Option<HoveredSceneObject>,
    movement_options: Vec<Point3>,
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
    let movement_option_tile_indices = movement_option_tile_indices(&scene3d, &movement_options);
    let (width, height) = arp3d::wgpu::render_scene_on_surface(
        &instance,
        &surface,
        client_width,
        client_height,
        &scene3d,
        hovered_tile,
        hovered_creature,
        &movement_option_tile_indices,
    )
    .await?;
    canvas.set_width(width);
    canvas.set_height(height);
    Ok(())
}

fn to_scene3d(scene: &Scene, game_source: &GameSource) -> (Scene3d, Vec<SceneCreatureRef>) {
    let controlled_creatures = player_controlled_creatures(game_source);
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
        .map(|(id, position)| Creature3d {
            x: cm_to_world(position.x_cm()),
            y: cm_to_world(position.z_cm()),
            z: cm_to_world(position.y_cm()),
            size_x: 0.82,
            size_y: 1.86,
            size_z: 0.82,
            controlled: controlled_creatures.contains(id),
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

fn player_controlled_creatures(game_source: &GameSource) -> HashSet<CreatureID> {
    match game_source {
        GameSource::GM(_) => HashSet::new(),
        GameSource::Player { player_id, game } => game
            .players
            .get(player_id)
            .map(|player| player.creatures.clone())
            .unwrap_or_default(),
    }
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

fn resolve_canvas_click(
    scene3d: &Scene3d,
    scene_creatures: &[SceneCreatureRef],
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
    gm_mode: Option<MovementModeState>,
    player_mode: Option<(CreatureID, Vec<Point3>)>,
    scene_id: SceneID,
    get_creature_actions: Option<Callback<CreatureID, Vec<CreatureMenuAction>>>,
) -> ClickResolution {
    if let Some(mode) = gm_mode {
        let Some(tile_idx) = pick_terrain_for_pointer(scene3d, canvas, client_x, client_y) else {
            return ClickResolution::KeepState;
        };
        let Some(tile) = scene3d.terrain.get(tile_idx).copied() else {
            return ClickResolution::KeepState;
        };
        let destination = terrain_tile_to_point(tile);
        if mode.action.requires_movement_options() && !mode.movement_options.contains(&destination) {
            return ClickResolution::KeepState;
        }
        return ClickResolution::QueueRequest(mode.action.destination_request(
            scene_id,
            mode.creature_id,
            destination,
        ));
    }

    if let Some((creature_id, options)) = player_mode {
        let Some(tile_idx) = pick_terrain_for_pointer(scene3d, canvas, client_x, client_y) else {
            return ClickResolution::KeepState;
        };
        let Some(tile) = scene3d.terrain.get(tile_idx).copied() else {
            return ClickResolution::KeepState;
        };
        let destination = terrain_tile_to_point(tile);
        if !options.contains(&destination) {
            return ClickResolution::KeepState;
        }
        return ClickResolution::QueueRequest(
            CreatureMenuAction::PlayerWalk.destination_request(scene_id, creature_id, destination),
        );
    }

    let Some(HoveredSceneObject::Creature(creature_index)) =
        pick_object_for_pointer(scene3d, canvas, client_x, client_y)
    else {
        return ClickResolution::CloseMenu;
    };
    let Some(creature_ref) = scene_creatures.get(creature_index).cloned() else {
        return ClickResolution::CloseMenu;
    };

    let actions = get_creature_actions
        .map(|callback| callback.call(creature_ref.id))
        .unwrap_or_default();
    if actions.is_empty() {
        return ClickResolution::CloseMenu;
    }

    ClickResolution::OpenMenu(CreatureMenuState {
        creature_id: creature_ref.id,
        creature_name: creature_ref.name,
        client_x,
        client_y,
        actions,
    })
}

fn terrain_tile_to_point(tile: TerrainTile3d) -> Point3 {
    Point3::new(world_to_cm(tile.x), world_to_cm(tile.z), world_to_cm(tile.y))
}

fn world_to_cm(world: f32) -> i64 {
    (world * 100.0).round() as i64
}

fn movement_option_tile_indices(scene3d: &Scene3d, options: &[Point3]) -> Vec<usize> {
    let option_set: HashSet<Point3> = options.iter().copied().collect();
    scene3d
        .terrain
        .iter()
        .enumerate()
        .filter_map(|(idx, tile)| {
            let point = terrain_tile_to_point(*tile);
            option_set.contains(&point).then_some(idx)
        })
        .collect()
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
