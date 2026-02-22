use anyhow::Context;
use arp3d::{Creature3d, PickedObject, Scene3d, SceneCursor, SceneViewParams, TerrainTile3d};
use arptypes::{CreatureID, GameLog, Point3, Scene, SceneID, multitenant::RPIGameRequest};
use dioxus::asset_resolver::read_asset_bytes;
use dioxus::events::{TouchData, WheelData};
use dioxus::prelude::*;
use dioxus_elements::input_data::MouseButton;
use keyboard_types::Modifiers;
use std::{collections::HashSet, sync::Arc};
use tracing::error;
use wasm_bindgen::JsCast;

use crate::{
    GAME_SOURCE, GameSource,
    grid::{CreatureMenuAction, MOVEMENT_OPTIONS},
    rpi::{send_request, use_ws},
};

const CANVAS_ID: &str = "gm-wgpu-canvas";
const MIN_CAMERA_ZOOM: f32 = 0.4;
const MAX_CAMERA_ZOOM: f32 = 3.0;
const ZOOM_SENSITIVITY: f32 = 0.0015;
const ROTATE_SENSITIVITY: f32 = 0.005;
const TOUCH_ROTATE_SENSITIVITY: f32 = 0.004;
const DRAG_CLICK_SUPPRESS_PX: f32 = 6.0;
const CREATURE_RENDER_Y_OFFSET_TILES: f32 = 1.0;
static TERRAIN_CUBE_MODEL_ASSET: Asset = asset!("/assets/models/terrain_cube.glb");
static CREATURE_MODEL_ASSET: Asset = asset!("/assets/models/creature.glb");
static CONTROL_CURSOR_MODEL_ASSET: Asset = asset!("/assets/models/control_cursor.glb");

pub static SCENE_MODEL_LIBRARY: GlobalSignal<Option<Arc<arp3d::SceneModelLibrary>>> =
    Signal::global(|| None);

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

#[derive(Clone, Copy)]
struct CanvasPointerInput {
    view: SceneViewParams,
    cursor: SceneCursor,
}

#[derive(Clone, Copy)]
struct DragState {
    mode: DragMode,
    last_client_x: f32,
    last_client_y: f32,
    total_drag_px: f32,
    rotate_pivot: Option<(f32, f32)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DragMode {
    Pan,
    Rotate,
}

#[derive(Clone, Copy)]
enum TouchGestureState {
    Pan(DragState),
    Pinch {
        last_distance: f32,
        last_center_x: f32,
        total_pinch_delta: f32,
        rotate_pivot: Option<(f32, f32)>,
    },
}

#[derive(Clone, Copy, Default)]
struct CameraPan {
    x: f32,
    z: f32,
}

#[component]
pub fn Scene3dView(
    scene: Scene,
    #[props(default)] get_creature_actions: Option<Callback<CreatureID, Vec<CreatureMenuAction>>>,
) -> Element {
    let mut model_load_error = use_signal(|| None::<String>);
    let _model_loader = use_resource(move || async move {
        if SCENE_MODEL_LIBRARY().is_some() || model_load_error().is_some() {
            return;
        }
        match load_scene_model_library().await {
            Ok(library) => {
                *SCENE_MODEL_LIBRARY.write() = Some(Arc::new(library));
            }
            Err(err) => {
                error!(?err, "Failed to load 3D model assets");
                model_load_error.set(Some(err.to_string()));
            }
        }
    });

    let game_source = GAME_SOURCE();
    let (scene3d, scene_creatures) = to_scene3d(&scene, &game_source);
    let scene3d_for_events = scene3d.clone();
    let scene_creatures_for_events = scene_creatures.clone();
    let Some(scene_models) = SCENE_MODEL_LIBRARY() else {
        return rsx! {
            div {
                class: "h-full w-full flex items-center justify-center bg-slate-950 text-slate-200",
                if let Some(load_error) = model_load_error() {
                    div {
                        class: "flex flex-col items-center gap-3 rounded border border-red-400/40 bg-red-900/20 px-4 py-3 max-w-lg",
                        div { class: "text-sm font-semibold text-red-200", "Failed to load 3D assets." }
                        div { class: "text-xs text-red-100/90 break-all", "{load_error}" }
                        button {
                            r#type: "button",
                            class: "rounded border border-red-300 bg-red-100 px-3 py-1 text-xs font-medium text-red-900 hover:bg-red-200",
                            onclick: move |_| model_load_error.set(None),
                            "Retry"
                        }
                    }
                } else {
                    div { class: "text-sm text-slate-300", "Loading 3D assets..." }
                }
            }
        };
    };
    let camera_zoom = use_signal(|| 1.0f32);
    let camera_yaw = use_signal(|| std::f32::consts::FRAC_PI_4);
    let camera_pan = use_signal(CameraPan::default);
    let drag_state = use_signal(|| None::<DragState>);
    let touch_gesture = use_signal(|| None::<TouchGestureState>);
    let suppress_next_click = use_signal(|| false);
    let hovered_object = use_signal(|| None::<HoveredSceneObject>);
    let mut creature_menu = use_signal(|| None::<CreatureMenuState>);
    let mut movement_mode = use_signal(|| None::<MovementModeState>);
    let ws = use_ws();
    let scene_id = scene.id;
    let scene_models_for_events = scene_models.clone();

    let _startup = use_resource(move || {
        let scene_models = scene_models.clone();
        let scene3d = scene3d.clone();
        let hovered_object = hovered_object();
        let gm_mode = movement_mode();
        let movement_options = movement_options_for_render(gm_mode, MOVEMENT_OPTIONS());
        let camera_zoom = camera_zoom();
        let camera_yaw = camera_yaw();
        let camera_pan = camera_pan();
        async move {
            if let Err(err) = render_scene_once(
                &scene_models,
                scene3d,
                hovered_object,
                movement_options,
                camera_zoom,
                camera_yaw,
                camera_pan,
            )
            .await
            {
                error!(?err, "Failed to render wgpu scene prototype");
            }
        }
    });

    let scene3d_for_move = scene3d_for_events.clone();
    let scene3d_for_mouse_down = scene3d_for_events.clone();
    let scene3d_for_touch_start = scene3d_for_events.clone();
    let scene3d_for_touch_move = scene3d_for_events.clone();
    let scene3d_for_click = scene3d_for_events.clone();
    let scene_models_for_move = scene_models_for_events.clone();
    let scene_models_for_mouse_down = scene_models_for_events.clone();
    let scene_models_for_touch_start = scene_models_for_events.clone();
    let scene_models_for_touch_move = scene_models_for_events.clone();
    let scene_models_for_click = scene_models_for_events.clone();
    let scene_creatures_for_click = scene_creatures_for_events.clone();
    let handle_mouse_down = move |evt: Event<MouseData>| {
        handle_canvas_mouse_down(
            evt,
            &scene_models_for_mouse_down,
            &scene3d_for_mouse_down,
            drag_state,
            camera_zoom,
            camera_yaw,
            camera_pan,
        );
    };
    let handle_mouse_move = move |evt: Event<MouseData>| {
        handle_canvas_mouse_move(
            evt,
            &scene_models_for_move,
            &scene3d_for_move,
            drag_state,
            camera_zoom,
            camera_yaw,
            camera_pan,
            hovered_object,
            suppress_next_click,
        );
    };
    let handle_mouse_up = move |evt: Event<MouseData>| {
        handle_canvas_mouse_up(evt, drag_state);
    };
    let handle_wheel = move |evt: Event<WheelData>| {
        handle_canvas_wheel(evt, camera_zoom);
    };
    let handle_touch_start = move |evt: Event<TouchData>| {
        handle_canvas_touch_start(
            evt,
            &scene_models_for_touch_start,
            &scene3d_for_touch_start,
            touch_gesture,
            camera_zoom,
            camera_yaw,
            camera_pan,
        );
    };
    let handle_touch_move = move |evt: Event<TouchData>| {
        handle_canvas_touch_move(
            evt,
            &scene_models_for_touch_move,
            &scene3d_for_touch_move,
            touch_gesture,
            camera_zoom,
            camera_yaw,
            camera_pan,
            suppress_next_click,
        );
    };
    let handle_touch_end = move |evt: Event<TouchData>| {
        handle_canvas_touch_end(evt, touch_gesture);
    };
    let handle_touch_cancel = move |evt: Event<TouchData>| {
        handle_canvas_touch_cancel(evt, touch_gesture);
    };
    let handle_click = move |evt: Event<MouseData>| {
        let client_x = evt.data().client_coordinates().x as f32;
        let client_y = evt.data().client_coordinates().y as f32;
        let maybe_request = prepare_canvas_click(
            &scene_models_for_click,
            &scene3d_for_click,
            &scene_creatures_for_click,
            client_x,
            client_y,
            suppress_next_click,
            creature_menu,
            movement_mode,
            scene_id,
            get_creature_actions.clone(),
            camera_zoom(),
            camera_yaw(),
            camera_pan(),
        );
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
    };
    let handle_mouse_leave = move |_evt: Event<MouseData>| {
        handle_canvas_mouse_leave(drag_state, hovered_object);
    };

    rsx! {
        div {
            class: "relative h-full w-full",
            canvas {
                id: "{CANVAS_ID}",
                class: "block h-full w-full",
                style: "display: block; width: 100%; height: 100%; background: #0c0f1a;",
                onmousedown: handle_mouse_down,
                onmousemove: handle_mouse_move,
                onmouseup: handle_mouse_up,
                onwheel: handle_wheel,
                oncontextmenu: move |evt: Event<MouseData>| evt.prevent_default(),
                ontouchstart: handle_touch_start,
                ontouchmove: handle_touch_move,
                ontouchend: handle_touch_end,
                ontouchcancel: handle_touch_cancel,
                onclick: handle_click,
                onmouseleave: handle_mouse_leave,
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

fn handle_canvas_mouse_down(
    evt: Event<MouseData>,
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut drag_state: Signal<Option<DragState>>,
    camera_zoom: Signal<f32>,
    camera_yaw: Signal<f32>,
    camera_pan: Signal<CameraPan>,
) {
    let is_primary = evt.data().trigger_button() == Some(MouseButton::Primary);
    let is_secondary = evt.data().trigger_button() == Some(MouseButton::Secondary);
    let shift_down = evt.data().modifiers().contains(Modifiers::SHIFT);
    let mode = if is_secondary || (is_primary && shift_down) {
        DragMode::Rotate
    } else if is_primary {
        DragMode::Pan
    } else {
        return;
    };
    evt.prevent_default();
    let client_x = evt.data().client_coordinates().x as f32;
    let client_y = evt.data().client_coordinates().y as f32;
    let rotate_pivot = if mode == DragMode::Rotate {
        find_canvas().and_then(|canvas| {
            rotation_pivot_for_pointer(
                scene_models,
                scene3d,
                &canvas,
                client_x,
                client_y,
                camera_zoom(),
                camera_yaw(),
                camera_pan(),
            )
        })
    } else {
        None
    };
    drag_state.set(Some(DragState {
        mode,
        last_client_x: client_x,
        last_client_y: client_y,
        total_drag_px: 0.0,
        rotate_pivot,
    }));
}

fn handle_canvas_mouse_move(
    evt: Event<MouseData>,
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut drag_state: Signal<Option<DragState>>,
    camera_zoom: Signal<f32>,
    camera_yaw: Signal<f32>,
    camera_pan: Signal<CameraPan>,
    mut hovered_object: Signal<Option<HoveredSceneObject>>,
    suppress_next_click: Signal<bool>,
) {
    if let Some(active_drag) = drag_state() {
        evt.prevent_default();
        let client_x = evt.data().client_coordinates().x as f32;
        let client_y = evt.data().client_coordinates().y as f32;
        let updated_drag = match active_drag.mode {
            DragMode::Pan => update_pan_drag_from_client_delta(
                scene_models,
                scene3d,
                active_drag,
                client_x,
                client_y,
                camera_zoom(),
                camera_yaw(),
                camera_pan(),
                camera_pan,
                suppress_next_click,
            ),
            DragMode::Rotate => update_rotate_drag_from_client_delta(
                scene_models,
                scene3d,
                active_drag,
                client_x,
                client_y,
                camera_yaw,
                camera_pan,
                suppress_next_click,
            ),
        };
        drag_state.set(Some(updated_drag));
        return;
    }

    let client_x = evt.data().client_coordinates().x as f32;
    let client_y = evt.data().client_coordinates().y as f32;
    let Some(canvas) = find_canvas() else {
        return;
    };
    let new_hovered = pick_object_for_pointer(
        scene_models,
        scene3d,
        &canvas,
        client_x,
        client_y,
        camera_zoom(),
        camera_yaw(),
        camera_pan(),
    );
    if hovered_object() != new_hovered {
        hovered_object.set(new_hovered);
    }
}

fn handle_canvas_mouse_up(evt: Event<MouseData>, mut drag_state: Signal<Option<DragState>>) {
    if evt.data().trigger_button().is_some() {
        drag_state.set(None);
    }
}

fn handle_canvas_wheel(evt: Event<WheelData>, mut camera_zoom: Signal<f32>) {
    evt.prevent_default();
    let delta_y = evt.data().delta().strip_units().y as f32;
    let zoom_multiplier = (-delta_y * ZOOM_SENSITIVITY).exp();
    let new_zoom = (camera_zoom() * zoom_multiplier).clamp(MIN_CAMERA_ZOOM, MAX_CAMERA_ZOOM);
    camera_zoom.set(new_zoom);
}

fn handle_canvas_touch_start(
    evt: Event<TouchData>,
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut touch_gesture: Signal<Option<TouchGestureState>>,
    camera_zoom: Signal<f32>,
    camera_yaw: Signal<f32>,
    camera_pan: Signal<CameraPan>,
) {
    evt.prevent_default();
    let points = touch_client_points(&evt);
    let rotate_pivot = if points.len() >= 2 {
        find_canvas().and_then(|canvas| {
            let center = touch_center(points[0], points[1]);
            rotation_pivot_for_pointer(
                scene_models,
                scene3d,
                &canvas,
                center.0,
                center.1,
                camera_zoom(),
                camera_yaw(),
                camera_pan(),
            )
        })
    } else {
        None
    };
    touch_gesture.set(touch_gesture_for_points(points.as_slice(), rotate_pivot));
}

fn handle_canvas_touch_move(
    evt: Event<TouchData>,
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut touch_gesture: Signal<Option<TouchGestureState>>,
    mut camera_zoom: Signal<f32>,
    camera_yaw: Signal<f32>,
    camera_pan: Signal<CameraPan>,
    mut suppress_next_click: Signal<bool>,
) {
    let Some(gesture) = touch_gesture() else {
        return;
    };
    let points = touch_client_points(&evt);
    if points.is_empty() {
        return;
    }
    evt.prevent_default();

    if points.len() >= 2 {
        let current_distance = touch_distance(points[0], points[1]);
        if current_distance <= f32::EPSILON {
            return;
        }
        match gesture {
            TouchGestureState::Pinch {
                last_distance,
                last_center_x,
                mut total_pinch_delta,
                mut rotate_pivot,
            } => {
                let center = touch_center(points[0], points[1]);
                if rotate_pivot.is_none() {
                    rotate_pivot = find_canvas().and_then(|canvas| {
                        rotation_pivot_for_pointer(
                            scene_models,
                            scene3d,
                            &canvas,
                            center.0,
                            center.1,
                            camera_zoom(),
                            camera_yaw(),
                            camera_pan(),
                        )
                    });
                }
                if last_distance > f32::EPSILON {
                    let zoom_multiplier = current_distance / last_distance;
                    let new_zoom =
                        (camera_zoom() * zoom_multiplier).clamp(MIN_CAMERA_ZOOM, MAX_CAMERA_ZOOM);
                    camera_zoom.set(new_zoom);
                    total_pinch_delta += (current_distance - last_distance).abs();
                    let current_center_x = center.0;
                    let delta_center_x = current_center_x - last_center_x;
                    if delta_center_x.abs() > f32::EPSILON {
                        let delta_yaw = -delta_center_x * TOUCH_ROTATE_SENSITIVITY;
                        rotate_camera_around_pivot(
                            scene_models,
                            scene3d,
                            delta_yaw,
                            rotate_pivot,
                            camera_yaw,
                            camera_pan,
                        );
                        total_pinch_delta += delta_center_x.abs();
                    }
                    if total_pinch_delta > DRAG_CLICK_SUPPRESS_PX {
                        suppress_next_click.set(true);
                    }
                }
                touch_gesture.set(Some(TouchGestureState::Pinch {
                    last_distance: current_distance,
                    last_center_x: center.0,
                    total_pinch_delta,
                    rotate_pivot,
                }));
            }
            TouchGestureState::Pan(_) => {
                let center = touch_center(points[0], points[1]);
                let rotate_pivot = find_canvas().and_then(|canvas| {
                    rotation_pivot_for_pointer(
                        scene_models,
                        scene3d,
                        &canvas,
                        center.0,
                        center.1,
                        camera_zoom(),
                        camera_yaw(),
                        camera_pan(),
                    )
                });
                touch_gesture.set(Some(TouchGestureState::Pinch {
                    last_distance: current_distance,
                    last_center_x: center.0,
                    total_pinch_delta: 0.0,
                    rotate_pivot,
                }));
            }
        }
        return;
    }

    let (client_x, client_y) = points[0];
    let pan_state = match gesture {
        TouchGestureState::Pan(state) => state,
        TouchGestureState::Pinch { .. } => DragState {
            mode: DragMode::Pan,
            last_client_x: client_x,
            last_client_y: client_y,
            total_drag_px: 0.0,
            rotate_pivot: None,
        },
    };
    let updated_pan = update_pan_drag_from_client_delta(
        scene_models,
        scene3d,
        pan_state,
        client_x,
        client_y,
        camera_zoom(),
        camera_yaw(),
        camera_pan(),
        camera_pan,
        suppress_next_click,
    );
    touch_gesture.set(Some(TouchGestureState::Pan(updated_pan)));
}

fn handle_canvas_touch_end(
    evt: Event<TouchData>,
    mut touch_gesture: Signal<Option<TouchGestureState>>,
) {
    evt.prevent_default();
    let points = touch_client_points(&evt);
    touch_gesture.set(touch_gesture_for_points(points.as_slice(), None));
}

fn handle_canvas_touch_cancel(
    evt: Event<TouchData>,
    mut touch_gesture: Signal<Option<TouchGestureState>>,
) {
    evt.prevent_default();
    touch_gesture.set(None);
}

fn handle_canvas_mouse_leave(
    mut drag_state: Signal<Option<DragState>>,
    mut hovered_object: Signal<Option<HoveredSceneObject>>,
) {
    if drag_state().is_some() {
        drag_state.set(None);
    }
    if hovered_object().is_some() {
        hovered_object.set(None);
    }
}

fn prepare_canvas_click(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    scene_creatures: &[SceneCreatureRef],
    client_x: f32,
    client_y: f32,
    mut suppress_next_click: Signal<bool>,
    mut creature_menu: Signal<Option<CreatureMenuState>>,
    mut movement_mode: Signal<Option<MovementModeState>>,
    scene_id: SceneID,
    get_creature_actions: Option<Callback<CreatureID, Vec<CreatureMenuAction>>>,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> Option<RPIGameRequest> {
    if suppress_next_click() {
        suppress_next_click.set(false);
        return None;
    }

    let gm_mode = movement_mode();
    let player_mode = MOVEMENT_OPTIONS();
    let mut maybe_request = None;

    if let Some(canvas) = find_canvas() {
        match resolve_canvas_click(
            scene_models,
            scene3d,
            scene_creatures,
            &canvas,
            client_x,
            client_y,
            gm_mode.clone(),
            player_mode.clone(),
            scene_id,
            get_creature_actions,
            camera_zoom,
            camera_yaw,
            camera_pan,
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

    maybe_request
}

fn update_pan_drag_from_client_delta(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut active_drag: DragState,
    client_x: f32,
    client_y: f32,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
    mut camera_pan_signal: Signal<CameraPan>,
    mut suppress_next_click: Signal<bool>,
) -> DragState {
    let delta_x = client_x - active_drag.last_client_x;
    let delta_y = client_y - active_drag.last_client_y;
    if delta_x.abs() <= f32::EPSILON && delta_y.abs() <= f32::EPSILON {
        return active_drag;
    }

    if let Some(canvas) = find_canvas() {
        let view = canvas_view(&canvas, camera_zoom, camera_yaw, camera_pan);
        let (pan_dx, pan_dz) = arp3d::drag_pan_delta(scene_models, scene3d, view, delta_x, delta_y);
        camera_pan_signal.with_mut(|pan| {
            pan.x += pan_dx;
            pan.z += pan_dz;
        });
    }

    active_drag.last_client_x = client_x;
    active_drag.last_client_y = client_y;
    active_drag.total_drag_px += delta_x.hypot(delta_y);
    if active_drag.total_drag_px > DRAG_CLICK_SUPPRESS_PX {
        suppress_next_click.set(true);
    }
    active_drag
}

fn update_rotate_drag_from_client_delta(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    mut active_drag: DragState,
    client_x: f32,
    client_y: f32,
    camera_yaw: Signal<f32>,
    camera_pan: Signal<CameraPan>,
    mut suppress_next_click: Signal<bool>,
) -> DragState {
    let delta_x = client_x - active_drag.last_client_x;
    let delta_y = client_y - active_drag.last_client_y;
    if delta_x.abs() <= f32::EPSILON && delta_y.abs() <= f32::EPSILON {
        return active_drag;
    }
    let delta_yaw = -delta_x * ROTATE_SENSITIVITY;
    rotate_camera_around_pivot(
        scene_models,
        scene3d,
        delta_yaw,
        active_drag.rotate_pivot,
        camera_yaw,
        camera_pan,
    );
    active_drag.last_client_x = client_x;
    active_drag.last_client_y = client_y;
    active_drag.total_drag_px += delta_x.hypot(delta_y);
    if active_drag.total_drag_px > DRAG_CLICK_SUPPRESS_PX {
        suppress_next_click.set(true);
    }
    active_drag
}

fn rotate_camera_around_pivot(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    delta_yaw: f32,
    rotate_pivot: Option<(f32, f32)>,
    mut camera_yaw: Signal<f32>,
    mut camera_pan: Signal<CameraPan>,
) {
    if delta_yaw.abs() <= f32::EPSILON {
        return;
    }
    camera_yaw.set(camera_yaw() + delta_yaw);

    let Some((pivot_x, pivot_z)) = rotate_pivot else {
        return;
    };
    let Some((base_x, base_z)) = scene_focus_center_xz(scene_models, scene3d) else {
        return;
    };

    camera_pan.with_mut(|pan| {
        let center_x = base_x + pan.x;
        let center_z = base_z + pan.z;
        let rel_x = center_x - pivot_x;
        let rel_z = center_z - pivot_z;
        let cos_yaw = delta_yaw.cos();
        let sin_yaw = delta_yaw.sin();
        let rotated_rel_x = rel_x * cos_yaw - rel_z * sin_yaw;
        let rotated_rel_z = rel_x * sin_yaw + rel_z * cos_yaw;
        let next_center_x = pivot_x + rotated_rel_x;
        let next_center_z = pivot_z + rotated_rel_z;
        pan.x += next_center_x - center_x;
        pan.z += next_center_z - center_z;
    });
}

fn scene_focus_center_xz(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
) -> Option<(f32, f32)> {
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut min_z = f32::INFINITY;
    let mut max_z = f32::NEG_INFINITY;

    for tile in &scene3d.terrain {
        min_x = min_x.min(tile.x);
        max_x = max_x.max(tile.x + 1.0);
        min_z = min_z.min(tile.z);
        max_z = max_z.max(tile.z + 1.0);
    }

    for creature in &scene3d.creatures {
        let (bounds_min, bounds_max) = arp3d::creature_bounds(scene_models, *creature);
        min_x = min_x.min(bounds_min[0]);
        max_x = max_x.max(bounds_max[0]);
        min_z = min_z.min(bounds_min[2]);
        max_z = max_z.max(bounds_max[2]);
    }

    if !min_x.is_finite() || !min_z.is_finite() || !max_x.is_finite() || !max_z.is_finite() {
        return None;
    }

    Some(((min_x + max_x) * 0.5, (min_z + max_z) * 0.5))
}

fn rotation_pivot_for_pointer(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> Option<(f32, f32)> {
    let pointer = canvas_pointer_input(
        canvas,
        client_x,
        client_y,
        camera_zoom,
        camera_yaw,
        camera_pan,
    );
    if let Some(tile_idx) =
        arp3d::pick_terrain_tile(scene_models, scene3d, pointer.view, pointer.cursor)
        && let Some(tile) = scene3d.terrain.get(tile_idx)
    {
        return Some((tile.x + 0.5, tile.z + 0.5));
    }
    if let Some(creature_idx) =
        arp3d::pick_creature(scene_models, scene3d, pointer.view, pointer.cursor)
        && let Some(creature) = scene3d.creatures.get(creature_idx)
    {
        return Some((creature.x, creature.z));
    }
    None
}

fn touch_client_points(evt: &Event<TouchData>) -> Vec<(f32, f32)> {
    let mut touches = evt.data().target_touches();
    if touches.is_empty() {
        touches = evt.data().touches();
    }
    touches
        .into_iter()
        .map(|touch| {
            let coords = touch.client_coordinates();
            (coords.x as f32, coords.y as f32)
        })
        .collect()
}

fn touch_gesture_for_points(
    points: &[(f32, f32)],
    rotate_pivot: Option<(f32, f32)>,
) -> Option<TouchGestureState> {
    if points.len() >= 2 {
        let center = touch_center(points[0], points[1]);
        return Some(TouchGestureState::Pinch {
            last_distance: touch_distance(points[0], points[1]),
            last_center_x: center.0,
            total_pinch_delta: 0.0,
            rotate_pivot,
        });
    }
    points.first().map(|(x, y)| {
        TouchGestureState::Pan(DragState {
            mode: DragMode::Pan,
            last_client_x: *x,
            last_client_y: *y,
            total_drag_px: 0.0,
            rotate_pivot: None,
        })
    })
}

fn touch_distance(a: (f32, f32), b: (f32, f32)) -> f32 {
    (a.0 - b.0).hypot(a.1 - b.1)
}

fn touch_center(a: (f32, f32), b: (f32, f32)) -> (f32, f32) {
    ((a.0 + b.0) * 0.5, (a.1 + b.1) * 0.5)
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

async fn load_scene_model_library() -> anyhow::Result<arp3d::SceneModelLibrary> {
    let terrain_cube_glb = read_asset_bytes(&TERRAIN_CUBE_MODEL_ASSET)
        .await
        .map_err(|err| anyhow::anyhow!("loading terrain_cube.glb: {err}"))?;
    let creature_glb = read_asset_bytes(&CREATURE_MODEL_ASSET)
        .await
        .map_err(|err| anyhow::anyhow!("loading creature.glb: {err}"))?;
    let control_cursor_glb = read_asset_bytes(&CONTROL_CURSOR_MODEL_ASSET)
        .await
        .map_err(|err| anyhow::anyhow!("loading control_cursor.glb: {err}"))?;
    arp3d::SceneModelLibrary::from_glb_bytes(&terrain_cube_glb, &creature_glb, &control_cursor_glb)
        .context("parsing scene model glb bytes")
}

async fn render_scene_once(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: Scene3d,
    hovered_object: Option<HoveredSceneObject>,
    movement_options: Vec<Point3>,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
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
    let view = SceneViewParams {
        viewport_width: canvas.client_width() as u32,
        viewport_height: canvas.client_height() as u32,
        camera_zoom,
        camera_yaw,
        pan_x: camera_pan.x,
        pan_z: camera_pan.z,
    };
    let (hovered_tile, hovered_creature) = match hovered_object {
        Some(HoveredSceneObject::Terrain(idx)) => (Some(idx), None),
        Some(HoveredSceneObject::Creature(idx)) => (None, Some(idx)),
        None => (None, None),
    };
    let movement_option_tile_indices = movement_option_tile_indices(&scene3d, &movement_options);
    let (width, height) = arp3d::render_scene_on_surface(
        &instance,
        &surface,
        scene_models,
        view,
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
            // Movement/pathfinding coordinates stay at terrain level; only lift visuals in 3D.
            y: cm_to_world(position.z_cm()) + CREATURE_RENDER_Y_OFFSET_TILES,
            z: cm_to_world(position.y_cm()),
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
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> Option<HoveredSceneObject> {
    let pointer = canvas_pointer_input(
        canvas,
        client_x,
        client_y,
        camera_zoom,
        camera_yaw,
        camera_pan,
    );
    arp3d::pick_scene_object(scene_models, scene3d, pointer.view, pointer.cursor).map(|picked| {
        match picked {
            PickedObject::Terrain(idx) => HoveredSceneObject::Terrain(idx),
            PickedObject::Creature(idx) => HoveredSceneObject::Creature(idx),
        }
    })
}

fn pick_terrain_for_pointer(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    pointer: CanvasPointerInput,
) -> Option<usize> {
    arp3d::pick_terrain_tile(scene_models, scene3d, pointer.view, pointer.cursor)
}

fn resolve_canvas_click(
    scene_models: &Arc<arp3d::SceneModelLibrary>,
    scene3d: &Scene3d,
    scene_creatures: &[SceneCreatureRef],
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
    gm_mode: Option<MovementModeState>,
    player_mode: Option<(CreatureID, Vec<Point3>)>,
    scene_id: SceneID,
    get_creature_actions: Option<Callback<CreatureID, Vec<CreatureMenuAction>>>,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> ClickResolution {
    let pointer = canvas_pointer_input(
        canvas,
        client_x,
        client_y,
        camera_zoom,
        camera_yaw,
        camera_pan,
    );

    if let Some(mode) = gm_mode {
        let Some(tile_idx) = pick_terrain_for_pointer(scene_models, scene3d, pointer) else {
            return ClickResolution::KeepState;
        };
        let Some(tile) = scene3d.terrain.get(tile_idx).copied() else {
            return ClickResolution::KeepState;
        };
        let destination = terrain_tile_to_point(tile);
        if mode.action.requires_movement_options() && !mode.movement_options.contains(&destination)
        {
            return ClickResolution::KeepState;
        }
        return ClickResolution::QueueRequest(mode.action.destination_request(
            scene_id,
            mode.creature_id,
            destination,
        ));
    }

    if let Some((creature_id, options)) = player_mode {
        let Some(tile_idx) = pick_terrain_for_pointer(scene_models, scene3d, pointer) else {
            return ClickResolution::KeepState;
        };
        let Some(tile) = scene3d.terrain.get(tile_idx).copied() else {
            return ClickResolution::KeepState;
        };
        let destination = terrain_tile_to_point(tile);
        if !options.contains(&destination) {
            return ClickResolution::KeepState;
        }
        return ClickResolution::QueueRequest(CreatureMenuAction::PlayerWalk.destination_request(
            scene_id,
            creature_id,
            destination,
        ));
    }

    let Some(HoveredSceneObject::Creature(creature_index)) = pick_object_for_pointer(
        scene_models,
        scene3d,
        canvas,
        client_x,
        client_y,
        camera_zoom,
        camera_yaw,
        camera_pan,
    ) else {
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
    Point3::new(
        world_to_cm(tile.x),
        world_to_cm(tile.z),
        world_to_cm(tile.y),
    )
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

fn canvas_pointer_input(
    canvas: &web_sys::HtmlCanvasElement,
    client_x: f32,
    client_y: f32,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> CanvasPointerInput {
    let rect = canvas.get_bounding_client_rect();
    CanvasPointerInput {
        view: canvas_view(canvas, camera_zoom, camera_yaw, camera_pan),
        cursor: SceneCursor {
            x: client_x - rect.left() as f32,
            y: client_y - rect.top() as f32,
        },
    }
}

fn canvas_view(
    canvas: &web_sys::HtmlCanvasElement,
    camera_zoom: f32,
    camera_yaw: f32,
    camera_pan: CameraPan,
) -> SceneViewParams {
    SceneViewParams {
        viewport_width: canvas.client_width().max(1) as u32,
        viewport_height: canvas.client_height().max(1) as u32,
        camera_zoom,
        camera_yaw,
        pan_x: camera_pan.x,
        pan_z: camera_pan.z,
    }
}
