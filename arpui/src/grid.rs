mod svg_pan_zoom;

use dioxus::prelude::*;

use crate::{
    components::creature::ClassIcon,
    grid::svg_pan_zoom::SVGPanZoom,
    player_view::GAME,
    rpi::{send_request, use_ws},
};
use arptypes::{multitenant::RPIGameRequest, *};

const TILE_SIZE: f64 = 100.0; // our SVG units are just centimeters, and each tile represents 1 meter.
const CORNER_RADIUS: f64 = 5.0;

pub static MOVEMENT_OPTIONS: GlobalSignal<Option<(CreatureID, Vec<Point3>)>> =
    Signal::global(|| None);

#[component]
pub fn SceneGrid(
    scene: Option<Scene>,
    get_creature_actions: Callback<CreatureID, Vec<CreatureMenuAction>>,
) -> Element {
    let mut selected_creature_id = use_signal(|| None::<CreatureID>);
    let mut menu_position = use_signal(|| None::<(f64, f64)>);

    let Some(scene) = scene else {
        return rsx! {
            div {
                class: "w-full h-full flex items-center justify-center text-gray-500",
                "Ask your GM to put you in a scene."
            }
        };
    };

    rsx! {
        div {
            class: "w-full h-full bg-gray-200 relative",
            SVGPanZoom {
                class: "w-full h-full",
                view_box: "-1000 -1000 2000 2000",
                preserve_aspect_ratio: "xMidYMid slice",

                // Background image if present
                if !scene.background_image_url.is_empty() {
                    BackgroundImage { scene: scene.clone() }
                }

                // Terrain tiles
                Terrain {
                    terrain: scene.terrain.clone(),
                    has_background_image: !scene.background_image_url.is_empty()
                }

                // Movement options (walkable tiles)
                if let Some((creature_id, options)) = MOVEMENT_OPTIONS() {
                    MovementOptions {
                        options: options,
                        on_tile_click: move |destination| async move {
                            *MOVEMENT_OPTIONS.write() = None;
                            let ws = use_ws();
                            let request = RPIGameRequest::PlayerCommand {
                                command: PlayerCommand::PathCreature {
                                    creature_id,
                                    destination,
                                },
                            };
                            let _ = send_request::<()>(request, ws).await;
                        }
                    }
                }

                // Creatures
                Creatures {
                    scene: scene.clone(),
                    selected_creature_id: selected_creature_id(),
                    on_creature_click: move |(creature_id, x, y)| {
                        selected_creature_id.set(Some(creature_id));
                        menu_position.set(Some((x, y)));
                    }
                }
            }

            // Render creature menu outside SVG
            if let Some(creature_id) = selected_creature_id() {
                if let Some((x, y)) = menu_position() {
                    CreatureMenu {
                        scene_id: scene.id,
                        creature_id: creature_id,
                        position: (x, y),
                        actions: get_creature_actions(creature_id),
                        on_close: move |_| {
                            selected_creature_id.set(None);
                            menu_position.set(None);
                        },
                    }
                }
            }
        }
    }
}

#[component]
fn BackgroundImage(scene: Scene) -> Element {
    let (bg_x_scale, bg_y_scale) = scene.background_image_scale;
    let (offset_x, offset_y) = scene.background_image_offset.unwrap_or((0, 0));

    rsx! {
        image {
            "xlink:href": scene.background_image_url,
            x: offset_x,
            y: offset_y,
            style: "transform: scale({bg_x_scale}, {bg_y_scale})",
            preserve_aspect_ratio: "none"
        }
    }
}

#[component]
fn Terrain(terrain: Vec<Point3>, has_background_image: bool) -> Element {
    let terrain_color = if has_background_image {
        "transparent"
    } else {
        "white"
    };

    let terrain_tiles = terrain.iter().map(|pt| {
        let key = format!("terrain-{pt}");
        rsx! {
            TerrainTile {
                key: "{key}",
                point: pt.clone(),
                color: terrain_color.to_string()
            }
        }
    });

    rsx! {
        g {
            id: "terrain",
            {terrain_tiles}
        }
    }
}

#[component]
fn TerrainTile(point: Point3, color: String) -> Element {
    // Convert Point3 coordinates to SVG coordinates
    let x = point.x_cm();
    let y = point.y_cm();

    rsx! {
        rect {
            x: x,
            y: y,
            width: TILE_SIZE,
            height: TILE_SIZE,
            rx: CORNER_RADIUS,
            ry: CORNER_RADIUS,
            fill: color,
            stroke: "black",
            stroke_width: "1",
            fill_opacity: 1.0
        }
    }
}

#[component]
fn Creatures(
    scene: Scene,
    selected_creature_id: Option<CreatureID>,
    on_creature_click: EventHandler<(CreatureID, f64, f64)>,
) -> Element {
    rsx! {
        g {
            id: "creatures",
            for (creature_id, (position, visibility)) in &scene.creatures {
                GridCreature {
                    key: "creature-{creature_id}",
                    creature_id: *creature_id,
                    position: *position,
                    visibility: *visibility,
                    selected: selected_creature_id == Some(*creature_id),
                    on_click: on_creature_click
                }
            }
        }
    }
}

#[component]
fn GridCreature(
    creature_id: CreatureID,
    position: Point3,
    visibility: Visibility,
    selected: bool,
    on_click: EventHandler<(CreatureID, f64, f64)>,
) -> Element {
    let game = GAME();
    let creature = game.creatures.get(&creature_id);
    let Some(creature) = creature else {
        return rsx! { g {} }; // Empty group if creature not found
    };

    let class = game.classes.get(&creature.class);
    let class_color = class.map(|c| c.color.as_str()).unwrap_or("gray");

    let x = position.x_cm();
    let y = position.y_cm();

    // Convert AABB size to tile units
    let width = creature.size.x_cm();
    let height = creature.size.y_cm();

    let opacity = match visibility {
        Visibility::GMOnly => 0.4,
        _ => 1.0,
    };

    let handle_click = move |evt: Event<MouseData>| {
        evt.stop_propagation();
        let page_x = evt.page_coordinates().x;
        let page_y = evt.page_coordinates().y;
        on_click.call((creature_id, page_x, page_y));
    };

    let anchor_id = format!("creature-{creature_id}");

    rsx! {
        g {
            opacity: opacity,
            style: "cursor: pointer",
            onclick: handle_click,

            if !creature.icon_url.is_empty() {
                // Render creature with icon
                image {
                    id: "{anchor_id}",
                    href: creature.icon_url.clone(),
                    width: width,
                    height: height,
                    rx: CORNER_RADIUS,
                    ry: CORNER_RADIUS,
                    x: x,
                    y: y,
                    stroke: "black",
                    stroke_width: "1",
                    fill: "white",
                    fill_opacity: "1"
                }
                // Invisible rect for click handling
                rect {
                    x: x,
                    y: y,
                    width: width,
                    height: height,
                    rx: CORNER_RADIUS,
                    ry: CORNER_RADIUS,
                    fill_opacity: "0"
                }
            } else {
                // Render creature as colored rectangle with name
                rect {
                    x: x,
                    y: y,
                    width: width,
                    height: height,
                    rx: CORNER_RADIUS,
                    ry: CORNER_RADIUS,
                    fill: class_color,
                    stroke: "black",
                    stroke_width: "1"
                }
                text {
                    x: x + (width as i64) / 2,
                    y: y + 20,
                    font_size: "50",
                    text_anchor: "middle",
                    dominant_baseline: "hanging",
                    style: "pointer-events: none",
                    fill: "white",
                    stroke: "black",
                    stroke_width: "1",
                    paint_order: "stroke",
                    {creature.name.chars().take(4).collect::<String>()}
                }
            }
        }
    }
}

#[component]
fn CreatureMenu(
    scene_id: SceneID,
    creature_id: CreatureID,
    position: (f64, f64),
    actions: Vec<CreatureMenuAction>,
    on_close: EventHandler<()>,
) -> Element {
    let (x, y) = position;

    let mut handle_action_click = use_action(move |action: CreatureMenuAction| async move {
        if let Err(error) = action.act(scene_id, creature_id).await {
            error!(?error, "Error executing action");
        }
        // Important that we don't close until after we do the action!
        // If we do, then this component gets unmounted and the action future gets dropped before
        // the result comes in.
        on_close.call(());
        Ok::<(), anyhow::Error>(())
    });

    let handle_backdrop_click = move |_evt: Event<MouseData>| {
        on_close.call(());
    };

    let game = GAME();
    let Some(creature) = game.creatures.get(&creature_id) else {
        return rsx! {};
    };
    rsx! {
        // Backdrop to close menu when clicking elsewhere
        div {
            class: "fixed inset-0 z-40 bg-transparent",
            onclick: handle_backdrop_click,
        }

        div {
            class: "fixed z-50 bg-white border border-gray-300 rounded-md shadow-lg py-2",
            style: "top: {y}px; left: {x}px; min-width: 120px;",
            role: "menu",

            div {
                ClassIcon { class_id: creature.class }
                span { class: "font-bold",
                    "{creature.name}"
                }
            }

            if handle_action_click.pending() {
                div { "Acting..." }
            }
            for action in actions {
                button {
                    class: "block w-full px-4 py-2 text-left text-sm hover:bg-gray-100 border-none bg-none cursor-pointer",
                    onclick: move |_e| handle_action_click.call(action.clone()),
                    disabled: handle_action_click.pending(),
                    "{action.name()}"
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum CreatureMenuAction {
    PlayerWalk,
}

impl CreatureMenuAction {
    fn name(&self) -> &'static str {
        match self {
            CreatureMenuAction::PlayerWalk => "walk",
        }
    }

    async fn act(&self, scene_id: SceneID, creature_id: CreatureID) -> anyhow::Result<()> {
        match self {
            CreatureMenuAction::PlayerWalk => {
                // Request movement options from server
                let ws = use_ws();
                let request = RPIGameRequest::MovementOptions {
                    scene_id,
                    creature_id,
                };
                let options = send_request::<Vec<Point3>>(request, ws).await?;
                *MOVEMENT_OPTIONS.write() = Some((creature_id, options));
            }
        }
        Ok(())
    }
}

#[component]
fn MovementOptions(options: Vec<Point3>, on_tile_click: EventHandler<Point3>) -> Element {
    rsx! {
        g {
            id: "movement-options",
            for option in options {
                MovementTile {
                    key: "movement-{option}",
                    point: option,
                    on_click: move |point| on_tile_click.call(point),
                }
            }
        }
    }
}

#[component]
fn MovementTile(point: Point3, on_click: EventHandler<Point3>) -> Element {
    let x = point.x_cm();
    let y = point.y_cm();

    let handle_click = move |evt: Event<MouseData>| {
        evt.stop_propagation();
        on_click.call(point);
    };

    rsx! {
        rect {
            x: x,
            y: y,
            width: TILE_SIZE,
            height: TILE_SIZE,
            rx: CORNER_RADIUS,
            ry: CORNER_RADIUS,
            fill: "cyan",
            fill_opacity: "0.4",
            stroke: "blue",
            stroke_width: "2",
            style: "cursor: pointer",
            onclick: handle_click,
        }
    }
}
