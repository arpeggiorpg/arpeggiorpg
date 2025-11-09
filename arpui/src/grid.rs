mod svg_pan_zoom;

use dioxus::prelude::*;


use crate::{grid::svg_pan_zoom::SVGPanZoom, player_view::GAME};
use arptypes::*;

#[component]
pub fn SceneGrid(player_id: PlayerID) -> Element {
    let game = GAME.read();
    let player = game.players.get(&player_id);
    let scene_id = player.and_then(|p| p.scene);
    let mut selected_creature_id = use_signal(|| None::<CreatureID>);
    let mut menu_position = use_signal(|| None::<(f64, f64)>);

    if scene_id.is_none() {
        return rsx! {
            div {
                class: "w-full h-full flex items-center justify-center text-gray-500",
                "Ask your GM to put you in a scene."
            }
        };
    }

    let Some(scene) = scene_id.and_then(|sid| game.scenes.get(&sid)) else {
        return rsx! {
            div {
                class: "w-full h-full flex items-center justify-center text-red-500",
                "Scene not found!"
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

                // Creatures
                Creatures {
                    scene: scene.clone(),
                    game: game.clone(),
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
                        creature_id: creature_id,
                        position: (x, y),
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
    let tile_size = 100.0; // our SVG units are just centimeters, and each tile represents 1 meter.
    let corner_radius = 5.0;

    // Convert Point3 coordinates to SVG coordinates
    let x = point.x_cm();
    let y = point.y_cm();

    rsx! {
        rect {
            x: x,
            y: y,
            width: tile_size,
            height: tile_size,
            rx: corner_radius,
            ry: corner_radius,
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
    game: Game,
    selected_creature_id: Option<CreatureID>,
    on_creature_click: EventHandler<(CreatureID, f64, f64)>
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
                    game: game.clone(),
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
    game: Game,
    selected: bool,
    on_click: EventHandler<(CreatureID, f64, f64)>,
) -> Element {
    let creature = game.creatures.get(&creature_id);
    if creature.is_none() {
        return rsx! { g {} }; // Empty group if creature not found
    }
    let creature = creature.unwrap();

    let class = game.classes.get(&creature.class);
    let class_color = class.map(|c| c.color.as_str()).unwrap_or("gray");

    let x = position.x_cm();
    let y = position.y_cm();

    // Convert AABB size to tile units
    let width = creature.size.x_cm();
    let height = creature.size.y_cm();
    info!(?width, height, creature.name, ?creature.size, "and in centimeters...");

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
                    rx: 5.0,
                    ry: 5.0,
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
                    rx: 5.0,
                    ry: 5.0,
                    fill_opacity: "0"
                }
            } else {
                // Render creature as colored rectangle with name
                rect {
                    x: x,
                    y: y,
                    width: width,
                    height: height,
                    rx: 5.0,
                    ry: 5.0,
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
fn CreatureMenu(creature_id: CreatureID, position: (f64, f64), on_close: EventHandler<()>) -> Element {
    let (x, y) = position;

    let handle_walk_click = move |_evt: Event<MouseData>| {
        // TODO: Implement walk action
        info!("Walk clicked for creature {creature_id}");
        on_close.call(());
    };

    let handle_backdrop_click = move |_evt: Event<MouseData>| {
        on_close.call(());
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
            
            button {
                class: "block w-full px-4 py-2 text-left text-sm hover:bg-gray-100 border-none bg-none cursor-pointer",
                onclick: handle_walk_click,
                "Walk"
            }
        }
    }
}




