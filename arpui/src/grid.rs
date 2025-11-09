mod svg_pan_zoom;

use dioxus::prelude::*;

use crate::{grid::svg_pan_zoom::SVGPanZoom, player_view::GAME};
use arptypes::*;

#[component]
pub fn SceneGrid(player_id: PlayerID) -> Element {
    let game = GAME.read();
    let player = game.players.get(&player_id);
    let scene_id = player.and_then(|p| p.scene);

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
            class: "w-full h-full bg-gray-200",
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
                    game: game.clone()
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
fn Creatures(scene: Scene, game: Game) -> Element {
    rsx! {
        g {
            id: "creatures",
            for (creature_id, (position, visibility)) in &scene.creatures {
                GridCreature {
                    key: "creature-{creature_id}",
                    creature_id: *creature_id,
                    position: *position,
                    visibility: *visibility,
                    game: game.clone()
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

    rsx! {
        g {
            opacity: opacity,
            style: "cursor: pointer",

            if !creature.icon_url.is_empty() {
                // Render creature with icon
                image {
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
