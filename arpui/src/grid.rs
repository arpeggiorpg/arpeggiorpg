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
    let tile_size = 100.0; // 100 units per tile, matching the React version
    let corner_radius = 5.0;

    // Convert Point3 coordinates to SVG coordinates
    let x = point.x_cm() as f64;
    let y = point.y_cm() as f64;

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
