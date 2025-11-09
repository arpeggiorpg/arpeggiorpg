use dioxus::prelude::*;
use dioxus::events::{MouseData, WheelData};
use dioxus_elements::input_data::MouseButton;

#[derive(Clone, Copy, Debug)]
struct Transform {
    pan_x: f64,
    pan_y: f64,
    scale: f64,
}

impl Default for Transform {
    fn default() -> Self {
        Transform {
            pan_x: 0.0,
            pan_y: 0.0,
            scale: 1.0,
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SVGPanZoomProps {
    children: Element,
    #[props(default = "w-full h-full".to_string())]
    class: String,
    #[props(default = "-1000 -1000 2000 2000".to_string())]
    view_box: String,
    #[props(default = "xMidYMid slice".to_string())]
    preserve_aspect_ratio: String,
}

#[component]
pub fn SVGPanZoom(props: SVGPanZoomProps) -> Element {
    // Pan/Zoom state
    let mut transform = use_signal(Transform::default);
    let mut is_panning = use_signal(|| false);
    let mut last_mouse_pos = use_signal(|| (0.0, 0.0));

    // Event handlers
    let handle_mouse_down = move |evt: Event<MouseData>| {
        if evt.data().trigger_button() == Some(MouseButton::Primary) {
            evt.prevent_default();
            is_panning.set(true);
            let (x, y) = (
                evt.data().screen_coordinates().x,
                evt.data().screen_coordinates().y,
            );
            last_mouse_pos.set((x, y));
        }
    };

    let handle_mouse_move = move |evt: Event<MouseData>| {
        if *is_panning.read() {
            evt.prevent_default();
            let (current_x, current_y) = (
                evt.data().screen_coordinates().x,
                evt.data().screen_coordinates().y,
            );
            let (last_x, last_y) = *last_mouse_pos.read();

            let dx = current_x - last_x;
            let dy = current_y - last_y;

            transform.with_mut(|t| {
                t.pan_x += dx;
                t.pan_y += dy;
            });

            last_mouse_pos.set((current_x, current_y));
        }
    };

    let handle_mouse_up = move |_evt: Event<MouseData>| {
        is_panning.set(false);
    };

    let handle_mouse_leave = move |_evt: Event<MouseData>| {
        is_panning.set(false);
    };

    let handle_wheel = move |evt: Event<WheelData>| {
        evt.prevent_default();
        let delta = evt.data().delta().strip_units().y;
        let zoom_factor = if delta > 0.0 { 0.9 } else { 1.1 };

        transform.with_mut(|t| {
            let new_scale = (t.scale * zoom_factor).clamp(0.1, 10.0);
            t.scale = new_scale;
        });
    };

    let current_transform = *transform.read();
    let cursor_class = if *is_panning.read() {
        "cursor-grabbing"
    } else {
        "cursor-grab"
    };

    rsx! {
        svg {
            class: "{props.class} {cursor_class}",
            view_box: props.view_box,
            preserve_aspect_ratio: props.preserve_aspect_ratio,
            onmousedown: handle_mouse_down,
            onmousemove: handle_mouse_move,
            onmouseup: handle_mouse_up,
            onmouseleave: handle_mouse_leave,
            onwheel: handle_wheel,

            // Viewport group with transform
            g {
                id: "svg-pan-zoom-viewport",
                transform: "translate({current_transform.pan_x}, {current_transform.pan_y}) scale({current_transform.scale})",

                // Invisible rect needed for proper bounds calculation
                rect {
                    fill_opacity: "0",
                    x: "0",
                    y: "0",
                    width: "5",
                    height: "5"
                }

                {props.children}
            }
        }
    }
}
