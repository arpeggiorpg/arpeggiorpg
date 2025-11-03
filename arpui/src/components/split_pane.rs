use dioxus::prelude::*;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::{MouseEvent, Window};

#[derive(Clone, Copy, PartialEq)]
pub enum SplitDirection {
    Horizontal,
    Vertical,
}

// This is unabashed AI slop but it works!!!


#[component]
pub fn SplitPane(
    direction: SplitDirection,
    #[props(default = 50.0)] initial_size: f64,
    #[props(default = 10.0)] min_size: f64,
    #[props(default = 90.0)] max_size: f64,
    first: Element,
    second: Element,
) -> Element {
    let mut split_position = use_signal(|| initial_size);
    let mut is_dragging = use_signal(|| false);
    let mut container_ref = use_signal(|| None::<web_sys::Element>);

    // Handle mouse down on the splitter
    let on_mouse_down = move |evt: Event<MouseData>| {
        evt.prevent_default();
        is_dragging.set(true);

        let window: Window = web_sys::window().unwrap();
        let document = window.document().unwrap();

        // Create mouse move handler
        let mouse_move_handler = Closure::wrap(Box::new(move |evt: MouseEvent| {
            if !is_dragging() {
                return;
            }

            if let Some(container) = container_ref() {
                let rect = container.get_bounding_client_rect();

                let (current_pos, container_size, container_start) = match direction {
                    SplitDirection::Horizontal => {
                        (evt.client_x() as f64, rect.width(), rect.left())
                    }
                    SplitDirection::Vertical => (evt.client_y() as f64, rect.height(), rect.top()),
                };

                if container_size > 0.0 {
                    let relative_pos = current_pos - container_start;
                    let new_position = ((relative_pos / container_size) * 100.0)
                        .max(min_size)
                        .min(max_size);
                    split_position.set(new_position);
                }
            }
        }) as Box<dyn FnMut(MouseEvent)>);

        // Create mouse up handler
        let mouse_up_handler = Closure::wrap(Box::new(move |_evt: MouseEvent| {
            is_dragging.set(false);
        }) as Box<dyn FnMut(MouseEvent)>);

        // Add event listeners
        let move_ref = mouse_move_handler.as_ref().unchecked_ref();
        let up_ref = mouse_up_handler.as_ref().unchecked_ref();

        let _ = document.add_event_listener_with_callback("mousemove", move_ref);
        let _ = document.add_event_listener_with_callback("mouseup", up_ref);

        // Forget closures - they will be cleaned up by browser GC.
        mouse_move_handler.forget();
        mouse_up_handler.forget();
    };

    let (container_class, first_pane_style, second_pane_style, splitter_class) = match direction {
        SplitDirection::Horizontal => (
            "flex h-full w-full",
            format!("width: {}%; height: 100%; overflow: auto;", split_position()),
            format!("width: {}%; height: 100%; overflow: auto;", 100.0 - split_position()),
            "w-1 bg-gray-300 hover:bg-gray-400 cursor-col-resize flex-shrink-0 select-none transition-colors duration-150 active:bg-gray-500",
        ),
        SplitDirection::Vertical => (
            "flex flex-col h-full w-full",
            format!("height: {}%; width: 100%; overflow: auto;", split_position()),
            format!("height: {}%; width: 100%; overflow: auto;", 100.0 - split_position()),
            "h-1 bg-gray-300 hover:bg-gray-400 cursor-row-resize flex-shrink-0 select-none transition-colors duration-150 active:bg-gray-500",
        ),
    };

    rsx! {
        div {
            class: "{container_class}",
            onmounted: move |evt| {
                if let Some(element) = evt.data().downcast::<web_sys::Element>() {
                    container_ref.set(Some(element.clone()));
                }
            },

            // First pane
            div {
                class: "flex-shrink-0",
                style: "{first_pane_style}",
                {first}
            }

            // Splitter
            div {
                class: "{splitter_class}",
                onmousedown: on_mouse_down,
                // Add visual feedback when dragging
                "data-dragging": if is_dragging() { "true" } else { "false" },
            }

            // Second pane
            div {
                class: "flex-shrink-0",
                style: "{second_pane_style}",
                {second}
            }
        }
    }
}
