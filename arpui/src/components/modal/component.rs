//! A generalized Modal component that uses the modern browser Popover API for centered dialogs.

use dioxus::core::AttributeValue;
use dioxus::prelude::*;
use uuid::Uuid;
use wasm_bindgen::JsCast;

#[component]
pub fn Modal(
    #[props(default = false)] open: bool,
    #[props(optional)] on_close: Option<EventHandler<()>>,
    #[props(optional)] id: Option<String>,
    #[props(optional)] modal_class: Option<String>,
    #[props(optional)] backdrop_class: Option<String>,
    #[props(extends = GlobalAttributes)]
    #[props(extends = div)]
    attributes: Vec<Attribute>,
    children: Element,
) -> Element {
    let id_value = id.clone();
    let base_id = use_signal(move || {
        id_value.unwrap_or_else(|| format!("modal-{}", Uuid::new_v4().as_simple()))
    });
    let popover_id = format!("{}-popover", base_id());

    let mut popover_ref = use_signal(|| None::<web_sys::HtmlElement>);

    // Effect to show/hide modal based on open prop
    use_effect(move || {
        if let Some(popover) = popover_ref() {
            if open {
                let _ = popover.show_popover();
            } else {
                let _ = popover.hide_popover();
            }
        }
    });

    // Handle escape key to close modal
    let handle_keydown = move |evt: Event<KeyboardData>| {
        if evt.key() == Key::Escape {
            if let Some(on_close) = &on_close {
                on_close.call(());
            }
        }
    };

    // Default classes
    let default_backdrop_class = "modal-backdrop fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4";
    let default_modal_class = "modal-content bg-white rounded-lg shadow-xl max-w-md w-full mx-auto";

    let final_backdrop_class = match backdrop_class {
        Some(custom) if !custom.trim().is_empty() => custom,
        _ => default_backdrop_class.to_string(),
    };

    let final_modal_class = match modal_class {
        Some(custom) if !custom.trim().is_empty() => custom,
        _ => default_modal_class.to_string(),
    };

    // Collect any additional classes from attributes
    let mut collected_classes = String::new();
    let mut filtered_attributes = Vec::new();
    for attr in attributes {
        if attr.name == "class" {
            if let AttributeValue::Text(text) = attr.value.clone() {
                if !text.trim().is_empty() {
                    if !collected_classes.is_empty() {
                        collected_classes.push(' ');
                    }
                    collected_classes.push_str(text.trim());
                }
            }
        } else {
            filtered_attributes.push(attr);
        }
    }

    let final_content_class = if collected_classes.is_empty() {
        final_modal_class
    } else {
        format!("{final_modal_class} {collected_classes}")
    };

    rsx! {
        Fragment {
            document::Link { rel: "stylesheet", href: asset!("./style.css") }
            div {
                id: "{popover_id}",
                class: "modal-popover",
                popover: "manual",
                role: "dialog",
                "aria-modal": "true",
                tabindex: "-1",
                onmounted: move |evt| {
                    if let Some(element) = evt.data().downcast::<web_sys::Element>() {
                        if let Ok(html_element) = element.clone().dyn_into::<web_sys::HtmlElement>() {
                            popover_ref.set(Some(html_element));
                        }
                    }
                },
                onkeydown: handle_keydown,
                div {
                    class: "{final_backdrop_class}",
                    div {
                        class: "{final_content_class}",
                        onclick: move |evt: Event<MouseData>| {
                            // Prevent backdrop click when clicking modal content
                            evt.stop_propagation();
                        },
                        ..filtered_attributes,
                        {children}
                    }
                }
            }
        }
    }
}
