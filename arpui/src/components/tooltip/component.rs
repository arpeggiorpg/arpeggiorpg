//! This is *not* the standard Dioxus Tooltip - this is my own AI slop that uses the modern Popover
//! browser API + CSS anchor positioning. This doesn't have any issues with being truncated by small
//! containers!

use dioxus::core::AttributeValue;
use dioxus::prelude::*;
use js_sys::{Function, Object, Reflect};
use uuid::Uuid;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TooltipDirection {
    Above,
    Below,
    Left,
    Right,
}

impl TooltipDirection {
    fn as_attr(self) -> &'static str {
        match self {
            TooltipDirection::Above => "above",
            TooltipDirection::Below => "below",
            TooltipDirection::Left => "left",
            TooltipDirection::Right => "right",
        }
    }
}

#[component]
pub fn Tooltip(
    content: Element,
    #[props(default = TooltipDirection::Above)] direction: TooltipDirection,
    #[props(optional)] trigger_class: Option<String>,
    #[props(optional)] id: Option<String>,
    #[props(extends = GlobalAttributes)]
    #[props(extends = span)]
    attributes: Vec<Attribute>,
    children: Element,
) -> Element {
    let id_value = id.clone();
    let base_id = use_signal(move || {
        id_value.unwrap_or_else(|| format!("tooltip-{}", Uuid::new_v4().as_simple()))
    });
    let anchor_id = format!("{}-anchor", base_id());
    let popover_id = format!("{}-popover", base_id());

    let base_class = "tooltip-anchor inline-flex items-center justify-center";
    let anchor_class = match trigger_class {
        Some(extra) if !extra.trim().is_empty() => format!("{base_class} {extra}"),
        _ => base_class.to_string(),
    };

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

    let final_anchor_class = if collected_classes.is_empty() {
        anchor_class
    } else {
        format!("{anchor_class} {collected_classes}")
    };

    let direction_attr = direction.as_attr();

    let mut anchor_ref = use_signal(|| None::<web_sys::HtmlElement>);
    let mut popover_ref = use_signal(|| None::<web_sys::HtmlElement>);

    let show_mouse = move |_evt: Event<MouseData>| {
        if let (Some(anchor), Some(popover)) = (anchor_ref(), popover_ref()) {
            show_hint_popover(&popover, &anchor);
        }
    };

    let show_focus = move |_evt: Event<dioxus::html::events::FocusData>| {
        if let (Some(anchor), Some(popover)) = (anchor_ref(), popover_ref()) {
            show_hint_popover(&popover, &anchor);
        }
    };

    let hide_mouse = move |_evt: Event<MouseData>| {
        if let Some(popover) = popover_ref() {
            let _ = popover.hide_popover();
        }
    };

    let hide_focus = move |_evt: Event<dioxus::html::events::FocusData>| {
        if let Some(popover) = popover_ref() {
            let _ = popover.hide_popover();
        }
    };

    rsx! {
        Fragment {
            document::Link { rel: "stylesheet", href: asset!("./style.css") }
            span {
                id: "{anchor_id}",
                class: "{final_anchor_class}",
                tabindex: "0",
                "aria-describedby": "{popover_id}",
                onmounted: move |evt| {
                    if let Some(element) = evt.data().downcast::<web_sys::Element>() {
                        if let Ok(html_element) =
                            element.clone().dyn_into::<web_sys::HtmlElement>()
                        {
                            anchor_ref.set(Some(html_element));
                        }
                    }
                },
                onmouseenter: show_mouse,
                onmouseleave: hide_mouse,
                onfocus: show_focus,
                onblur: hide_focus,
                ..filtered_attributes,
                {children}
            }
            div {
                id: "{popover_id}",
                class: "tooltip-popover",
                popover: "hint",
                "data-direction": "{direction_attr}",
                role: "tooltip",
                onmounted: move |evt| {
                    if let Some(element) = evt.data().downcast::<web_sys::Element>() {
                        if let Ok(html_element) =
                            element.clone().dyn_into::<web_sys::HtmlElement>()
                        {
                            popover_ref.set(Some(html_element));
                        }
                    }
                },
                {content}
            }
        }
    }
}

fn show_hint_popover(popover: &web_sys::HtmlElement, anchor: &web_sys::HtmlElement) {
    // wasm-bindgen does not support show_popover_with_options yet, so we have
    // to do things the hard way!

    // popover.show_popover_with_options(...);

    let options = Object::new();
    let anchor_js = JsValue::from(anchor.clone());
    let _ = Reflect::set(&options, &JsValue::from_str("source"), &anchor_js);
    let options_js = JsValue::from(options);

    if let Ok(show_fn_value) = Reflect::get(popover.as_ref(), &JsValue::from_str("showPopover")) {
        if let Ok(show_fn) = show_fn_value.dyn_into::<Function>() {
            let _ = show_fn.call1(popover.as_ref(), &options_js);
            return;
        }
    }
}
