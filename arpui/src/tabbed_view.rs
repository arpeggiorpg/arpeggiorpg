use dioxus::prelude::*;

use crate::components::toolbar::{Toolbar, ToolbarButton};

#[derive(Clone, Copy)]
pub struct TabDefinition {
    pub key: &'static str,
    pub label: &'static str,
}

impl TabDefinition {
    pub const fn new(key: &'static str, label: &'static str) -> Self {
        Self { key, label }
    }
}

pub trait TabbedContent: Clone {
    fn key(&self) -> &'static str;
    fn definitions() -> Vec<TabDefinition>
    where
        Self: Sized;

    fn switch(position: usize) -> Option<Self>
    where
        Self: Sized;

    fn render(&self) -> Element;
}

#[component]
pub fn TabbedView<T>(tab: Signal<T>, #[props(default)] aria_label: Option<String>) -> Element
where
    T: TabbedContent + 'static,
{
    let definitions = T::definitions();

    let current = tab();
    let current_position = definitions
        .iter()
        .position(|d| d.key == current.key())
        .unwrap();
    let content = current.render();

    let buttons: Vec<_> = definitions
        .iter()
        .enumerate()
        .map(|(index, definition)| {
            let key = definition.key;
            let label = definition.label;
            let aria_pressed = (current_position == index).to_string();
            rsx! {
              ToolbarButton {
                index,
                key: "{key}",
                aria_pressed,
                on_click: move |_| {
                  if let Some(next) = T::switch(index) {
                    tab.set(next);
                  }
                },
                "{label}"
              }
            }
        })
        .collect();

    rsx! {
      div { class: "tabbed-view",
        Toolbar {
          aria_label,
          {buttons.into_iter()}
        }
        div { class: "tabbed-view__content", {content} }
      }
    }
}
