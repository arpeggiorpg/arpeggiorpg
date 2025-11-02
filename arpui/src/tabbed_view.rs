use std::rc::Rc;

use dioxus::prelude::*;

use crate::components::toolbar::{Toolbar, ToolbarButton};

#[derive(Clone)]
pub struct TabDefinition {
    key: String,
    label: String,

    // Not totally sure about this but it seems like it should be fine to ensure we lazily evaluate
    // the tab contents. I think this means if you have a dynamic closure it will not update
    // properly.
    render: Rc<dyn Fn() -> Element>,
}

impl TabDefinition {
    pub fn new<F>(key: impl Into<String>, label: impl Into<String>, render: F) -> Self
    where
        F: Fn() -> Element + 'static,
    {
        Self {
            key: key.into(),
            label: label.into(),
            render: Rc::new(render),
        }
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn render(&self) -> Element {
        (self.render)()
    }
}

impl PartialEq for TabDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.label == other.label
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TabbedViewProps {
    pub tabs: Vec<TabDefinition>,
    #[props(default = 0)]
    pub initial_active: usize,
    #[props(default)]
    pub aria_label: Option<String>,
}

#[component]
pub fn TabbedView(props: TabbedViewProps) -> Element {
    let tab_count = props.tabs.len();
    let initial_index = if tab_count == 0 {
        0
    } else {
        props.initial_active.min(tab_count.saturating_sub(1))
    };
    let mut active_index = use_signal(|| initial_index);

    use_effect(move || {
        if tab_count == 0 {
            active_index.set(0);
        } else if active_index() >= tab_count {
            active_index.set(tab_count.saturating_sub(1));
        }
    });

    let tabs = props.tabs.clone();
    let current_index = active_index();
    let current_content = tabs
        .get(current_index)
        .map(|tab| tab.render())
        .unwrap_or_else(|| rsx!(div { "No tabs available." }).into());

    rsx! {
      div { class: "tabbed-view",
        Toolbar {
          aria_label: props.aria_label,
          children: {
            let mut active_signal = active_index;
            rsx! {
              for (idx, tab) in tabs.iter().enumerate() {
                ToolbarButton {
                  index: idx,
                  on_click: move |_| {
                    active_signal.set(idx);
                  },
                  aria_pressed: (current_index == idx).to_string(),
                  "{tab.label()}"
                }
              }
            }
            .into()
          }
        }
        div { class: "tabbed-view__content", {current_content} }
      }
    }
}
