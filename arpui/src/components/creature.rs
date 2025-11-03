use arptypes::{ClassID, Condition, Creature};
use dioxus::prelude::*;

use crate::player_view::GAME;

#[component]
pub fn CreatureCard(creature: Creature) -> Element {
    let mut condition_icons: Vec<(String, String)> = creature
        .conditions
        .iter()
        .map(|(id, applied)| (id.to_string(), condition_icon(&applied.condition)))
        .collect();
    condition_icons.sort_by(|a, b| a.0.cmp(&b.0));

    rsx! {
        div {
            class: "bg-white rounded-lg shadow-md p-4 w-full border border-gray-200",
            div {
                class: "flex justify-between",
                div {
                    class: "flex gap-3",
                    CreatureIcon { creature: creature.clone(), size: 80 }
                    div {
                        div {
                            class: "flex items-center gap-2",
                            h3 {
                                class: "text-lg font-semibold text-gray-900",
                                "{creature.name}"
                            }
                            ClassIcon { class_id: creature.class }
                        }
                        if !condition_icons.is_empty() {
                            div {
                                class: "flex flex-wrap items-center gap-2 mt-1 text-xl leading-none",
                                for (key, icon) in condition_icons.iter() {
                                    span {
                                        key: "{key}",
                                        class: "inline-flex items-center justify-center",
                                        "{icon}"
                                    }
                                }
                            }
                        }
                    }
                }
                // Menu will go here later
            }
        }
    }
}

#[component]
pub fn CreatureIcon(creature: Creature, size: Option<u32>) -> Element {
    let size = size.unwrap_or(50);
    let game = GAME.read();
    let class_color = game.classes.get(&creature.class).map(|c| c.color.clone());

    if !creature.icon_url.is_empty() {
        rsx! {
            SquareImageIcon {
                url: creature.icon_url.clone(),
                size
            }
        }
    } else {
        let color = class_color.unwrap_or_else(|| "red".to_string());
        rsx! {
            div {
                class: "rounded-lg border border-black flex items-center justify-center text-white font-semibold",
                style: "width: {size}px; height: {size}px; background-color: {color};",
                "{creature.name}"
            }
        }
    }
}

#[component]
pub fn SquareImageIcon(url: String, size: Option<u32>) -> Element {
    let size = size.unwrap_or(50);
    rsx! {
        img {
            src: url,
            class: "rounded-lg border border-black object-cover",
            style: "width: {size}px; height: {size}px;",
        }
    }
}

#[component]
pub fn ClassIcon(class_id: ClassID) -> Element {
    let game = GAME.read();
    let class = game.classes.get(&class_id);

    if let Some(class) = class {
        let emoji = class.emoji.as_deref().unwrap_or("🧑‍🎓");
        rsx! {
            span {
                class: "text-xl",
                title: "{class.name}",
                "{emoji}"
            }
        }
    } else {
        rsx! { span {} }
    }
}

fn condition_icon(condition: &Condition) -> String {
    match condition {
        Condition::Dead => "💀".to_string(),
        Condition::Incapacitated => "😞".to_string(),
        Condition::DoubleMaxMovement => "🏃".to_string(),
        Condition::RecurringEffect(_) => "🔁".to_string(),
        Condition::AddDamageBuff(_) => "😈".to_string(),
        Condition::ActivateAbility(ability_id) => format!("Ability Activated: {}", ability_id),
    }
}
