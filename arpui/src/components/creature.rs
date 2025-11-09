use arptypes::{
    AbilityID, AppliedCondition, ClassID, Condition, Creature, CreatureEffect, Dice, Duration,
};
use dioxus::prelude::*;

use crate::components::tooltip::{Tooltip, TooltipDirection};
use crate::player_view::GAME;

#[component]
pub fn CreatureCard(creature: Creature) -> Element {
    let mut conditions: Vec<(String, AppliedCondition)> = creature
        .conditions
        .iter()
        .map(|(id, applied)| (id.to_string(), applied.clone()))
        .collect();
    // TODO: we're sorting by a UUID here which is dumb. I guess we should have a timestamp
    // associated with each applied condition?
    conditions.sort_by(|a, b| a.0.cmp(&b.0));

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
                        if !conditions.is_empty() {
                            div {
                                class: "flex flex-wrap items-center gap-2 mt-1 text-xl leading-none",
                                for (key, applied) in conditions.iter() {
                                    Tooltip {
                                        key: "{key}",
                                        direction: TooltipDirection::Left,
                                        role: "img",
                                        content: rsx! {
                                            ConditionTooltip {
                                                applied: applied.clone(),
                                            }
                                        },
                                        "{condition_icon(&applied.condition)}"
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

#[component]
fn ConditionTooltip(applied: AppliedCondition) -> Element {
    let details = condition_detail_lines(&applied.condition);
    let detail_rows: Vec<(String, String)> = details
        .into_iter()
        .map(|(indent, text)| {
            (
                format!("margin-left: {}px;", indent * 12),
                format!("{}{}", symbol_for_indent(indent), text),
            )
        })
        .collect();
    let duration = format_duration(applied.remaining);

    rsx! {
        div {
            class: "w-96 space-y-1 text-sm text-gray-900",
            div { class: "font-semibold", "{condition_name(&applied.condition)}" }
            div { class: "text-xs text-gray-600", "Remaining: {duration}" }
            if !detail_rows.is_empty() {
                for (index, (style, line)) in detail_rows.iter().enumerate() {
                    div {
                        key: "{index}",
                        class: "text-xs text-gray-800",
                        style: "{style}",
                        "{line}"
                    }
                }
            }
        }
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

fn condition_name(condition: &Condition) -> &'static str {
    match condition {
        Condition::Dead => "Dead",
        Condition::Incapacitated => "Incapacitated",
        Condition::DoubleMaxMovement => "Double Max Movement",
        Condition::RecurringEffect(_) => "Recurring Effect",
        Condition::AddDamageBuff(_) => "Damage Buff",
        Condition::ActivateAbility(_) => "Activate Ability",
    }
}

fn format_duration(duration: Duration) -> String {
    match duration {
        Duration::Interminate => "Indefinite".to_string(),
        Duration::Rounds(1) => "1 round".to_string(),
        Duration::Rounds(rounds) => format!("{rounds} rounds"),
    }
}

fn condition_detail_lines(condition: &Condition) -> Vec<(usize, String)> {
    let mut lines = Vec::new();
    append_condition_details(condition, 0, &mut lines);
    lines
}

fn append_condition_details(
    condition: &Condition,
    indent: usize,
    lines: &mut Vec<(usize, String)>,
) {
    match condition {
        Condition::Dead => {
            lines.push((indent, "This creature has died.".to_string()));
        }
        Condition::Incapacitated => {
            lines.push((
                indent,
                "The creature is incapacitated and cannot act or move.".to_string(),
            ));
        }
        Condition::AddDamageBuff(amount) => {
            lines.push((
                indent,
                format!(
                    "Grants +{} damage to the creature's damaging actions.",
                    amount.0
                ),
            ));
        }
        Condition::DoubleMaxMovement => {
            lines.push((
                indent,
                "Doubles the creature's maximum movement distance.".to_string(),
            ));
        }
        Condition::RecurringEffect(effect) => {
            lines.push((
                indent,
                "Applies an effect at the start of each turn:".to_string(),
            ));
            append_effect_details(effect, indent + 1, lines);
        }
        Condition::ActivateAbility(ability_id) => {
            append_activate_ability_details(ability_id, indent, lines);
        }
    }
}

fn append_activate_ability_details(
    ability_id: &AbilityID,
    indent: usize,
    lines: &mut Vec<(usize, String)>,
) {
    let game = GAME.read();
    if let Some(ability) = game.abilities.get(ability_id) {
        lines.push((indent, format!("Activates ability: {}", ability.name)));
        lines.push((indent + 1, format!("Ability ID: {}", ability.id)));
        lines.push((indent + 1, format!("Energy cost: {}", ability.cost.0)));
        lines.push((
            indent + 1,
            format!(
                "Usable outside combat: {}",
                if ability.usable_ooc { "Yes" } else { "No" }
            ),
        ));
    } else {
        lines.push((
            indent,
            format!(
                "Activates ability with ID {} (ability details unavailable).",
                ability_id
            ),
        ));
    }
}

fn append_effect_details(effect: &CreatureEffect, indent: usize, lines: &mut Vec<(usize, String)>) {
    match effect {
        CreatureEffect::ApplyCondition(duration, condition) => {
            lines.push((
                indent,
                format!(
                    "Applies condition `{}` for {}.",
                    condition_name(condition),
                    format_duration(*duration)
                ),
            ));
            append_condition_details(condition, indent + 1, lines);
        }
        CreatureEffect::Heal(dice) => {
            lines.push((indent, format!("Heals for {}.", format_dice(dice))));
        }
        CreatureEffect::Damage(dice) => {
            lines.push((indent, format!("Deals {} damage.", format_dice(dice))));
        }
        CreatureEffect::MultiEffect(effects) => {
            lines.push((indent, "Executes multiple effects:".to_string()));
            for effect in effects {
                append_effect_details(effect, indent + 1, lines);
            }
        }
        CreatureEffect::GenerateEnergy(energy) => {
            lines.push((indent, format!("Generates {} energy.", energy.0)));
        }
    }
}

fn format_dice(dice: &Dice) -> String {
    match dice {
        Dice::Expr { num, size } => format!("{num}d{size}"),
        Dice::Plus(left, right) => format!("{} + {}", format_dice(left), format_dice(right)),
        Dice::Flat { value } => value.to_string(),
        Dice::BestOf(count, value) => format!("best of {count} {}", format_dice(value)),
    }
}

fn symbol_for_indent(indent: usize) -> &'static str {
    if indent == 0 {
        ""
    } else {
        "• "
    }
}
