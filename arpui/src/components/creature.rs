use arptypes::{
    AbilityID, Action, AppliedCondition, ClassID, Condition, Creature, CreatureEffect, 
    CreatureTarget, Dice, Duration, SceneEffect, SceneTarget, Volume,
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

fn condition_icon(condition: &Condition) -> &'static str {
    match condition {
        Condition::Dead => "💀",
        Condition::Incapacitated => "😞",
        Condition::DoubleMaxMovement => "🏃",
        Condition::RecurringEffect(_) => "🔁",
        Condition::AddDamageBuff(_) => "😈",
        Condition::ActivateAbility(_) => "💪",
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
        lines.push((indent + 1, format!("Energy cost: {}", ability.cost.0)));
        lines.push((
            indent + 1,
            format!(
                "Usable outside combat: {}",
                if ability.usable_ooc { "Yes" } else { "No" }
            ),
        ));
        lines.push((indent + 1, "Action details:".to_string()));
        append_action_details(&ability.action, indent + 2, lines);
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

fn append_action_details(action: &Action, indent: usize, lines: &mut Vec<(usize, String)>) {
    match action {
        Action::Creature { effect, target } => {
            lines.push((indent, "Targets creatures:".to_string()));
            append_creature_target_details(target, indent + 1, lines);
            lines.push((indent, "Effect:".to_string()));
            append_effect_details(effect, indent + 1, lines);
        }
        Action::SceneVolume { effect, target } => {
            lines.push((indent, "Targets scene volume:".to_string()));
            append_scene_target_details(target, indent + 1, lines);
            lines.push((indent, "Scene effect:".to_string()));
            append_scene_effect_details(effect, indent + 1, lines);
        }
    }
}

fn append_creature_target_details(
    target: &CreatureTarget,
    indent: usize,
    lines: &mut Vec<(usize, String)>,
) {
    match target {
        CreatureTarget::Melee => {
            lines.push((indent, "Melee range target".to_string()));
        }
        CreatureTarget::Range(distance) => {
            lines.push((indent, format!("Ranged target ({}cm range)", distance.get::<uom::si::length::centimeter>())));
        }
        CreatureTarget::Actor => {
            lines.push((indent, "Self-targeting".to_string()));
        }
        CreatureTarget::LineFromActor { distance } => {
            lines.push((indent, format!("Piercing line from actor ({}cm length)", distance.get::<uom::si::length::centimeter>())));
        }
        CreatureTarget::SomeCreaturesInVolumeInRange { volume, maximum, range } => {
            lines.push((indent, format!("Up to {} creatures in volume within {}cm:", maximum, range.get::<uom::si::length::centimeter>())));
            append_volume_details(volume, indent + 1, lines);
        }
        CreatureTarget::AllCreaturesInVolumeInRange { volume, range } => {
            lines.push((indent, format!("All creatures in volume within {}cm:", range.get::<uom::si::length::centimeter>())));
            append_volume_details(volume, indent + 1, lines);
        }
    }
}

fn append_scene_target_details(
    target: &SceneTarget,
    indent: usize,
    lines: &mut Vec<(usize, String)>,
) {
    match target {
        SceneTarget::RangedVolume { volume, range } => {
            lines.push((indent, format!("Volume within {}cm range:", range.get::<uom::si::length::centimeter>())));
            append_volume_details(volume, indent + 1, lines);
        }
    }
}

fn append_scene_effect_details(
    effect: &SceneEffect,
    indent: usize,
    lines: &mut Vec<(usize, String)>,
) {
    match effect {
        SceneEffect::CreateVolumeCondition { duration, condition } => {
            lines.push((indent, format!("Creates volume condition for {}:", format_duration(*duration))));
            append_condition_details(condition, indent + 1, lines);
        }
    }
}

fn append_volume_details(volume: &Volume, indent: usize, lines: &mut Vec<(usize, String)>) {
    match volume {
        Volume::Sphere(radius) => {
            lines.push((indent, format!("Sphere ({}cm radius)", radius.get::<uom::si::length::centimeter>())));
        }
        Volume::Line { vector } => {
            lines.push((indent, format!("Line to point ({}, {}, {})", vector.x_cm(), vector.y_cm(), vector.z_cm())));
        }
        Volume::VerticalCylinder { radius, height } => {
            lines.push((indent, format!("Vertical cylinder ({}cm radius, {}cm height)", radius.get::<uom::si::length::centimeter>(), height.get::<uom::si::length::centimeter>())));
        }
        Volume::AABB(aabb) => {
            lines.push((indent, format!("Rectangular area ({}cm × {}cm × {}cm)", aabb.x_cm(), aabb.y_cm(), aabb.z_cm())));
        }
    }
}

fn symbol_for_indent(indent: usize) -> &'static str {
    if indent == 0 {
        ""
    } else {
        "• "
    }
}
