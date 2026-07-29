use arptypes::{
    CombatLog, CreatureID, CreatureLog, Game, GameLog, InventoryOwner, ResourceRef,
    protocol::{GameIndex, GameRequest},
};
use dioxus::prelude::*;

use crate::{
    GAME_LOGS,
    components::{
        button::{Button, ButtonVariant},
        modal::Modal,
    },
    rpi::{send_request, use_ws},
};

#[derive(Clone, PartialEq)]
struct RollbackTarget {
    game_index: GameIndex,
    event: String,
}

#[component]
pub fn HistoryPanel(game: Game) -> Element {
    let ws = use_ws();
    let logs = GAME_LOGS.read();
    let mut rollback_target = use_signal(|| None::<RollbackTarget>);
    let mut attempted_target = use_signal(|| None::<GameIndex>);

    let mut rollback_action = use_action(move |game_index: GameIndex| async move {
        let _: Vec<GameLog> = send_request(GameRequest::GMRollback { game_index }, ws).await?;
        rollback_target.set(None);
        Ok::<(), anyhow::Error>(())
    });

    rsx! {
        div {
            class: "flex h-full min-h-0 flex-col",
            div {
                class: "flex shrink-0 items-center justify-between gap-3 border-b border-gray-200 px-4 py-3",
                p {
                    class: "text-xs text-gray-500",
                    "Most recent first. Restore returns the game to immediately after an event."
                }
                span {
                    class: "shrink-0 rounded-full bg-gray-100 px-2 py-0.5 text-xs font-medium text-gray-600",
                    "{logs.len()} events"
                }
            }

            div {
                class: "min-h-0 flex-1 overflow-y-auto",
                if logs.is_empty() {
                    div {
                        class: "flex h-32 items-center justify-center px-4 text-center text-sm italic text-gray-500",
                        "No recent history."
                    }
                } else {
                    ol {
                        class: "divide-y divide-gray-200",
                        for (index, log) in logs.iter().rev() {
                            {
                                let text = describe_game_log(&game, log);
                                let target_index = rollback_index_after(*index);
                                let target = RollbackTarget {
                                    game_index: target_index,
                                    event: text.clone(),
                                };

                                rsx! {
                                    li {
                                        key: "{index.game_idx}-{index.log_idx}",
                                        class: "group flex items-start gap-3 px-4 py-3 hover:bg-gray-50",
                                        div {
                                            class: "min-w-0 flex-1",
                                            p {
                                                class: "text-sm leading-5 text-gray-800",
                                                "{text}"
                                            }
                                            p {
                                                class: "mt-0.5 font-mono text-[11px] text-gray-400",
                                                "Snapshot {index.game_idx}, event {index.log_idx}"
                                            }
                                        }
                                        button {
                                            r#type: "button",
                                            class: "shrink-0 rounded px-2 py-1 text-xs font-medium text-gray-500 opacity-60 transition hover:bg-gray-200 hover:text-gray-800 hover:opacity-100 focus:opacity-100 group-hover:opacity-100 disabled:cursor-not-allowed disabled:opacity-40",
                                            disabled: rollback_action.pending(),
                                            onclick: move |_| {
                                                attempted_target.set(None);
                                                rollback_target.set(Some(target.clone()));
                                            },
                                            "Restore"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if let Some(target) = rollback_target() {
                Modal {
                    open: true,
                    on_close: move |_| {
                        if !rollback_action.pending() {
                            rollback_target.set(None);
                        }
                    },
                    class: "p-6",
                    h3 {
                        class: "text-lg font-semibold text-gray-900",
                        "Restore game to this point?"
                    }
                    p {
                        class: "mt-2 text-sm text-gray-700",
                        "The game will return to immediately after:"
                    }
                    p {
                        class: "mt-2 rounded-md border border-gray-200 bg-gray-50 p-3 text-sm text-gray-800",
                        "{target.event}"
                    }
                    p {
                        class: "mt-3 text-xs text-gray-500",
                        "Existing history is retained, and play continues from a new snapshot."
                    }

                    if attempted_target() == Some(target.game_index) {
                        if let Some(Err(err)) = rollback_action.value() {
                            p {
                                class: "mt-3 text-sm text-red-600",
                                "Restore failed: {err}"
                            }
                        }
                    }

                    div {
                        class: "mt-5 flex justify-end gap-2",
                        Button {
                            variant: ButtonVariant::Ghost,
                            disabled: rollback_action.pending(),
                            onclick: move |_| rollback_target.set(None),
                            "Cancel"
                        }
                        Button {
                            variant: ButtonVariant::Destructive,
                            disabled: rollback_action.pending(),
                            onclick: move |_| {
                                attempted_target.set(Some(target.game_index));
                                rollback_action.call(target.game_index);
                            },
                            if rollback_action.pending() {
                                "Restoring..."
                            } else {
                                "Restore game"
                            }
                        }
                    }
                }
            }
        }
    }
}

fn rollback_index_after(index: GameIndex) -> GameIndex {
    GameIndex {
        game_idx: index.game_idx,
        log_idx: index.log_idx + 1,
    }
}

fn describe_game_log(game: &Game, log: &GameLog) -> String {
    match log {
        GameLog::SetActiveScene { id } => match id {
            Some(id) => format!("Set {} as the active scene", scene_name(game, *id)),
            None => "Cleared the active scene".to_string(),
        },
        GameLog::RegisterPlayer { id } => format!("Registered player {id}"),
        GameLog::GiveCreaturesToPlayer {
            player_id,
            creature_ids,
        } => format!("Gave {} to {player_id}", creature_list(game, creature_ids)),
        GameLog::UnregisterPlayer { id } => format!("Unregistered player {id}"),
        GameLog::RemoveCreaturesFromPlayer {
            player_id,
            creature_ids,
        } => format!(
            "Removed {} from {player_id}",
            creature_list(game, creature_ids)
        ),
        GameLog::SetPlayerScene {
            player_id,
            scene_id,
        } => match scene_id {
            Some(scene_id) => {
                format!("Moved {player_id} to {}", scene_name(game, *scene_id))
            }
            None => format!("Removed {player_id} from their scene"),
        },
        GameLog::ChatFromGM { message } => format!("GM: {message}"),
        GameLog::ChatFromPlayer { player_id, message } => {
            format!("{player_id}: {message}")
        }
        GameLog::AttributeCheckResult {
            creature_id,
            attribute_check,
            actual,
            success,
        } => format!(
            "{} {} an {} check with {actual}",
            creature_name(game, *creature_id),
            if *success { "passed" } else { "failed" },
            attribute_check.attr.0
        ),
        GameLog::CreateCollection { collection } => {
            format!("Created collection {}", collection.name)
        }
        GameLog::EditCollection { collection } => {
            format!("Edited collection {}", collection.name)
        }
        GameLog::RenameCollection {
            collection_id,
            name,
        } => format!(
            "Renamed collection {} to {name}",
            collection_name(game, *collection_id)
        ),
        GameLog::AddResourcesToCollection {
            collection_id,
            resources,
        } => format!(
            "Added {} to collection {}",
            resource_count(resources),
            collection_name(game, *collection_id)
        ),
        GameLog::RemoveResourcesFromCollection {
            collection_id,
            resources,
        } => format!(
            "Removed {} from collection {}",
            resource_count(resources),
            collection_name(game, *collection_id)
        ),
        GameLog::MergeCollections {
            destination_id,
            source_ids,
        } => format!(
            "Merged {} into collection {}",
            plural(source_ids.len(), "collection", "collections"),
            collection_name(game, *destination_id)
        ),
        GameLog::DeleteCollection { collection_id } => {
            format!(
                "Deleted collection {}",
                collection_name(game, *collection_id)
            )
        }
        GameLog::DeleteResource { resource } => {
            format!("Deleted {}", resource_name(game, *resource))
        }
        GameLog::RenameResource { resource, new_name } => {
            format!("Renamed {} to {new_name}", resource_name(game, *resource))
        }
        GameLog::CopyResource {
            source,
            destination,
        } => format!(
            "Copied {} as {}",
            resource_name(game, *source),
            resource_name(game, *destination)
        ),
        GameLog::CreateItem { item } => format!("Created item {}", item.name),
        GameLog::EditItem { item } => format!("Edited item {}", item.name),
        GameLog::CreateNote { note } => format!("Created note {}", note.name),
        GameLog::EditNote { note } => format!("Edited note {}", note.name),
        GameLog::TransferItem {
            from,
            to,
            item_id,
            count,
        } => format!(
            "Transferred {count} × {} from {} to {}",
            item_name(game, *item_id),
            inventory_owner_name(game, from),
            inventory_owner_name(game, to)
        ),
        GameLog::RemoveItem {
            owner,
            item_id,
            count,
        } => format!(
            "Removed {count} × {} from {}",
            item_name(game, *item_id),
            inventory_owner_name(game, owner)
        ),
        GameLog::SetItemCount {
            owner,
            item_id,
            count,
        } => format!(
            "Set {} count to {count} for {}",
            item_name(game, *item_id),
            inventory_owner_name(game, owner)
        ),
        GameLog::CreateScene { scene } => format!("Created scene {}", scene.name),
        GameLog::EditSceneDetails { details, .. } => {
            format!("Edited scene details for {}", details.name)
        }
        GameLog::SetSceneCreatureVisibility {
            scene_id,
            creature_id,
            visibility,
        } => format!(
            "Set {} visibility in {} to {visibility:?}",
            creature_name(game, *creature_id),
            scene_name(game, *scene_id)
        ),
        GameLog::AddCreatureToScene {
            scene_id,
            creature_id,
            ..
        } => format!(
            "Added {} to {}",
            creature_name(game, *creature_id),
            scene_name(game, *scene_id)
        ),
        GameLog::RemoveCreatureFromScene {
            scene_id,
            creature_id,
        } => format!(
            "Removed {} from {}",
            creature_name(game, *creature_id),
            scene_name(game, *scene_id)
        ),
        GameLog::AddSceneChallenge {
            scene_id,
            description,
            ..
        } => format!(
            "Added challenge “{description}” to {}",
            scene_name(game, *scene_id)
        ),
        GameLog::RemoveSceneChallenge {
            scene_id,
            description,
        } => format!(
            "Removed challenge “{description}” from {}",
            scene_name(game, *scene_id)
        ),
        GameLog::SetFocusedSceneCreatures {
            scene_id,
            creatures,
        } => format!(
            "Focused {} in {}",
            creature_list(game, creatures),
            scene_name(game, *scene_id)
        ),
        GameLog::RemoveSceneVolumeCondition { scene_id, .. } => {
            format!(
                "Removed a volume condition from {}",
                scene_name(game, *scene_id)
            )
        }
        GameLog::EditSceneTerrain { scene_id, .. } => {
            format!("Edited terrain in {}", scene_name(game, *scene_id))
        }
        GameLog::EditSceneHighlights { scene_id, .. } => {
            format!("Edited highlights in {}", scene_name(game, *scene_id))
        }
        GameLog::EditSceneAnnotations { scene_id, .. } => {
            format!("Edited annotations in {}", scene_name(game, *scene_id))
        }
        GameLog::EditSceneRelatedScenes { scene_id, .. } => {
            format!("Edited related scenes for {}", scene_name(game, *scene_id))
        }
        GameLog::EditSceneSceneHotspots { scene_id, .. } => {
            format!("Edited scene links in {}", scene_name(game, *scene_id))
        }
        GameLog::CombatLog { log } => describe_combat_log(game, log),
        GameLog::CreatureLog { creature_id, log } => describe_creature_log(game, *creature_id, log),
        GameLog::SetCreaturePos {
            scene_id,
            creature_id,
            pos,
        } => format!(
            "Moved {} to {pos} in {}",
            creature_name(game, *creature_id),
            scene_name(game, *scene_id)
        ),
        GameLog::PathCreature {
            scene_id,
            creature_id,
            path,
        } => match path.last() {
            Some(destination) => format!(
                "Moved {} to {destination} in {}",
                creature_name(game, *creature_id),
                scene_name(game, *scene_id)
            ),
            None => format!(
                "Moved {} in {}",
                creature_name(game, *creature_id),
                scene_name(game, *scene_id)
            ),
        },
        GameLog::AddVolumeCondition { scene_id, .. } => {
            format!(
                "Added a volume condition to {}",
                scene_name(game, *scene_id)
            )
        }
        GameLog::StartCombat {
            scene_id,
            combatants,
        } => format!(
            "Started combat in {} with {}",
            scene_name(game, *scene_id),
            plural(combatants.len(), "combatant", "combatants")
        ),
        GameLog::StopCombat => "Stopped combat".to_string(),
        GameLog::CreateClass { class } => format!("Created class {}", class.name),
        GameLog::EditClass { class } => format!("Edited class {}", class.name),
        GameLog::CreateAbility { ability } => format!("Created ability {}", ability.name),
        GameLog::EditAbility { ability } => format!("Edited ability {}", ability.name),
        GameLog::CreateCreature { creature } => {
            format!("Created creature {}", creature.name)
        }
        GameLog::EditCreatureDetails { details, .. } => {
            format!("Edited creature {}", details.name)
        }
        GameLog::EditCreature { creature } => format!("Edited creature {}", creature.name),
        GameLog::AddCreatureToCombat {
            creature_id,
            initiative,
        } => format!(
            "Added {} to combat at initiative {initiative}",
            creature_name(game, *creature_id)
        ),
        GameLog::RemoveCreatureFromCombat { creature_id } => {
            format!("Removed {} from combat", creature_name(game, *creature_id))
        }
    }
}

fn describe_combat_log(game: &Game, log: &CombatLog) -> String {
    match log {
        CombatLog::ConsumeMovement { .. } => "Used combat movement".to_string(),
        CombatLog::ChangeCreatureInitiative {
            creature_id,
            initiative,
        } => format!(
            "Changed {} initiative to {initiative}",
            creature_name(game, *creature_id)
        ),
        CombatLog::EndTurn { creature_id } => {
            format!("Ended {}’s turn", creature_name(game, *creature_id))
        }
        CombatLog::ForceNextTurn => "Moved combat to the next turn".to_string(),
        CombatLog::ForcePrevTurn => "Moved combat to the previous turn".to_string(),
        CombatLog::RerollInitiative { combatants } => format!(
            "Rerolled initiative for {}",
            plural(combatants.len(), "combatant", "combatants")
        ),
    }
}

fn describe_creature_log(game: &Game, creature_id: CreatureID, log: &CreatureLog) -> String {
    let name = creature_name(game, creature_id);
    match log {
        CreatureLog::Damage { hp, rolls } => {
            format!("{name} took {} damage (rolls: {rolls:?})", hp.0)
        }
        CreatureLog::Heal { hp, rolls } => {
            format!("{name} healed {} HP (rolls: {rolls:?})", hp.0)
        }
        CreatureLog::GenerateEnergy { energy } => {
            format!("{name} gained {} energy", energy.0)
        }
        CreatureLog::ReduceEnergy { energy } => {
            format!("{name} lost {} energy", energy.0)
        }
        CreatureLog::ApplyCondition {
            condition,
            duration,
            ..
        } => format!("{name} gained {condition:?} for {duration:?}"),
        CreatureLog::DecrementConditionRemaining { .. } => {
            format!("Decremented a condition on {name}")
        }
        CreatureLog::RemoveCondition { .. } => format!("Removed a condition from {name}"),
    }
}

fn scene_name(game: &Game, id: arptypes::SceneID) -> String {
    game.scenes
        .get(&id)
        .map(|scene| scene.name.clone())
        .unwrap_or_else(|| id.to_string())
}

fn creature_name(game: &Game, id: CreatureID) -> String {
    game.creatures
        .get(&id)
        .map(|creature| creature.name.clone())
        .unwrap_or_else(|| id.to_string())
}

fn collection_name(game: &Game, id: arptypes::CollectionID) -> String {
    game.collections
        .get(&id)
        .map(|collection| collection.name.clone())
        .unwrap_or_else(|| id.to_string())
}

fn item_name(game: &Game, id: arptypes::ItemID) -> String {
    game.items
        .get(&id)
        .map(|item| item.name.clone())
        .unwrap_or_else(|| id.to_string())
}

fn resource_name(game: &Game, resource: ResourceRef) -> String {
    match resource {
        ResourceRef::Scene(id) => format!("scene {}", scene_name(game, id)),
        ResourceRef::Creature(id) => format!("creature {}", creature_name(game, id)),
        ResourceRef::Note(id) => format!(
            "note {}",
            game.notes
                .get(&id)
                .map(|note| note.name.clone())
                .unwrap_or_else(|| id.to_string())
        ),
        ResourceRef::Item(id) => format!("item {}", item_name(game, id)),
        ResourceRef::Ability(id) => format!(
            "ability {}",
            game.abilities
                .get(&id)
                .map(|ability| ability.name.clone())
                .unwrap_or_else(|| id.to_string())
        ),
        ResourceRef::Class(id) => format!(
            "class {}",
            game.classes
                .get(&id)
                .map(|class| class.name.clone())
                .unwrap_or_else(|| id.to_string())
        ),
    }
}

fn inventory_owner_name(game: &Game, owner: &InventoryOwner) -> String {
    match owner {
        InventoryOwner::Scene(id) => format!("scene {}", scene_name(game, *id)),
        InventoryOwner::Creature(id) => format!("creature {}", creature_name(game, *id)),
    }
}

fn creature_list(game: &Game, ids: &[CreatureID]) -> String {
    match ids {
        [] => "no creatures".to_string(),
        [id] => creature_name(game, *id),
        _ => plural(ids.len(), "creature", "creatures"),
    }
}

fn resource_count(resources: &arptypes::CollectionResources) -> String {
    let count = resources.scenes.len()
        + resources.creatures.len()
        + resources.notes.len()
        + resources.items.len()
        + resources.abilities.len()
        + resources.classes.len();
    plural(count, "resource", "resources")
}

fn plural(count: usize, singular: &str, plural: &str) -> String {
    let noun = if count == 1 { singular } else { plural };
    format!("{count} {noun}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rollback_target_is_immediately_after_the_log() {
        assert_eq!(
            rollback_index_after(GameIndex {
                game_idx: 3,
                log_idx: 8,
            }),
            GameIndex {
                game_idx: 3,
                log_idx: 9,
            }
        );
    }
}
