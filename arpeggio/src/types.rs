//! Simple types, with pure operations.

use std::collections::{HashMap, HashSet};

use rand::Rng;

pub use arptypes::*;

use crate::game::GameExt;

pub trait DiceExt {
    fn expr(n: u8, d: u8) -> Dice;

    fn flat(value: i8) -> Dice;

    fn plus(&self, d: Dice) -> Dice;

    /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
    /// final result.
    fn roll(&self) -> (Vec<i16>, i32);
}

impl DiceExt for Dice {
    fn expr(n: u8, d: u8) -> Dice {
        Dice::Expr { num: n, size: d }
    }

    fn flat(value: i8) -> Dice {
        Dice::Flat { value }
    }

    fn plus(&self, d: Dice) -> Dice {
        Dice::Plus(Box::new(self.clone()), Box::new(d))
    }

    /// Roll the dice, returning a vector containing all of the individual die rolls, and then the
    /// final result.
    fn roll(&self) -> (Vec<i16>, i32) {
        match *self {
            Dice::Expr { num, size } => {
                let mut intermediate = vec![];
                let mut result = 0i32;
                let mut rng = rand::thread_rng();

                for _ in 0..num {
                    let val = rng.gen_range(1..i32::from(size) + 1);
                    result += val;
                    intermediate.push(val as i16);
                }
                (intermediate, result)
            }
            Dice::Flat { value } => (vec![i16::from(value)], i32::from(value)),
            Dice::Plus(ref l, ref r) => {
                let (mut intermediate, left_result) = l.roll();
                let (right_intermediate, right_result) = r.roll();
                intermediate.extend(right_intermediate);
                (intermediate, left_result + right_result)
            }
            Dice::BestOf(count, ref dice) => {
                if count == 0 {
                    panic!("Sorry, can't roll best of 0.")
                }
                let (mut best_rolls, mut best_result) = dice.roll();
                for _ in 1..count {
                    let (rolls, result) = dice.roll();
                    if result > best_result {
                        best_rolls = rolls;
                        best_result = result;
                    }
                }
                (best_rolls, best_result)
            }
        }
    }
}

pub type CollisionWorld = ::ncollide3d::world::CollisionWorld<f64, CollisionData>;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum CollisionData {
    Creature(CreatureID),
    ConditionVolume(ConditionID),
    // BlockedTerrain ????
}

#[derive(Clone, Debug, PartialEq)]
pub struct DynamicCombat<'game> {
    pub scene: &'game Scene,
    pub combat: &'game Combat,
    pub game: &'game Game,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DynamicCreature<'creature, 'game: 'creature> {
    pub creature: &'creature Creature,
    pub game: &'game Game,
    pub class: &'game Class,
}

impl<'c, 'g> DynamicCreature<'c, 'g> {
    pub fn serialize_creature(&self) -> SerializedCreature {
        SerializedCreature {
            id: self.creature.id,
            name: self.creature.name.clone(),
            max_energy: self.creature.max_energy,
            cur_energy: self.creature.cur_energy,
            class: self.creature.class,
            max_health: self.creature.max_health,
            cur_health: self.creature.cur_health,
            note: self.creature.note.clone(),
            bio: self.creature.bio.clone(),
            portrait_url: self.creature.portrait_url.clone(),
            icon_url: self.creature.icon_url.clone(),
            attributes: self.creature.attributes.clone(),
            initiative: self.creature.initiative.clone(),
            size: self.creature.size,
            inventory: self.creature.inventory.clone(),
            conditions: self.creature.conditions.clone(),
            // overriden fields:
            speed: self.speed(),
            abilities: self.ability_statuses(),

            // synthesized fields:
            own_conditions: self.own_conditions().clone(),
            volume_conditions: self.volume_conditions(),
            can_act: self.can_act(),
            can_move: self.can_move(),
        }
    }
}

/// A newtype wrapper over Game can serialize the game data to a SerializedGame.
pub struct RPIGame<'a>(pub &'a Game);

impl<'a> RPIGame<'a> {
    pub fn serialize_game(&self) -> Result<SerializedGame, GameError> {
        let game = self.0;
        let creatures = game
            .creatures()?
            .iter()
            .map(|(creature_id, creature)| (*creature_id, creature.serialize_creature()))
            .collect();
        let sgame = SerializedGame {
            current_combat: game.current_combat.clone(),
            abilities: game.abilities.clone(),
            creatures,
            classes: game.classes.clone(),
            tile_system: game.tile_system,
            scenes: game.scenes.clone(),
            campaign: game.campaign.clone(),
            items: game.items.clone(),
            players: game.players.clone(),
            active_scene: game.active_scene,
        };
        Ok(sgame)
    }
}

/// Serialize a game for a specific player, including only data relevant to that player
pub fn serialize_player_game(
    player_id: &PlayerID,
    game: &Game,
) -> Result<SerializedPlayerGame, GameError> {
    let player = game
        .players
        .get(player_id)
        .ok_or_else(|| GameError::PlayerNotFound(player_id.clone()))?;

    let active_scene = if let Some(scene_id) = player.scene {
        Some(game.get_scene(scene_id)?.clone())
    } else {
        None
    };

    // Get creatures in the current scene
    let mut creatures = HashMap::new();
    let mut creature_class_ids = HashSet::new();

    if let Some(scene) = &active_scene {
        for &creature_id in scene.creatures.keys() {
            if let Some(creature) = game.creatures.get(&creature_id) {
                if let Some(class) = game.classes.get(&creature.class) {
                    let dynamic_creature = DynamicCreature {
                        creature,
                        game,
                        class,
                    };
                    let serialized_creature = dynamic_creature.serialize_creature();
                    creatures.insert(creature_id, serialized_creature);
                    creature_class_ids.insert(creature.class);
                }
            }
        }
    }

    // Get classes for creatures in the scene
    let classes = creature_class_ids
        .iter()
        .filter_map(|class_id| game.classes.get(class_id).cloned())
        .collect();

    // Get items from scene and player's creatures
    let items = active_scene
        .iter()
        .flat_map(|scene| scene.inventory.keys())
        .chain(
            player
                .creatures
                .iter()
                .filter_map(|creature_id| game.creatures.get(creature_id))
                .flat_map(|creature| creature.inventory.keys()),
        )
        .filter_map(|&item_id| game.items.get(&item_id).cloned())
        .collect();

    // Get player's notes from /Players/{PlayerID}/Notes folder
    let mut notes = indexed::IndexedHashMap::new();
    let player_notes_path: foldertree::FolderPath = vec![
        "Players".to_string(),
        player_id.0.clone(),
        "Notes".to_string(),
    ]
    .into();

    if let Ok(player_folder) = game.campaign.get(&player_notes_path) {
        for note in player_folder.notes.values() {
            notes.insert(note.clone());
        }
    }

    Ok(SerializedPlayerGame {
        current_combat: game.current_combat.clone(),
        active_scene,
        abilities: game.abilities.clone(),
        creatures,
        classes,
        items,
        tile_system: game.tile_system,
        players: game.players.clone(),
        notes,
    })
}

#[cfg(test)]
pub mod test {
    use crate::{creature::CreatureExt, grid::test::*, types::*};
    use indexed::IndexedHashMap;
    use maplit::hashmap;
    use std::{
        collections::{HashMap, HashSet},
        iter::FromIterator,
    };
    use uuid::Uuid;

    use serde_json;
    use serde_yaml;
    pub fn uuid_0() -> Uuid {
        "00000000-0000-0000-0000-000000000000".parse().unwrap()
    }
    pub fn uuid_1() -> Uuid {
        "00000000-0000-0000-0000-000000000001".parse().unwrap()
    }
    pub fn uuid_2() -> Uuid {
        "00000000-0000-0000-0000-000000000002".parse().unwrap()
    }
    pub fn uuid_3() -> Uuid {
        "00000000-0000-0000-0000-000000000003".parse().unwrap()
    }
    pub fn uuid_4() -> Uuid {
        "00000000-0000-0000-0000-000000000004".parse().unwrap()
    }
    pub fn uuid_5() -> Uuid {
        "00000000-0000-0000-0000-000000000005".parse().unwrap()
    }
    pub fn cid_cleric() -> CreatureID {
        CreatureID(uuid_0())
    }
    pub fn cid_ranger() -> CreatureID {
        CreatureID(uuid_1())
    }
    pub fn cid_rogue() -> CreatureID {
        CreatureID(uuid_2())
    }

    pub fn t_creature(name: &str, class: ClassID, init: i8) -> Creature {
        Creature::create(&CreatureCreation {
            name: name.to_string(),
            note: "".to_string(),
            bio: "".to_string(),
            class,
            portrait_url: "".to_string(),
            icon_url: "".to_string(),
            initiative: Dice::flat(init),
            size: AABB {
                x: u32cm(100),
                y: u32cm(100),
                z: u32cm(100),
            },
        })
    }

    pub fn t_rogue(name: &str) -> Creature {
        Creature {
            id: cid_rogue(),
            ..t_creature(name, classid_rogue(), 20)
        }
    }

    pub fn t_ranger(name: &str) -> Creature {
        Creature {
            id: cid_ranger(),
            ..t_creature(name, classid_ranger(), 10)
        }
    }

    pub fn t_cleric(name: &str) -> Creature {
        Creature {
            id: cid_rogue(),
            ..t_creature(name, classid_cleric(), 0)
        }
    }

    pub fn t_scene_id() -> SceneID {
        SceneID(uuid_3())
    }

    pub fn t_scene() -> Scene {
        Scene {
            id: t_scene_id(),
            name: "Test Scene".to_string(),
            background_image_url: "".to_string(),
            background_image_offset: None,
            background_image_scale: (1., 1.),
            terrain: huge_box(),
            highlights: HashMap::new(),
            annotations: HashMap::new(),

            scene_hotspots: HashMap::new(),
            related_scenes: HashSet::new(),

            attribute_checks: HashMap::new(),
            creatures: hashmap! {
              cid_rogue() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
              cid_cleric() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
              cid_ranger() => (Point3::new(0, 0, 0), Visibility::AllPlayers),
            },
            inventory: HashMap::new(),
            volume_conditions: HashMap::new(),
            focused_creatures: vec![],
        }
    }

    pub fn app_cond(c: Condition, r: Duration) -> AppliedCondition {
        AppliedCondition {
            condition: c,
            remaining: r,
        }
    }

    pub fn classid_rogue() -> ClassID {
        ClassID(uuid_0())
    }
    pub fn classid_cleric() -> ClassID {
        ClassID(uuid_1())
    }
    pub fn classid_ranger() -> ClassID {
        ClassID(uuid_2())
    }

    pub fn abid_punch() -> AbilityID {
        AbilityID(uuid_0())
    }
    pub fn abid_shoot() -> AbilityID {
        AbilityID(uuid_1())
    }
    pub fn abid_heal() -> AbilityID {
        AbilityID(uuid_2())
    }
    pub fn abid_fireball() -> AbilityID {
        AbilityID(uuid_3())
    }
    pub fn abid_piercing_shot() -> AbilityID {
        AbilityID(uuid_4())
    }
    pub fn abid_thorn_patch() -> AbilityID {
        AbilityID(uuid_5())
    }

    pub fn t_punch() -> Ability {
        Ability {
            id: abid_punch(),
            name: "Punch".to_string(),
            cost: Energy(0),
            usable_ooc: true,
            action: Action::Creature {
                target: CreatureTarget::Melee,
                effect: CreatureEffect::Damage(Dice::flat(3)),
            },
        }
    }

    pub fn t_shoot() -> Ability {
        Ability {
            id: abid_shoot(),
            name: "Shoot".to_string(),
            cost: Energy(0),
            usable_ooc: true,
            action: Action::Creature {
                target: CreatureTarget::Range(u32cm(500)),
                effect: CreatureEffect::Damage(Dice::flat(3)),
            },
        }
    }

    pub fn t_heal() -> Ability {
        Ability {
            id: abid_heal(),
            name: "Heal".to_string(),
            cost: Energy(0),
            usable_ooc: true,
            action: Action::Creature {
                target: CreatureTarget::Range(u32cm(500)),
                effect: CreatureEffect::Heal(Dice::flat(3)),
            },
        }
    }

    pub fn t_fireball() -> Ability {
        Ability {
            id: abid_fireball(),
            name: "Fireball".to_string(),
            cost: Energy(8),
            usable_ooc: true,
            action: Action::Creature {
                target: CreatureTarget::AllCreaturesInVolumeInRange {
                    volume: Volume::Sphere(u32cm(1000)),
                    range: u32cm(2000),
                },
                effect: CreatureEffect::Damage(Dice::flat(3)),
            },
        }
    }

    pub fn t_piercing_shot() -> Ability {
        Ability {
            id: abid_piercing_shot(),
            name: "Piercing Shot".to_string(),
            cost: Energy(8),
            usable_ooc: true,
            action: Action::Creature {
                target: CreatureTarget::LineFromActor {
                    distance: u32cm(1000),
                },
                effect: CreatureEffect::Damage(Dice::flat(3)),
            },
        }
    }

    pub fn t_thorn_patch() -> Ability {
        Ability {
            id: abid_thorn_patch(),
            name: "Thorn Patch".to_string(),
            cost: Energy(8),
            usable_ooc: true,
            action: Action::SceneVolume {
                target: SceneTarget::RangedVolume {
                    volume: Volume::Sphere(u32cm(200)),
                    range: u32cm(1000),
                },
                effect: SceneEffect::CreateVolumeCondition {
                    duration: Duration::Interminate,
                    condition: Condition::RecurringEffect(Box::new(CreatureEffect::Damage(
                        Dice::flat(3),
                    ))),
                },
            },
        }
    }

    pub fn t_abilities() -> IndexedHashMap<Ability> {
        IndexedHashMap::from_iter(vec![
            t_punch(),
            t_shoot(),
            t_heal(),
            t_fireball(),
            t_piercing_shot(),
            t_thorn_patch(),
        ])
    }

    #[test]
    fn serde_ids() {
        let id = abid_heal();
        let serialized = serde_yaml::to_string(&id).unwrap();
        assert_eq!(serialized, "---\n00000000-0000-0000-0000-000000000002\n");
        let deserialized = serde_yaml::from_str::<AbilityID>(&serialized).unwrap();
        assert_eq!(deserialized, id);
    }

    #[test]
    fn serde_condition_duration() {
        let cd = Duration::Interminate;
        assert_eq!(serde_json::to_string(&cd).unwrap(), "\"Interminate\"");
        let cd = Duration::Rounds(3);
        assert_eq!(serde_json::to_string(&cd).unwrap(), "{\"Rounds\":3}");
    }

    #[test]
    fn dice_plus() {
        let d = Dice::flat(1).plus(Dice::flat(1));
        assert_eq!(d.roll(), (vec![1, 1], 2));
    }

    #[test]
    fn dice_negative() {
        let d = Dice::flat(1).plus(Dice::flat(-5));
        assert_eq!(d.roll(), (vec![1, -5], -4));
    }

    #[test]
    fn serialize_hashmap_point3() {
        let p = Point3::new(0, 0, 0);
        let hm = hashmap! {p => 5};
        assert_eq!(serde_json::to_string(&hm).unwrap(), "{\"0/0/0\":5}");
    }

    #[test]
    fn test_serialize_player_game() {
        use super::serialize_player_game;
        use crate::game::test::t_game;

        let game = t_game();
        let player_id = PlayerID("test_player".to_string());

        // Test with player not found
        let result = serialize_player_game(&player_id, &game);
        assert!(result.is_err());

        // Add a player and test successful serialization
        let mut game_with_player = game.clone();
        game_with_player
            .players
            .insert(Player::new(player_id.clone()));

        let result = serialize_player_game(&player_id, &game_with_player);
        assert!(result.is_ok());

        let player_game = result.unwrap();
        // Player game should have the same combat, abilities, tile system, and players
        assert_eq!(player_game.current_combat, game_with_player.current_combat);
        assert_eq!(player_game.abilities, game_with_player.abilities);
        assert_eq!(player_game.tile_system, game_with_player.tile_system);
        assert_eq!(player_game.players, game_with_player.players);
        // Active scene should be None since player isn't in a scene
        assert_eq!(player_game.active_scene, None);
        // Creatures should be empty since player isn't in a scene
        assert!(player_game.creatures.is_empty());
        // Classes and items should be empty since no creatures in scope
        assert!(player_game.classes.is_empty());
        assert!(player_game.items.is_empty());
        // Notes should be empty since no player notes folder exists
        assert!(player_game.notes.is_empty());
    }
}
