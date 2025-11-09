use std::{collections::HashSet, iter::FromIterator};

use maplit::hashset;

use crate::{combat::test::*, game::*, types::test::*};
use indexed::IndexedHashMap;

pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
    t_perform(
        game,
        GMCommand::StartCombat {
            scene_id: t_scene_id(),
            combatants,
        },
    )
}

pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
    t_perform(game, GMCommand::CombatAct { ability_id, target })
}

pub fn t_game() -> Game {
    let mut game: Game = Game {
        abilities: t_abilities(),
        classes: t_classes(),
        ..Default::default()
    };
    let mut rogue = t_rogue("rogue");
    rogue.id = cid_rogue();
    let mut ranger = t_ranger("ranger");
    ranger.id = cid_ranger();
    let mut cleric = t_cleric("cleric");
    cleric.id = cid_cleric();
    game.creatures.insert(rogue);
    game.creatures.insert(ranger);
    game.creatures.insert(cleric);
    game.scenes.insert(t_scene());
    let mut folder = Folder::new();
    for creature_id in game.creatures.keys() {
        folder.creatures.insert(*creature_id);
    }
    for scene_id in game.scenes.keys() {
        folder.scenes.insert(*scene_id);
    }
    for class_id in game.classes.keys() {
        folder.classes.insert(*class_id);
    }
    for ab_id in game.abilities.keys() {
        folder.abilities.insert(*ab_id);
    }
    game.campaign
        .make_folder(&FolderPath::root(), "testdata".to_string(), folder)
        .unwrap();
    game
}

#[test]
fn validate_test_game() {
    t_game()
        .validate_campaign()
        .expect("Test game must validate");
}

pub fn t_classes() -> IndexedHashMap<Class> {
    let rogue_abs = vec![abid_punch()];
    let ranger_abs = vec![abid_shoot(), abid_piercing_shot()];
    let cleric_abs = vec![abid_heal(), abid_fireball()];
    IndexedHashMap::from_iter(vec![
        Class {
            id: classid_rogue(),
            name: "Rogue".to_string(),
            abilities: rogue_abs,
            conditions: vec![],
            color: "purple".to_string(),
            emoji: Some("🗡️".to_string()),
        },
        Class {
            id: classid_ranger(),
            name: "Ranger".to_string(),
            abilities: ranger_abs,
            conditions: vec![],
            color: "darkgreen".to_string(),
            emoji: Some("🏹".to_string()),
        },
        Class {
            id: classid_cleric(),
            name: "Cleric".to_string(),
            abilities: cleric_abs,
            conditions: vec![],
            color: "lightgreen".to_string(),
            emoji: Some("💉".to_string()),
        },
    ])
}

pub fn perf(game: &Game, cmd: GMCommand) -> Result<ChangedGame, GameError> {
    game.perform_gm_command(cmd)
}

pub fn t_perform(game: &Game, cmd: GMCommand) -> Game {
    perf(game, cmd).unwrap().game
}

pub fn t_player_perform(game: &Game, player_id: PlayerID, cmd: PlayerCommand) -> Game {
    game.perform_player_command(player_id, cmd).unwrap().game
}

#[test]
fn start_combat_not_found() {
    let game = t_game();
    let non = CreatureID::gen();
    let result = game.perform_gm_command(GMCommand::StartCombat {
        scene_id: t_scene_id(),
        combatants: vec![non],
    });
    match result {
        Err(GameError::CreatureNotFound(id)) => assert_eq!(id, non.to_string()),
        x => panic!("Unexpected result: {:?}", x),
    }
}

#[test]
fn combat_must_have_creatures() {
    let game = t_game();
    let result = game.perform_gm_command(GMCommand::StartCombat {
        scene_id: t_scene_id(),
        combatants: vec![],
    });
    match result {
        Err(GameError::CombatMustHaveCreatures) => {}
        x => panic!("Unexpected result: {:?}", x),
    }
}

#[test]
fn stop_combat() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    let game = t_perform(
        &game,
        GMCommand::CombatAct {
            ability_id: abid_punch(),
            target: DecidedTarget::Creature(cid_ranger()),
        },
    );
    assert_eq!(
        game.get_creature(cid_ranger())
            .unwrap()
            .creature
            .cur_health(),
        HP(7)
    );
    let game = t_perform(&game, GMCommand::StopCombat);
    assert_eq!(
        game.get_creature(cid_ranger())
            .unwrap()
            .creature
            .cur_health(),
        HP(7)
    );
}

#[test]
fn movement() {
    let game = t_game();
    let game = t_start_combat(&game, vec![cid_rogue(), cid_ranger(), cid_cleric()]);
    t_perform(
        &game,
        GMCommand::PathCurrentCombatCreature {
            destination: Point3::new(100, 0, 0),
        },
    );
}

#[test]
fn change_creature_initiative() {
    let game = t_combat();
    assert_eq!(
        game.get_combat().unwrap().combat.creature_ids(),
        vec![cid_rogue(), cid_ranger(), cid_cleric()]
    );
    // move ranger to have an initiative higher than the rogue
    let game = t_perform(
        &game,
        GMCommand::ChangeCreatureInitiative {
            creature_id: cid_ranger(),
            initiative: 30,
        },
    );
    assert_eq!(
        game.get_combat().unwrap().combat.creature_ids(),
        vec![cid_ranger(), cid_rogue(), cid_cleric()]
    );
}

#[test]
fn three_char_infinite_combat() {
    let game = t_game();
    let game = t_perform(
        &game,
        GMCommand::StartCombat {
            scene_id: t_scene_id(),
            combatants: vec![cid_rogue(), cid_ranger(), cid_cleric()],
        },
    );
    let iter = |game: &Game| -> Result<Game, GameError> {
        let game = t_game_act(game, abid_punch(), DecidedTarget::Creature(cid_ranger()));
        let game = t_perform(&game, GMCommand::EndTurn);
        let game = t_perform(&game, GMCommand::EndTurn);
        let game = t_game_act(&game, abid_heal(), DecidedTarget::Creature(cid_ranger()));
        let game = t_perform(&game, GMCommand::EndTurn);
        Ok(game)
    };
    iter(&game).unwrap();
}

#[test]
fn ability_creatures_within_area() {
    // the cleric moves away, then casts a fireball at the ranger and rogue.
    let game = t_game();
    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_cleric(),
            pos: Point3::new(1100, 0, 0),
        },
    );
    let game = t_perform(
        &game,
        GMCommand::ActCreature {
            scene_id: t_scene_id(),
            creature_id: cid_cleric(),
            ability_id: abid_fireball(),
            target: DecidedTarget::Point(Point3::new(0, 0, 0)),
        },
    );
    assert_eq!(
        game.get_creature(cid_rogue()).unwrap().creature.cur_health,
        HP(7)
    );
    assert_eq!(
        game.get_creature(cid_ranger()).unwrap().creature.cur_health,
        HP(7)
    );
    assert_eq!(
        game.get_creature(cid_cleric()).unwrap().creature.cur_health,
        HP(10)
    );
}

#[test]
fn test_creatures_in_sphere() {
    let game = t_game();
    let volume = Volume::Sphere(u32cm(200));
    let pt = Point3::new(500, 0, 0);

    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_rogue(),
            pos: Point3::new(500, 0, 0),
        },
    );
    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_cleric(),
            pos: Point3::new(600, 0, 0),
        },
    );
    let scene = game.get_scene(t_scene_id()).unwrap();

    let cids = scene.creatures_in_volume(game.tile_system, pt, volume);
    let cids = HashSet::<CreatureID>::from_iter(cids);
    assert_eq!(cids, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
}

#[test]
fn test_sphere_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::AllCreaturesInVolumeInRange {
        range: u32cm(1000),
        volume: Volume::Sphere(u32cm(200)),
    };
    let pt = Point3::new(500, 0, 0);

    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_rogue(),
            pos: Point3::new(500, 0, 0),
        },
    );
    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_cleric(),
            pos: Point3::new(600, 0, 0),
        },
    );

    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game
        .volume_creature_targets(scene, cid_ranger(), target_spec, pt)
        .unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
}

#[test]
fn test_line_targets() {
    let game = t_game();
    let target_spec = CreatureTarget::LineFromActor {
        distance: u32cm(1000),
    };
    let pt = Point3::new(100, 0, 0);

    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_rogue(),
            pos: Point3::new(100, 0, 0),
        },
    );
    let game = t_perform(
        &game,
        GMCommand::SetCreaturePos {
            scene_id: t_scene_id(),
            creature_id: cid_cleric(),
            pos: Point3::new(200, 0, 0),
        },
    );
    let scene = game.get_scene(t_scene_id()).unwrap();

    let targets = game
        .volume_creature_targets(scene, cid_ranger(), target_spec, pt)
        .unwrap();
    let targets = HashSet::<CreatureID>::from_iter(targets);
    assert_eq!(targets, HashSet::from_iter(vec![cid_rogue(), cid_cleric()]));
}

#[test]
fn preview_volume_targets_shows_creatures_for_scene_volume_actions() {
    // When previewing volume targets for an action that affects a Scene Volume, the creatures
    // within the volume will be returned.

    let game = t_game();
    let scene = game.get_scene(t_scene_id()).unwrap();
    let cleric = cid_cleric();
    let ability_id = abid_thorn_patch();
    let preview = game
        .preview_volume_targets(scene, cleric, ability_id, Point3::new(0, 0, 0))
        .unwrap();
    let expected = hashset! {cid_cleric(), cid_ranger(), cid_rogue()};
    assert_eq!(HashSet::from_iter(preview.0), expected);
}

#[test]
fn test_export_module() {
    let root_path = FolderPath::root();
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let note = Note {
        name: "My Note".to_string(),
        content: "My Content".to_string(),
    };
    let mut folder = Folder::new();
    folder.notes.insert(note.clone());

    let mut game = t_game();
    game.campaign
        .make_folder(&root_path, "Rules".to_string(), folder)
        .expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    let new_note = new_game
        .campaign
        .get(&root_path)
        .unwrap()
        .notes
        .get("My Note");
    assert_eq!(new_note.expect("My Note wasn't at root"), &note);
}

#[test]
fn test_export_module_references() {
    let root_path = FolderPath::root();
    let rules_path = FolderPath::from_vec(vec!["Rules".to_string()]);
    let mut folder = Folder::new();
    folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game.campaign
        .make_folder(&root_path, "Rules".to_string(), folder)
        .expect("Betta woik");
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
        new_game
            .get_class(classid_ranger())
            .expect("new game didn't have ranger class"),
        game.get_class(classid_ranger())
            .expect("Old game didn't have ranger class")
    );
}

#[test]
fn test_export_subfolders() {
    let root_path = FolderPath::root();
    let rules_path = "/Rules".parse().unwrap();
    let root = Folder::new();
    let mut classes_folder = Folder::new();
    classes_folder.classes.insert(classid_ranger());

    let mut game = t_game();
    game.campaign
        .make_folder(&root_path, "Rules".to_string(), root)
        .unwrap();
    game.campaign
        .make_folder(&rules_path, "Classes".to_string(), classes_folder)
        .unwrap();
    let new_game = game.export_module(&rules_path).expect("Couldn't export");
    assert_eq!(
        new_game
            .get_class(classid_ranger())
            .expect("new game didn't have ranger class"),
        game.get_class(classid_ranger())
            .expect("Old game didn't have ranger class")
    );

    let new_classes = &new_game
        .campaign
        .get(&"/Classes".parse().unwrap())
        .unwrap()
        .classes;
    let old_classes = &game
        .campaign
        .get(&"/Rules/Classes".parse().unwrap())
        .unwrap()
        .classes;
    assert_eq!(new_classes, old_classes);
}

#[test]
fn test_import_module() {
    let mut module: Game = Default::default();
    let classid = ClassID::gen();
    let class = Class {
        id: classid,
        name: "Blood Hunter".to_string(),
        abilities: vec![],
        conditions: vec![],
        color: "blue".to_string(),
        emoji: Some("🩸".to_string()),
    };
    module.classes.insert(class);
    module
        .link_folder_item(&FolderPath::root(), &FolderItemID::ClassID(classid))
        .unwrap();

    let sys_path = "/System".parse().unwrap();

    let mut game = t_game();
    game.import_module(&sys_path, &module)
        .expect("import must succeed");

    assert_eq!(
        game.get_class(classid).expect("New game missing class"),
        module.get_class(classid).expect("Old game missing class")
    );
    assert!(game
        .campaign
        .get(&sys_path)
        .unwrap()
        .classes
        .contains(&classid));
}

fn t_item() -> Item {
    Item {
        id: ItemID::gen(),
        name: "Test Sword".to_string(),
    }
}

fn t_player(name: &str, scene_id: Option<SceneID>, creatures: Vec<CreatureID>) -> Player {
    let mut player = Player::new(PlayerID(name.to_string()));
    player.scene = scene_id;
    player.creatures = creatures.into_iter().collect();
    player
}

fn setup_inventory_test_game() -> (Game, ItemID, PlayerID, PlayerID) {
    let mut game = t_game();

    // Create a test item
    let test_item = t_item();
    let item_id = test_item.id;
    game.items.insert(test_item);

    // Create two players
    let player1_id = PlayerID("player1".to_string());
    let player2_id = PlayerID("player2".to_string());

    let player1 = t_player(
        "player1",
        Some(t_scene_id()),
        vec![cid_rogue(), cid_ranger()],
    );
    let player2 = t_player("player2", Some(t_scene_id()), vec![cid_cleric()]);

    game.players.insert(player1);
    game.players.insert(player2);

    // Give some items to the rogue
    game.set_item_count(InventoryOwner::Creature(cid_rogue()), item_id, 5)
        .unwrap();

    (game, item_id, player1_id, player2_id)
}

#[test]
fn test_give_item_success() {
    let (game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Player1 gives 2 items from rogue to ranger
    let result = t_player_perform(
        &game,
        player1_id,
        PlayerCommand::GiveItem {
            from_creature_id: cid_rogue(),
            to_creature_id: cid_ranger(),
            item_id,
            count: 2,
        },
    );

    // Check that rogue now has 3 items (complete inventory)
    let expected_rogue_inventory = HashMap::from([(item_id, 3)]);
    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Creature(cid_rogue()))
            .unwrap(),
        expected_rogue_inventory
    );

    // Check that ranger now has 2 items (complete inventory)
    let expected_ranger_inventory = HashMap::from([(item_id, 2)]);
    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Creature(cid_ranger()))
            .unwrap(),
        expected_ranger_inventory
    );
}

#[test]
fn test_give_item_player_does_not_own_source_creature() {
    let (game, item_id, _player1_id, player2_id) = setup_inventory_test_game();

    // Player2 tries to give items from rogue (which they don't own) to cleric
    let result = game.perform_player_command(
        player2_id,
        PlayerCommand::GiveItem {
            from_creature_id: cid_rogue(),
            to_creature_id: cid_cleric(),
            item_id,
            count: 1,
        },
    );

    assert_eq!(
        result.unwrap_err(),
        GameError::CreatureNotFound(cid_rogue().0.to_string())
    );
}

#[test]
fn test_give_item_target_not_in_scene() {
    let (mut game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Remove cleric from the scene
    game.scenes.mutate(&t_scene_id(), |scene| {
        scene.creatures.remove(&cid_cleric());
    });

    // Player1 tries to give items to cleric (not in scene)
    let result = game.perform_player_command(
        player1_id,
        PlayerCommand::GiveItem {
            from_creature_id: cid_rogue(),
            to_creature_id: cid_cleric(),
            item_id,
            count: 1,
        },
    );

    assert_eq!(
        result.unwrap_err(),
        GameError::CreatureNotFound(cid_cleric().0.to_string())
    );
}

#[test]
fn test_give_item_player_not_in_scene() {
    let (mut game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Remove player1 from scene
    game.players.mutate(&player1_id, |player| {
        player.scene = None;
    });

    // Player1 tries to give items while not in a scene
    let result = game.perform_player_command(
        player1_id,
        PlayerCommand::GiveItem {
            from_creature_id: cid_rogue(),
            to_creature_id: cid_ranger(),
            item_id,
            count: 1,
        },
    );

    assert!(matches!(result, Err(GameError::BuggyProgram(_))));
}

#[test]
fn test_drop_item_success() {
    let (game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Player1 drops 3 items from rogue into the scene
    let result = t_player_perform(
        &game,
        player1_id,
        PlayerCommand::DropItem {
            creature_id: cid_rogue(),
            item_id,
            count: 3,
        },
    );

    // Check that rogue now has 2 items (complete inventory)
    let expected_rogue_inventory = HashMap::from([(item_id, 2)]);
    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Creature(cid_rogue()))
            .unwrap(),
        expected_rogue_inventory
    );

    // Check that scene now has 3 items (complete inventory)
    let expected_scene_inventory = HashMap::from([(item_id, 3)]);
    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Scene(t_scene_id()))
            .unwrap(),
        expected_scene_inventory
    );
}

#[test]
fn test_drop_item_player_does_not_own_creature() {
    let (game, item_id, _player1_id, player2_id) = setup_inventory_test_game();

    // Player2 tries to drop items from rogue (which they don't own)
    let result = game.perform_player_command(
        player2_id,
        PlayerCommand::DropItem {
            creature_id: cid_rogue(),
            item_id,
            count: 1,
        },
    );

    assert_eq!(
        result.unwrap_err(),
        GameError::CreatureNotFound(cid_rogue().0.to_string())
    );
}

#[test]
fn test_drop_item_creature_not_in_scene() {
    let (mut game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Remove rogue from the scene
    game.scenes.mutate(&t_scene_id(), |scene| {
        scene.creatures.remove(&cid_rogue());
    });

    // Player1 tries to drop items from rogue (not in scene)
    let result = game.perform_player_command(
        player1_id,
        PlayerCommand::DropItem {
            creature_id: cid_rogue(),
            item_id,
            count: 1,
        },
    );
    assert_eq!(
        result.unwrap_err(),
        GameError::CreatureNotFound(cid_rogue().0.to_string())
    );
}

#[test]
fn test_drop_item_player_not_in_scene() {
    let (mut game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Remove player1 from scene
    game.players.mutate(&player1_id, |player| {
        player.scene = None;
    });

    // Player1 tries to drop items while not in a scene
    let result = game.perform_player_command(
        player1_id,
        PlayerCommand::DropItem {
            creature_id: cid_rogue(),
            item_id,
            count: 1,
        },
    );

    assert!(matches!(result, Err(GameError::BuggyProgram(_))));
}

#[test]
fn test_give_item_all_items() {
    let (game, item_id, player1_id, _player2_id) = setup_inventory_test_game();

    // Player1 gives all 5 items from rogue to ranger
    let result = t_player_perform(
        &game,
        player1_id,
        PlayerCommand::GiveItem {
            from_creature_id: cid_rogue(),
            to_creature_id: cid_ranger(),
            item_id,
            count: 5,
        },
    );

    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Creature(cid_rogue()))
            .unwrap(),
        HashMap::new(),
    );

    assert_eq!(
        *result
            .get_owner_inventory(InventoryOwner::Creature(cid_ranger()))
            .unwrap(),
        HashMap::from([(item_id, 5)])
    );
}
