#!/usr/bin/python
import sys
import yaml
import json
import uuid



def main():
    data = yaml.load(open(sys.argv[1]).read())

    game = data["current_game"]
    fix_game(game)
    data["snapshots"] = []
    with open(sys.argv[2], 'w') as f:
        json.dump(data, f, indent=2, separators=(', ', ': '))


def fix_game(game):
    fix_action_effects(game)
    fix_ability_ids(game)
    fix_class_ids(game)
    fix_duration(game)
    fix_dice(game)
    fix_maps(game)
    fix_points(game)
    fix_add_scene_missing(game)
    fix_missing_campaign_objects(game)

def walk_folders(folder, path="/"):
    yield path, folder
    for subname in folder["children"]:
        yield from walk_folders(folder["children"][subname], path=path + subname + "/")

def fix_missing_campaign_objects(game):
    """Make sure all objects are in folders"""
    CATEGORIES = ["abilities", "creatures", "scenes", "items", "classes"]
    found = {cat: set() for cat in CATEGORIES}

    for (path, folder) in walk_folders(game["campaign"]):
        for cat in CATEGORIES:
            found[cat].update(folder["data"].get(cat, []))

    missing = {cat: set(game[cat].keys()) - found[cat] for cat in CATEGORIES}

    if not any(missing.values()):
        return

    missing_folder = game["campaign"]["children"].setdefault("Missing", {"data": {"notes": {}}, "children": {}})
    missing_data = missing_folder["data"]

    for cat in CATEGORIES:
        missing_data[cat] = list(missing[cat].union(missing_data.get(cat, [])))



def fix_add_scene_missing(game):
    for scene in game["scenes"].values():
        if "highlights" not in scene:
            scene["highlights"] = {}
        if "annotations" not in scene:
            scene["annotations"] = {}
        if "background_image_scale" not in scene:
            scene["background_image_scale"] = [1, 1]
        if "background_image_url" not in scene:
            scene["background_image_url"] = ""


def fix_maps(game):
    """Maps are no longer; terrain is now directly in scenes"""
    for scene in game["scenes"].values():
        if "map" in scene:
            scene["terrain"] = game["maps"][scene["map"]]["terrain"]

def fix_points(game):
    """Convert all [x,y,z] points into 0/0/0"""
    for scene in game["scenes"].values():
        for pos_and_vis in scene["creatures"].values():
            pos_and_vis[0] = strpt(pos_and_vis[0])
        scene["terrain"] = list(map(strpt, scene["terrain"]))

def strpt(pt):
    return f"{pt[0] * 100}/{pt[1] * 100}/{pt[2] * 100}"

def fix_class_ids(game):
    new_names = {} # old class name to new class ID
    if (not game["classes"]) or is_uuid(list(game["classes"].keys())[0]):
        return

    new_names = {class_name: str(uuid.uuid4()) for class_name in game["classes"].keys()}

    game["classes"] = {
        new_names[name]: {**class_, "id": new_names[name], "name": name} for name, class_ in game["classes"].items()
    }
    for creature in game["creatures"].values():
        creature["class"] = new_names[creature["class"]]

def fix_dice(game):
    for creature in game["creatures"].values():
        for die in walk_dice(creature["initiative"]):
            if "Flat" in die:
                die["Flat"] = {"value": die["Flat"]}

def walk_dice(dice):
    if "BestOf" in dice:
        yield dice
        yield from walk_dice(dice["BestOf"][1])
    elif "Plus" in dice:
        yield dice
        yield from walk_dice(dice["Plus"][0])
        yield from walk_dice(dice["Plus"][1])
    elif "Expr" in dice or "Flat" in dice:
        yield dice



def fix_duration(game):
    """The "Rounds" variant of Duration used to be called "Duration".
    """
    for ability in game["abilities"].values():
        if "Creature" in ability["action"]:
            for effect in flatten_effects(ability["action"]["Creature"]["effect"]):
                if apply_cond := effect.get("ApplyCondition"):
                    duration = apply_cond[0]
                    if not isinstance(duration, dict): continue
                    if "Duration" in duration:
                        duration["Rounds"] = duration.pop("Duration")



def fix_action_effects(game):
    """Ability.effects used to be a list of CreatureEffect, now it's been replaced by
    Ability.action, one variant of which is Creature. Ability.target is also moved inside of the
    action.
    """
    for ability in game["abilities"].values():
        if "effects" in ability:
            effects = ability.pop("effects")
            if len(effects) == 1:
                effect = effects[0]
            else:
                effect = {"MultiEffect": effects}
            ability["action"] = {"Creature": {"target": ability.pop("target"), "effect": effect}}


def fix_ability_ids(game):
    """Ensure Ability IDs are UUIDs."""
    if (not game["abilities"]) or is_uuid(list(game["abilities"].keys())[0]):
        return
    new_names = {ability_name: str(uuid.uuid4()) for ability_name in game["abilities"].keys()}
    game["abilities"] = {
        new_names[name]: {**ability, "id": new_names[name]} for name, ability in game["abilities"].items()
    }

    for creature in game["creatures"].values():
        creature["abilities"] = {
            new_names[ab_name]: {**ab_stat, "ability_id": new_names[ab_name]}
            for ab_name, ab_stat in creature["abilities"].items()
        }
    for class_ in game["classes"].values():
        class_["abilities"] = [new_names[ab_name] for ab_name in class_["abilities"]]

    for ability in game["abilities"].values():
        action = ability["action"]
        if "Creature" not in action: break
        for effect in flatten_effects(action["Creature"]["effect"]):
            if "ApplyCondition" in effect:
                for condition in effect["ApplyCondition"]:
                    if "ActivateAbility" in condition:
                        condition["ActivateAbility"] = new_names[condition["ActivateAbility"]]


def flatten_effects(effect):
    if "MultiEffect" in effect:
        for subeff in effect["MultiEffect"]:
            yield from flatten_effects(subeff)
    else:
        yield effect


def is_uuid(s):
    try:
        uuid.UUID(s)
        return True
    except ValueError:
        return False

if __name__ == '__main__':
    main()
