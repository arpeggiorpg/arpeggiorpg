module CommonView exposing (visibleCreatures)

import Dict

import Model as M
import Types as T
import Grid

visibleCreatures : M.Model -> T.Game -> List Grid.MapCreature
visibleCreatures model game =
  let mapInfo creature =
    Maybe.map (\class -> {creature = creature, highlight = False, movable = Nothing, class = class})
              (Dict.get creature.class game.classes)
  in
    case game.current_combat of
      Just combat ->
        if model.showOOC
        then List.filterMap mapInfo <| combat.creatures.data ++ (Dict.values game.creatures)
        else List.filterMap mapInfo combat.creatures.data
      Nothing -> (List.filterMap mapInfo (Dict.values game.creatures))

