module Model exposing (..)

import Dict
import Set

import Types as T

defaultModel : Model
defaultModel =
    { app = Nothing
    , selectedAbility = Nothing
    , pendingCreatureId = Nothing
    , pendingCreatureName = Nothing
    , pendingCreatureClass = Nothing
    , selectedCreatures = Set.empty
    , moving = Nothing
    , error = "No current error!"
    , saveMapName = "" -- this could be inside of editingMap sumtype
    , editingMap = False
    , currentMap = [{x=0, y=0, z=0}]
    , playerID = Nothing
    , potentialTargets = []
  }


type alias Model =
  { app : Maybe T.App
  , pendingCreatureId : Maybe T.CreatureID
  , pendingCreatureName : Maybe String
  , pendingCreatureClass : Maybe String
  , selectedAbility : Maybe (T.CreatureID, T.AbilityID)
  -- Creatures which have been selected for combat
  , selectedCreatures : Set.Set T.CreatureID
  , error: String
  , moving: Maybe MovementRequest
  , saveMapName: String
  , currentMap : T.Map
  , editingMap : Bool
  , playerID : Maybe T.PlayerID
  , potentialTargets: List T.PotentialTarget
  }

type alias MovementRequest = {
  max_distance: T.Distance,
  movement_options: List T.Point3,
  -- This field is a Just when we're performing out-of-combat movement.
  -- It's Nothing for in-combat movement, because in-combat movement is only for the current
  -- creature.
  ooc_creature: Maybe T.Creature
}

getMap : Model -> T.Map
getMap model =
  case model.app of
    Just app -> let mapName = (Maybe.withDefault "empty" app.current_game.current_map)
                in (Maybe.withDefault [] (Dict.get mapName app.current_game.maps))
    Nothing -> []
