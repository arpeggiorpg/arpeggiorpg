module Model exposing (..)

import Dict
import Http
import Set

import Types as T

type alias GotCreatures = List T.CreatureID -> Cmd Msg

type Msg
    = MorePlease
    | SetPlayerID T.PlayerID
    | RegisterPlayer
    | SelectMap T.MapName
    | UpdateSaveMapName T.MapName
    | StartEditingMap
    | EditMap T.Map
    | CancelEditingMap
    | PendingCreatureId T.CreatureID
    | PendingCreatureName String
    | PendingCreatureClass String
    | CreateCreature T.CreatureCreation
    | CommandComplete (Result Http.Error T.RustResult)
    | AppUpdate (Result Http.Error T.App)
    | ShowError String
    | ToggleSelectedCreature T.CreatureID
    | StopCombat
    | AddToCombat T.CreatureID
    | RemoveFromCombat T.CreatureID
    | RemoveFromGame T.CreatureID
    | SelectAbility T.CreatureID T.AbilityID
    | CancelAbility
    | GotTargetOptions (Result Http.Error (List T.PotentialTarget))
    | CombatAct T.AbilityID T.DecidedTarget
    | ActCreature T.CreatureID T.AbilityID T.DecidedTarget
    | RequestMove MovementRequest
    | CancelMovement
    | CombatMove T.Point3
    | MoveCreature T.CreatureID T.Point3
    | TurnDone
    | GetMovementOptions T.Creature
    | GotMovementOptions T.Creature (Result Http.Error (List T.Point3))
    | ToggleTerrain T.Point3
    | SelectCreatures GotCreatures String
    | DoneSelectingCreatures
    | CancelSelectingCreatures
    | ToggleShowOOC

defaultModel : Model
defaultModel =
    { app = Nothing
    , selectedAbility = Nothing
    , pendingCreatureId = Nothing
    , pendingCreatureName = Nothing
    , pendingCreatureClass = Nothing
    , selectedCreatures = Set.empty
    , selectingCreatures = Nothing
    , moving = Nothing
    , error = "No current error!"
    , saveMapName = "" -- this could be inside of editingMap sumtype
    , editingMap = False
    , currentMap = [{x=0, y=0, z=0}]
    , playerID = Nothing
    , potentialTargets = []
    , showOOC = False
  }


type alias Model =
  { app : Maybe T.App
  , pendingCreatureId : Maybe T.CreatureID
  , pendingCreatureName : Maybe String
  , pendingCreatureClass : Maybe String
  , selectedAbility : Maybe (T.CreatureID, T.AbilityID)
  -- Creatures which have been selected for combat
  , selectedCreatures : Set.Set T.CreatureID
  , selectingCreatures : Maybe (GotCreatures, String)
  , error: String
  , moving: Maybe MovementRequest
  , saveMapName: String
  , currentMap : T.Map
  , editingMap : Bool
  , playerID : Maybe T.PlayerID
  , potentialTargets: List T.PotentialTarget
  , showOOC: Bool
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
