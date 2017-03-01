module Model exposing (..)

import Dict
import Http
import Time

import Types as T


type alias GotCreatures = List T.CreatureID -> Cmd Msg

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.showingMovement of
    ShowingMovement _ _ -> Time.every (Time.second / 4) Tick
    _ -> Sub.none

type Msg
    = MorePlease
    | PollApp
    | ReceivedAppUpdate (Result Http.Error T.App)
    | AppUpdate (Result Http.Error T.App)
    | ShowError String
    | SetPlayerID T.PlayerID
    | RegisterPlayer
    | UpdateSaveMapName T.MapName
    | StartEditingMap
    | EditMap T.Map
    | CancelEditingMap
    | PendingCreatureId T.CreatureID
    | PendingCreatureName String
    | PendingCreatureClass String
    | CommandComplete (Result Http.Error T.RustResult)
    | ToggleSelectedCreature T.CreatureID
    | SelectAbility T.CreatureID T.AbilityID
    | CancelAbility
    | GotTargetOptions (Result Http.Error (List T.PotentialTarget))
    | CombatAct T.AbilityID T.DecidedTarget
    | ActCreature T.CreatureID T.AbilityID T.DecidedTarget
    | RequestMove MovementRequest
    | CancelMovement
    | PathCurrentCombatCreature T.Point3
    | PathCreature T.CreatureID T.Point3
    | GetMovementOptions T.Creature
    | GetCombatMovementOptions
    | GotCombatMovementOptions (Result Http.Error (List T.Point3))
    | GotMovementOptions T.Creature (Result Http.Error (List T.Point3))
    | ToggleTerrain T.Point3
    | SelectCreatures GotCreatures String
    | DoneSelectingCreatures
    | CancelSelectingCreatures
    | ToggleShowOOC
    | ToggleMoveAnywhere
    | Tick Time.Time
    | SendCommand T.GameCommand
    | SetCreatureNote T.CreatureID String

defaultModel : ProgramFlags -> Model
defaultModel flags =
    { app = Nothing
    , selectedAbility = Nothing
    , pendingCreatureId = Nothing
    , pendingCreatureName = Nothing
    , pendingCreatureClass = Nothing
    , selectingCreatures = Nothing
    , moving = Nothing
    , error = "No current error!"
    , saveMapName = "" -- this could be inside of editingMap sumtype
    , editingMap = False
    , currentMap = [{x=0, y=0, z=0}]
    , playerID = Nothing
    , potentialTargets = []
    , showOOC = False
    , moveAnywhere = False
    , showingMovement = NotShowingMovement
    , creatureNotes = Dict.empty
    , rpiURL = flags.rpi
  }

devFlags : ProgramFlags
devFlags = {rpi = "http://localhost:1337/"}

type alias ProgramFlags =
  { rpi : String }

type alias Model =
  { app : Maybe T.App
  , pendingCreatureId : Maybe T.CreatureID
  , pendingCreatureName : Maybe String
  , pendingCreatureClass : Maybe String
  , selectedAbility : Maybe (T.CreatureID, T.AbilityID)
  -- Creatures which have been selected for combat
  , selectingCreatures : Maybe (List T.CreatureID, GotCreatures, String)
  , error: String
  , moving: Maybe MovementRequest
  , saveMapName: String
  , currentMap : T.Map
  , editingMap : Bool
  , playerID : Maybe T.PlayerID
  , potentialTargets: List T.PotentialTarget
  , showOOC: Bool
  , showingMovement: MovementAnimation
  , creatureNotes : Dict.Dict T.CreatureID String
  , moveAnywhere : Bool
  , rpiURL : String
  }
  

type MovementAnimation
  = ShowingMovement (List T.Point3) (List T.Point3) -- first is what's been shown so far, second is what's left to animate
  | DoneShowingMovement (List T.Point3)
  | NotShowingMovement


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
