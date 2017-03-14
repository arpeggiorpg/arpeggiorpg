module Model exposing (..)

import Dict
import Http
import Keyboard
import Keyboard.Key as Key
import Time

import Types as T


type alias GotCreatures = List T.CreatureID -> Cmd Msg

subscriptions : Model -> Sub Msg
subscriptions model =
  let ticks =
        case model.showingMovement of
          ShowingMovement _ _ -> Time.every (Time.second / 4) Tick
          _ -> Sub.none
      handleKey key =
        case (Key.fromCode key) of
          Key.Up -> MapPan Up
          Key.Down -> MapPan Down
          Key.Left -> MapPan Left
          Key.Right -> MapPan Right
          _ -> NoMsg
          -- Key.Add -> MapZoom In
          -- Key.Subtract -> MapZoom Out
      keys = Keyboard.downs handleKey
  in Sub.batch [ticks, keys]

type Msg
    = Start
    | SetFocus Focus
    | MorePlease
    | PollApp
    | ReceivedAppUpdate (Result Http.Error T.App)
    | AppUpdate (Result Http.Error T.App)
    | ShowError String
    | ClearError
    | SetPlayerID T.PlayerID
    | RegisterPlayer
    | UpdateSaveMapName T.MapName
    | StartEditingMap
    | SaveMap
    | CancelEditingMap

    | StartCreatingCreature
    | CancelCreatingCreature
    | SetCreatureName String
    | SetCreatureClass String
    | CreateCreature T.CreatureCreation

    | StartCreatingScene
    | CancelCreatingScene
    | SetSceneName String
    | SetSceneMapName T.MapName
    | CreateScene T.Scene

    | CommandComplete (Result Http.Error T.RustResult)
    | ToggleSelectedCreature T.CreatureID
    | SelectAbility T.SceneName T.CreatureID T.AbilityID
    | CancelAbility
    | GotTargetOptions (Result Http.Error (List T.PotentialTarget))
    | CombatAct T.AbilityID T.DecidedTarget
    | ActCreature T.SceneName  T.CreatureID T.AbilityID T.DecidedTarget
    | RequestMove MovementRequest
    | CancelMovement
    | PathCurrentCombatCreature T.Point3
    | PathCreature T.SceneName T.CreatureID T.Point3
    | SetCreaturePos T.SceneName T.CreatureID T.Point3
    | GetMovementOptions T.SceneName T.Creature
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
    | MapZoom MapInOut
    | MapPan Direction
    | ToggleCollapsed String

    | NoMsg

type MapInOut
  = In | Out

type Direction
  = Left
  | Right
  | Up
  | Down

defaultModel : ProgramFlags -> Model
defaultModel flags =
  { app = Nothing
  , selectedAbility = Nothing
  , creatingCreature = Nothing
  , selectingCreatures = Nothing
  , moving = Nothing
  , error = ""
  , playerID = Nothing
  , potentialTargets = []
  , showOOC = False
  , moveAnywhere = False
  , showingMovement = NotShowingMovement
  , creatureNotes = Dict.empty
  , rpiURL = flags.rpi
  , gridSize = 60
  , gridOffset = {x = -15, y = 10}
  , collapsed = Dict.empty
  , creatingScene = Nothing
  , focus = NoFocus
  }

type Focus
  = NoFocus
  | Scene String
  | EditingMap T.MapName T.Map
  | PreviewMap T.MapName

devFlags : ProgramFlags
devFlags = {rpi = "http://localhost:1337/"}

type alias ProgramFlags =
  { rpi : String }

type alias PendingCreature = {name: Maybe T.CreatureID, class: Maybe String}

type alias Model =
  { app : Maybe T.App
  , creatingCreature : Maybe PendingCreature
  , creatingScene : Maybe T.Scene
  , selectedAbility : Maybe (T.SceneName, T.CreatureID, T.AbilityID)
  -- Creatures which have been selected for combat
  , selectingCreatures : Maybe (List T.CreatureID, GotCreatures, String)
  , error: String
  , moving: Maybe MovementRequest
  , playerID : Maybe T.PlayerID
  , potentialTargets: List T.PotentialTarget
  , showOOC: Bool
  , showingMovement: MovementAnimation
  , creatureNotes : Dict.Dict T.CreatureID String
  , moveAnywhere : Bool
  , rpiURL : String
  -- gridSize: how many SQUARE METERS to show
  , gridSize : Int
  -- gridOffset: offset in METERS
  , gridOffset : {x : Int, y: Int}
  , collapsed : Dict.Dict String Bool
  , focus: Focus
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

getScene : Model -> String -> Maybe T.Scene
getScene model name =
  case model.app of
    Just app -> Dict.get name app.current_game.scenes
    Nothing -> Nothing

getMap : Model -> T.Map
getMap model =
  case model.focus of
    EditingMap name terrain -> terrain
    PreviewMap name ->
      model.app
      |> Maybe.andThen (getMapNamed name)
      |> Maybe.withDefault []
    Scene name ->
      getMapForScene model name
    NoFocus -> []

getMapForScene : Model -> String -> T.Map
getMapForScene model name =
  getScene model name
  |> Maybe.andThen (\scene -> model.app |> Maybe.andThen (getMapNamed scene.map))
  |> Maybe.withDefault []


getMapNamed : String -> T.App -> Maybe T.Map
getMapNamed name app =
  Dict.get name app.current_game.maps

tryGetMapNamed : String -> T.App -> T.Map
tryGetMapNamed name app = getMapNamed name app |> Maybe.withDefault []
