module Model exposing (..)

import Dict
import Http
import Keyboard
import Keyboard.Key as Key
import Time
import Json.Decode as JD

import Types as T


type alias GotCreatures = List T.CreatureID -> Msg

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
    | Batch (List Msg)
    | SetFocus Focus
    | SetSecondaryFocus SecondaryFocus
    | SetModal Modal
    | PollApp
    | ReceivedAppUpdate (Result Http.Error T.App)
    | AppUpdate (Result Http.Error T.App)
    | ShowError String
    | ClearError
    | SetPlayerID T.PlayerID
    | RegisterPlayer

    | ToggleTerrain T.Point3

    | SelectAbility SelectingAbility
    | CancelAbility
    | GotTargetOptions (Result Http.Error T.PotentialTargets)
    | CombatAct T.AbilityID T.DecidedTarget
    | ActCreature T.SceneID  T.CreatureID T.AbilityID T.DecidedTarget
    | RequestMove MovementRequest
    | CancelMovement
    | PathCurrentCombatCreature T.Point3
    | PathCreature T.SceneID T.CreatureID T.Point3
    | SetCreaturePos T.SceneID T.CreatureID T.Point3
    | GetMovementOptions T.SceneID T.Creature
    | GetCombatMovementOptions
    | GotCombatMovementOptions (Result Http.Error (List T.Point3))
    | GotMovementOptions T.Creature (Result Http.Error (List T.Point3))
    | ToggleMoveAnywhere
    | Tick Time.Time
    | SendCommand T.GameCommand
    | CommandComplete (Result Http.Error (Result JD.Value JD.Value))
    | SendCommandCB T.GameCommand (Model -> List T.GameLog -> Msg)
    | CommandCompleteCB (Model -> List T.GameLog -> Msg) (Result Http.Error (Result JD.Value (List T.GameLog)))

    | GetSavedGames (List String -> Msg)
    | GotSavedGames (Result Http.Error (List String))
    | SaveGame String
    | SavedGame (Result Http.Error ())
    | LoadGame String
    | SetCreatureNote T.CreatureID String
    | MapZoom MapInOut
    | MapPan Direction
    | ToggleCollapsed String
    | SelectView String String

    | ShowGameLogs (List T.GameLog)

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
  , selectingAbility = Nothing
  , moving = Nothing
  , error = ""
  , playerID = Nothing
  , moveAnywhere = False
  , showingMovement = NotShowingMovement
  , creatureNotes = Dict.empty
  , rpiURL = flags.rpi
  , gridSize = 60
  , gridOffset = {x = -15, y = 10}
  , collapsed = Dict.empty
  , selectedViews = Dict.empty
  , focus = NoFocus
  , secondaryFocus = Focus2None
  , modal = NoModal
  , gettingSavedGames = Nothing
  }

type alias Model =
  { app : Maybe T.App
  , selectingAbility : Maybe SelectingAbility
  -- Creatures which have been selected for combat
  , error: String
  , moving: Maybe MovementRequest
  , playerID : Maybe T.PlayerID
  , showingMovement: MovementAnimation
  , creatureNotes : Dict.Dict T.CreatureID String
  , moveAnywhere : Bool
  , rpiURL : String
  -- gridSize: how many SQUARE METERS to show
  , gridSize : Int
  -- gridOffset: offset in METERS
  , gridOffset : {x : Int, y: Int}
  , collapsed : Dict.Dict String Bool
  , selectedViews : Dict.Dict String String
  , focus: Focus
  , secondaryFocus: SecondaryFocus
  , modal: Modal
  , gettingSavedGames: Maybe (List String -> Msg)
  }

type alias SelectingAbility = 
  { scene: T.SceneID
  , creature: T.CreatureID
  , ability: T.AbilityID
  , potentialTargets: Maybe T.PotentialTargets
  }

type Focus
  = NoFocus
  | Scene String
  | EditingMap T.FolderPath T.Map (Maybe (String, String, T.Visibility))
  | PreviewMap T.MapID

type SecondaryFocus
  = Focus2None
  | Focus2Creature T.FolderPath T.CreatureID
  | Focus2Note T.FolderPath String T.Note
  | Focus2Map T.FolderPath T.MapID
  | Focus2Scene T.FolderPath T.SceneID

type Modal
  = NoModal
  | CreateFolder CreatingFolder
  | CreateCreature PendingCreature
  | CreateScene CreatingScene
  | CreateMap CreatingMap
  | MoveFolderItem MovingFolderItem
  | RenameFolder RenamingFolder
  | SelectOrderedCreatures SelectingOrderedCreatures
  | SelectCreaturesFromCampaign SelectingCreatures
  | ModalLoadGame (List String)
  | ModalSaveGame SavingGame
  | ModalEditCreature EditingCreature
  | ModalSimpleSelectCreatures SimpleSelectingCreatures
  | ModalShowGameLogs (List T.GameLog)
  | ModalAdHocChallenge SceneChallenge
  | ModalCreateNewChallenge SceneChallenge

type alias SavingGame = {existing: List String, newGame: String}
type alias CreatingFolder = {parent: T.FolderPath , child: String}
type alias CreatingScene = {path: T.FolderPath , scene: T.SceneCreation}
type alias CreatingMap = {path: T.FolderPath, name: String}
type alias MovingFolderItem = {src: T.FolderPath, item: T.FolderItemID, dst: T.FolderPath}
type alias RenamingFolder = {path: T.FolderPath, newName: String}
type alias SelectingOrderedCreatures = {from: List T.CreatureID, selected: Dict.Dict T.CreatureID Int, cb: GotCreatures, title: String}
type alias SelectingCreatures = {cb: GotCreatures, reason: String, selectedCreatures : List T.CreatureID}
type alias SimpleSelectingCreatures = {from: List T.CreatureID, selected: List T.CreatureID, cb: GotCreatures, title: String}
type alias EditingCreature = {cid: T.CreatureID, note: String, portrait_url: String}
type alias SceneChallenge = {scene: T.SceneID, description: String, check: T.AttrCheck}

devFlags : ProgramFlags
devFlags = {rpi = "http://localhost:1337/"}

type alias ProgramFlags =
  { rpi : String }

type alias PendingCreature = {name: Maybe T.CreatureID, class: Maybe String, path: T.FolderPath}

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

type alias FolderItem =
  { key: T.FolderItemID
  , path: T.FolderPath
  , prettyName : String
  }

getScene : Model -> String -> Maybe T.Scene
getScene model name =
  case model.app of
    Just app -> Dict.get name app.current_game.scenes
    Nothing -> Nothing

getMap : Model -> T.Map
getMap model =
  case model.focus of
    EditingMap _ map _ -> map
    PreviewMap name ->
      model.app
      |> Maybe.andThen (getMapNamed name)
      |> Maybe.withDefault T.emptyMap
    Scene name ->
      getMapForScene model name
    NoFocus -> T.emptyMap

getMapForScene : Model -> String -> T.Map
getMapForScene model name =
  getScene model name
  |> Maybe.andThen (\scene -> model.app |> Maybe.andThen (getMapNamed scene.map))
  |> Maybe.withDefault T.emptyMap


getMapNamed : String -> T.App -> Maybe T.Map
getMapNamed name app =
  Dict.get name app.current_game.maps

tryGetMapNamed : String -> T.App -> T.Map
tryGetMapNamed name app = getMapNamed name app |> Maybe.withDefault T.emptyMap
