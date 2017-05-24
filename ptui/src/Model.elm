module Model exposing (..)

import Dict
import Http
import Time
import Json.Decode as JD
import Window

import Types as T
import PanZoom

type alias GotCreatures = List T.CreatureID -> Msg

subscriptions : Model -> Sub Msg
subscriptions model =
  let ticks =
        case model.showingMovement of
          ShowingMovement _ _ -> Time.every (Time.second / 4) Tick
          _ -> Sub.none
  in Sub.batch [ticks, PanZoom.panning GridPanning, Window.resizes WindowResized]

type Msg
    = Start
    | WindowResized Window.Size
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

    | GridPaint T.Point3

    -- This is to refesh the PanZoom state when we change the svg significantly (in a way that can
    -- change the bounding box of all elements)
    | GridRefreshPanZoom -- svg-pan-zoom: updateBBox
    -- This is to initialize the PanZoom state when we first render SVG to the screen.
    | GridInitializePanZoom -- svg-pan-zoom: svgPanZoom
    | GridPanning Bool

    -- Ability-related messages
    | SelectAbility SelectingAbility
    | CancelAbility
    | GotTargetOptions (Result Http.Error T.PotentialTargets)
    | SelectVolumeTarget T.Point3
    | GotCreaturesInVolume T.Point3 (Result Http.Error (List T.CreatureID, List T.Point3))
    | CombatAct T.AbilityID T.DecidedTarget
    | ActCreature T.SceneID  T.CreatureID T.AbilityID T.DecidedTarget

    | EditInitiativeFor (Maybe (T.CreatureID, Int))

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
    | ToggleGridSpecial T.Point3
    | ToggleCollapsed String
    | ToggleFolderCollapsed String
    | SelectView String String (Maybe Msg)

    | ShowGameLogs (List T.GameLog)

    | Lazy (Model -> Msg)

    | NoMsg

defaultModel : ProgramFlags -> Model
defaultModel flags =
  { app = Nothing
  , windowSize = let (w, h) = flags.windowSize in {width = w, height = h}
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
  , gridSpecialExpanded = Nothing
  , gridPanning = False
  , collapsed = Dict.empty
  , selectedViews = Dict.empty
  , focus = NoFocus
  , secondaryFocus = Focus2None
  , modal = NoModal
  , gettingSavedGames = Nothing
  , folderState = Dict.empty
  , editingInitiative = Nothing
  }

type alias Model =
  { app : Maybe T.App
  , windowSize: Window.Size
  , selectingAbility : Maybe SelectingAbility
  , error: String
  , moving: Maybe MovementRequest
  , playerID : Maybe T.PlayerID
  , showingMovement: MovementAnimation
  , creatureNotes : Dict.Dict T.CreatureID String
  , moveAnywhere : Bool
  , rpiURL : String
  -- gridSize: how many SQUARE METERS to show
  , gridSize: Int
  -- gridOffset: offset in METERS
  , gridOffset: {x: Float, y: Float}
  , gridSpecialExpanded: Maybe T.Point3
  , gridPanning: Bool
  , collapsed : Dict.Dict String Bool
  , folderState: FolderState
  , selectedViews : Dict.Dict String String
  , focus: Focus
  , secondaryFocus: SecondaryFocus
  , modal: Modal
  , gettingSavedGames: Maybe (List String -> Msg)
  , editingInitiative: Maybe (T.CreatureID, Int)
  }

type alias GridData =
  { paintStyle: PaintStyle
  , map: T.Map
  }

type alias FolderState = Dict.Dict String Bool

type alias SelectingAbility =
  { scene: T.SceneID
  , creature: T.CreatureID
  , ability: T.AbilityID
  , potentialTargets: Maybe T.PotentialTargets
  -- chosenPoint is data about what is affected by the current selected target. This is
  -- populated after the player chooses a point for a volume-affecting ability, and is used to show
  -- the player who and what will be affected before they confirm the ability.
  , chosenPoint : Maybe (T.Point3, List T.CreatureID, List T.Point3)
  }

type Focus
  = NoFocus
  | FocusScene String
  | EditingMap T.FolderPath GridData
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
type alias EditingCreature =
  { cid: T.CreatureID
  , name: String
  , note: String
  , portrait_url: String
  , initiative: T.Dice
  }
type alias SceneChallenge = {scene: T.SceneID, description: String, check: T.AttrCheck}


devFlags : ProgramFlags
devFlags = {rpi = "http://localhost:1337/", windowSize = (0, 0)}

type alias ProgramFlags =
  { rpi : String, windowSize: (Int, Int) }

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

-- Information about a creature that is relevant to the map.
type alias MapCreature =
  { creature: T.Creature
  , highlight : Maybe Highlight
  , clickable : Maybe (T.Creature -> Msg)
  , class : T.Class
  , pos : T.Point3
  , visible: Bool
  }

type Highlight
  = Moving
  | Targetable
  | Current
  | Affected

type PaintStyle
  = NoPaint
  | PaintTerrain
  | PaintSpecial {color: T.Color, note: String, vis: T.Visibility}


getScene : Model -> String -> Maybe T.Scene
getScene model name =
  case model.app of
    Just app -> Dict.get name app.current_game.scenes
    Nothing -> Nothing

getMap : Model -> T.Map
getMap model =
  case model.focus of
    EditingMap _ gridData -> gridData.map
    PreviewMap name ->
      model.app
      |> Maybe.andThen (getMapNamed name)
      |> Maybe.withDefault T.emptyMap
    FocusScene name ->
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
