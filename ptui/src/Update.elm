module Update exposing (..)

import Dict
import Http
import Json.Decode as JD
import Set

import Model as M exposing (CreatureID, AbilityID)

type Msg
    = MorePlease
    | SelectMap M.MapName
    | UpdateSaveMapName M.MapName
    | StartEditingMap
    | EditMap M.Map
    | CancelEditingMap
    | PendingCreatureId CreatureID
    | PendingCreatureName String
    | PendingCreatureClass String
    | CreateCreature M.CreatureCreation
    | CommandComplete (Result Http.Error M.RustResult)
    | AppUpdate (Result Http.Error M.App)
    | AppUpdateForPlayers (Result Http.Error M.App)
    | ShowError String
    | ToggleSelectedCreature CreatureID
    | StartCombat
    | StopCombat
    | AddToCombat CreatureID
    | RemoveFromCombat CreatureID
    | RemoveFromGame CreatureID
    | SelectAbility CreatureID AbilityID
    | GotTargetOptions (Result Http.Error (List M.PotentialTarget))
    | Act AbilityID M.DecidedTarget
    | RequestMove M.MovementRequest
    | CancelMovement
    | Move M.Point3
    | MoveOutOfCombat M.CreatureID M.Point3
    | TurnDone
    | GetMovementOptions M.Creature
    | GotMovementOptions M.Creature (Result Http.Error (List M.Point3))
    | ToggleTerrain M.Point3
    | TakeOverCreatures

update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model = case msg of

  MorePlease -> ( model, refreshApp)

  PendingCreatureId input ->
    let newId = if (String.isEmpty input) then Nothing else Just input
    in ( { model | pendingCreatureId = newId }
       , Cmd.none )
  PendingCreatureName input ->
    let newName = if (String.isEmpty input) then Nothing else Just input
    in ( { model | pendingCreatureName = newName }
       , Cmd.none )
  PendingCreatureClass input ->
    let newClass = if (String.isEmpty input) then Nothing else Just input
    in ( {model | pendingCreatureClass = newClass}
       , Cmd.none)

  CommandComplete (Ok (M.RustOk x)) -> Debug.log (toString x) (model, refreshApp)
  CommandComplete (Ok (M.RustErr x)) -> ({model | error = toString x}, refreshApp)
  CommandComplete (Err x) -> ({ model | error = toString x}, refreshApp)

  AppUpdate (Ok newApp) ->
    let model2 = { model | app = Just newApp}
        currentMap = M.getMap model2
    in ( { model2 | currentMap = currentMap
                  , moving = Nothing
                  , selectedAbility = Nothing }
       , Cmd.none )
  AppUpdate (Err x) -> Debug.log "Got an error from App" ( { model | error = toString x}, Cmd.none )

  -- This is just a stopgap for testing; initialize controlledCreatures to all out-of-combat creatures
  AppUpdateForPlayers (Err x) -> Debug.log "Got an error from App" ( { model | error = toString x}, Cmd.none )
  AppUpdateForPlayers (Ok newApp) -> 
    let model2 = { model | app = Just newApp}
        currentMap = M.getMap model2
        creatures = Dict.keys newApp.current_game.creatures
    in ( { model2 | currentMap = currentMap, controlledCreatures = Just creatures }
       , Cmd.none )
  
  ShowError s -> ( {model | error = s}, Cmd.none)
  
  ToggleSelectedCreature cid ->
    ( { model | selectedCreatures = toggleSet cid model.selectedCreatures }
    , Cmd.none )
  
  GetMovementOptions creature ->
    let endpoint = (url ++ "/movement_options/" ++ creature.id)
        cmd = Http.send (GotMovementOptions creature) (Http.get endpoint (JD.list M.point3Decoder))
    in (model, cmd)

  GotMovementOptions creature (Ok pts) ->
    let mreq = M.MovementRequest creature.speed pts (Just creature)
    in ({ model | moving = Just <| mreq}, Cmd.none)
  GotMovementOptions _ (Err e) -> ({ model | error = toString e}, Cmd.none)

  StartEditingMap ->  ({ model | editingMap = True},  Cmd.none)
  CancelEditingMap -> ({ model | editingMap = False}, Cmd.none)

  ToggleTerrain pt ->
    let terrain = if not (List.member pt model.currentMap)
                  then pt :: model.currentMap
                  else List.filter (\el -> el /= pt) model.currentMap
    in ({model | currentMap = terrain}, Cmd.none)

  UpdateSaveMapName name -> ( {model | saveMapName = name }
                            , Cmd.none)

  EditMap terrain -> ({ model | editingMap = False}, sendCommand (M.EditMap model.saveMapName terrain))

  TakeOverCreatures -> ( {model | controlledCreatures = Just (Set.toList model.selectedCreatures)}
                       , Cmd.none)

  SelectAbility cid abid ->
    let endpoint = url ++ "/target_options/" ++ cid ++ "/" ++ abid
        req = Http.send GotTargetOptions (Http.get endpoint (JD.list M.potentialTargetDecoder))
    in ({ model | selectedAbility = Just abid}, req)
  
  GotTargetOptions (Ok potTargets) -> ({model | potentialTargets = potTargets}, Cmd.none)
  GotTargetOptions (Err e) -> ({ model | error = toString e}, Cmd.none)

  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)


  -- Basic GameCommands
  CreateCreature creation -> (model, sendCommand (M.CreateCreature creation))
  RemoveFromGame cid -> (model, sendCommand (M.RemoveCreature cid))
  AddToCombat cid -> (model, sendCommand (M.AddCreatureToCombat cid))
  RemoveFromCombat cid -> (model, sendCommand (M.RemoveCreatureFromCombat cid))
  Act abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand (M.Act abid dtarget))
  Move pt -> ({model | moving = Nothing}, sendCommand (M.Move pt))
  MoveOutOfCombat cid pt -> ({model | moving = Nothing}, sendCommand (M.MoveOutOfCombat cid pt))
  TurnDone -> (model, sendCommand M.Done)
  SelectMap mapName -> (model, sendCommand (M.SelectMap mapName))
  StartCombat -> (model, sendCommand (M.StartCombat (Set.toList model.selectedCreatures)))
  StopCombat -> (model, sendCommand M.StopCombat)


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

url : String
url = "http://localhost:1337/"

refreshApp : Cmd Msg
refreshApp = Http.send AppUpdate (Http.get url M.appDecoder)

--- Initialize the application for players. When we receive the game state, we will set
--- controlledPlayers to all creatures already in the game. (TODO: this isn't what we need to do
--- for the real workflow)
refreshAppForPlayers : Cmd Msg
refreshAppForPlayers = Http.send AppUpdateForPlayers (Http.get url M.appDecoder)

sendCommand : M.GameCommand -> Cmd Msg
sendCommand cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (M.gameCommandEncoder cmd)) M.rustResultDecoder)
