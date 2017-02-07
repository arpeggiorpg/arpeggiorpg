module Update exposing (..)

import Http
import Json.Decode as JD
import Set

import Model as M exposing (CreatureID, AbilityID)

type Msg
    = MorePlease
    | SelectMap M.MapName
    | SaveMapName M.MapName
    | EditMap M.Map
    | PendingCreatureId CreatureID
    | PendingCreatureName String
    | PendingCreatureClass String
    | CreateCreature M.CreatureCreation
    | CommandComplete (Result Http.Error M.RustResult)
    | AppUpdate (Result Http.Error M.App)
    | ShowError String
    | ToggleSelectedCreature CreatureID
    | StartCombat
    | StopCombat
    | AddToCombat CreatureID
    | RemoveFromCombat CreatureID
    | RemoveFromGame CreatureID
    | SelectAbility AbilityID
    | Act AbilityID M.DecidedTarget
    | RequestMove M.MovementRequest
    | CancelMovement
    | Move M.Point3
    | MoveOutOfCombat M.CreatureID M.Point3
    | TurnDone
    | GetMovementOptions M.Creature
    | GotMovementOptions M.Creature (Result Http.Error (List M.Point3))
    | ToggleTerrain M.Point3

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
    in ( { model2 | currentMap = currentMap }
       , Cmd.none )
  AppUpdate (Err x) -> Debug.log "Got an error from App" ( { model | error = toString x}, Cmd.none )
  
  ShowError s -> ( {model | error = s}, Cmd.none)
  
  ToggleSelectedCreature cid ->
    ( { model | pendingCombatCreatures = toggleSet cid model.pendingCombatCreatures }
    , Cmd.none )
  
  GetMovementOptions creature ->
    let endpoint = (url ++ "/movement_options/" ++ creature.id)
        cmd = Http.send (GotMovementOptions creature) (Http.get endpoint (JD.list M.point3Decoder))
    in (model, cmd)

  GotMovementOptions creature (Ok pts) ->
    let mreq = M.MovementRequest creature creature.speed pts
    in ({ model | moving = Just <| mreq}, Cmd.none)
  GotMovementOptions _ (Err e) -> ({ model | error = toString e}, Cmd.none)

  ToggleTerrain pt ->
    let terrain = if not (List.member pt model.currentMap)
                  then pt :: model.currentMap
                  else List.filter (\el -> el /= pt) model.currentMap
    in ({model | currentMap = terrain}, Cmd.none)

  SaveMapName name -> ({model | saveMapName = name}, Cmd.none)

  EditMap terrain -> (model, sendCommand (M.EditMap model.saveMapName terrain))

  -- Basic GameCommands
  CreateCreature creation -> (model, sendCommand (M.CreateCreature creation))
  RemoveFromGame cid -> (model, sendCommand (M.RemoveCreature cid))
  AddToCombat cid -> (model, sendCommand (M.AddCreatureToCombat cid))
  RemoveFromCombat cid -> (model, sendCommand (M.RemoveCreatureFromCombat cid))
  SelectAbility abid -> ({ model | selectedAbility = Just abid}, Cmd.none)
  Act abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand (M.Act abid dtarget))
  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)
  Move pt -> ({model | moving = Nothing}, sendCommand (M.Move pt))
  MoveOutOfCombat cid pt -> ({model | moving = Nothing}, sendCommand (M.MoveOutOfCombat cid pt))
  TurnDone -> (model, sendCommand M.Done)
  SelectMap mapName -> (model, sendCommand (M.SelectMap mapName))
  StartCombat -> (model, sendCommand (M.StartCombat (Set.toList model.pendingCombatCreatures)))
  StopCombat -> (model, sendCommand M.StopCombat)


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

url : String
url = "http://localhost:1337/"

refreshApp : Cmd Msg
refreshApp = Http.send AppUpdate (Http.get url M.appDecoder)

sendCommand : M.GameCommand -> Cmd Msg
sendCommand cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (M.gameCommandEncoder cmd)) M.rustResultDecoder)
