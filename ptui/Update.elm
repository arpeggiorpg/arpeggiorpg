module Update exposing (..)

import Http
import Json.Decode as JD
import Set

import Model as M exposing (CreatureID, AbilityID)

type Msg
    = MorePlease
    | SelectMap M.MapName
    | PendingCreatureId CreatureID
    | PendingCreatureName String
    | PendingCreatureClass String
    | CreateCreature
    | CommandComplete (Result Http.Error JD.Value)
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

update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model = case msg of

  MorePlease -> ( model, refreshApp)

  PendingCreatureId newId ->
    let oldPC = model.pendingCreature
    in ( { model | pendingCreature = {oldPC | id = Just newId } }
       , Cmd.none )
  PendingCreatureName newName ->
    let oldPC = model.pendingCreature
    in ( { model | pendingCreature = { oldPC | name = Just newName } }
       , Cmd.none )
  PendingCreatureClass newAS ->
    let oldPC = model.pendingCreature
    in ({model | pendingCreature = {oldPC | class = Just newAS}},
        Cmd.none)

  CommandComplete (Ok x) -> (model, refreshApp)
  CommandComplete (Err x) -> ({ model | error = toString x}, Cmd.none)

  AppUpdate (Ok newApp) -> Debug.log "Got an app" ( { model | app = (Just newApp) }, Cmd.none )
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

  -- Basic GameCommands
  AddToCombat cid -> (model, sendCommand (M.AddCreatureToCombat cid))
  RemoveFromCombat cid -> (model, sendCommand (M.RemoveCreatureFromCombat cid))
  RemoveFromGame cid -> (model, sendCommand (M.RemoveCreature cid))
  SelectAbility abid -> ({ model | selectedAbility = Just abid}, Cmd.none)
  Act abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand (M.Act abid dtarget))
  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)
  Move pt -> ({model | moving = Nothing}, sendCommand (M.Move pt))
  MoveOutOfCombat cid pt -> ({model | moving = Nothing}, sendCommand (M.MoveOutOfCombat cid pt))
  TurnDone -> (model, sendCommand M.Done)
  SelectMap mapName -> (model, sendCommand (M.SelectMap mapName))
  CreateCreature -> createCreature model model.pendingCreature
  StartCombat -> (model, sendCommand (M.StartCombat (Set.toList model.pendingCombatCreatures)))
  StopCombat -> (model, sendCommand M.StopCombat)


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

createCreature : M.Model -> M.PendingCreature -> (M.Model, Cmd Msg)
createCreature model pc =
  case (M.finalizePending pc) of
    Nothing -> ({ model | error = "Fill out the stuff."}, Cmd.none)
    Just creature -> (model, sendCommand (M.CreateCreature creature))

url : String
url = "http://localhost:1337/"

refreshApp : Cmd Msg
refreshApp = Http.send AppUpdate (Http.get url M.appDecoder)

sendCommand : M.GameCommand -> Cmd Msg
sendCommand cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (M.gameCommandEncoder cmd)) JD.value)
