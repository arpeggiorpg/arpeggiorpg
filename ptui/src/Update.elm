module Update exposing (..)

import Dict
import Http
import Json.Decode as JD
import Set
import Process
import Task
import Time

import Model as M exposing (Msg(..))
import Types as T exposing (CreatureID, AbilityID)

delay : Time.Time -> msg -> Cmd msg
delay time msg =
  Process.sleep time
  |> Task.andThen (always <| Task.succeed msg)
  |> Task.perform identity

message : msg -> Cmd msg
message msg = Task.perform (always msg) (Task.succeed ())

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model = case msg of

  MorePlease -> ( model, message PollApp)

  PollApp -> (model, Http.send ReceivedAppUpdate (Http.get (url ++ "poll") T.appDecoder))

  ReceivedAppUpdate (Ok newApp) ->
    let model2 = { model | app = Just newApp}
        currentMap = M.getMap model2
        showingMovement =
          case T.mostRecentLog newApp of
            Just (T.GLCombatLog (T.ComLPathCurrentCreature (first::rest))) -> Just ([first], (first::rest))
            _ -> Nothing
    in ( { model2 | currentMap = currentMap
                  , showingMovement = showingMovement}
       , delay Time.second PollApp )
  ReceivedAppUpdate (Err x) -> Debug.log "[APP-ERROR]"
    ( { model | error = toString x}
    , delay Time.second PollApp )

  SetPlayerID pid -> ({model | playerID = Just pid}, Cmd.none)

  RegisterPlayer ->
    case model.playerID of
      Just playerID -> (model, sendCommand (T.RegisterPlayer playerID))
      Nothing -> ({model | error = "Can't register without player ID"}, Cmd.none)

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

  CommandComplete (Ok (T.RustOk x)) -> Debug.log ("[COMMAND-COMPLETE] "++ (toString x)) (model, refreshApp)
  CommandComplete (Ok (T.RustErr x)) -> ({model | error = toString x}, refreshApp)
  CommandComplete (Err x) -> ({ model | error = toString x}, refreshApp)

  AppUpdate (Ok newApp) ->
    let model2 = { model | app = Just newApp}
        currentMap = M.getMap model2
        showingMovement =
          case T.mostRecentLog newApp of
            Just (T.GLCombatLog (T.ComLPathCurrentCreature (first::rest))) -> Just ([first], (first::rest))
            _ -> Nothing
    in ( { model2 | currentMap = currentMap
                  , moving = Nothing
                  , selectedAbility = Nothing
                  , showingMovement = showingMovement}
       , Cmd.none )
  AppUpdate (Err x) -> Debug.log "[APP-ERROR]" ( { model | error = toString x}, Cmd.none )
 
  Tick time ->
    let _ = Debug.log "[TICK]" ()
        showingMovement =
          case model.showingMovement of
            Just (soFar, total) -> 
              let newSoFar = List.take ((List.length soFar) + 1) total
              in if (List.length newSoFar) == (List.length total)
                 then Nothing
                 else Just (newSoFar, total)
            Nothing -> Nothing -- this shouldn't happen maybe
    in ({ model | showingMovement = showingMovement }, Cmd.none)

  ShowError s -> ( {model | error = s}, Cmd.none)

  SelectCreatures cb commandName ->
    ( { model | selectingCreatures = Just (cb, commandName)}, Cmd.none)
  ToggleSelectedCreature cid ->
    ( { model | selectedCreatures = toggleSet cid model.selectedCreatures }
    , Cmd.none )
  DoneSelectingCreatures ->
    case model.selectingCreatures of
      Just (cb, _) -> 
        let cids = Set.toList model.selectedCreatures
        in ( { model | selectedCreatures = Set.empty, selectingCreatures = Nothing}
           , cb cids)
      Nothing ->
        ( model , Cmd.none)
  CancelSelectingCreatures ->
    ( { model | selectedCreatures = Set.empty, selectingCreatures = Nothing}
    , Cmd.none)

  
  GetMovementOptions creature ->
    let endpoint = (url ++ "/movement_options/" ++ creature.id)
        cmd = Http.send (GotMovementOptions creature) (Http.get endpoint (JD.list T.point3Decoder))
    in (model, cmd)

  GotMovementOptions creature (Ok pts) ->
    let mreq = M.MovementRequest creature.speed pts (Just creature)
    in ({ model | moving = Just mreq}, Cmd.none)
  GotMovementOptions _ (Err e) -> ({ model | error = toString e}, Cmd.none)

  GetCombatMovementOptions ->
    let endpoint = (url ++ "/combat_movement_options")
        cmd = Http.send GotCombatMovementOptions (Http.get endpoint (JD.list T.point3Decoder))
    in (model, cmd)
  
  GotCombatMovementOptions (Ok pts) ->
    case model.app of
      Just app ->
        case app.current_game.current_combat of
          Just combat ->
            let mreq = M.MovementRequest (T.combatCreature combat).speed pts Nothing
            in ({model | moving = Just mreq}, Cmd.none)
          Nothing -> ({model | error = "No combat when receiving combat movement options"}, Cmd.none)
      Nothing -> ({model | error = "No app when receiving combat movement options"}, Cmd.none)
  GotCombatMovementOptions (Err e) -> ({model | error = toString e}, Cmd.none)

  StartEditingMap ->  ({ model | editingMap = True},  Cmd.none)
  CancelEditingMap -> ({ model | editingMap = False}, Cmd.none)

  ToggleTerrain pt ->
    let terrain = if not (List.member pt model.currentMap)
                  then pt :: model.currentMap
                  else List.filter (\el -> el /= pt) model.currentMap
    in ({model | currentMap = terrain}, Cmd.none)

  UpdateSaveMapName name -> ( {model | saveMapName = name }
                            , Cmd.none)

  EditMap terrain -> ({ model | editingMap = False}, sendCommand (T.EditMap model.saveMapName terrain))

  SelectAbility cid abid ->
    let endpoint = url ++ "/target_options/" ++ cid ++ "/" ++ abid
        req = Http.send GotTargetOptions (Http.get endpoint (JD.list T.potentialTargetDecoder))
    in ({ model | selectedAbility = Just (cid, abid)}, req)
  
  CancelAbility -> ({model | selectedAbility = Nothing}, Cmd.none)

  GotTargetOptions (Ok potTargets) -> ({model | potentialTargets = potTargets}, Cmd.none)
  GotTargetOptions (Err e) -> ({ model | error = toString e}, Cmd.none)

  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)

  ToggleShowOOC -> ({model | showOOC = not model.showOOC}, Cmd.none)

  SetCreatureNote cid note ->
    let newNotes = Dict.insert cid note model.creatureNotes
    in ({model | creatureNotes = newNotes}, Cmd.none)

  -- Basic GameCommands
  SendCommand cmd -> (model, sendCommand cmd)
  CreateCreature creation -> (model, sendCommand (T.CreateCreature creation))
  RemoveFromGame cid -> (model, sendCommand (T.RemoveCreature cid))
  AddToCombat cid -> (model, sendCommand (T.AddCreatureToCombat cid))
  RemoveFromCombat cid -> (model, sendCommand (T.RemoveCreatureFromCombat cid))
  CombatAct abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand (T.CombatAct abid dtarget))
  ActCreature cid abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand (T.ActCreature cid abid dtarget))
  PathCurrentCombatCreature pt -> ({model | moving = Nothing}, sendCommand (T.PathCurrentCombatCreature pt))
  PathCreature cid pt -> ({model | moving = Nothing}, sendCommand (T.PathCreature cid pt))
  TurnDone -> (model, sendCommand T.Done)
  SelectMap mapName -> (model, sendCommand (T.SelectMap mapName))
  StopCombat -> (model, sendCommand T.StopCombat)
  Rollback snapIdx logIdx -> (model, sendCommand (T.Rollback snapIdx logIdx))


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

url : String
url = "http://localhost:1337/"

refreshApp : Cmd Msg
refreshApp = Http.send AppUpdate (Http.get url T.appDecoder)

sendCommand : T.GameCommand -> Cmd Msg
sendCommand cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) T.rustResultDecoder)
