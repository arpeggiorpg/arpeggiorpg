module Update exposing (..)

import Array
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

updateModelFromApp : M.Model -> T.App -> M.Model
updateModelFromApp model newApp =
  let model2 = { model | app = Just newApp}
      currentMap = if model.editingMap then model.currentMap else M.getMap model2
      showingMovement =
        case T.mostRecentLog newApp of
          Just (T.GLCombatLog (T.ComLPathCurrentCreature (first::rest))) ->
            -- most recent action was movement. Only start animating it if we haven't already
            -- started animating it.
            case model.showingMovement of
              M.ShowingMovement alreadyShown toShow ->
                if (alreadyShown ++ toShow) /= (first::rest)
                then M.ShowingMovement [first] (first::rest)
                else M.ShowingMovement alreadyShown toShow
              M.DoneShowingMovement shown ->
                if shown /= (first::rest)
                then M.ShowingMovement [first] rest
                else M.DoneShowingMovement shown
              M.NotShowingMovement -> M.ShowingMovement [first] rest
          _ -> M.NotShowingMovement
  in { model2 | currentMap = currentMap, showingMovement = showingMovement}



start = message Start

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model = case msg of

  Start -> ( model, Http.send ReceivedAppUpdate (Http.get model.rpiURL T.appDecoder) )

  MorePlease -> ( model, message PollApp)

  PollApp ->
    case model.app of
      Nothing -> (model, message Start)
      Just app -> 
        let snapshotLength = Array.length app.snapshots
            logLength = Maybe.withDefault 0 (Maybe.map (\(g, logs) -> List.length logs) <| Array.get (snapshotLength - 1) app.snapshots)
            url = model.rpiURL ++ "poll/" ++ (toString snapshotLength) ++ "/" ++ (toString logLength)
            cmd = Http.send ReceivedAppUpdate (Http.get url T.appDecoder)
        in (model, cmd)

  ReceivedAppUpdate (Ok newApp) -> (updateModelFromApp model newApp, message PollApp)
  ReceivedAppUpdate (Err x) -> Debug.log "[APP-ERROR]"
    ( { model | error = toString x}
    , delay Time.second PollApp )

  SetPlayerID pid -> ({model | playerID = Just pid}, Cmd.none)

  RegisterPlayer ->
    case model.playerID of
      Just playerID -> (model, sendCommand model.rpiURL (T.RegisterPlayer playerID))
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

  CommandComplete (Ok (T.RustOk x)) -> Debug.log ("[COMMAND-COMPLETE] "++ (toString x)) (model, Cmd.none)
  CommandComplete (Ok (T.RustErr x)) -> ({model | error = toString x}, Cmd.none)
  CommandComplete (Err x) -> ({ model | error = toString x}, Cmd.none)

  AppUpdate (Ok newApp) ->
    let model2 = updateModelFromApp model newApp
    in ( { model2 | moving = Nothing , selectedAbility = Nothing }, Cmd.none )
  AppUpdate (Err x) -> Debug.log "[APP-ERROR]" ( { model | error = toString x}, Cmd.none )
 
  Tick time ->
    let _ = Debug.log "[TICK]" ()
        showingMovement =
          case model.showingMovement of
            M.ShowingMovement soFar rest -> 
              let newSoFar = soFar ++ (List.take 1 rest)
                  newRest = List.drop 1 rest
              in if (List.length newRest) == 0
                 then M.DoneShowingMovement newSoFar
                 else M.ShowingMovement newSoFar newRest
            x -> x -- this shouldn't happen maybe
    in ({ model | showingMovement = showingMovement }, Cmd.none)

  ShowError s -> ( {model | error = s}, Cmd.none)

  SelectCreatures cb commandName ->
    ( { model | selectingCreatures = Just ([], cb, commandName)}, Cmd.none)

  ToggleSelectedCreature cid ->
    case model.selectingCreatures of
      Just (selectedCreatures, cb, descr) ->
        let newSelectedCreatures =
              if List.member cid selectedCreatures
              then List.filter (\c -> c /= cid) selectedCreatures
              else List.append selectedCreatures [cid]
            newSelectingCreatures = Just (newSelectedCreatures, cb, descr)
        in ( { model | selectingCreatures = newSelectingCreatures }, Cmd.none)
      Nothing -> ({model | error = "Can't select creature when not selecting creatures"}, Cmd.none)

  DoneSelectingCreatures ->
    case model.selectingCreatures of
      Just (selectedCreatures, cb, _) -> 
        let cids = selectedCreatures
        in ( { model |selectingCreatures = Nothing}
           , cb cids)
      Nothing -> ( model , Cmd.none)

  CancelSelectingCreatures ->
    ( { model | selectingCreatures = Nothing}
    , Cmd.none)

  GetMovementOptions creature ->
    let endpoint = (model.rpiURL ++ "/movement_options/" ++ creature.id)
        cmd = Http.send (GotMovementOptions creature) (Http.get endpoint (JD.list T.point3Decoder))
    in (model, cmd)

  GotMovementOptions creature (Ok pts) ->
    let mreq = M.MovementRequest creature.speed pts (Just creature)
    in ({ model | moving = Just mreq}, Cmd.none)
  GotMovementOptions _ (Err e) -> ({ model | error = toString e}, Cmd.none)

  GetCombatMovementOptions ->
    let endpoint = (model.rpiURL ++ "/combat_movement_options")
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

  EditMap terrain -> ({ model | editingMap = False}, sendCommand model.rpiURL (T.EditMap model.saveMapName terrain))

  SelectAbility cid abid ->
    let endpoint = model.rpiURL ++ "/target_options/" ++ cid ++ "/" ++ abid
        req = Http.send GotTargetOptions (Http.get endpoint (JD.list T.potentialTargetDecoder))
    in ({ model | selectedAbility = Just (cid, abid)}, req)

  CancelAbility -> ({model | selectedAbility = Nothing}, Cmd.none)

  GotTargetOptions (Ok potTargets) -> ({model | potentialTargets = potTargets}, Cmd.none)
  GotTargetOptions (Err e) -> ({ model | error = toString e}, Cmd.none)

  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)

  ToggleShowOOC -> ({model | showOOC = not model.showOOC}, Cmd.none)

  ToggleMoveAnywhere -> ({ model | moveAnywhere = not model.moveAnywhere}, Cmd.none)

  SetCreatureNote cid note ->
    let newNotes = Dict.insert cid note model.creatureNotes
    in ({model | creatureNotes = newNotes}, Cmd.none)

  -- Basic GameCommands
  SendCommand cmd -> (model, sendCommand model.rpiURL cmd)
  CombatAct abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand model.rpiURL (T.CombatAct abid dtarget))
  ActCreature cid abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand model.rpiURL (T.ActCreature cid abid dtarget))
  PathCurrentCombatCreature pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCurrentCombatCreature pt))
  PathCreature cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCreature cid pt))

toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

sendCommand : String -> T.GameCommand -> Cmd Msg
sendCommand url cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) T.rustResultDecoder)
