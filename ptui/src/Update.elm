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

-- Merge a new app into an existing model.
-- This is where we handle various "transitionary" effects that rely on knowledge of both the old
-- and new state of the game.
-- e.g., when a player is becoming registered, or when we receive a new PathCreature GameLog.
updateModelFromApp : M.Model -> T.App -> M.Model
updateModelFromApp model newApp =
  let model2 = { model | app = Just newApp}
      showingMovement =
        case getLatestPath model newApp of
          Just (T.GLPathCreature _ _ (first::rest)) ->
            -- Only start animating it if we haven't already started animating it.
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
      focus =
        model.playerID
        |> Maybe.andThen (flip Dict.get newApp.players)
        |> Maybe.andThen (\p -> p.scene)
        |> Maybe.map M.FocusScene
        |> Maybe.withDefault model.focus
      newGridOffset = maybeResetGridOffset model focus 
  in {model2 | showingMovement = showingMovement
             , focus = focus}

maybeResetGridOffset : M.Model -> M.Focus -> {x: Float, y: Float}
maybeResetGridOffset model newFocus =
  let
    getRelevantCreatureID app sceneID =
      case model.playerID of
        Just pid ->
          List.head (T.getPlayerCreatures app pid)
          |> Maybe.map (.id)
        Nothing -> Nothing
    getCreatureOffset app sceneID cid =
      T.getScene app sceneID
      |> Maybe.andThen (T.getCreaturePos cid)
      |> Maybe.map (\pos -> {x=toFloat pos.x, y=toFloat pos.y})
    resetOffset app sceneID =
      getRelevantCreatureID app sceneID
      |> Maybe.andThen (getCreatureOffset app sceneID)
      |> Maybe.withDefault {x=-15.0, y=10.0} -- FIXME: magic constant, duplicated from Model.elm
  in
    case (model.app, model.focus) of
      (Just app, M.FocusScene oldSceneID) ->
        case newFocus of
          M.FocusScene newSceneID ->
            if oldSceneID /= newSceneID then Debug.log "[RESET-OFFSET]" (resetOffset app newSceneID)
            else model.gridOffset
          _ -> model.gridOffset
      (Just app, _) ->
        case model.focus of
          M.FocusScene sceneID -> resetOffset app sceneID
          _ -> model.gridOffset
      _ -> model.gridOffset

{-| Return the most recent PathCreature log item  -}
getLatestPath : M.Model -> T.App -> Maybe T.GameLog
getLatestPath model newApp =
  case model.app of
    Just oldApp ->
      let baseSnapIdx = (Array.length oldApp.snapshots) - 1
          baseLogIdx =
            Array.get baseSnapIdx oldApp.snapshots
              |> Maybe.map (\(_, l) -> Array.length l)
              |> Maybe.withDefault 0
          findLog (_, logs) = arrayRFind baseLogIdx checkPath logs
          checkPath log =
            case log of
              T.GLPathCreature _ _ _ -> Just log
              _ -> Nothing
      in arrayRFind baseSnapIdx findLog newApp.snapshots
    Nothing -> Nothing

{-| Search backwards through an array. -}
arrayRFind : Int -> (a -> Maybe b) -> Array.Array a -> Maybe b
arrayRFind limit fn data =
  let walk cur =
        Array.get cur data
          |> Maybe.map (\el ->
              case fn el of
                Just x -> Just x
                Nothing -> if cur - 1 == -1 then Nothing else walk (cur - 1))
          |> Maybe.withDefault Nothing
      lastIdx = (Array.length data) - 1
  in walk lastIdx

start : Cmd Msg
start = message Start

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model = case msg of

  NoMsg -> (model, Cmd.none)

  Start -> (model, Http.send ReceivedAppUpdate (Http.get model.rpiURL T.appDecoder))

  PollApp ->
    case model.app of
      Nothing -> (model, message Start)
      Just app ->
        let snapshotLength = Array.length app.snapshots
            logLength = Maybe.withDefault 0 (Maybe.map (\(g, logs) -> Array.length logs) <| Array.get (snapshotLength - 1) app.snapshots)
            url = model.rpiURL ++ "poll/" ++ (toString snapshotLength) ++ "/" ++ (toString logLength)
            cmd = Http.send ReceivedAppUpdate (Http.get url T.appDecoder)
        in (model, cmd)

  ReceivedAppUpdate (Ok newApp) -> (updateModelFromApp model newApp, message PollApp)
  ReceivedAppUpdate (Err x) ->
    let _ = Debug.log "[APP-ERROR] " x
    in ( { model | error = toString x}
       , delay Time.second PollApp )

  SetPlayerID pid ->
    -- TODO: This stuff shouldn't be in SetPlayerID anyway...
    let modelWPlayer = {model | playerID = Just pid}
        newModel =
          case model.app of
            Just app -> updateModelFromApp modelWPlayer app
            Nothing -> modelWPlayer
    in (newModel, Cmd.none)

  RegisterPlayer ->
    case model.playerID of
      Just playerID -> (model, sendCommand model.rpiURL (T.RegisterPlayer playerID))
      Nothing -> ({model | error = "Can't register without player ID"}, Cmd.none)

  SetFocus focus ->
    let
      newGridOffset = maybeResetGridOffset model focus
      m_ = {model | focus = focus, gridOffset = newGridOffset}
    in (m_, Cmd.none)

  SetSecondaryFocus f2 -> ({model | secondaryFocus = f2}, Cmd.none)

  SetModal m -> ({model | modal = m}, Cmd.none)

  Batch messages -> (model, Cmd.batch (List.map message messages))

  AppUpdate (Ok newApp) ->
    let model2 = updateModelFromApp model newApp
    in ( { model2 | moving = Nothing , selectingAbility = Nothing }, Cmd.none )
  AppUpdate (Err x) ->
    let _ = Debug.log "[APP-ERROR] " x
    in ({model | error = toString x}, Cmd.none)

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
  ClearError -> ({model | error = ""}, Cmd.none)

  ToggleCollapsed name ->
    let currentlyCollapsed = Dict.get name model.collapsed |> Maybe.withDefault False
        newCollapsed = Dict.insert name (not currentlyCollapsed) model.collapsed
    in ({model | collapsed = newCollapsed}, Cmd.none)

  ToggleFolderCollapsed name ->
    let currentlyCollapsed = Dict.get name model.folderState |> Maybe.withDefault False
        newCollapsed = Dict.insert name (not currentlyCollapsed) model.folderState
    in ({model | folderState = newCollapsed}, Cmd.none)

  SelectView category name ->
    let newSelected = Dict.insert category name model.selectedViews
    in ({model | selectedViews = newSelected}, Cmd.none)

  GetMovementOptions sceneName creature ->
    let endpoint = (model.rpiURL ++ "/movement_options/" ++ Http.encodeUri sceneName ++ "/" ++ creature.id)
        cmd = Http.send (GotMovementOptions creature) (Http.get endpoint (JD.list T.point3Decoder))
    in (model, cmd)

  GotMovementOptions creature (Ok pts) ->
    let mreq = M.MovementRequest creature.speed pts (Just creature)
    in ({ model | moving = Just mreq}, Cmd.none)
  GotMovementOptions _ (Err e) -> ({ model | error = toString e}, Cmd.none)

  GetCombatMovementOptions ->
    let endpoint = (model.rpiURL ++ "/combat_movement_options")
        cmd = Http.send GotCombatMovementOptions (Http.get endpoint (JD.list T.point3Decoder))
    in ({model | moveAnywhere = False}, cmd)

  GotCombatMovementOptions (Ok pts) ->
    case model.app of
      Just app ->
        case app.current_game.current_combat of
          Just combat ->
            let mreq = M.MovementRequest (T.combatCreature app.current_game combat).speed pts Nothing
            in ({model | moving = Just mreq}, Cmd.none)
          Nothing -> ({model | error = "No combat when receiving combat movement options"}, Cmd.none)
      Nothing -> ({model | error = "No app when receiving combat movement options"}, Cmd.none)
  GotCombatMovementOptions (Err e) -> ({model | error = toString e}, Cmd.none)

  GridPaint pt ->
    let tup = T.point3ToTup pt
    in case model.focus of
      M.EditingMap path gridData ->
        let
          map = gridData.map
          newGrid =
            case gridData.paintStyle of
              M.NoPaint -> gridData
              M.PaintTerrain ->
                let newT = (if Set.member tup map.terrain then Set.remove else Set.insert)
                           tup map.terrain
                in {gridData | map = {map | terrain = newT}}
              M.PaintSpecial special ->
                let newSpecials =
                      if Dict.member tup map.specials
                      then Dict.remove tup map.specials
                      else Dict.insert tup (special.color, special.note, special.vis) map.specials
                in {gridData | map = {map | specials = newSpecials}}
        in ({model | focus = M.EditingMap path newGrid}, Cmd.none)
      _ -> (model, Cmd.none)

  MapZoom zoom ->
    let newSize =
          case zoom of
            M.In -> model.gridSize - 5
            M.Out -> model.gridSize + 5
    in ({ model | gridSize = newSize}, Cmd.none)
  MapPan dir ->
    let offsetSize = 1
        newOffset =
          case dir of
            M.Right -> {x = model.gridOffset.x + offsetSize, y = model.gridOffset.y}
            M.Left -> {x = model.gridOffset.x - offsetSize, y = model.gridOffset.y}
            M.Up -> {x = model.gridOffset.x, y = model.gridOffset.y + offsetSize}
            M.Down -> {x = model.gridOffset.x, y = model.gridOffset.y - offsetSize}
    in ({ model | gridOffset = newOffset}, Cmd.none)

  ToggleGridSpecial pt ->
    let newExpanded =
          case model.gridSpecialExpanded of
            Just pt_ ->
              if pt_ == pt then Nothing
              else Just pt
            Nothing -> Just pt
    in ({model | gridSpecialExpanded = newExpanded}, Cmd.none)

  SelectAbility sa ->
    let endpoint = model.rpiURL ++ "/target_options/" ++ Http.encodeUri sa.scene ++ "/" ++ sa.creature ++ "/" ++ sa.ability
        req = Http.send GotTargetOptions (Http.get endpoint T.potentialTargetsDecoder)
    in ({ model | selectingAbility = Just sa}, req)

  CancelAbility -> ({model | selectingAbility = Nothing}, Cmd.none)

  GotTargetOptions (Ok potTargets) ->
    let newSA = case model.selectingAbility of
          Just oldSA -> Just {oldSA | potentialTargets = Just potTargets}
          Nothing -> Nothing
    in ({model | selectingAbility = newSA}, Cmd.none)
  GotTargetOptions (Err e) -> ({ model | error = toString e}, Cmd.none)

  RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
  CancelMovement -> ({model | moving = Nothing}, Cmd.none)

  ToggleMoveAnywhere -> ({ model | moveAnywhere = not model.moveAnywhere}, Cmd.none)

  SetCreatureNote cid note ->
    let newNotes = Dict.insert cid note model.creatureNotes
    in ({model | creatureNotes = newNotes}, Cmd.none)

  GetSavedGames cb ->
    ( {model | gettingSavedGames = Just cb}
    , Http.send GotSavedGames (Http.get (model.rpiURL ++ "/saved_games") (JD.list JD.string)))
  GotSavedGames (Ok ns) ->
    case model.gettingSavedGames of
      Just cb -> ({model | gettingSavedGames = Nothing}, message (cb ns))
      Nothing -> (model, Cmd.none)
  GotSavedGames (Err x) ->
    ({model | error = toString x}, Cmd.none)

  SaveGame name ->
    (model, Http.send SavedGame (Http.post (model.rpiURL ++ "/saved_games/" ++ name) Http.emptyBody (JD.succeed ())))
  SavedGame (Ok _) -> (model, Cmd.none)
  SavedGame (Err x) -> ({model | error = toString x}, Cmd.none)

  LoadGame name ->
    (model, Http.send AppUpdate (Http.post (model.rpiURL ++ "/saved_games/" ++ name ++ "/load") Http.emptyBody (T.appDecoder)))

  ShowGameLogs logs ->
    let newLogs =
          case model.modal of
            M.ModalShowGameLogs lgs -> lgs ++ logs
            _ -> logs
    in ({model | modal = M.ModalShowGameLogs newLogs}, Cmd.none)

  Lazy f -> (model, message <| f model)

  -- Basic GameCommands
  SendCommand cmd -> (model, sendCommand model.rpiURL cmd)

  CommandComplete (Ok (Ok x)) ->
    let _ = Debug.log ("[COMMAND-COMPLETE] "++ (toString x)) ()
    in (model, Cmd.none)
  CommandComplete (Ok (Err x)) -> ({model | error = toString x}, Cmd.none)
  CommandComplete (Err x) -> ({ model | error = toString x}, Cmd.none)

  SendCommandCB cmd cb -> (model, sendCommandCB model.rpiURL cmd cb)

  CommandCompleteCB cb (Ok (Ok x)) ->
    let _ = Debug.log ("[COMMAND-COMPLETE-CB] "++ (toString x)) ()
    in (model, message (cb model x))
  CommandCompleteCB _ (Ok (Err x)) -> ({model | error = toString x}, Cmd.none)
  CommandCompleteCB _ (Err x) -> ({ model | error = toString x}, Cmd.none)

  CombatAct abid dtarget -> ({model | selectingAbility = Nothing}, sendCommand model.rpiURL (T.CombatAct abid dtarget))
  ActCreature sceneName cid abid dtarget -> ({model | selectingAbility = Nothing}, sendCommand model.rpiURL (T.ActCreature sceneName cid abid dtarget))
  PathCurrentCombatCreature pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCurrentCombatCreature pt))
  PathCreature scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCreature scene cid pt))
  SetCreaturePos scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.SetCreaturePos scene cid pt))

  DragStart pos -> let _ = Debug.log ("[DragStart]" ++ toString pos) () in ({model | gridPanning = Just (pos, model.gridOffset)}, Cmd.none)
  DragAt pos ->
    case model.gridPanning of
      Just (startedDraggingPos, origGridOffset) ->
        let diffx = toFloat (pos.x - startedDraggingPos.x) / 10
            diffy = toFloat (pos.y - startedDraggingPos.y) / 10
            newGridOffset = {x=origGridOffset.x - diffx, y=origGridOffset.y + diffy}
        in ({model | gridOffset = newGridOffset}, Cmd.none)
      Nothing -> (model, Cmd.none)
  DragEnd pos -> let _ = Debug.log ("[DragEnd]" ++ toString pos) () in ({model | gridPanning = Nothing}, Cmd.none)

toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

sendCommand : String -> T.GameCommand -> Cmd Msg
sendCommand url cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) (T.resultDecoder JD.value JD.value))

sendCommandCB : String -> T.GameCommand -> (M.Model -> List T.GameLog -> Msg) -> Cmd Msg
sendCommandCB url cmd cb =
  let decoder = T.resultDecoder JD.value (JD.list T.gameLogDecoder)
  in Http.send
    (CommandCompleteCB cb)
    (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) decoder)
