module Update exposing (..)

import Array
import Dict
import Http
import Json.Decode as JD
import Json.Encode as JE
import Set
import Process
import Task
import Time

import DomUtils -- Even though we don't use this import, it must be imported for a port to be declared
import PanZoom
import Components
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
updateModelFromApp : M.Model -> T.App -> JD.Value -> (M.Model, M.Msg)
updateModelFromApp model newApp rawApp =
  let model2 = { model | app = Just newApp, raw_app = rawApp}
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
  -- TODO: fix animation
  in ( {model2 | -- showingMovement = showingMovement
                focus = focus}
     , mapChangeShenanigans model focus)

mapChangeShenanigans : M.Model -> M.Focus -> M.Msg
mapChangeShenanigans model newFocus =
  case (model.focus, newFocus) of
    (_, M.NoFocus) -> M.NoMsg
    (M.NoFocus, _) -> M.GridInitializePanZoom
    (M.EditingMap fp1 gd1, M.EditingMap fp2 gd2) ->
      if fp1 /= fp2 && gd1.map /= gd2.map then M.GridRefreshPanZoom else M.NoMsg
    _ -> if model.focus /= newFocus then M.GridRefreshPanZoom else M.NoMsg

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

renderComponent : M.Model -> M.Model -> String -> M.ReactComponent -> Cmd Msg
renderComponent oldModel newModel id componentType =
  if oldModel.app /= newModel.app || oldModel.reactComponents /= newModel.reactComponents || oldModel.playerID /= newModel.playerID then
    case componentType of
      M.ReactHistory -> Components.renderHistory (id, newModel.raw_app)
      M.ReactPlayers ->
        let scene = case newModel.focus of
                      M.FocusScene scene -> Just scene
                      _ -> Nothing
        in Components.renderPlayers (id, scene, newModel.raw_app)
      M.ReactTextInput -> Cmd.none
      M.ReactSideBar ->
        let
          _ = Debug.log "A ReactSideBar appears!" id
          scene =
            case newModel.focus of
              M.FocusScene sid -> Just sid
              _ -> Nothing
        in
          case newModel.playerID of
            Just pid ->
              let _ = Debug.log "[RENDERPLAYERUI]" pid
              in Components.renderPlayerUI (id, pid, scene, newModel.raw_app)
            Nothing -> Cmd.none
  else Cmd.none

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model =
  let (newModel, cmd) = update_ msg model
      refreshReactComponent (id, componentType) = renderComponent model newModel id componentType
      refreshReactComponents =
        List.map refreshReactComponent (Dict.toList newModel.reactComponents)
  in (newModel, Cmd.batch <| [cmd] ++ refreshReactComponents)

update_ : Msg -> M.Model -> (M.Model, Cmd Msg)
update_ msg model = case msg of

  NoMsg -> (model, Cmd.none)

  WindowResized s ->
    let maybeReinitMap =
          if (model.windowSize.width < 880 && s.width >= 880)
          || (model.windowSize.width >= 880 && s.width < 880)
          then 
            -- TODO: If we are switching from wide to narrow, and we already have a map on the
            -- screen, and we know that we will not have a map on the screen after this switch
            -- (model.selectedView != "Map"), then run PanZoom.destroyPanZoom.
            M.GridInitializePanZoom
          else M.NoMsg
    in ({model | windowSize = s}, message maybeReinitMap)

  Start -> (model, Http.send ReceivedAppUpdate (Http.get model.rpiURL (JD.map2 (,) T.appDecoder JD.value)))

  PollApp ->
    case model.app of
      Nothing -> (model, message Start)
      Just app ->
        let snapshotLength = Array.length app.snapshots
            logLength = Maybe.withDefault 0 (Maybe.map (\(g, logs) -> Array.length logs) <| Array.get (snapshotLength - 1) app.snapshots)
            url = model.rpiURL ++ "poll/" ++ (toString snapshotLength) ++ "/" ++ (toString logLength)
            cmd = Http.send ReceivedAppUpdate (Http.get url (JD.map2 (,) T.appDecoder JD.value))
        in (model, cmd)

  ReceivedAppUpdate (Ok (newApp, rawApp)) ->
    let (newModel, msg) = updateModelFromApp model newApp rawApp
    in (newModel, message (M.Batch [msg, PollApp]))
  ReceivedAppUpdate (Err x) ->
    let _ = Debug.log "[APP-ERROR] " x
    in ( { model | error = toString x}
       , delay Time.second PollApp )

  SetPlayerID pid ->
    -- TODO: This stuff shouldn't be in SetPlayerID anyway...
    let modelWPlayer = {model | playerID = Just pid}
        (newModel, msg) =
          case model.app of
            Just app -> updateModelFromApp modelWPlayer app model.raw_app
            Nothing -> (modelWPlayer, M.NoMsg)
    in (newModel, message msg)

  RegisterPlayer ->
    case model.playerID of
      Just playerID -> (model, sendCommand model.rpiURL (T.RegisterPlayer playerID))
      Nothing -> ({model | error = "Can't register without player ID"}, Cmd.none)

  SetFocus focus ->
    ({model | focus = focus}, message (mapChangeShenanigans model focus))

  SetSecondaryFocus f2 -> ({model | secondaryFocus = f2}, Cmd.none)

  SetModal m -> ({model | modal = m}, Cmd.none)

  Batch messages -> (model, Cmd.batch (List.map message messages))

  AppUpdate (Ok (newApp, rawApp)) ->
    let (model2, msg) = updateModelFromApp model newApp rawApp
    in ( { model2 | moving = Nothing , selectingAbility = Nothing }, message msg )
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

  SelectView name ->
    if name == model.selectedView then (model, Cmd.none) else 
    let
      onLoad =
        case (model.app, name) of
          (Just app, "History") -> message <| M.LoadComponent "history-view" M.ReactHistory
          (Just app, "Players") -> message <| M.LoadComponent "players-view" M.ReactPlayers
          (_, "Map") -> message M.GridInitializePanZoom
          _ -> Cmd.none
      onUnload = 
        case model.selectedView of
          "History" -> message <| M.UnloadComponent "history-view"
          "Players" -> message <| M.UnloadComponent "players-view"
          "Map" -> PanZoom.destroyPanZoom "#grid-svg"
          _ -> Cmd.none
    in ( {model | selectedView = name}, Cmd.batch [onLoad, onUnload])

  LoadComponent id componentType ->
    let newModel = {model | reactComponents = Dict.insert id componentType model.reactComponents}
    in ( newModel, Cmd.none ) --renderComponent newModel id componentType )

  UnloadComponent id ->
    ( {model | reactComponents = Dict.remove id model.reactComponents}
    , Components.unloadComponent id)

  GetMovementOptions sceneName creature ->
    if model.gridPanning then (model, Cmd.none) else
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

  GridInitializePanZoom -> (model, PanZoom.initializePanZoom "#grid-svg")
  GridRefreshPanZoom -> (model, PanZoom.updateBoundingBox "#grid-svg")
  GridPaint pt ->
    if model.gridPanning then
      ({model | gridPanning = False}, Cmd.none)
    else
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
  GridPanning bool ->
    ({model | gridPanning = bool}, Cmd.none)

  ToggleGridSpecial pt ->
    if model.gridPanning then
      ({model | gridPanning = False}, Cmd.none)
    else
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

  SelectVolumeTarget pt ->
    if model.gridPanning then (model, Cmd.none) else
    case (model.app, model.selectingAbility) of
      (Just app, Just {scene, ability}) ->
        case Dict.get ability app.current_game.abilities of
          Just {target} ->
            case target of
              T.AllCreaturesInVolumeInRange {volume} -> 
                let url = model.rpiURL ++ "affected_by_volume/" ++ scene ++ "/"
                      ++ toString pt.x ++ "/" ++ toString pt.y ++ "/" ++ toString pt.z
                    post = Http.post url
                                     (Http.jsonBody (T.volumeEncoder volume))
                                     (JD.map2 (,) (JD.index 0 (JD.list JD.string)) (JD.index 1 (JD.list T.point3Decoder)))
                in ( model, Http.send (GotCreaturesInVolume pt) post)
              _ -> (model, Cmd.none)
          Nothing -> (model, Cmd.none)
      _ -> (model, Cmd.none)

  GotCreaturesInVolume pt (Ok (cids, pts)) ->
    case model.selectingAbility of
      Just sa -> ({model | selectingAbility = Just {sa | chosenPoint = Just (pt, cids, pts)}}, Cmd.none)
      Nothing -> (model, Cmd.none)
  GotCreaturesInVolume pt (Err e) -> ({model | error = toString e}, Cmd.none)

  CancelMovement -> ({model | moving = Nothing}, Cmd.none)

  ToggleMoveAnywhere -> ({ model | moveAnywhere = not model.moveAnywhere}, Cmd.none)

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
    let post = Http.post (model.rpiURL ++ "/saved_games/" ++ name) Http.emptyBody (JD.succeed ())
    in (model, Http.send SavedGame post)
  SavedGame (Ok _) -> (model, Cmd.none)
  SavedGame (Err x) -> ({model | error = toString x}, Cmd.none)

  LoadGame name ->
    let post = Http.post (model.rpiURL ++ "/saved_games/" ++ name ++ "/load")
                         Http.emptyBody (JD.map2 (,) T.appDecoder JD.value)
    in (model, Http.send AppUpdate post)

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
  EditInitiativeFor x ->
    case (model.editingInitiative, x) of
      (_, Just (cid, i)) ->
        ( {model | editingInitiative = Just cid}
        , message <| LoadTextInput "focus-init" (toString i) (Dict.fromList [("width", "25px")]) True )
      (Just _, Nothing) ->
        ( {model | editingInitiative = Nothing}, Components.unloadComponent "focus-init")
      (Nothing, Nothing) -> (model, Cmd.none)
  EditCreatureNote mnote ->
    case (model.editingNote, mnote) of
      (_, Just (c, n)) ->
        ({model | editingNote = Just c}, message <| LoadTextInput "focus-note" n Dict.empty False)
      (Just _, Nothing) ->
        ({model | editingNote = Nothing}, Components.unloadComponent "focus-note" )
      (Nothing, Nothing) -> (model, Cmd.none)
  EditItemName x ->
    ({ model | editingItemName = x}, Cmd.none)
  UpdateScratchNote note ->
    ({model | scratchNote = Just note}, Cmd.none)

  PathCurrentCombatCreature pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCurrentCombatCreature pt))
  PathCreature scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCreature scene cid pt))
  SetCreaturePos scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.SetCreaturePos scene cid pt))

  -- External Components
  RenderHello id -> (model, Components.renderHello id)
  LoadTextInput id defaultValue style numbersOnly ->
    let styleValue = T.encodeStringDict JE.string style
    in (model, Components.renderTextInput (id, defaultValue, styleValue, numbersOnly))

  TextInputSubmit (id, content) ->
    if id == "focus-init" then
      case model.editingInitiative of
        Nothing -> (model, Cmd.none)
        Just cid ->
          case String.toInt content of
            Ok num -> 
              ( {model | editingInitiative = Nothing}
              , message (M.SendCommand (T.ChangeCreatureInitiative cid num)))
            Err _ -> (model, Cmd.none)
    else if id == "focus-note" then
      case model.editingNote of
        Nothing -> (model, Cmd.none)
        Just cid ->
          case model.app of
            Just app ->
              case T.getCreature app.current_game cid of
                Just creature ->
                  ( {model | editingNote = Nothing}
                  , message (M.SendCommand (T.EditCreature {creature | note = content})))
                Nothing -> (model, Cmd.none)
            Nothing -> (model, Cmd.none)
    else (model, Cmd.none)

  TextInputCancel (id, content) ->
    if id == "focus-init" then
      ({model | editingInitiative = Nothing}, Cmd.none)
    else if id == "focus-note" then
      ({model | editingNote = Nothing}, Cmd.none)
    else (model, Cmd.none)

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
