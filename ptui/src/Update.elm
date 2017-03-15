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
  in {model2 | showingMovement = showingMovement}

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
          |> Maybe.withDefault (Debug.log ("Couldn't find index" ++ (toString cur)) Nothing)
      lastIdx = (Array.length data) - 1
  in walk lastIdx

start : Cmd Msg
start = message Start

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model = case msg of

  NoMsg -> (model, Cmd.none)

  Start -> (model, Http.send ReceivedAppUpdate (Http.get model.rpiURL T.appDecoder))

  MorePlease -> ( model, message PollApp)

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

  SetPlayerID pid -> ({model | playerID = Just pid}, Cmd.none)

  RegisterPlayer ->
    case model.playerID of
      Just playerID -> (model, sendCommand model.rpiURL (T.RegisterPlayer playerID))
      Nothing -> ({model | error = "Can't register without player ID"}, Cmd.none)

  SetFocus focus -> ({model | focus = focus}, Cmd.none)

  StartCreatingScene ->
    ({model | creatingScene = Just {name = "", map = "", creatures = Dict.empty}}, Cmd.none)
  CancelCreatingScene -> ({model | creatingScene = Nothing}, Cmd.none)
  SetSceneName name ->
    let newScene =
          case model.creatingScene of 
            Just cscene -> Just {cscene | name = name}
            Nothing -> Nothing
    in ({ model | creatingScene = newScene}, Cmd.none)
  SetSceneMapName mapName ->
    let newScene =
          case model.creatingScene of
            Just cscene -> Just { cscene | map = mapName}
            Nothing -> Nothing
    in ({model | creatingScene = newScene}, Cmd.none)
  CreateScene scene ->
    ({model | creatingScene = Nothing}, sendCommand model.rpiURL (T.EditScene scene))

  AddCreatureToScene sceneName cid ->
    (model, modScene model (\scene -> {scene | creatures = Dict.insert cid {x=0, y=0, z=0} scene.creatures}) sceneName)

  RemoveCreatureFromScene sceneName cid ->
    (model, modScene model (\scene -> {scene | creatures = Dict.remove cid scene.creatures}) sceneName)

  StartCreatingCreature ->
    ( {model | creatingCreature = Just {name = Nothing, class = Nothing}}
    , Cmd.none)
  CancelCreatingCreature -> ({model | creatingCreature = Nothing}, Cmd.none)

  SetCreatureName input ->
    let newName = if (String.isEmpty input) then Nothing else Just input
        newCreating =
          case model.creatingCreature of
            Just x -> Just {x | name = newName}
            Nothing -> Nothing
    in ( { model | creatingCreature = newCreating }, Cmd.none )

  SetCreatureClass input ->
    let newClass = if (String.isEmpty input) then Nothing else Just input
        newCreating =
          case model.creatingCreature of
            Just x -> Just {x | class = newClass}
            Nothing -> Nothing
    in ( {model | creatingCreature = newCreating}
       , Cmd.none)

  CreateCreature creation ->
    ({model | creatingCreature = Nothing}, sendCommand model.rpiURL (T.CreateCreature creation))

  CommandComplete (Ok (T.RustOk x)) -> Debug.log ("[COMMAND-COMPLETE] "++ (toString x)) (model, Cmd.none)
  CommandComplete (Ok (T.RustErr x)) -> ({model | error = toString x}, Cmd.none)
  CommandComplete (Err x) -> ({ model | error = toString x}, Cmd.none)

  AppUpdate (Ok newApp) ->
    let model2 = updateModelFromApp model newApp
    in ( { model2 | moving = Nothing , selectedAbility = Nothing }, Cmd.none )
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

  SelectCreatures creatureIds cb commandName ->
    ( { model | selectingCreatures = Just (creatureIds, [], cb, commandName)}, Cmd.none)

  ToggleSelectedCreature cid ->
    case model.selectingCreatures of
      Just (selectableCreatures, selectedCreatures, cb, descr) ->
        let newSelectedCreatures =
              if List.member cid selectedCreatures
              then List.filter (\c -> c /= cid) selectedCreatures
              else List.append selectedCreatures [cid]
            newSelectingCreatures = Just (selectableCreatures, newSelectedCreatures, cb, descr)
        in ( { model | selectingCreatures = newSelectingCreatures }, Cmd.none)
      Nothing -> ({model | error = "Can't select creature when not selecting creatures"}, Cmd.none)

  DoneSelectingCreatures ->
    case model.selectingCreatures of
      Just (allC, selectedCreatures, cb, _) -> 
        let cids = selectedCreatures
        in ( { model |selectingCreatures = Nothing}
           , cb cids)
      Nothing -> ( model , Cmd.none)

  CancelSelectingCreatures ->
    ( { model | selectingCreatures = Nothing}
    , Cmd.none)

  ToggleCollapsed name ->
    let currentlyCollapsed = Dict.get name model.collapsed |> Maybe.withDefault False
        newCollapsed = Dict.insert name (not currentlyCollapsed) model.collapsed
    in ({model | collapsed = newCollapsed}, Cmd.none)
  
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

  StartEditingMap -> 
    let focus =
          case model.focus of
            M.PreviewMap name -> case model.app of
              Just app -> M.EditingMap name (M.tryGetMapNamed name app)
              Nothing -> M.EditingMap name []
            _ -> M.NoFocus
    in ({model | focus = focus}, Cmd.none)
  CancelEditingMap ->
    let focus =
          case model.focus of
            M.EditingMap name _ -> M.PreviewMap name
            _ -> M.NoFocus
    in ({model | focus = focus}, Cmd.none)


  ToggleTerrain pt ->
    let focus =
          case model.focus of
            M.EditingMap name terrain ->
              let newTerrain = 
                    if not (List.member pt terrain)
                    then pt :: terrain
                    else List.filter (\el -> el /= pt) terrain
              in M.EditingMap name newTerrain
            x -> x
    in ({model | focus = focus}, Cmd.none)

  UpdateSaveMapName name ->
    let focus = case model.focus of
          M.EditingMap _ terrain -> M.EditingMap name terrain
          x -> x
    in ( {model | focus = focus } , Cmd.none)

  SaveMap ->
    let maybeNameAndTerrain = case model.focus of
          M.EditingMap name terrain -> Just (name, terrain)
          _ -> Nothing
        command = case maybeNameAndTerrain of
          Just (name, terrain) -> sendCommand model.rpiURL (T.EditMap name terrain)
          Nothing -> Cmd.none
        focus = case maybeNameAndTerrain of
          Just (name, _) -> M.PreviewMap name
          Nothing -> M.NoFocus
    in ( { model | focus = focus} , command)

  MapZoom zoom ->
    let newSize =
          case zoom of
            M.In -> model.gridSize - 5
            M.Out -> model.gridSize + 5
    in ({ model | gridSize = newSize}, Cmd.none)
  MapPan dir ->
    let offsetSize = 1 -- pan by meter increments
        newOffset =
          case dir of
            M.Right -> {x = model.gridOffset.x + offsetSize, y = model.gridOffset.y}
            M.Left -> {x = model.gridOffset.x - offsetSize, y = model.gridOffset.y}
            M.Up -> {x = model.gridOffset.x, y = model.gridOffset.y + offsetSize}
            M.Down -> {x = model.gridOffset.x, y = model.gridOffset.y - offsetSize}
    in ({ model | gridOffset = newOffset}, Cmd.none)

  SelectAbility sceneName cid abid ->
    let endpoint = model.rpiURL ++ "/target_options/" ++ Http.encodeUri sceneName ++ "/" ++ cid ++ "/" ++ abid
        req = Http.send GotTargetOptions (Http.get endpoint (JD.list T.potentialTargetDecoder))
    in ({ model | selectedAbility = Just (sceneName, cid, abid)}, req)

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
  ActCreature sceneName cid abid dtarget -> ({model | selectedAbility = Nothing}, sendCommand model.rpiURL (T.ActCreature sceneName cid abid dtarget))
  PathCurrentCombatCreature pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCurrentCombatCreature pt))
  PathCreature scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.PathCreature scene cid pt))
  SetCreaturePos scene cid pt -> ({model | moving = Nothing}, sendCommand model.rpiURL (T.SetCreaturePos scene cid pt))

toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

sendCommand : String -> T.GameCommand -> Cmd Msg
sendCommand url cmd =
  Debug.log ("[COMMAND] " ++ (toString cmd)) <|
  Http.send CommandComplete (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) T.rustResultDecoder)

modScene model fn sceneName =
  case model.app of
    Just app ->
      case Dict.get sceneName app.current_game.scenes of
        Just scene ->
          let newScene = fn scene
          in sendCommand model.rpiURL (T.EditScene newScene)
        Nothing -> Cmd.none
    Nothing -> Cmd.none
