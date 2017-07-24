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
mapChangeShenanigans model newFocus = M.NoMsg

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
            in Components.renderPlayerUI (id, newModel.rpiURL, pid, scene, newModel.raw_app)
          Nothing -> Cmd.none

update : Msg -> M.Model -> (M.Model, Cmd Msg)
update msg model =
  let (newModel, cmd) = update_ msg model
      refreshReactComponent (id, componentType) = renderComponent model newModel id componentType
      refreshReactComponents =
        if model.app /= newModel.app || model.reactComponents /= newModel.reactComponents || model.playerID /= newModel.playerID then
          (if newModel.mainReactComponent /= "" then Components.renderReactMain (newModel.mainReactElement, newModel.rpiURL, newModel.mainReactComponent, newModel.raw_app) else Cmd.none)
          :: (List.map refreshReactComponent (Dict.toList newModel.reactComponents))
        else []
  in (newModel, Cmd.batch <| [cmd] ++ refreshReactComponents)

update_ : Msg -> M.Model -> (M.Model, Cmd Msg)
update_ msg model = case msg of

  NoMsg -> (model, Cmd.none)

  WindowResized s -> (model, Cmd.none)

  Start ->
    let _ = Debug.log "[Update:Start] Starting up Elm app!" ()
    in (model, Http.send ReceivedAppUpdate (Http.get model.rpiURL (JD.map2 (,) T.appDecoder JD.value)))

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

  Batch messages -> (model, Cmd.batch (List.map message messages))

  AppUpdate (Ok (newApp, rawApp)) ->
    let (model2, msg) = updateModelFromApp model newApp rawApp
    in ( { model2 | moving = Nothing , selectingAbility = Nothing }, message msg )
  AppUpdate (Err x) ->
    let _ = Debug.log "[APP-ERROR] " x
    in ({model | error = toString x}, Cmd.none)


  LoadComponent id componentType ->
    let newModel = {model | reactComponents = Dict.insert id componentType model.reactComponents}
    in ( newModel, Cmd.none ) --renderComponent newModel id componentType )

  UnloadComponent id ->
    ( {model | reactComponents = Dict.remove id model.reactComponents}
    , Components.unloadComponent id)

  Lazy f -> (model, message <| f model)

  -- Basic GameCommands
  SendCommand cmd -> (model, sendCommand model.rpiURL cmd)
  SendCommandRaw cmd -> (model, sendCommandRaw model.rpiURL cmd)

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

  _ -> (model, Cmd.none)

toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

sendCommand : String -> T.GameCommand -> Cmd Msg
sendCommand url cmd =
  sendCommandRaw url (T.gameCommandEncoder cmd)

sendCommandRaw : String -> JE.Value -> Cmd Msg
sendCommandRaw url cmd =
  let _ = Debug.log "[sendCommandRaw]" cmd in
  Http.send CommandComplete (Http.post url (Http.jsonBody cmd) (T.resultDecoder JD.value JD.value))

sendCommandCB : String -> T.GameCommand -> (M.Model -> List T.GameLog -> Msg) -> Cmd Msg
sendCommandCB url cmd cb =
  let decoder = T.resultDecoder JD.value (JD.list T.gameLogDecoder)
  in Http.send
    (CommandCompleteCB cb)
    (Http.post url (Http.jsonBody (T.gameCommandEncoder cmd)) decoder)
