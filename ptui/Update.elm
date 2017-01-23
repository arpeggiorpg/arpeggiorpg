module Update exposing (..)

import Debug exposing (log)
import Http
import Json.Decode as JD
import Set

import Model as M exposing (CreatureID, AbilityID)

-- These are all the things that can *happen* in the app.
-- e.g., clicking a button creates a Msg.
-- Receiving a response from an API call creates a Msg.
type Msg
    = MorePlease
    | SelectMap M.MapName
    | PendingCreatureId CreatureID
    | PendingCreatureName String
    | PendingCreatureAbilitySet String
    | PostCreateCreature
    | PostComplete (Result Http.Error JD.Value)
    | AppUpdate (Result Http.Error M.App)
    | ShowError String
    | ToggleSelectedCreature CreatureID
    | PostStartCombat
    | PostStopCombat
    | AddToCombat CreatureID
    | RemoveFromCombat CreatureID
    | RemoveFromGame CreatureID
    | SelectAbility AbilityID
    | Act AbilityID M.DecidedTarget
    | RequestMove M.MovementRequest -- max amount they can move
    | Move M.Point3
    | TurnDone


-- Do something on behalf of a Msg. Has the opportunity to modify the state of the app AND to
-- trigger an Effect.
update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease -> ( model, updateApp  )
        SelectMap mapName -> ({ model | currentMap = Just mapName}, Cmd.none)
        PendingCreatureId newId ->
          let oldPC = model.pendingCreature
          in ( { model | pendingCreature = {oldPC | id = Just newId } }
          , Cmd.none )
        PendingCreatureName newName ->
          let oldPC = model.pendingCreature
          in ( { model | pendingCreature = { oldPC | name = Just newName } }
          , Cmd.none )
        PendingCreatureAbilitySet newAS ->
          let oldPC = model.pendingCreature
          in ({model | pendingCreature = {oldPC | ability_set = Just newAS}},
              Cmd.none)
        PostCreateCreature -> createCreature model model.pendingCreature
        PostStartCombat -> startCombat model model.pendingCombatCreatures
        PostStopCombat -> stopCombat model
        PostComplete (Ok x) -> ( model, successfulResponse x)
        PostComplete (Err x) -> ({ model | error = toString x}, Cmd.none)
        AppUpdate (Ok newApp) -> ( { model | app = (Just newApp) }, Cmd.none )
        AppUpdate (Err x) -> ( { model | error = toString x}, Cmd.none )
        ShowError s -> ( {model | error = s}, Cmd.none)
        ToggleSelectedCreature cid ->
          ( { model | pendingCombatCreatures = toggleSet cid model.pendingCombatCreatures }
          , Cmd.none )
        AddToCombat cid -> (model, addToCombat cid)
        RemoveFromCombat cid -> (model, removeFromCombat cid)
        RemoveFromGame cid -> (model, removeFromGame cid)
        SelectAbility abid -> ({ model | selectedAbility = Just abid}, Cmd.none)
        Act abid dtarget -> ({model | selectedAbility = Nothing}, act abid dtarget)
        RequestMove movement -> ({model | moving = Just movement}, Cmd.none)
        Move pt -> ({model | moving = Nothing}, move pt)
        TurnDone -> (model, turnDone)


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

successfulResponse : a -> Cmd Msg
successfulResponse x = log (toString x) updateApp

createCreature : M.Model -> M.PendingCreature -> (M.Model, Cmd Msg)
createCreature model pc =
  case (M.finalizePending pc) of
    Nothing -> ({ model | error = "Fill out the stuff."}, Cmd.none)
    Just creature -> (model, sendCommand (M.CreateCreature creature))

startCombat : M.Model -> Set.Set CreatureID -> (M.Model, Cmd Msg)
startCombat model cids = (model, sendCommand (M.StartCombat (Set.toList cids)))

stopCombat : M.Model -> (M.Model, Cmd Msg)
stopCombat model = (model, sendCommand M.StopCombat)

addToCombat : CreatureID -> Cmd Msg
addToCombat cid = sendCommand (M.AddCreatureToCombat cid)

removeFromCombat : CreatureID -> Cmd Msg
removeFromCombat cid = sendCommand (M.RemoveCreatureFromCombat cid)

removeFromGame : CreatureID -> Cmd Msg
removeFromGame cid = sendCommand (M.RemoveCreature cid)

turnDone : Cmd Msg
turnDone = sendCommand M.Done

act : AbilityID -> M.DecidedTarget -> Cmd Msg
act abid dtarget = sendCommand (M.Act abid dtarget)

move : M.Point3 -> Cmd Msg
move pt = sendCommand (M.Move pt)


url : String
url = "http://localhost:1337/"

updateApp : Cmd Msg
updateApp = Http.send AppUpdate (Http.get url M.appDecoder)

sendCommand : M.GameCommand -> Cmd Msg
sendCommand cmd =
  log (toString cmd)
  Http.send PostComplete (Http.post url (Http.jsonBody (M.gameCommandEncoder cmd)) JD.value)