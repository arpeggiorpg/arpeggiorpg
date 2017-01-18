-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html


module Main exposing (..)

import Debug exposing (log)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JSON
import Json.Encode as JE
import Set

import Model as M

main : Program Never M.Model Msg
main =
    Html.program
        { init = (M.defaultModel, updateApp)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- UPDATE
type Msg
    = MorePlease
    | PendingCreatureId String
    | PendingCreatureName String
    | PostCreateCreature
    | PostComplete (Result Http.Error JSON.Value)
    | AppUpdate (Result Http.Error M.App)
    | ShowError String
    | ToggleSelectedCreature String
    | PostStartCombat
    | PostStopCombat

update : Msg -> M.Model -> ( M.Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease -> ( model, updateApp  )
        PendingCreatureId newId ->
          let oldPC = model.pendingCreature
          in ( { model | pendingCreature = {oldPC | id = Just newId } }
          , Cmd.none )
        PendingCreatureName newName ->
          let oldPC = model.pendingCreature
          in ( { model | pendingCreature = { oldPC | name = Just newName } }
          , Cmd.none )
        PostCreateCreature -> createCreature model model.pendingCreature
        PostStartCombat -> startCombat model model.pendingCombatCreatures
        PostStopCombat -> stopCombat model
        PostComplete (Ok x) -> ( { model | lastResponse = x}, updateApp)
        PostComplete (Err x) -> ({ model | error = toString x}, Cmd.none)
        AppUpdate (Ok newApp) -> ( { model | app = (Just newApp) }, Cmd.none )
        AppUpdate (Err x) -> ( { model | error = toString x}, Cmd.none )
        ShowError s -> ( {model | error = s}, Cmd.none)
        ToggleSelectedCreature cid ->
          ( { model | pendingCombatCreatures = toggleSet cid model.pendingCombatCreatures }
          , Cmd.none )


toggleSet : comparable -> Set.Set comparable -> Set.Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

-- VIEW
view : M.Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "P&T" ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , case model.app of Just app -> viewApp model app
                            Nothing -> div [] [text "No app yet. Maybe reload."]
        , div [] [text "Last error:", text model.error]
        , div [] [text "Last Response:", text (JE.encode 4 model.lastResponse)]
        ]

viewApp : M.Model -> M.App -> Html Msg
viewApp model app = div []
  [ div []
    [ input [type_ "text", placeholder "id", onInput PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput PendingCreatureName ] []
    , button [ onClick PostCreateCreature ] [ text "Create Creature!" ]
    ]
  , h3 [] [text "Creatures"]
  , div [] [ renderCreatures model.pendingCombatCreatures app.current_game.creatures ]
  , case app.current_game.current_combat of
      Just combat -> div [] [renderStopCombat, renderCombat combat]
      Nothing -> renderStartCombat
  , div [] [ text (toString app)]
  ]

renderCreatures : Set.Set String -> Dict.Dict String M.Creature -> Html Msg
renderCreatures pendingCreatures creatures = div []
  (List.map (renderCreature pendingCreatures) (Dict.values creatures))

renderCreature : Set.Set String -> M.Creature -> Html Msg
renderCreature pendingCreatures creature = div []
  [ text creature.name
  , input [ type_ "checkbox"
          , checked (Set.member creature.id pendingCreatures)
          , onClick (ToggleSelectedCreature creature.id)] []]

renderStopCombat : Html Msg
renderStopCombat = button [onClick PostStopCombat] [text "Stop Combat"]
renderStartCombat : Html Msg
renderStartCombat = button [onClick PostStartCombat] [text "Start Combat"]

renderCombat : M.Combat -> Html Msg
renderCombat { creatures, movement_used } = div []
  [ h3 [] [text "Combat!"]
  , div [] [text "Current movement used:", text (toString movement_used)]
  , div [] (List.map (renderCreatureInCombat creatures.cursor) (List.indexedMap (,) creatures.data))
  ]

renderCreatureInCombat : Int -> (Int, M.Creature) -> Html Msg
renderCreatureInCombat cursor (idx, creature) = div []
  [ div [] [text "Name: ", text creature.name]
  , div [] [text "HP: ", text (toString creature.cur_health)]
  , if cursor == idx then div [] [text "THIS M'S TURN!"]
    else div [] []
  ]

-- SUBSCRIPTIONS
subscriptions : M.Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP
url : String
url = "http://localhost:1337/"

updateApp : Cmd Msg
updateApp = Http.send AppUpdate (Http.get url M.appDecoder)

createCreature : M.Model -> M.PendingCreature -> (M.Model, Cmd Msg)
createCreature model pc =
  case (M.finalizePending pc) of
    Nothing -> ({ model | error = "Fill out the stuff."}, Cmd.none)
    Just creature -> (model, sendCommand (M.CreateCreature creature))

startCombat : M.Model -> Set.Set String -> (M.Model, Cmd Msg)
startCombat model cids = (model, sendCommand (M.StartCombat (Set.toList cids)))

stopCombat : M.Model -> (M.Model, Cmd Msg)
stopCombat model = (model, sendCommand M.StopCombat)

sendCommand : M.GameCommand -> Cmd Msg
sendCommand cmd =
  Http.send PostComplete (Http.post url (Http.jsonBody (M.gameCommandEncoder cmd)) JSON.value)
