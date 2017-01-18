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
    | PostComplete (Result Http.Error String)
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
        PostComplete (Ok _) -> (model, updateApp)
        PostComplete (Err x) -> ({ model | error = toString x}, Cmd.none)
        AppUpdate (Ok newApp) -> ( { model | app = (Just newApp) }, Cmd.none )
        AppUpdate (Err x) -> ( { model | error = toString x}, Cmd.none )
        ShowError s -> log "SETTING ERROR" ( {model | error = s}, Cmd.none)
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
        , case model.app of Just app -> viewApp app
                            Nothing -> div [] [text "No app yet. Maybe reload."]
        , div [] [text model.error]
        ]

viewApp : M.App -> Html Msg
viewApp app = div []
  [ div []
    [ input [type_ "text", placeholder "id", onInput PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput PendingCreatureName ] []
    , button [ onClick PostCreateCreature ] [ text "Create Creature!" ]
    ]
  , h3 [] [text "Creatures"]
  , div [] [ renderCreatures app.current_game.creatures ]
  , case app.current_game.current_combat of
      Just combat -> renderStopCombat
      Nothing -> renderStartCombat
  , div [] [ text (toString app)]
  ]

renderCreatures : Dict.Dict String M.Creature -> Html Msg
renderCreatures creatures = div []
  (List.map renderCreature (Dict.values creatures))

renderCreature : M.Creature -> Html Msg
renderCreature creature = div []
  [ text creature.name
  , input [ type_ "checkbox"
          , onClick (ToggleSelectedCreature creature.id)] []]

renderStopCombat : Html Msg
renderStopCombat = button [onClick PostStopCombat] [text "Stop Combat"]
renderStartCombat : Html Msg
renderStartCombat = button [onClick PostStartCombat] [text "Start Combat"]

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
    Just creature ->
      let createCreature = M.CreateCreature creature
          body = M.gameCommandEncoder createCreature
      in (model, Http.send PostComplete (Http.post url (Http.jsonBody body) JSON.string))

startCombat : M.Model -> Set.Set String -> (M.Model, Cmd Msg)
startCombat model cids =
  let body = M.gameCommandEncoder (M.StartCombat (Set.toList cids))
  in (model, Http.send PostComplete (Http.post url (Http.jsonBody body) JSON.string))

stopCombat : M.Model -> (M.Model, Cmd Msg)
stopCombat model =
  let body = M.gameCommandEncoder M.StopCombat
  in (model, Http.send PostComplete (Http.post url (Http.jsonBody body) JSON.string))
