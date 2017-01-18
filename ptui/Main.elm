-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html


module Main exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JSON

import Model exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = (defaultModel, updateApp)
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
    | AppUpdate (Result Http.Error App)
    | ShowError String

update : Msg -> Model -> ( Model, Cmd Msg )
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
        PostComplete (Ok _) -> (model, updateApp)
        PostComplete (Err x) -> ({ model | error = toString x}, Cmd.none)
        AppUpdate (Ok newApp) -> ( { model | app = (Just newApp) }, Cmd.none )
        AppUpdate (Err x) -> ( { model | error = toString x}, Cmd.none )
        ShowError s -> log "SETTING ERROR" ( {model | error = s}, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "P&T" ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , div []
          [ input [type_ "text", placeholder "id", onInput PendingCreatureId ] []
          , input [type_ "text", placeholder "name", onInput PendingCreatureName ] []
          , button [ onClick PostCreateCreature ] [ text "Create Creature!" ]
          ]
        , br [] []
        , div [] [ text (toString model.app)]
        , div [] [text model.error]
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP
url : String
url = "http://localhost:1337/"

updateApp : Cmd Msg
updateApp = Http.send AppUpdate (Http.get url appDecoder)

createCreature : Model -> PendingCreature -> (Model, Cmd Msg)
createCreature model pc =
  case (finalizePending pc) of
    Nothing -> ({ model | error = "Fill out the stuff."}, Cmd.none)
    Just creature ->
      let createCreature = CreateCreature creature
          body = gameCommandEncoder createCreature
      in (model, Http.send PostComplete (Http.post url (Http.jsonBody body) JSON.string))
