-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html


module Main exposing (..)

import Dict exposing (Dict(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JSON


main =
    Html.program
        { init = (Model Nothing, updateApp)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
type alias Model = { app : Maybe App }
type alias App = { currentGame : Game }

appDecoder = JSON.map App (JSON.field "current_game" gameDecoder)

type alias Game =
    { currentCombat : Maybe Combat
    , abilities : Dict String Ability
    }

gameDecoder = JSON.map2 Game (JSON.field "current_combat" (JSON.maybe combatDecoder))
                             (JSON.field "abilities" (JSON.dict abilityDecoder))

type alias Combat =
  { creatures: List Creature
  , movementUsed: Int
  }

combatDecoder =
  JSON.map2 Combat (JSON.field "creatures" (JSON.list creatureDecoder))
                   (JSON.field "movement_used" JSON.int)

type alias Creature =
  { id: String
  , name: String
  }

creatureDecoder =
  JSON.map2 Creature (JSON.field "id" JSON.string)
                     (JSON.field "name" JSON.string)

type alias Ability = { name : String }


abilityDecoder = JSON.map Ability (JSON.field "name" JSON.string)




-- UPDATE
type Msg
    = MorePlease
    | AppUpdate (Result Http.Error App)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease -> ( model, updateApp  )
        AppUpdate (Ok newApp) -> ( Model (Just newApp), Cmd.none )
        AppUpdate (Err _) -> ( model, Cmd.none )


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "P&T" ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , div [] [ text (toString model.app)]
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- HTTP
updateApp : Cmd Msg
updateApp =
    let url = "http://localhost:1337"
    in Http.send AppUpdate (Http.get url appDecoder)
