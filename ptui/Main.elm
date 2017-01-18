module Main exposing (..)

import Debug exposing (log)
import Dict
import Html
import Http
import Json.Decode as JSON
import Set

import Model as M
import View as V
import Update as U

main : Program Never M.Model U.Msg
main =
    Html.program
        { init = (M.defaultModel, U.updateApp)
        , view = V.view
        , update = U.update
        , subscriptions = \_ -> Sub.none
        }
