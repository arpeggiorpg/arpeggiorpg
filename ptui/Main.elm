module Main exposing (..)

import Html
import Model as M
import View as V
import Update as U

main : Program Never M.Model U.Msg
main =
    Html.program
        { init = (M.defaultModel, U.refreshApp)
        , view = V.view
        , update = U.update
        , subscriptions = \_ -> Sub.none
        }
