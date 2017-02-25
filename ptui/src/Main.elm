module Main exposing (..)

import Html
import Model as M
import View as V
import Update as U

main : Program Never M.Model M.Msg
main =
    Html.program
        { init = (M.defaultModel, U.pollApp)
        , view = V.gmView
        , update = U.update
        , subscriptions = M.subscriptions
        }
