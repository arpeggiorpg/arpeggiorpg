module Main exposing (..)

import Html
import Model as M
import GMView
import Update as U

main : Program M.ProgramFlags M.Model M.Msg
main =
  Html.programWithFlags
    { init = \flags -> (M.defaultModel flags, U.start)
    , view = GMView.gmView
    , update = U.update
    , subscriptions = M.subscriptions
    }
