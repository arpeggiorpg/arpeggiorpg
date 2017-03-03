module Main exposing (..)

import Html
import Model as M
import View as V
import Update as U

main : Program M.ProgramFlags M.Model M.Msg
main =
  Html.programWithFlags
    { init = \flags -> (M.defaultModel flags, U.start)
    , view = V.playerView
    , update = U.update
    , subscriptions = M.subscriptions
    }
