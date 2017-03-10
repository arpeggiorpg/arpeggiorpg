module Main exposing (..)

import Html
import Model as M
import GMView
import Update as U

main : Program Never M.Model M.Msg
main =
  Html.program
    { init = (M.defaultModel M.devFlags, U.start)
    , view = GMView.gmView
    , update = U.update
    , subscriptions = M.subscriptions
    }
