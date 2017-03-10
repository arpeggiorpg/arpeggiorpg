module Main exposing (..)

import Html
import Model as M
import PlayerView
import Update as U

main : Program Never M.Model M.Msg
main =
  Html.program
    { init = (M.defaultModel M.devFlags, U.start)
    , view = PlayerView.playerView
    , update = U.update
    , subscriptions = M.subscriptions
    }
