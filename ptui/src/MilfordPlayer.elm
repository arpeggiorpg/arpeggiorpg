module Main exposing (..)

import Html
import Model as M exposing (devFlags)
import PlayerView
import Update as U

main : Program Never M.Model M.Msg
main =
  Html.program
    { init = (M.defaultModel {devFlags | rpi = "http://10.0.0.14:1337/"}, U.start)
    , view = PlayerView.playerView
    , update = U.update
    , subscriptions = M.subscriptions
    }
