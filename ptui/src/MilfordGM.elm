module Main exposing (..)

import Html
import Model as M exposing (devFlags)
import GMView
import Update as U

main : Program Never M.Model M.Msg
main =
  Html.program
    { init = (M.defaultModel {devFlags | rpi = "http://10.0.0.14:1337/"} , U.start)
    , view = GMView.gmView
    , update = U.update
    , subscriptions = M.subscriptions
    }
