port module Components exposing (..)

import Json.Encode as JE

port renderHello : String -> Cmd msg

port renderTextInput : (String, String, JE.Value, Bool) -> Cmd msg
port textInputSubmit : ((String, String) -> msg) -> Sub msg
port textInputCancel : ((String, String) -> msg) -> Sub msg

port renderHistory : (String, JE.Value) -> Cmd msg
port historyRollback : ((Int, Int) -> msg) -> Sub msg

port renderPlayers : (String, JE.Value) -> Cmd msg
port playersGrantCreatures : (String -> msg) -> Sub msg
port playersRemoveFromScene : (String -> msg) -> Sub msg

port unloadComponent : String -> Cmd msg
