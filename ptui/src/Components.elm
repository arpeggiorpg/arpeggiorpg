port module Components exposing (..)

import Json.Encode as JE

port renderHello : String -> Cmd msg

port renderTextInput : (String, String, JE.Value) -> Cmd msg
port textInputSubmit : ((String, String) -> msg) -> Sub msg
port textInputCancel : ((String, String) -> msg) -> Sub msg

port unloadComponent : String -> Cmd msg
