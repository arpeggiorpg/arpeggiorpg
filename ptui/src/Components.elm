port module Components exposing (..)

import Json.Encode as JE

import Types as T

port renderHello : String -> Cmd msg

port renderTextInput : (String, String, JE.Value, Bool) -> Cmd msg
port textInputSubmit : ((String, String) -> msg) -> Sub msg
port textInputCancel : ((String, String) -> msg) -> Sub msg

port renderHistory : (String, JE.Value) -> Cmd msg
port historyRollback : ((Int, Int) -> msg) -> Sub msg

port renderPlayers : (String, Maybe String, JE.Value) -> Cmd msg
port playersGrantCreatures : (String -> msg) -> Sub msg
port playersSetScene : ((String, Maybe String) -> msg) -> Sub msg

port renderPlayerUI : (String, T.PlayerID, Maybe T.SceneID, JE.Value) -> Cmd msg

port unloadComponent : String -> Cmd msg

port sendCommand: (JE.Value -> msg) -> Sub msg
