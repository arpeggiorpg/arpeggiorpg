port module PanZoom exposing (..)

port updateBoundingBox : String -> Cmd msg
port initializePanZoom : String -> Cmd msg

port panning : (() -> msg) -> Sub msg
