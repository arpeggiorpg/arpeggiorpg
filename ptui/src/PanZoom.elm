port module PanZoom exposing (..)

port updateBoundingBox : String -> Cmd msg
port initializePanZoom : String -> Cmd msg
port destroyPanZoom : String -> Cmd msg

port panning : (Bool -> msg) -> Sub msg
