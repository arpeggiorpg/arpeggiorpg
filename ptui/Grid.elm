module Grid exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Elements exposing (..)
import Model as M
import Update as U


-- how many meters across the grid should be
gridSize = 25
-- pixels per meter. 10 means 1 pixel = 1 decimeter
scale = 30

px x = (toString x) ++ "px"

metersToPx m = m * scale
metersToPxPx = px << metersToPx

pxToMeters m = m // scale

cmToPx cm = (cm // 100) * scale
cmToPxPx = px << cmToPx

distanceCm : (Int, Int) -> (Int, Int) -> Int
distanceCm (ax, ay) (bx, by) =
  round <| 100 * sqrt (toFloat ((ax - bx)^2) + toFloat ((bx - by)^2))

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distance calculation is done in CENTIMETERS.
coord : Int -> Int
coord c = (metersToPx (gridSize // 2)) + metersToPx c
coordPx = px << coord

terrainMap : (M.Point3 -> U.Msg) -> Maybe M.MovementRequest -> M.Map -> List M.Creature -> H.Html U.Msg
terrainMap moveMsg moving terrain creatures =
  let creatureEls = List.map gridCreature creatures
      terrainEls = List.map gridTerrain terrain
      cancelButton = case moving of Just _ -> cancelMove
                                    Nothing -> H.div [] []
      movementCirc =
        case moving of
          Just {creature, max_distance, movement_options} -> movementCircle moveMsg movement_options terrain creature.pos max_distance
          Nothing -> []
  in
    vbox <|
      cancelButton :: 
        [ svg
            [ width (toString <| metersToPx gridSize)
            , height (toString <| metersToPx gridSize)
            , HA.style [ ("border", "2px solid black")
                      , ("position", "relative") ]
            ]
            (terrainEls ++ movementCirc ++ creatureEls)
        ]   

cancelMove : H.Html U.Msg
cancelMove = H.button [HE.onClick U.CancelMovement] [H.text "Cancel Movement"]

movementCircle : (M.Point3 -> U.Msg) -> (List M.Point3) -> M.Map -> M.Point3 -> Int -> List (Svg U.Msg)
movementCircle moveMsg pts terrain origin max_distance =
  let debugEl = debugCircle origin max_distance
      movementCells = List.map (movementTarget moveMsg origin max_distance terrain) pts
  in debugEl :: movementCells

debugCircle : M.Point3 -> Int -> Svg U.Msg
debugCircle origin max_distance =
  circle [ width (toString (cmToPx max_distance * 2))
           , height (toString (cmToPx max_distance * 2))
           , stroke "black"
           , strokeWidth "1"
           , x (toString (coord origin.x))
           , y (toString (coord origin.y))]
           []

movementTarget : (M.Point3 -> U.Msg) -> M.Point3 -> Int -> M.Map -> M.Point3 -> H.Html U.Msg
movementTarget moveMsg origin max_distance terrain pt =
  rect [ width (toString (metersToPx 1)), height (toString (metersToPx 1))
         , x (toString (coord pt.x)), y (toString (coord pt.y))
         , stroke "black"
         , strokeWidth "1"
         , fill "green"
         , onClick (moveMsg pt) ]
         []

gridCreature : M.Creature -> Svg U.Msg
gridCreature creature =
  g []
    [ rect [ width (toString <| metersToPx 1)
             , height (toString <| metersToPx 1)
             , x <| toString (coord creature.pos.x), y <| toString (coord creature.pos.y)
             , fill "cyan"
             , stroke "black"
             , strokeWidth "1"]
             []
    , text_ [x <| toString (coord creature.pos.x), y <| toString (coord creature.pos.y)]
              [ text creature.id]
    ]

gridTerrain : M.Point3 -> Svg a
gridTerrain pt = 
  rect [ width (toString <| metersToPx 1)
         , height (toString <| metersToPx 1)
         , x <| toString (coord pt.x)
         , y <| toString (coord pt.y)
         , fill "lightgrey"
         , stroke "black"
         , strokeWidth "1" ]
         []
