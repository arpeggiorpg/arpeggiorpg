module Grid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

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

terrainMap : (M.Point3 -> U.Msg) -> Maybe M.MovementRequest -> M.Map -> List M.Creature -> Html U.Msg
terrainMap moveMsg moving terrain creatures =
  let creatureEls = List.map gridCreature creatures
      terrainEls = List.map gridTerrain terrain
      movementCirc =
        case moving of
          Just {creature, max_distance, movement_options} -> movementCircle moveMsg movement_options terrain creature.pos max_distance
          Nothing -> []
  in
    S.svg
      [ SA.width (toString <| metersToPx gridSize)
      , SA.height (toString <| metersToPx gridSize)
      , style [ ("border", "2px solid black")
              , ("position", "relative") ]
      ]
      (terrainEls ++ movementCirc ++ creatureEls)

movementCircle : (M.Point3 -> U.Msg) -> (List M.Point3) -> M.Map -> M.Point3 -> Int -> List (S.Svg U.Msg)
movementCircle moveMsg pts terrain origin max_distance =
  let debugEl = debugCircle origin max_distance
      movementCells = List.map (movementTarget moveMsg origin max_distance terrain) pts
  in debugEl :: movementCells
  -- ++ [cancelMove]

cancelMove : Html U.Msg
cancelMove = button [onClick U.CancelMovement] [text "Cancel Movement"]

debugCircle : M.Point3 -> Int -> S.Svg U.Msg
debugCircle origin max_distance =
  S.circle [ SA.width (toString (cmToPx max_distance * 2))
           , SA.height (toString (cmToPx max_distance * 2))
           , SA.stroke "black"
           , SA.strokeWidth "1"
           , SA.x (toString (coord origin.x))
           , SA.y (toString (coord origin.y))]
           []

movementTarget : (M.Point3 -> U.Msg) -> M.Point3 -> Int -> M.Map -> M.Point3 -> Html U.Msg
movementTarget moveMsg origin max_distance terrain pt =
  S.rect [ SA.width (toString (metersToPx 1)), SA.height (toString (metersToPx 1))
         , SA.x (toString (coord pt.x)), SA.y (toString (coord pt.y))
         , SA.stroke "black"
         , SA.strokeWidth "1"
         , SA.fill "green" ]
         []

gridCreature : M.Creature -> S.Svg U.Msg
gridCreature creature =
  S.g []
    [ S.rect [ SA.width (toString <| metersToPx 1)
             , SA.height (toString <| metersToPx 1)
             , SA.x <| toString (coord creature.pos.x), SA.y <| toString (coord creature.pos.y)
             , SA.fill "cyan"
             , SA.stroke "black"
             , SA.strokeWidth "1"]
             []
    , S.text_ [SA.x <| toString (coord creature.pos.x), SA.y <| toString (coord creature.pos.y)]
              [ S.text creature.id]
    ]

gridTerrain : M.Point3 -> S.Svg a
gridTerrain pt = 
  S.rect [ SA.width (toString <| metersToPx 1)
         , SA.height (toString <| metersToPx 1)
         , SA.x <| toString (coord pt.x)
         , SA.y <| toString (coord pt.y)
         , SA.fill "lightgrey"
         , SA.stroke "black"
         , SA.strokeWidth "1" ]
         []
