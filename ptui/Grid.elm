module Grid exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import MouseEvent exposing (onMouseClick)

import Elements exposing (..)
import Model as M
import Update as U


-- how many meters across the grid should be
gridSize = 25
-- pixels per meter. 10 means 1 pixel = 1 decimeter
scale = 10

px x = (toString x) ++ "px"

-- These functions make 1 pixel = 1 decimeter

metersToPx m = m * scale
metersToPxPx = px << metersToPx

pxToMeters m = m // scale

cmToPx cm = (cm // 100) * scale
cmToPxPx = px << cmToPx

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distance calculation is done in CENTIMETERS.
coord : Int -> Int
coord c = (metersToPx (gridSize // 2)) + metersToPx c
coordPx = px << coord

combatGrid : Maybe M.MovementRequest -> Maybe M.Map -> M.Combat -> Html U.Msg
combatGrid moving maybeMap combat =
  let creatureEls = (List.map gridCreature combat.creatures.data)
      terrainEls = case maybeMap of
        Just m -> (List.map gridTerrain m)
        Nothing -> []
      movementCirc =
        case moving of
          Just {origin, max_distance} -> [movementCircle origin max_distance]
          Nothing -> []
  in div [style [ ("border", "2px solid black"), ("position", "relative")
                , ("width", metersToPxPx gridSize), ("height", metersToPxPx gridSize)]]
         (movementCirc ++ terrainEls ++ creatureEls)

movementCircle : M.Point3 -> Int -> Html U.Msg
movementCircle origin max_distance =
  let radius = cmToPx max_distance
  in centerPositionedBox (coordPx origin.x) (coordPx origin.y)
       [ style [ ("width", px (radius * 2))
               , ("height", px (radius * 2))
               , ("border-radius", px radius)
               , ("background", "lightgreen")
               ]
       , onMouseClick (clickedMove origin radius)
       ]
       []

clickedMove : M.Point3 -> Int -> MouseEvent.MouseEvent -> U.Msg
clickedMove origin radius me = log (toString me) <|
  let elementX = (me.elementPos.x - radius)
      elementY = (me.elementPos.y - radius)
  in log (toString elementX ++ "/" ++ toString elementY)
         U.Move { x=(pxToMeters elementX) + origin.x
                , y=(pxToMeters elementY) + origin.y
                , z=0}


gridCreature : M.Creature -> Html U.Msg
gridCreature creature = centerPositionedBox (coordPx creature.pos.x) (coordPx creature.pos.y)
  [style [("background-color", "cyan"), ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  [text creature.id]

gridTerrain : M.Point3 -> Html a
gridTerrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [style [("background-color", "grey") , ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  []
