module Grid exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set

import MouseEvent exposing (onMouseClick)

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

combatGrid : Maybe M.MovementRequest -> M.Map -> M.Combat -> Html U.Msg
combatGrid moving terrain combat =
  let creatureEls = List.map gridCreature combat.creatures.data
      terrainEls = List.map gridTerrain terrain
      movementCirc =
        case moving of
          Just {origin, max_distance} -> movementCircle combat terrain origin max_distance
          Nothing -> []
  in div [style [ ("border", "2px solid black"), ("position", "relative")
                , ("width", metersToPxPx gridSize), ("height", metersToPxPx gridSize)]]
         (movementCirc ++ terrainEls ++ creatureEls)

movementCircle : M.Combat -> M.Map -> M.Point3 -> Int -> List (Html U.Msg)
movementCircle combat terrain origin max_distance =
  let pts = combat.movement_options
      debugEl = debugCircle origin max_distance
      movementCells = List.map (movementTarget origin max_distance terrain) pts
  in debugEl :: movementCells ++ [cancelMove]

cancelMove : Html U.Msg
cancelMove = button [onClick U.CancelMovement] [text "Cancel Movement"]

debugCircle : M.Point3 -> Int -> Html U.Msg
debugCircle origin max_distance = centerPositionedBox (coordPx origin.x) (coordPx origin.y)
  [id "debug-circle", style [ ("width", cmToPxPx <| max_distance * 2), ("height", cmToPxPx <| max_distance * 2)
         , ("border-radius", "50%"), ("border", "2px solid black")]]
  []

movementTarget : M.Point3 -> Int -> M.Map -> M.Point3 -> Html U.Msg
movementTarget origin max_distance terrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [ style [ ("width", metersToPxPx 1), ("height", metersToPxPx 1)
          , ("border", "1px solid black"), ("background", "lightgreen")]
  , onClick (U.Move pt)]
  []

gridCreature : M.Creature -> Html U.Msg
gridCreature creature = centerPositionedBox (coordPx creature.pos.x) (coordPx creature.pos.y)
  [style [("background-color", "cyan"), ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  [text creature.id]

gridTerrain : M.Point3 -> Html a
gridTerrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [style [("background-color", "grey") , ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  []
