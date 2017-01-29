module Grid exposing (..)

import AStar
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

-- These functions make 1 pixel = 1 decimeter

metersToPx m = m * scale
metersToPxPx = px << metersToPx

pxToMeters m = m // scale

cmToPx cm = (cm // 100) * scale
cmToPxPx = px << cmToPx

distanceCm : (Int, Int) -> (Int, Int) -> Int
distanceCm (ax, ay) (bx, by) =
  round <| 100 * sqrt (toFloat ((ax - bx)^2) + toFloat ((bx - by)^2))

-- Get a list of all points within a distance that can be pathed to from an origin.
getPathablePts : M.Map -> Int -> M.Point3 -> List M.Point3
getPathablePts terrain distance origin =
  let meters = distance // 100
      xyf x y = calculatePath origin distance {x=x, y=y, z=0} terrain
      yf x = List.concatMap (xyf x) (List.range (origin.y - meters) (origin.y + meters))
  in List.concatMap yf (List.range (origin.x - meters) (origin.x + meters))

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
  in List.map (movementTarget origin max_distance terrain) pts

movementTarget : M.Point3 -> Int -> M.Map -> M.Point3 -> Html U.Msg
movementTarget origin max_distance terrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [ style [ ("width", metersToPxPx 1), ("height", metersToPxPx 1)
          , ("border", "1px solid black"), ("background", "lightgreen")]
  , onClick (U.Move (calculatePath origin max_distance pt terrain))]
  []

calculatePath : M.Point3 -> Int -> M.Point3 -> M.Map -> List M.Point3
calculatePath origin max_distance destination terrain =
  -- We *must* avoid pathing to an inaccessible destination!
  if (List.member destination terrain) then [] else
  let path = AStar.findPath
               AStar.pythagoreanCost
                 (gridStep origin max_distance terrain )
                 (origin.x, origin.y)
                 (destination.x, destination.y)
  in case path of
      Just path -> List.map (\(x, y) -> {x=x, y=y, z=0}) path
      Nothing -> []

gridStep : M.Point3 -> Int -> M.Map -> (Int, Int) -> Set.Set (Int, Int)
gridStep origin max_distance terrain (fromx, fromy) =
  let
    f (offsetx, offsety) result =
      let adjacentSquare = {x=fromx + offsetx, y=fromy + offsety, z=0}
          terrainExists = List.member adjacentSquare terrain
          isInRange = distanceCm (origin.x, origin.y) (offsetx, offsety) < max_distance
      in if terrainExists && isInRange then result else Set.insert (adjacentSquare.x, adjacentSquare.y) result
    result = List.foldl f Set.empty
      [ (-1, -1)
      , (-1,  0)
      , (-1,  1)
      , ( 0, -1)
      -- , ( 0,  0) Don't connect to self!
      , ( 0,  1)
      , ( 1, -1)
      , ( 1,  0)
      , ( 1,  1)
      ]
  in  result

gridCreature : M.Creature -> Html U.Msg
gridCreature creature = centerPositionedBox (coordPx creature.pos.x) (coordPx creature.pos.y)
  [style [("background-color", "cyan"), ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  [text creature.id]

gridTerrain : M.Point3 -> Html a
gridTerrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [style [("background-color", "grey") , ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  []
