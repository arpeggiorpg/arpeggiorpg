module Grid exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import MouseEvent exposing (onMouseClick)

import Model as M
import Update as U

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distance calculation is done in CENTIMETERS.
-- These functions make 1 pixel = 1 decimeter
px x = (toString x) ++ "px"
metersToPx m = m * 10
metersToPxPx = px << metersToPx
cmToPx cm = cm // 10
cmToPxPx = px << cmToPx
coord c = 250 + metersToPx c
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
  in div [style [("border", "2px solid black"), ("position", "relative"), ("width", metersToPxPx 50), ("height", metersToPxPx 50)]]
         (movementCirc ++ terrainEls ++ creatureEls)

clickedMove : M.Point3 -> Int -> MouseEvent.MouseEvent -> U.Msg
clickedMove origin radius me = log (toString me) <|
  let offsetX = (me.clientPos.x - radius) * 10
      offsetY = (me.clientPos.y - radius) * 10
  in (U.Move {x=(origin.x + offsetX) // 100, y=(origin.y + offsetY) // 100, z=0})

centerPositionedBox x y attrs content =
  div [style [ ("position", "absolute")
             , ("left", x)
             , ("top", y)]]
      [div (attrs ++ [style [("position", "relative"), ("margin-left", "-50%"), ("margin-top", "-50%")]])
           content]

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

gridCreature : M.Creature -> Html U.Msg
gridCreature creature = centerPositionedBox (coordPx creature.pos.x) (coordPx creature.pos.y)
  [style [("background-color", "cyan"), ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  [text creature.id]

gridTerrain : M.Point3 -> Html a
gridTerrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [style [("background-color", "grey") , ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  []

-- End grid insanity