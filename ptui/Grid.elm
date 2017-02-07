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
gridSize : Int
gridSize = 25

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distance calculation is done in CENTIMETERS.
coord : Int -> String
coord c = toString (c * 100)

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
            [ viewBox (String.join " " (List.map toString [-gridSize * 50, -gridSize * 50, gridSize * 100, gridSize * 100]))
            , width "800"
            , height "800"
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
  circle [ r (toString <| max_distance * 2)
           , fill "none"
           , stroke "black"
           , strokeWidth "1"
           , cx (coord origin.x)
           , cy (coord origin.y)]
           []

movementTarget : (M.Point3 -> U.Msg) -> M.Point3 -> Int -> M.Map -> M.Point3 -> H.Html U.Msg
movementTarget moveMsg origin max_distance terrain pt =
  rect [ width "100", height "100"
         , x (coord pt.x), y (coord pt.y)
         , stroke "black"
         , strokeWidth "1"
         , fill "green"
         , onClick (moveMsg pt) ]
         []

gridCreature : M.Creature -> Svg U.Msg
gridCreature creature =
  g []
    [ rect [ width "100"
             , height "100"
             , x (coord creature.pos.x), y (coord creature.pos.y)
             , fill "cyan"
             , stroke "black"
             , strokeWidth "1"]
             []
    , text_ [fontSize "50", x (coord creature.pos.x), y (coord creature.pos.y)]
              [ text creature.id]
    ]

gridTerrain : M.Point3 -> Svg a
gridTerrain pt = 
  rect [ width "100"
         , height "100"
         , x (coord pt.x)
         , y (coord pt.y)
         , fill "lightgrey"
         , stroke "black"
         , strokeWidth "1" ]
         []
