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
      terrainEls = terrainRects terrain
      cancelButton = case moving of Just _ -> cancelMove
                                    Nothing -> H.div [] []
      movementCirc =
        case moving of
          Just {creature, max_distance, movement_options} -> movementCircle moveMsg movement_options terrain creature.pos max_distance
          Nothing -> []
  in
    vbox <|
      hbox [cancelButton, saveForm terrain] :: 
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

saveForm : M.Map -> H.Html U.Msg
saveForm terrain = hbox
  [ H.input [HA.type_ "text", HA.placeholder "map name", HE.onInput U.SaveMapName ] []
  , H.button [HE.onClick (U.EditMap terrain)] [text "Save"]
  ]

movementCircle : (M.Point3 -> U.Msg) -> (List M.Point3) -> M.Map -> M.Point3 -> Int -> List (Svg U.Msg)
movementCircle moveMsg pts terrain origin max_distance =
  let circleEl = distanceCircle origin max_distance
      movementCells = List.map (movementTarget moveMsg origin max_distance terrain) pts
  in circleEl :: movementCells

distanceCircle : M.Point3 -> Int -> Svg U.Msg
distanceCircle origin max_distance =
  let centerCoord n = toString ((n * 100) + 50) in
  circle [ r (toString max_distance)
           , fill "none"
           , stroke "black"
           , strokeWidth "1"
           , cx (centerCoord origin.x)
           , cy (centerCoord origin.y)]
           []

movementTarget : (M.Point3 -> U.Msg) -> M.Point3 -> Int -> M.Map -> M.Point3 -> H.Html U.Msg
movementTarget moveMsg origin max_distance terrain pt =
  tile "green" [onClick (moveMsg pt)] pt

gridCreature : M.Creature -> Svg U.Msg
gridCreature creature =
  g []
    [ tile "cyan" [] creature.pos
    , text_ [fontSize "50", x (coord creature.pos.x), y (coord creature.pos.y)]
              [ text creature.id]
    ]

terrainRects : List M.Point3 -> List (Svg U.Msg)
terrainRects terrain =
  let blocks = List.map gridTerrain terrain
      empties = emptyTerrain terrain
  in blocks ++ empties

gridTerrain : M.Point3 -> Svg U.Msg
gridTerrain pt = tile "lightgrey" [onClick (U.ToggleTerrain pt)] pt

emptyTerrain : List M.Point3 -> List (Svg U.Msg)
emptyTerrain terrain =
  let halfGrid = gridSize // 2
      g x y = let pt = {x= x, y=y, z=0}
              in if not (List.member pt terrain) then [emptyTerrainTile pt] else []
      f x = List.concatMap (g x) (List.range -halfGrid halfGrid)
  in List.concatMap f (List.range -halfGrid halfGrid)


emptyTerrainTile pt =
  tile "white" [onClick (U.ToggleTerrain pt)] pt

tile cl attrs pt =
  rect (attrs ++ [ width "100"
       , height "100"
       , x (coord pt.x)
       , y (coord pt.y)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ])
       []
