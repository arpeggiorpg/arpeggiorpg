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

baseMap : M.Map -> List M.Creature -> List (Svg U.Msg) -> Bool -> Svg U.Msg
baseMap terrain creatures extras editable =
  let creatureEls = List.map gridCreature creatures
      terrainEls = baseTerrainRects editable terrain
  in svg
      [ viewBox (String.join " " (List.map toString [-gridSize * 50, -gridSize * 50, gridSize * 100, gridSize * 100]))
      , width "800"
      , height "800"
      , HA.style [ ("border", "2px solid black")
                 , ("position", "relative") ]
      ]
      (terrainEls ++ extras ++ creatureEls)

terrainMap : M.Map -> List M.Creature -> Svg U.Msg
terrainMap t c = baseMap t c [] False

editMap : M.Map -> List M.Creature -> H.Html U.Msg
editMap terrain creatures = vbox
  [ saveForm terrain
  , baseMap terrain creatures [] True ]

movementMap : (M.Point3 -> U.Msg) -> M.MovementRequest -> M.Map -> M.Creature -> List M.Creature -> H.Html U.Msg
movementMap moveMsg {max_distance, movement_options} terrain creature creatures =
  let cancelButton = cancelMove
      movementCirc = movementCircle moveMsg movement_options terrain creature.pos max_distance
  in
    vbox
      [ cancelButton
      , baseMap terrain creatures movementCirc False ]

cancelMove : H.Html U.Msg
cancelMove = H.button [HE.onClick U.CancelMovement] [H.text "Cancel Movement"]

saveForm : M.Map -> H.Html U.Msg
saveForm terrain = vbox <|
  [ H.button [HE.onClick U.CancelEditingMap] [text "Cancel Editing Map"]
  , hbox
    [ H.input [HA.type_ "text", HA.placeholder "map name", HE.onInput U.UpdateSaveMapName ] []
    , H.button [HE.onClick (U.EditMap terrain)] [text "Save"]
    ]
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
terrainRects = baseTerrainRects False

baseTerrainRects : Bool -> List M.Point3 -> List (Svg U.Msg)
baseTerrainRects editable terrain =
  let blocks = List.map (gridTerrain editable) terrain
      empties = emptyTerrain editable terrain
  in blocks ++ empties

gridTerrain : Bool -> M.Point3 -> Svg U.Msg
gridTerrain editable pt =
  tile "lightgrey" (if editable then [onClick (U.ToggleTerrain pt)] else []) pt

emptyTerrain : Bool -> List M.Point3 -> List (Svg U.Msg)
emptyTerrain editable terrain =
  let halfGrid = gridSize // 2
      g x y = let pt = {x= x, y=y, z=0}
              in if not (List.member pt terrain) then [emptyTerrainTile editable pt] else []
      f x = List.concatMap (g x) (List.range -halfGrid halfGrid)
  in List.concatMap f (List.range -halfGrid halfGrid)

emptyTerrainTile : Bool -> M.Point3 -> Svg U.Msg
emptyTerrainTile editable pt =
  tile "white" (if editable then [onClick (U.ToggleTerrain pt)] else []) pt

tile : String -> List (Svg.Attribute U.Msg) -> M.Point3 -> Svg U.Msg
tile cl attrs pt =
  rect (attrs ++ [ width "100"
       , height "100"
       , x (coord pt.x)
       , y (coord pt.y)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ])
       []
