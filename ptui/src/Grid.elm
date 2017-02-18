module Grid exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Elements exposing (..)
import Types as T
import Model as M
import Update as U


-- how many meters across the grid should be
gridSize : Int
gridSize = 25

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distances are in CENTIMETERS.
coord : Int -> String
coord c = toString (c * 100)

baseMap : Bool -> T.Map -> List T.Creature -> List (Svg U.Msg) -> Bool -> Svg U.Msg
baseMap movable terrain creatures extras editable =
  let creatureEls = List.map (gridCreature movable) creatures
      terrainEls = baseTerrainRects editable terrain
  in svg
      [ viewBox (String.join " " (List.map toString [-gridSize * 50, -gridSize * 50, gridSize * 100, gridSize * 100]))
      , width "800"
      , height "800"
      , HA.style [ ("border", "2px solid black")
                 , ("position", "relative") ]
      ]
      (terrainEls ++ extras ++ creatureEls)

terrainMap : Bool -> T.Map -> List T.Creature -> Svg U.Msg
terrainMap movable terrain creatures = baseMap movable terrain creatures [] False

editMap : T.Map -> List T.Creature -> H.Html U.Msg
editMap terrain creatures = vbox
  [ saveForm terrain
  , baseMap False terrain creatures [] True ]

movementMap : (T.Point3 -> U.Msg) -> M.MovementRequest -> T.Map -> T.Creature -> List T.Creature -> H.Html U.Msg
movementMap moveMsg {max_distance, movement_options} terrain creature creatures =
  let cancelButton = cancelMove
      movementCirc = movementCircle moveMsg movement_options terrain creature.pos max_distance
  in
    vbox
      [ cancelButton
      , baseMap False terrain creatures movementCirc False ]

cancelMove : H.Html U.Msg
cancelMove = H.button [HE.onClick U.CancelMovement] [H.text "Cancel Movement"]

saveForm : T.Map -> H.Html U.Msg
saveForm terrain = vbox <|
  [ H.button [HE.onClick U.CancelEditingMap] [text "Cancel Editing Map"]
  , hbox
    [ H.input [HA.type_ "text", HA.placeholder "map name", HE.onInput U.UpdateSaveMapName ] []
    , H.button [HE.onClick (U.EditMap terrain)] [text "Save"]
    ]
  ]
movementCircle : (T.Point3 -> U.Msg) -> (List T.Point3) -> T.Map -> T.Point3 -> Int -> List (Svg U.Msg)
movementCircle moveMsg pts terrain origin max_distance =
  let circleEl = distanceCircle origin max_distance
      movementCells = List.map (movementTarget moveMsg origin max_distance terrain) pts
  in circleEl :: movementCells

distanceCircle : T.Point3 -> Int -> Svg U.Msg
distanceCircle origin max_distance =
  let centerCoord n = toString ((n * 100) + 50) in
  circle [ r (toString max_distance)
           , fill "none"
           , stroke "black"
           , strokeWidth "1"
           , cx (centerCoord origin.x)
           , cy (centerCoord origin.y)]
           []

movementTarget : (T.Point3 -> U.Msg) -> T.Point3 -> Int -> T.Map -> T.Point3 -> H.Html U.Msg
movementTarget moveMsg origin max_distance terrain pt =
  tile "green" [onClick (moveMsg pt)] pt

gridCreature : Bool -> T.Creature -> Svg U.Msg
gridCreature movable creature =
  g []
    [ tile "cyan" (if movable then [onClick (U.GetMovementOptions creature)] else []) creature.pos
    , text_ [fontSize "50", x (coord creature.pos.x), y (coord creature.pos.y)]
              [ text creature.id]
    ]

terrainRects : List T.Point3 -> List (Svg U.Msg)
terrainRects = baseTerrainRects False

baseTerrainRects : Bool -> List T.Point3 -> List (Svg U.Msg)
baseTerrainRects editable terrain =
  let blocks = List.map (gridTerrain editable) terrain
      empties = emptyTerrain editable terrain
  in blocks ++ empties

gridTerrain : Bool -> T.Point3 -> Svg U.Msg
gridTerrain editable pt =
  tile "lightgrey" (if editable then [onClick (U.ToggleTerrain pt)] else []) pt

emptyTerrain : Bool -> List T.Point3 -> List (Svg U.Msg)
emptyTerrain editable terrain =
  let halfGrid = gridSize // 2
      g x y = let pt = {x= x, y=y, z=0}
              in if not (List.member pt terrain) then [emptyTerrainTile editable pt] else []
      f x = List.concatMap (g x) (List.range -halfGrid halfGrid)
  in List.concatMap f (List.range -halfGrid halfGrid)

emptyTerrainTile : Bool -> T.Point3 -> Svg U.Msg
emptyTerrainTile editable pt =
  tile "white" (if editable then [onClick (U.ToggleTerrain pt)] else []) pt

tile : String -> List (Svg.Attribute U.Msg) -> T.Point3 -> Svg U.Msg
tile cl attrs pt =
  rect (attrs ++ [ width "100"
       , height "100"
       , x (coord pt.x)
       , y (coord pt.y)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ])
       []
