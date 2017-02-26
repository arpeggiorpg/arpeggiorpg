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


-- how many meters across the grid should be
gridSize : Int
gridSize = 25

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distances are in CENTIMETERS.
coord : Int -> String
coord c = toString (c * 100)

baseMap : Bool -> Maybe T.Point3 -> T.Map -> List T.Creature -> List (Svg M.Msg) -> Bool -> Svg M.Msg
baseMap movable ghost terrain creatures extras editable =
  let creatureEls = List.map (gridCreature movable) creatures
      terrainEls = baseTerrainRects editable terrain
      ghostEl = case ghost of
                  Just pt -> [tile "black" [] pt]
                  Nothing -> []
  in svg
      [ viewBox (String.join " " (List.map toString [-gridSize * 40, -gridSize * 40, gridSize * 80, gridSize * 80]))
      , width "1000"
      , height "800"
      , HA.style [ ("border", "2px solid black")
                 , ("position", "relative") ]
      ]
      (terrainEls ++ extras ++ creatureEls ++ ghostEl)

terrainMap : Bool -> Maybe T.Point3 -> T.Map -> List T.Creature -> Svg M.Msg
terrainMap movable ghost terrain creatures = baseMap movable ghost terrain creatures [] False

editMap : T.Map -> List T.Creature -> H.Html M.Msg
editMap terrain creatures = vbox
  [ saveForm terrain
  , baseMap False Nothing terrain creatures [] True ]

movementMap : (T.Point3 -> M.Msg) -> M.MovementRequest -> T.Map -> T.Creature -> List T.Creature -> H.Html M.Msg
movementMap moveMsg {max_distance, movement_options} terrain creature creatures =
  let cancelButton = cancelMove
      movementCirc = movementCircle moveMsg movement_options terrain creature.pos max_distance
  in
    vbox
      [ cancelButton
      , baseMap False Nothing terrain creatures movementCirc False ]

cancelMove : H.Html M.Msg
cancelMove = H.button [HE.onClick M.CancelMovement] [H.text "Cancel Movement"]

saveForm : T.Map -> H.Html M.Msg
saveForm terrain = vbox <|
  [ H.button [HE.onClick M.CancelEditingMap] [text "Cancel Editing Map"]
  , hbox
    [ H.input [HA.type_ "text", HA.placeholder "map name", HE.onInput M.UpdateSaveMapName ] []
    , H.button [HE.onClick (M.EditMap terrain)] [text "Save"]
    ]
  ]

movementCircle : (T.Point3 -> M.Msg) -> (List T.Point3) -> T.Map -> T.Point3 -> Int -> List (Svg M.Msg)
movementCircle moveMsg pts terrain origin max_distance =
  let movementCells = List.map (movementTarget moveMsg origin max_distance terrain) pts
  in movementCells

distanceCircle : T.Point3 -> Int -> Svg M.Msg
distanceCircle origin max_distance =
  let centerCoord n = toString ((n * 100) + 50) in
  circle [ r (toString max_distance)
           , fill "none"
           , stroke "black"
           , strokeWidth "1"
           , cx (centerCoord origin.x)
           , cy (centerCoord origin.y)]
           []

movementTarget : (T.Point3 -> M.Msg) -> T.Point3 -> Int -> T.Map -> T.Point3 -> H.Html M.Msg
movementTarget moveMsg origin max_distance terrain pt =
  tile "green" [onClick (moveMsg pt)] pt

gridCreature : Bool -> T.Creature -> Svg M.Msg
gridCreature movable creature =
  let creatureColor =
        case creature.class of
          "baddie" -> "red"
          _ -> "cyan"
  in g []
    [ tile creatureColor (if movable then [onClick (M.GetMovementOptions creature)] else []) creature.pos
    , text_ [HA.style [("pointer-events", "none")], fontSize "50", x (coord creature.pos.x), y (toString <| (creature.pos.y * 100) + 50)]
              [ text creature.id]
    ]

terrainRects : List T.Point3 -> List (Svg M.Msg)
terrainRects = baseTerrainRects False

baseTerrainRects : Bool -> List T.Point3 -> List (Svg M.Msg)
baseTerrainRects editable terrain =
  let blocks = List.map (gridTerrain editable) terrain
      empties = emptyTerrain editable terrain
  in blocks ++ empties

gridTerrain : Bool -> T.Point3 -> Svg M.Msg
gridTerrain editable pt =
  tile "lightgrey" (if editable then [onClick (M.ToggleTerrain pt)] else []) pt

emptyTerrain : Bool -> List T.Point3 -> List (Svg M.Msg)
emptyTerrain editable terrain =
  let halfGrid = gridSize // 2
      g x y = let pt = {x= x, y=y, z=0}
              in if not (List.member pt terrain) then [emptyTerrainTile editable pt] else []
      f x = List.concatMap (g x) (List.range -halfGrid halfGrid)
  in List.concatMap f (List.range -halfGrid halfGrid)

emptyTerrainTile : Bool -> T.Point3 -> Svg M.Msg
emptyTerrainTile editable pt =
  tile "white" (if editable then [onClick (M.ToggleTerrain pt)] else []) pt

tile : String -> List (Svg.Attribute M.Msg) -> T.Point3 -> Svg M.Msg
tile cl attrs pt =
  rect (attrs ++ [ width "100"
       , height "100"
       , x (coord pt.x)
       , y (coord pt.y)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ])
       []
