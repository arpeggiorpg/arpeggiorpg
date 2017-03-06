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

-- Information about a creature that is relevant to the map.
type alias MapCreature =
  { creature: T.Creature
  , highlight : Bool
  , movable : Maybe (T.Creature -> M.Msg)
  , class : T.Class
  }

baseMap : Maybe T.Point3 -> T.Map -> List MapCreature -> List (Svg M.Msg) -> Bool -> Svg M.Msg
baseMap ghost terrain creatures extras editable =
  let creatureEls = List.map gridCreature creatures
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

terrainMap : Maybe T.Point3 -> T.Map -> List MapCreature -> Svg M.Msg
terrainMap ghost terrain creatures = baseMap ghost terrain creatures [] False

editMap : T.Map -> List MapCreature -> H.Html M.Msg
editMap terrain creatures = vbox
  [ saveForm terrain
  , baseMap Nothing terrain creatures [] True ]

movementMap : (T.Point3 -> M.Msg) -> M.MovementRequest -> Bool -> T.Map -> T.Creature -> List MapCreature -> H.Html M.Msg
movementMap moveMsg {max_distance, movement_options, ooc_creature} moveAnywhere terrain creature creatures =
  let cancelButton = cancelMove
      targetPoints =
        if moveAnywhere
        then calculateAllMovementOptions creature.pos (max_distance // 100)
        else movement_options
      movementTiles = movementTargets moveMsg targetPoints terrain creature.pos max_distance
      highlightMovingCreature : MapCreature -> MapCreature
      highlightMovingCreature mapc =
        if (Just mapc.creature.id) == (Maybe.map (\c -> c.id) ooc_creature)
        then {mapc | highlight = True}
        else mapc
      vCreatures = List.map highlightMovingCreature creatures
  in
    vbox
      [ cancelButton
      , baseMap Nothing terrain vCreatures movementTiles False ]

calculateAllMovementOptions : T.Point3 -> Int -> List T.Point3
calculateAllMovementOptions from distance =
  let xs = List.range (from.x - distance) (from.x + distance)
      ys = List.range (from.y - distance) (from.y + distance)
      result = List.concatMap (\x -> List.map (\y -> { x=x, y=y, z=0 }) ys) xs
  in result

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

movementTargets : (T.Point3 -> M.Msg) -> List T.Point3 -> T.Map -> T.Point3 -> Int -> List (Svg M.Msg)
movementTargets moveMsg pts terrain origin max_distance =
  let movementTarget pt = tile "lawngreen" [fillOpacity "0.3", onClick (moveMsg pt)] pt
  in List.map movementTarget pts

gridCreature : MapCreature -> Svg M.Msg
gridCreature creature =
  let creatureColor = creature.class.color
      strokeColor =
        if creature.highlight
        then "white"
        else "black"
      strokeWidthSize =
        if creature.highlight
        then 10
        else 1
      movableEventHandler =
        case creature.movable of
          Just fn -> [onClick (fn creature.creature)]
          Nothing -> []
      attrs = [stroke strokeColor, strokeWidth (toString strokeWidthSize)] ++ movableEventHandler
      pos = creature.creature.pos
      creatureNameEl name =
        text_ [ HA.style [("pointer-events", "none")]
              , fontSize "50"
              , x (coord pos.x)
              , y (toString <| (pos.y * 100) + 50)
              ]
              [text name]
      creatureImageEl url =
        image
          [ HA.style [ ("pointer-events", "none") ]
          , x (coord pos.x), y (coord pos.y)
          , xlinkHref url
          , width "100", height "100"
          ] []
      foreground =
        if creature.creature.portrait_url == ""
        then creatureNameEl creature.creature.id
        else creatureImageEl creature.creature.portrait_url
  in g []
    [ tile creatureColor attrs creature.creature.pos
    , foreground ]

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
  rect ([ width "100"
       , height "100"
       , x (coord pt.x)
       , y (coord pt.y)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ] ++ attrs)
       []
