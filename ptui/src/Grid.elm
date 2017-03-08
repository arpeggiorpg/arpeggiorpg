module Grid exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Css as S

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Elements exposing (hbox, vbox, s)
import Types as T
import Model as M


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

terrainMap : M.Model -> Maybe T.Point3 -> T.Map -> List MapCreature -> Svg M.Msg
terrainMap model ghost terrain creatures = baseMap model ghost terrain creatures [] False

editMap : M.Model -> T.Map -> List MapCreature -> H.Html M.Msg
editMap model terrain creatures = vbox
  [ saveForm terrain
  , baseMap model Nothing terrain creatures [] True ]

movementMap : M.Model -> (T.Point3 -> M.Msg) -> M.MovementRequest -> Bool -> T.Map -> T.Creature -> List MapCreature -> H.Html M.Msg
movementMap model moveMsg {max_distance, movement_options, ooc_creature} moveAnywhere terrain creature creatures =
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
      , baseMap model Nothing terrain vCreatures movementTiles False ]

baseMap : M.Model -> Maybe T.Point3 -> T.Map -> List MapCreature -> List (Svg M.Msg) -> Bool -> H.Html M.Msg
baseMap model ghost terrain creatures extras editable =
  let creatureEls = List.map gridCreature creatures
      terrainEls = baseTerrainRects model editable terrain
      ghostEl = case ghost of
                  Just pt -> [tile "black" [] pt]
                  Nothing -> []
      -- gridSize is 25 ("meters across")
      -- our coordinate system is in centimeters. We want to pan by 100 * offset.
      -- gridLeft = (-model.gridSize + model.gridOffset.x * 2) * 50
      -- gridTop = (-model.gridSize + -model.gridOffset.y * 2) * 50
      -- gridWidth = model.gridSize * 100
      -- gridHeight = model.gridSize * 100
      gridTranslateX = toString <| -model.gridOffset.x * 50
      gridTranslateY = toString <| model.gridOffset.y * 50
      gridScale = toString <|  1 + (toFloat -model.gridSize / 100)
      matrixArgs = String.join ", " [gridScale, "0", "0", gridScale, gridTranslateX, gridTranslateY]
      mapSVG = svg
        [ -- viewBox (String.join " " (List.map toString [gridLeft, gridTop, gridWidth, gridHeight]))
         preserveAspectRatio "xMinYMid slice"
        , transform <| "matrix(" ++ matrixArgs ++ ")"
        , s [S.border2 (S.px 2) S.solid, S.width (S.pct 100), S.height (S.pct 100)]
        ]
        (terrainEls ++ extras ++ creatureEls ++ ghostEl)
  in
    mapSVG

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

baseTerrainRects : M.Model -> Bool -> List T.Point3 -> List (Svg M.Msg)
baseTerrainRects model editable terrain =
  let blocks = List.map (gridTerrain editable) terrain
      -- TODO re-enable editing-only empties
      empties = emptyTerrain model editable terrain -- if editable then emptyTerrain model editable terrain else []
  in blocks ++ empties

gridTerrain : Bool -> T.Point3 -> Svg M.Msg
gridTerrain editable pt =
  tile "lightgrey" (if editable then [onClick (M.ToggleTerrain pt)] else []) pt

-- TODO: panning/zooming empty terrain in edit mode is very slow in Firefox.
-- Using Svg.Keyed.node pervasively for all tiles didn't help at all.
-- I'm guessing that the slowness is from rendering Elm nodes, but I'm not sure why.
emptyTerrain : M.Model -> Bool -> List T.Point3 -> List (Svg M.Msg)
emptyTerrain model editable terrain =
  let halfGrid = model.gridSize // 2
      g x y = let pt = {x = x, y = y, z = 0}
              in if not (List.member pt terrain) then [emptyTerrainTile editable pt] else []
      f x = List.concatMap (g x) (List.range (-halfGrid - model.gridOffset.y) (halfGrid - model.gridOffset.y))
  in List.concatMap f (List.range (-halfGrid + model.gridOffset.x) (halfGrid + model.gridOffset.x))

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
