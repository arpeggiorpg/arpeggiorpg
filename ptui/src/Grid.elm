module Grid exposing (..)

import Html.Attributes as HA
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

terrainMap : M.Model -> T.Map -> List MapCreature -> Svg M.Msg
terrainMap model terrain creatures = baseMap model terrain creatures [] False

editMap : M.Model -> T.Map -> List MapCreature -> Svg M.Msg
editMap model terrain creatures =
  baseMap model terrain creatures [] True

movementMap : M.Model -> (T.Point3 -> M.Msg) -> M.MovementRequest -> Bool -> T.Map -> T.Creature -> List MapCreature -> Svg M.Msg
movementMap model moveMsg {max_distance, movement_options, ooc_creature} moveAnywhere terrain creature creatures =
  let targetPoints =
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
    baseMap model terrain vCreatures movementTiles False

movementGhost : M.Model -> Maybe T.Point3
movementGhost model =
  case model.showingMovement of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing


baseMap : M.Model -> T.Map -> List MapCreature -> List (Svg M.Msg) -> Bool -> Svg M.Msg
baseMap model terrain creatures extras editable =
  let creatureEls = List.map gridCreature creatures
      terrainEls = baseTerrainRects model editable terrain
      ghostEl = case movementGhost model of
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
        , s [ S.width (S.pct 100)
            , S.height (S.pct 100)]
        ]
        [g [transform <| "matrix(" ++ matrixArgs ++ ")"] (terrainEls ++ extras ++ creatureEls ++ ghostEl)]
  in
    mapSVG

calculateAllMovementOptions : T.Point3 -> Int -> List T.Point3
calculateAllMovementOptions from distance =
  let xs = List.range (from.x - distance) (from.x + distance)
      ys = List.range (from.y - distance) (from.y + distance)
      result = List.concatMap (\x -> List.map (\y -> { x=x, y=y, z=0 }) ys) xs
  in result

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
      empties = if editable then emptyTerrain terrain else []
  in blocks ++ empties

gridTerrain : Bool -> T.Point3 -> Svg M.Msg
gridTerrain editable pt =
  tile "lightgrey" (if editable then [onClick (M.ToggleTerrain pt)] else []) pt


emptyTerrain : List T.Point3 -> List (Svg M.Msg)
emptyTerrain terrain =
  let g x y = let pt = {x = x, y = y, z = 0}
              in if not (List.member pt terrain) then [emptyTerrainTile pt] else []
      f x = List.concatMap (g x) (List.range -50 50)
      empties = List.concatMap f (List.range -50 50)
      _ = Debug.log "Number of empties: " (List.length empties)
  in empties

emptyTerrainTile : T.Point3 -> Svg M.Msg
emptyTerrainTile pt =
  tile "white" [onClick (M.ToggleTerrain pt)] pt

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
