module Grid exposing (..)

import Dict
import Html.Attributes as HA
import Css as S
import Set

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Maybe.Extra as MEx

import Elements exposing (hbox, vbox, s)
import Types as T
import Model as M


-- Convert Point3 coordinates to on-screen coordinates.
-- Point3 coordinates are in METERS, and Distances are in CENTIMETERS.
coord : Int -> String
coord c = toString (c * 100)

-- Information about a creature that is relevant to the map.
type alias MapCreature =
  { creature: T.Creature
  , highlight : Maybe Highlight
  , clickable : Maybe (T.Creature -> M.Msg)
  , class : T.Class
  , pos : T.Point3
  , visible: Bool
  }

type Highlight
  = Moving
  | Targetable
  | Current

terrainMap : M.Model -> T.Map -> List MapCreature -> Svg M.Msg
terrainMap model map creatures = baseMap model map creatures [] Nothing

editMap : M.Model -> T.Map -> List MapCreature -> (T.Point3 -> M.Msg) -> Svg M.Msg
editMap model map creatures paint =
  baseMap model map creatures [] (Just paint)

movementMap : M.Model -> (T.Point3 -> M.Msg) -> M.MovementRequest -> Bool -> T.Map -> T.Point3 -> List MapCreature -> Svg M.Msg
movementMap model moveMsg {max_distance, movement_options, ooc_creature} moveAnywhere map movingFrom creatures =
  let targetPoints =
        if moveAnywhere
        then calculateAllMovementOptions movingFrom (max_distance // 100)
        else movement_options
      movementTiles = targetTiles moveMsg targetPoints (always M.NoMsg)
      highlightMovingCreature mapc =
        if (Just mapc.creature.id) == (Maybe.map (\c -> c.id) ooc_creature)
        then {mapc | highlight = Just Moving}
        else mapc
      vCreatures = List.map highlightMovingCreature creatures
  in
    tileTargetingMap model moveMsg map targetPoints vCreatures (always M.NoMsg)

movementGhost : M.Model -> Maybe T.Point3
movementGhost model =
  case model.showingMovement of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing

-- a map with arbitrary clickable tiles. Clicking those tiles will trigger the targetMsg.
tileTargetingMap : M.Model -> (T.Point3 -> M.Msg) -> T.Map -> List T.Point3 -> List MapCreature
                 -> (T.Point3 -> M.Msg)
                 -> Svg M.Msg
tileTargetingMap model targetMsg map targetableTiles vCreatures onHover =
  let extras = targetTiles targetMsg targetableTiles onHover
  in baseMap model map vCreatures extras Nothing

targetTiles : (T.Point3 -> M.Msg) -> List T.Point3 -> (T.Point3 -> M.Msg) -> List (Svg M.Msg)
targetTiles targetMsg pts onHover =
  let movementTarget pt =
        tile "lawngreen" [fillOpacity "0.3", onClick (targetMsg pt), onMouseOver (onHover pt)]
             (T.point3ToTup pt)
  in List.map movementTarget pts

baseMap : M.Model -> T.Map -> List MapCreature -> List (Svg M.Msg) -> Maybe (T.Point3 -> M.Msg)
        -> Svg M.Msg
baseMap model map creatures extras paint =
  let
    contents =
      let creatureEls = List.map gridCreature creatures
          terrainEls = baseTerrainRects model paint map.terrain
          ghostEl = case movementGhost model of
                      Just pt -> [tile "black" [] (T.point3ToTup pt)]
                      Nothing -> []
          (specialEls, overlays) = List.unzip <| List.map (specialTile model paint) (Dict.toList map.specials)
          _ = Debug.log "Updating map contents!" ()
      in g [] (terrainEls ++ specialEls ++ extras ++ creatureEls ++ ghostEl ++ overlays)
    gridTranslateX = toString <| -model.gridOffset.x * 50
    gridTranslateY = toString <| model.gridOffset.y * 50
    gridScale = toString <|  1 + (toFloat -model.gridSize / 100)
    matrixArgs = String.join ", " [gridScale, "0", "0", gridScale, gridTranslateX, gridTranslateY]
  in
    svg
      [ preserveAspectRatio "xMinYMid slice"
      , s [ S.width (S.pct 100)
          , S.height (S.pct 100)
          , S.backgroundColor (S.rgb 215 215 215)]
      ]
      [g [transform <| "matrix(" ++ matrixArgs ++ ")"]
        [contents]
      ]

specialTile : M.Model -> Maybe (T.Point3 -> M.Msg) -> (T.Point3Tup, (T.Color, String, T.Visibility)) -> (Svg M.Msg, Svg M.Msg)
specialTile model paint ((ptx, pty, ptz), (color, note, vis)) =
  let
    positionedText t =
      text_ [ HA.style [("pointer-events", "none")]
            , x (toString <| (ptx * 100) + 50)
            , y (toString <| (pty * 100) + 50)
            , fontSize "100px"
            , dominantBaseline "central"
            , textAnchor "middle"
            , fill "white"
            , stroke "black"
            , strokeWidth "2px"
            ]
            [text t]
    star =
      if note /= "" && (not <| Maybe.withDefault False (Dict.get key model.collapsed)) then
        positionedText "*"
      else text ""
    expandedNote =
      if Maybe.withDefault False (Dict.get key model.collapsed)
      then positionedText note
      else text ""
    key = "special-tile:" ++ toString ptx ++ "," ++ toString pty
    click =
      case paint of
        Just f -> M.Lazy (\() -> f (T.tupToPoint3 (ptx, pty, ptz)))
        Nothing -> M.ToggleCollapsed key
  in
    ( g [] [tile color [onClick click] (ptx, pty, ptz), star]
    , expandedNote)

-- return all points within a square with half-distance `distance`.
calculateAllMovementOptions : T.Point3 -> Int -> List T.Point3
calculateAllMovementOptions from distance =
  let xs = List.range (from.x - distance) (from.x + distance)
      ys = List.range (from.y - distance) (from.y + distance)
  in List.concatMap (\x -> List.map (\y -> { x=x, y=y, z=0 }) ys) xs

gridCreature : MapCreature -> Svg M.Msg
gridCreature creature =
  let creatureColor = creature.class.color
      strokeColor =
        case creature.highlight of
          Nothing -> "black"
          Just Moving -> "blue"
          Just Targetable -> "red"
          Just Current -> "black"
      strokeWidthSize =
        if MEx.isJust creature.highlight
        then 10
        else 1
      clickableEventHandler =
        case creature.clickable of
          Just fn -> [onClick (fn creature.creature)]
          Nothing -> []
      attrs = [stroke strokeColor, strokeWidth (toString strokeWidthSize), rx "10", ry "10"] ++ clickableEventHandler
      pos = creature.pos
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
          , rx "10"
          , ry "10"
          ] []
      opa = if creature.visible then "1" else "0.4" 
      foreground =
        if creature.creature.portrait_url == ""
        then creatureNameEl (String.slice 0 4 creature.creature.name)
        else creatureImageEl creature.creature.portrait_url
  in g [opacity opa]
    [ tile creatureColor attrs (T.point3ToTup creature.pos)
    , foreground ]

baseTerrainRects : M.Model -> Maybe (T.Point3 -> M.Msg) -> Set.Set T.Point3Tup -> List (Svg M.Msg)
baseTerrainRects model paint terrain =
  let blocks = List.map (gridTerrain paint) (Set.toList terrain)
      empties = case paint of
        Just paint -> emptyTerrain model terrain paint
        Nothing -> []
  in blocks ++ empties

gridTerrain : Maybe (T.Point3 -> M.Msg) -> T.Point3Tup -> Svg M.Msg
gridTerrain paint pt =
  let attrs =
        case paint of
          Just paint -> [onClick (M.Lazy (\() -> paint (T.tupToPoint3 pt)))]
          Nothing -> []
  in tile "white" attrs pt

emptyTerrain : M.Model -> Set.Set T.Point3Tup -> (T.Point3 -> M.Msg) -> List (Svg M.Msg)
emptyTerrain model terrain paint =
  let
    ptTup {x, y, z} = (x, y, z)
    emptyTerrainTile pt = tile "grey" [onClick (M.Lazy (\() -> paint pt))] (T.point3ToTup pt)
    g x y = let pt = {x = x, y = y, z = 0}
            in if not (Set.member (ptTup pt) terrain) then [emptyTerrainTile pt] else []
    f x = List.concatMap (g x) (List.range -50 50)
    empties = List.concatMap f (List.range -50 50)
  in empties

tile : String -> List (Svg.Attribute M.Msg) -> T.Point3Tup -> Svg M.Msg
tile cl attrs (ptx, pty, _) =
  rect ([ width "100"
       , height "100"
       , x (coord ptx)
       , y (coord pty)
       , fill cl
       , stroke "black"
       , strokeWidth "1" ] ++ attrs)
       []
