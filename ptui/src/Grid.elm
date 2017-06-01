module Grid exposing (..)

import Dict
import Html.Attributes as HA
import Css as S
import Set

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Lazy as Lazy

import Maybe.Extra as MEx

import Elements exposing (hbox, vbox, s)
import Types as T
import Model as M


type alias GridModel a =
  -- The subset of the model that we care about in grid rendering.
  -- it'd be nice if this were actually a regular record that was embedded in the main Model, but
  -- using this instead of M.Model at least ensures we're not doing too much wacky stuff with the
  -- main app model.
  { a
  | gridOffset: {x: Float, y: Float}
  , gridSize: Int
  , showingMovement: M.MovementAnimation
  , gridSpecialExpanded: Maybe T.Point3
  }


-- Convert Point3 coordinates to on-screen coordinates.
-- Point3 coordinates are in METERS, and Distances are in CENTIMETERS.
coord : Int -> String
coord c = toString (c * 100)

terrainMap : GridModel a -> T.Map -> List M.MapCreature -> Svg M.Msg
terrainMap model map creatures = baseMap model map creatures []

editMap : GridModel a -> T.Map -> List M.MapCreature -> Svg M.Msg
editMap model map creatures =
  mapContainer (mapContents True model map creatures [])

movementMap : GridModel a -> (T.Point3 -> M.Msg) -> M.MovementRequest -> Bool -> T.Map -> T.Point3
           -> List M.MapCreature
           -> Svg M.Msg
movementMap model moveMsg {max_distance, movement_options, ooc_creature} moveAnywhere map movingFrom creatures =
  let targetPoints =
        if moveAnywhere
        then calculateAllMovementOptions movingFrom (max_distance // 100)
        else movement_options
      movementTiles = targetTiles moveMsg targetPoints
      highlightMovingCreature mapc =
        if (Just mapc.creature.id) == (Maybe.map .id ooc_creature)
        then {mapc | highlight = Just M.Moving}
        else mapc
      vCreatures = List.map highlightMovingCreature creatures
  in
    tileTargetingMap model moveMsg map targetPoints [] vCreatures

movementGhost : M.MovementAnimation -> Maybe T.Point3
movementGhost anim =
  case anim of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing

-- a map with arbitrary clickable tiles. Clicking those tiles will trigger the targetMsg.
tileTargetingMap : GridModel a -> (T.Point3 -> M.Msg) -> T.Map -> List T.Point3 -> List T.Point3
                 -> List M.MapCreature
                 -> Svg M.Msg
tileTargetingMap model targetMsg map targetableTiles highlightedPoints vCreatures =
  let extras = targetTiles targetMsg targetableTiles ++ highlightedTiles highlightedPoints
  in baseMap model map vCreatures extras

highlightedTiles : List T.Point3 -> List (Svg M.Msg)
highlightedTiles pts =
  let
    ht pt = tile "pink" [fillOpacity "0.9", HA.style [("pointer-events", "none")]]
                 (T.point3ToTup pt)
  in List.map ht pts

targetTiles : (T.Point3 -> M.Msg) -> List T.Point3 -> List (Svg M.Msg)
targetTiles targetMsg pts =
  let movementTarget pt =
        tile "lawngreen" [fillOpacity "0.3", onClick (targetMsg pt)]
             (T.point3ToTup pt)
  in List.map movementTarget pts

baseMap : GridModel a -> T.Map -> List M.MapCreature -> List (Svg M.Msg) -> Svg M.Msg
baseMap model map creatures extras =
  mapContainer (mapContents False model map creatures extras)

mapContainer : Svg M.Msg -> Svg M.Msg
mapContainer content =
  svg
    [ id "grid-svg"
    , preserveAspectRatio "xMinYMid slice"
    , s [ S.width (S.pct 100)
        , S.height (S.pct 100)
        , S.backgroundColor (S.rgb 215 215 215)]
    ]
    [g [] [content]]

mapContents : Bool -> GridModel a -> T.Map -> List M.MapCreature -> List (Svg M.Msg) -> Svg M.Msg
mapContents editable model map creatures extras =
  let creatureEls = List.map gridCreature creatures
      terrainEls = if editable then Lazy.lazy editTerrainRects map.terrain else Lazy.lazy viewTerrainRects map.terrain
      emptyEls = if editable then Lazy.lazy emptyTerrain map.terrain else text ""
      ghostEl = case movementGhost model.showingMovement of
                  Just pt -> [tile "black" [] (T.point3ToTup pt)]
                  Nothing -> []
      specialEls =
        if editable
        then Lazy.lazy2 specialTerrainEdit model.gridSpecialExpanded map.specials
        else Lazy.lazy2 specialTerrainView model.gridSpecialExpanded map.specials
      annotationEl = expandedNote model.gridSpecialExpanded map.specials
      origin = rect [ width "10" , height "10" , x "0", y "0"
                    , fill "black"
                    , stroke "black" , strokeWidth "1" ]
                    []
  in g [] <| [origin, terrainEls, emptyEls, specialEls] ++ extras ++ creatureEls ++ [annotationEl] ++ ghostEl

-- these specialized versions of specialTerrain are necessary due to the way Svg.Lazy works.
specialTerrainView : Maybe T.Point3 -> Dict.Dict T.Point3Tup (T.Color, String, T.Visibility) -> Svg M.Msg
specialTerrainView = specialTerrain False

specialTerrainEdit : Maybe T.Point3 -> Dict.Dict T.Point3Tup (T.Color, String, T.Visibility) -> Svg M.Msg
specialTerrainEdit = specialTerrain True

expandedNote : Maybe T.Point3 -> Dict.Dict T.Point3Tup (T.Color, String, T.Visibility) -> Svg M.Msg
expandedNote expanded specials =
  case expanded of
    Just pt ->
      case Dict.get (T.point3ToTup pt) specials of
        Just (_, note, _) -> positionedText note ((pt.x * 100) + 50, (pt.y * 100) + 50) "100px"
        Nothing -> text ""
    Nothing -> text ""

specialTerrain : Bool -> Maybe T.Point3 -> Dict.Dict T.Point3Tup (T.Color, String, T.Visibility) -> Svg M.Msg
specialTerrain editable expanded specials =
  let
    _ = Debug.log "[EXPENSIVE:specialTerrain]" ()
    specialEls = List.map (specialTile editable expanded) (Dict.toList specials)
  in g [] specialEls


positionedText t (ptx, pty) fsize =
  text_ [ HA.style [("pointer-events", "none")]
        , x (toString <| ptx)
        , y (toString <| pty)
        , fontSize fsize
        , dominantBaseline "central"
        , textAnchor "middle"
        , fill "white"
        , stroke "black"
        , strokeWidth "2px"
        ]
        [text t]

specialTile : Bool -> Maybe T.Point3 -> (T.Point3Tup, (T.Color, String, T.Visibility)) -> Svg M.Msg
specialTile editable expanded (pt, (color, note, vis)) =
  let
    (ptx, pty, ptz) = pt
    noteExpanded = expanded == Just (T.tupToPoint3 pt)
    centeredText note = positionedText note (((ptx * 100) + 50), ((pty * 100) + 50)) "100px"
    star =
      if note /= "" && (not noteExpanded)
      then centeredText "*"
      else text ""
    key = "special-tile:" ++ toString ptx ++ "," ++ toString pty
    click = if editable then M.GridPaint else M.ToggleGridSpecial
    hiddenIcon = if vis == T.GMOnly then positionedText "👁️" ((ptx * 100) + 85, ((pty * 100) + 85)) "25px" else text ""
  in
    g [] [tile color [onClick (click (T.tupToPoint3 pt))] (ptx, pty, ptz)
         , star
         , hiddenIcon]

-- return all points within a square with half-distance `distance`.
calculateAllMovementOptions : T.Point3 -> Int -> List T.Point3
calculateAllMovementOptions from distance =
  let xs = List.range (from.x - distance) (from.x + distance)
      ys = List.range (from.y - distance) (from.y + distance)
  in List.concatMap (\x -> List.map (\y -> { x=x, y=y, z=0 }) ys) xs

gridCreature : M.MapCreature -> Svg M.Msg
gridCreature creature =
  let creatureColor = creature.class.color
      strokeColor =
        case creature.highlight of
          Nothing -> "black"
          Just M.Moving -> "blue"
          Just M.Targetable -> "red"
          Just M.Current -> "black"
          Just M.Affected -> "red"
      strokeWidthSize =
        if MEx.isJust creature.highlight
        then 10
        else 1
      clickableEventHandler =
        case creature.clickable of
          Just fn -> [onClick (fn creature.creature)]
          Nothing -> []
      creatureWidth = creature.creature.size.x * 100
      creatureHeight = creature.creature.size.y * 100
      attrs = [stroke strokeColor, strokeWidth (toString strokeWidthSize)
              , rx "10", ry "10"
              , width (toString creatureWidth), height (toString creatureHeight)
              ] ++ clickableEventHandler
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
          , width (toString creatureWidth)
          , height (toString creatureHeight)
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

baseTerrainRects : Bool -> Set.Set T.Point3Tup -> Svg M.Msg
baseTerrainRects editable terrain =
  let blocks = List.map (gridTerrain editable) (Set.toList terrain)
      _ = Debug.log "[EXPENSIVE:baseTerrainRects]" ()
  in g [] blocks

-- these specialized versions of baseTerrainRects are necessary due to the way Svg.Lazy works.
editTerrainRects = baseTerrainRects True
viewTerrainRects = baseTerrainRects False

gridTerrain : Bool -> T.Point3Tup -> Svg M.Msg
gridTerrain editable pt =
  let 
    attrs =
      if editable
      then [onClick (M.GridPaint (T.tupToPoint3 pt))]
      else []
  in tile "white" attrs pt

emptyTerrain : Set.Set T.Point3Tup -> Svg M.Msg
emptyTerrain terrain =
  let
    _ = Debug.log "[EXPENSIVE:emptyTerrain]" ()
    ptTup {x, y, z} = (x, y, z)
    emptyTerrainTile pt = tile "grey" [onClick (M.GridPaint pt)] (T.point3ToTup pt)
    col x y = let pt = {x = x, y = y, z = 0}
              in if not (Set.member (ptTup pt) terrain) then [emptyTerrainTile pt] else []
    row x = List.concatMap (col x) (List.range -50 50)
    empties = List.concatMap row (List.range -50 50)
  in g [] empties

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
