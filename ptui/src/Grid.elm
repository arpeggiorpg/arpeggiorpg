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

type PaintStyle
  = NoPaint
  | PaintTerrain
  | PaintSpecial T.Color String T.Visibility

terrainMap : M.Model -> T.Map -> List MapCreature -> Svg M.Msg
terrainMap model map creatures = baseMap model map creatures [] NoPaint

editMap : M.Model -> T.Map -> List MapCreature -> PaintStyle -> Svg M.Msg
editMap model map creatures paint =
  baseMap model map creatures [] paint

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

movementGhost : M.MovementAnimation -> Maybe T.Point3
movementGhost anim =
  case anim of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing

-- a map with arbitrary clickable tiles. Clicking those tiles will trigger the targetMsg.
tileTargetingMap : M.Model -> (T.Point3 -> M.Msg) -> T.Map -> List T.Point3 -> List MapCreature
                 -> (T.Point3 -> M.Msg)
                 -> Svg M.Msg
tileTargetingMap model targetMsg map targetableTiles vCreatures onHover =
  let extras = targetTiles targetMsg targetableTiles onHover
  in baseMap model map vCreatures extras NoPaint

targetTiles : (T.Point3 -> M.Msg) -> List T.Point3 -> (T.Point3 -> M.Msg) -> List (Svg M.Msg)
targetTiles targetMsg pts onHover =
  let movementTarget pt =
        tile "lawngreen" [fillOpacity "0.3", onClick (targetMsg pt), onMouseOver (onHover pt)]
             (T.point3ToTup pt)
  in List.map movementTarget pts

baseMap : M.Model -> T.Map -> List MapCreature -> List (Svg M.Msg) -> PaintStyle -> Svg M.Msg
baseMap model map creatures extras paint =
  let
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
      [g [transform <| "matrix(" ++ matrixArgs ++ ")"] [Lazy.lazy mapContents (model.showingMovement, model.collapsed, map, creatures, paint, extras)]
      ]

mapContents (showingMovement, collapsed, map, creatures, paint, extras) =
  let creatureEls = List.map gridCreature creatures
      terrainEls = baseTerrainRects paint map.terrain
      ghostEl = case movementGhost showingMovement of
                  Just pt -> [tile "black" [] (T.point3ToTup pt)]
                  Nothing -> []
      (specialEls, overlays) = List.unzip <| List.map (specialTile collapsed paint) (Dict.toList map.specials)
      _ = Debug.log "OMG OMG RENDERING" ()
  in g [] (terrainEls ++ specialEls ++ extras ++ creatureEls ++ ghostEl ++ overlays)


specialTile : Dict.Dict String Bool -> PaintStyle -> (T.Point3Tup, (T.Color, String, T.Visibility)) -> (Svg M.Msg, Svg M.Msg)
specialTile collapsed paint (pt, (color, note, vis)) =
  let
    (ptx, pty, ptz) = pt
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
      if note /= "" && (not <| Maybe.withDefault False (Dict.get key collapsed)) then
        positionedText "*"
      else text ""
    expandedNote =
      if Maybe.withDefault False (Dict.get key collapsed)
      then positionedText note
      else text ""
    key = "special-tile:" ++ toString ptx ++ "," ++ toString pty
    click model =
      case model.focus of
        M.EditingMap path map special ->
          case paint of
            NoPaint -> M.ToggleCollapsed key
            PaintSpecial _ _ _ ->
              let
                newSpecials = Dict.remove pt map.specials
                newMap = {map | specials = newSpecials}
              in M.SetFocus (M.EditingMap path newMap special)
            _ -> M.NoMsg
        _ -> M.NoMsg
  in
    ( g [] [tile color [onClick (M.Lazy click)] (ptx, pty, ptz), star]
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

baseTerrainRects : PaintStyle -> Set.Set T.Point3Tup -> List (Svg M.Msg)
baseTerrainRects paint terrain =
  let blocks = List.map (gridTerrain paint) (Set.toList terrain)
      empties = case paint of
        NoPaint -> []
        p -> emptyTerrain terrain p
  in blocks ++ empties

gridTerrain : PaintStyle -> T.Point3Tup -> Svg M.Msg
gridTerrain paint pt =
  let click model =
        case paint of
          NoPaint -> M.NoMsg
          PaintTerrain ->
            case model.focus of
              M.EditingMap path map special ->
                let newMap = {map | terrain = Set.remove pt map.terrain}
                in M.SetFocus (M.EditingMap path newMap special)
              _ -> M.NoMsg
          PaintSpecial color note vis -> 
            case model.focus of
              M.EditingMap path map special ->
                let newMap = {map | specials = Dict.insert pt (color, note, vis) map.specials}
                in M.SetFocus (M.EditingMap path newMap special)
              _ -> M.NoMsg
  in tile "white" [onClick (M.Lazy click)] pt

emptyTerrain : Set.Set T.Point3Tup -> PaintStyle -> List (Svg M.Msg)
emptyTerrain terrain paint =
  let
    ptTup {x, y, z} = (x, y, z)
    emptyTerrainTile pt =
      let
        click model =
          case model.focus of
            M.EditingMap path map special ->
              case paint of
                NoPaint -> M.NoMsg
                PaintTerrain -> M.ToggleTerrain pt
                PaintSpecial color note vis ->
                  let newMap = {map | specials = Dict.insert (ptTup pt) (color, note, vis) map.specials}
                  in M.SetFocus (M.EditingMap path newMap special)
            _ -> M.NoMsg
      in tile "grey" [onClick (M.Lazy click)] (T.point3ToTup pt)
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
