module Grid exposing (..)

import Dict
import Html.Attributes as HA
import Css as S

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

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
  , highlight : Bool
  , clickable : Maybe (T.Creature -> M.Msg)
  , class : T.Class
  , pos : T.Point3
  , visible: Bool
  }

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
      movementTiles = movementTargets moveMsg targetPoints map.terrain movingFrom max_distance
      highlightMovingCreature : MapCreature -> MapCreature
      highlightMovingCreature mapc =
        if (Just mapc.creature.id) == (Maybe.map (\c -> c.id) ooc_creature)
        then {mapc | highlight = True}
        else mapc
      vCreatures = List.map highlightMovingCreature creatures
  in
    baseMap model map vCreatures movementTiles Nothing

movementGhost : M.Model -> Maybe T.Point3
movementGhost model =
  case model.showingMovement of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing

baseMap : M.Model -> T.Map -> List MapCreature -> List (Svg M.Msg) -> Maybe (T.Point3 -> M.Msg) -> Svg M.Msg
baseMap model map creatures extras paint =
  let creatureEls = List.map gridCreature creatures
      terrainEls = baseTerrainRects model paint map.terrain
      ghostEl = case movementGhost model of
                  Just pt -> [tile "black" [] pt]
                  Nothing -> []
      (specialEls, overlays) = List.unzip <| List.map (specialTile model paint) map.specials
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
         (terrainEls ++ extras ++ specialEls ++ creatureEls ++ ghostEl ++ overlays)
      ]

specialTile : M.Model -> Maybe (T.Point3 -> M.Msg) -> (T.Point3, T.Color, String, T.Visibility) -> (Svg M.Msg, Svg M.Msg)
specialTile model paint (pt, color, note, vis) =
  let
    positionedText t =
      text_ [ HA.style [("pointer-events", "none")]
            , x (toString <| (pt.x * 100) + 50)
            , y (toString <| (pt.y * 100) + 50)
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
    key = "special-tile:" ++ toString pt
    click =
      case paint of
        Just f -> f pt
        Nothing -> M.ToggleCollapsed key
  in
    ( g [] [tile color [onClick click] pt, star]
    , expandedNote)

calculateAllMovementOptions : T.Point3 -> Int -> List T.Point3
calculateAllMovementOptions from distance =
  let xs = List.range (from.x - distance) (from.x + distance)
      ys = List.range (from.y - distance) (from.y + distance)
  in List.concatMap (\x -> List.map (\y -> { x=x, y=y, z=0 }) ys) xs

movementTargets : (T.Point3 -> M.Msg) -> List T.Point3 -> List T.Point3 -> T.Point3 -> Int -> List (Svg M.Msg)
movementTargets moveMsg pts terrain origin max_distance =
  let movementTarget pt = tile "lawngreen" [fillOpacity "0.3", onClick (moveMsg pt)] pt
  in List.map movementTarget pts

gridCreature : MapCreature -> Svg M.Msg
gridCreature creature =
  let creatureColor = creature.class.color
      strokeColor =
        if creature.highlight
        then "blue"
        else "black"
      strokeWidthSize =
        if creature.highlight
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
    [ tile creatureColor attrs creature.pos
    , foreground ]

baseTerrainRects : M.Model -> Maybe (T.Point3 -> M.Msg) -> List T.Point3 -> List (Svg M.Msg)
baseTerrainRects model paint terrain =
  let blocks = List.map (gridTerrain paint) terrain
      empties = case paint of
        Just paint -> emptyTerrain model terrain paint
        Nothing -> []
  in blocks ++ empties

gridTerrain : Maybe (T.Point3 -> M.Msg) -> T.Point3 -> Svg M.Msg
gridTerrain paint pt =
  let attrs =
        case paint of
          Just paint -> [onClick (paint pt)]
          Nothing -> []
  in tile "white" attrs pt

emptyTerrain : M.Model -> List T.Point3 -> (T.Point3 -> M.Msg) -> List (Svg M.Msg)
emptyTerrain model terrain paint =
  let
    emptyTerrainTile pt = tile "grey" [onClick (paint pt)] pt
    g x y = let pt = {x = x, y = y, z = 0}
            in if not (List.member pt terrain) then [emptyTerrainTile pt] else []
    f x = List.concatMap (g x) (List.range -50 50)
    empties = List.concatMap f (List.range -50 50)
    _ = Debug.log "Number of empties: " (List.length empties)
  in empties

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
