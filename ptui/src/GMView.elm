module GMView exposing (viewGame)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set

import Model as M
import Types as T
import Grid
import Update as U
import Elements exposing (..)

import CommonView

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports


-- Layout the entire game. This renders the map in the "background", and overlays various UI
-- elements on top.
viewGame : M.Model -> T.App -> Html M.Msg
viewGame model app =
  let theMap = chooseMap model app
      mapControlsOverlay = mapControls model app
      creaturesOverlay = creatureList model app
      -- combatOverlay = combatList model app
      -- creatureSelectorOverlay = creatureSelector model app
  in
    div
      [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98)]]
      [ div [s [S.position S.absolute, S.left (S.px 0), S.top (S.px 0), S.height (S.pct 100)]] [theMap]
      , div [s [S.position S.absolute, S.left (S.px 0)]] [mapControlsOverlay]
      , div [s [S.position S.absolute, S.left (S.vw -20)]] [creaturesOverlay]
      ]


-- Figure out which map to render. There are various modes that the game might be in which affect
-- how we render the map: movement, editing, regular play.
chooseMap : M.Model -> T.App -> Html M.Msg
chooseMap model app = Grid.terrainMap model Nothing model.currentMap (CommonView.visibleCreatures model app.current_game)

mapControls model app =
  vabox
    [s [ S.border2 (S.px 2) S.solid
       , S.backgroundColor (S.rgb 230 230 230)]]
    [ button [onClick (M.MapZoom M.In)] [text "+"]
    , button [onClick (M.MapZoom M.Out)] [text "-"]
    , vbox
        [ button [onClick (M.MapPan M.Up)] [text "^"]
        , hbox
            [ button [onClick (M.MapPan M.Left)] [text "<"]
            , button [onClick (M.MapPan M.Right)] [text ">"]
            ]
        , button [onClick (M.MapPan M.Down)] [text "v"]
        ]
    ]

creatureList model app = div [] []
