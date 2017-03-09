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
button = Elements.button

{-| Layout the entire game. This renders the map in the "background", and overlays various UI
elements on top.
-}
viewGame : M.Model -> T.App -> Html M.Msg
viewGame model app =
  let theMap = chooseMap model app
      creaturesOverlay = availableCreaturesView model app
      mapConsoleOverlay = mapConsole model app
      -- combatOverlay = combatList model app
      -- creatureSelectorOverlay = creatureSelector model app
  in
    div
      -- TODO: I should maybe move the "vh" to a div up all the way to the very top of the tree.
      [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98), S.overflow S.hidden]]
      [ overlay (S.px 0)  (S.px 0) [S.height (S.pct 100)] [theMap]
      , overlay (S.px 0)  (S.px 0) [S.width (S.px 80)] [CommonView.mapControls]
      , overlay (S.px 80) (S.px 0) [] [mapConsoleOverlay]
      , overlay (S.vw 80) (S.px 0) [] [creaturesOverlay]
      ]

{-| Figure out which map to render. There are various modes that the game might be in which affect
how we render the map: movement, editing, regular play.
-}
chooseMap : M.Model -> T.App -> Html M.Msg
chooseMap model app = Grid.terrainMap model Nothing model.currentMap (CommonView.visibleCreatures model app.current_game)

{-| A navigator for available creatures, i.e., those that aren't in combat.
-}
availableCreaturesView : M.Model -> T.App -> Html M.Msg
availableCreaturesView model app =
  let game = app.current_game
  in div []
  [ div [] (List.map (availableCreatureEntry model game) (Dict.values game.creatures)) ]

{-| A creature card plus some UI relevant for when they are out-of-combat. -}
availableCreatureEntry : M.Model -> T.Game -> T.Creature -> Html M.Msg
availableCreatureEntry model game creature = vbox <|
  [hbox <|
    [ CommonView.creatureCard [noteBox model creature] model creature
    ] ++ case game.current_combat of
        Just _ -> [engageButton creature]
        Nothing -> []
    ++ [
      deleteCreatureButton creature
    ], hbox (CommonView.oocActionBar game creature)]

{-| An area for writing notes about a Creature. Intended to be passed as the "extras" argument to 
creatureCard.
-}
noteBox : M.Model -> T.Creature -> Html M.Msg
noteBox model creature = 
  let note = Maybe.withDefault creature.note (Dict.get creature.id model.creatureNotes)
      inp = input [type_ "text", value note, onInput (M.SetCreatureNote creature.id)] []
      saveButton =
        if creature.note /= note
        then [button [onClick (M.SendCommand (T.SetCreatureNote creature.id note))] [text "Save Note"]]
        else []
  in hbox <| [inp] ++ saveButton

{-| A button for engaging a creature in combat. -}
engageButton : T.Creature -> Html M.Msg
engageButton creature =
  button [onClick (M.SendCommand (T.AddCreatureToCombat creature.id))] [text "Engage"]

{-| A button for removing a creature from combat. -}
disengageButton : T.Creature -> Html M.Msg
disengageButton creature =
  button [onClick (M.SendCommand (T.RemoveCreatureFromCombat creature.id))] [text ("Disengage " ++ creature.id)]

{-| A button for deleting a creature entirely -}
deleteCreatureButton : T.Creature -> Html M.Msg
deleteCreatureButton creature =
  button [onClick (M.SendCommand (T.RemoveCreature creature.id))] [text "Delete"]


mapConsole model app = hbox [editMapButton, mapSelector app.current_game, oocToggler model]

editMapButton : Html M.Msg
editMapButton = button [onClick M.StartEditingMap] [text "Edit this map"]

mapSelector : T.Game -> Html M.Msg
mapSelector game = vbox <|
  let mapSelectorItem name = button [onClick (M.SendCommand (T.SelectMap name))] [text name]
  in (List.map mapSelectorItem (Dict.keys game.maps))

oocToggler : M.Model -> Html M.Msg
oocToggler model =
  hbox [text "Show Out-of-Combat creatures: "
       , input [type_ "checkbox", checked model.showOOC, onClick M.ToggleShowOOC] []
       ]
