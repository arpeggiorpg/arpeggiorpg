module GMView exposing (viewGame)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeEx
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

collapsible collapsed msg content =
  vbox <|
    [ button [onClick msg] [text ">"]
     ] ++ if collapsed then [] else [content]

{-| Layout the entire game. This renders the map in the "background", and overlays various UI
elements on top.
-}
viewGame : M.Model -> T.App -> Html M.Msg
viewGame model app =
  div
    -- TODO: I should maybe move the "vh" to a div up all the way to the very top of the tree.
    [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98), S.overflow S.hidden]]
    <| 
    [ overlay (S.px 0)  (S.px 0) [S.height (S.pct 100)]
        [mapView model app]
    , overlay (S.px 0)  (S.px 0) [S.width (S.px 80)]
        [CommonView.mapControls]
    , overlay (S.px 80) (S.px 0) []
        [mapConsole model app]
    , overlay (S.vw 80) (S.px 0) []
        [
          vbox 
            [ collapsible model.collapsed.availableCreatures M.ToggleCollapsedAvailableCreatures
                (availableCreaturesView model app)
            , combatView model app 
            ]
        ]
    ] ++ (modalOverlay model app)

{-| Check if any modal prompts should be done based on the model, and render the appropriate one. -}
modalOverlay : M.Model -> T.App -> List (Html M.Msg)
modalOverlay model app =
  let
    game = app.current_game
    selectingCreatures =
      Maybe.map (\(selected, cb, name) -> selectCreaturesView model app selected cb name)
        model.selectingCreatures
    selectingTargets =
      -- TODO: target selection should be done on the map
      case model.selectedAbility of
        Just (cid, abid) -> 
          if T.isCreatureInCombat game cid
          then Just (CommonView.targetSelector model game M.CombatAct abid)
          else Nothing
        Nothing -> Nothing
    modal = MaybeEx.or selectingCreatures selectingTargets
  in
    case modal of
      Just m ->
        let box =
              div [s [S.position S.absolute
                     , S.left (S.pct 50)
                     , S.transform (S.translateX (S.pct -50))
                     , plainBorder
                     , S.backgroundColor (S.rgb 255 255 255)]]
                  [m]
            cover =
              div [s [S.position S.absolute
                     , S.width (S.vw 100)
                     , S.height (S.vh 100)
                     , S.backgroundColor (S.rgba 0 0 0 0.5)]]
                  []
        in [cover, box]
      Nothing -> []

{-| Figure out which map to render. There are various modes that the game might be in which affect
how we render the map: movement, editing, regular play.
-}
mapView : M.Model -> T.App -> Html M.Msg
mapView model app = Grid.terrainMap model Nothing model.currentMap (CommonView.visibleCreatures model app.current_game)

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

{-| Various GM-specific controls for affecting the map. -}
mapConsole model app =
  let
    editMapButton = button [onClick M.StartEditingMap] [text "Edit this map"]
    mapSelector = vbox <|
      let mapSelectorItem name = button [onClick (M.SendCommand (T.SelectMap name))] [text name]
      in (List.map mapSelectorItem (Dict.keys app.current_game.maps))
    oocToggler =
        hbox [text "Show Out-of-Combat creatures: "
        , input [type_ "checkbox", checked model.showOOC, onClick M.ToggleShowOOC] []
        ]
  in hbox [editMapButton, mapSelector, oocToggler]

{-| A view that allows selecting creatures in a particular order and calling a callback when done.
-}
selectCreaturesView : M.Model -> T.App -> List T.CreatureID -> M.GotCreatures -> String -> Html M.Msg
selectCreaturesView model app selectedCreatures callback commandName =
  let selectButton creature =
        button [onClick (M.ToggleSelectedCreature creature.id)
               , s [S.height (S.px 100), S.width (S.px 100)]]
               [text "Add"]
      unselectButton cid =
        button [ onClick (M.ToggleSelectedCreature cid)
               , s [S.height (S.px 100), S.width (S.px 100)]]
               [text "Remove"]
      selectableCreature creature =
        hbox [selectButton creature, CommonView.creatureCard [noteBox model creature] model creature]
      selectableCreatureItems =
        vbox <| List.map selectableCreature (Dict.values app.current_game.creatures)
      selectedCreatureItem cid =
        hbox [strong [] [text cid], unselectButton cid]
      selectedCreatureItems = vbox <| List.map selectedCreatureItem selectedCreatures
      doneSelectingButton = button [onClick M.DoneSelectingCreatures] [text commandName]
      cancelButton = button [onClick M.CancelSelectingCreatures] [text "Cancel"]
  in vbox <|
    [h3 [] [text <| "Select Creatures to " ++ commandName]
    , hbox [selectableCreatureItems, selectedCreatureItems]
    , hbox [doneSelectingButton, cancelButton]]

{-| Render combat if we're in combat, or a Start Combat button if not -}
combatView : M.Model -> T.App -> Html M.Msg
combatView model app =
  case app.current_game.current_combat of
    Just com -> inCombatView model app com
    Nothing -> startCombatButton

inCombatView : M.Model -> T.App -> T.Combat -> Html M.Msg
inCombatView model app combat =
  let game = app.current_game
      bar = CommonView.combatActionBar game combat (T.combatCreature combat)
      disengageButtons = hbox (List.map disengageButton combat.creatures.data)
      extraGutter idx creature =
        [ button [ onClick (M.SendCommand (T.ChangeCreatureInitiative creature.id (idx - 1)))
                 , disabled (idx == 0)]
                 [text "⬆️️"]
        , button [ onClick (M.SendCommand (T.ChangeCreatureInitiative creature.id (idx + 1)))
                 , disabled (idx == (List.length combat.creatures.data) - 1)]
                 [text "⬇️️"]
        ]
      combatView = vbox [ bar, CommonView.combatantList extraGutter model game combat, stopCombatButton, disengageButtons]
  in combatView

{-| A button for starting combat. -}
startCombatButton : Html M.Msg
startCombatButton =
  let gotCreatures cids = U.message (M.SendCommand (T.StartCombat cids))
  in button [onClick (M.SelectCreatures gotCreatures "Start Combat")] [text "Start Combat"]

{-| A button for stopping combat. -}
stopCombatButton : Html M.Msg
stopCombatButton = button [onClick (M.SendCommand T.StopCombat)] [text "Stop Combat"]
