module GMView exposing (viewGame)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeEx

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
  div
    -- TODO: I should maybe move the "vh" to a div up all the way to the very top of the tree.
    -- TODO: history
    -- TODO: player control
    [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98)]]
    <| 
    [ overlay (S.px 0)  (S.px 0) [S.height (S.pct 100)]
        [mapView model app]
    , overlay (S.px 0)  (S.px 0) [S.width (S.px 80)]
        [CommonView.mapControls]
    , overlay (S.px 80) (S.px 0) []
        [mapConsole model app]
    , overlayRight (S.px 0) (S.px 0) [S.width (S.px 325)]
        [
          vbox 
            [ CommonView.collapsible "Available Creatures" model (availableCreaturesView model app)
            , combatView model app 
            ]
        ]
    ]
    ++ movementConsole model
    ++ editMapConsole model
    ++ modalOverlay model app

editMapConsole : M.Model -> List (Html M.Msg)
editMapConsole model =
  let console =
        overlay (S.px 80) (S.px 60) []
          [ button [onClick M.CancelEditingMap] [text "Cancel Editing Map"]
          , hbox
            [ input [type_ "text", placeholder "map name", onInput M.UpdateSaveMapName ] []
            , button [onClick (M.EditMap model.currentMap)] [text "Save"]
            ]
          ]
  in if model.editingMap then [console] else []

movementConsole : M.Model -> List (Html M.Msg)
movementConsole model =
  case model.moving of
    Just _ ->
      [overlay (S.px 80) (S.px 60) []
        [ button [onClick M.CancelMovement] [text "Cancel Movement"]
        , hbox [ text "Allow movement anywhere: ", input [type_ "checkbox", checked model.moveAnywhere, onClick M.ToggleMoveAnywhere] []]]]
    Nothing -> []

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
    creatingCreature = Maybe.map (createCreatureDialog model app) model.creatingCreature
    modal = selectingCreatures |> MaybeEx.orElse selectingTargets |> MaybeEx.orElse creatingCreature
  in
    case modal of
      Just m ->
        let box =
              div [s [ S.position S.absolute
                     , S.left (S.pct 50)
                     , S.top (S.pct 50)
                     , S.transform (S.translate2 (S.pct -50) (S.pct -50))
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
how we render the map: combat, movement, editing, regular play.
-}
mapView : M.Model -> T.App -> Html M.Msg
mapView model app =
  let game = app.current_game
      movementGrid msg mvmtReq creature = Grid.movementMap model msg mvmtReq model.moveAnywhere model.currentMap creature vCreatures
      movementMap =
        case (game.current_combat, model.moving) of
          (Nothing, Just mvmtReq) ->
            Maybe.map (\creature -> movementGrid (M.PathCreature creature.id) mvmtReq creature)
                      mvmtReq.ooc_creature
          (Just combat, Just mvmtReq) ->
            let (creature, moveMessage) =
                  case mvmtReq.ooc_creature of
                    Just creature -> (creature, M.PathCreature creature.id)
                    Nothing -> (T.combatCreature combat, M.PathCurrentCombatCreature)
            in Just <| movementGrid moveMessage mvmtReq creature
          _ -> Nothing
      editMap () = if model.editingMap
                   then Just <| Grid.editMap model model.currentMap vCreatures
                   else Nothing
      currentCombatCreature = Maybe.map (\com -> (T.combatCreature com).id) game.current_combat
      modifyMapCreature mapc =
        let highlight = (Just mapc.creature.id) == currentCombatCreature
        in { mapc | highlight = highlight
                  , movable = Just M.GetMovementOptions}
      vCreatures = List.map modifyMapCreature (CommonView.visibleCreatures model app.current_game)
      defaultMap () = Grid.terrainMap model model.currentMap vCreatures
  in movementMap
      |> MaybeEx.orElseLazy editMap
      |> withDefaultLazy defaultMap

withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy df m =
  case m of
    Just x -> x
    Nothing -> df ()

{-| A navigator for available creatures, i.e., those that aren't in combat.
-}
availableCreaturesView : M.Model -> T.App -> Html M.Msg
availableCreaturesView model app =
  let game = app.current_game
  in
    vbox
      [ button [onClick M.StartCreatingCreature] [text "Create Creature"]
      , hr [s [S.width (S.pct 100)]] []
      , vbox (List.map (availableCreatureEntry model game) (Dict.values game.creatures))
      ]

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
mapConsole : M.Model -> T.App -> Html M.Msg
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
    Just com -> CommonView.collapsible "Combat" model (inCombatView model app com)
    Nothing -> startCombatButton

{-| The content of what's rendered when we're actually in combat. -}
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
      extraCreatureCard creature = [noteBox model creature]
      combatView =
        vbox
          [ bar
          , CommonView.combatantList extraGutter extraCreatureCard model game combat
          , stopCombatButton
          , disengageButtons]
  in combatView

{-| A button for starting combat. -}
startCombatButton : Html M.Msg
startCombatButton =
  let gotCreatures cids = U.message (M.SendCommand (T.StartCombat cids))
  in button [onClick (M.SelectCreatures gotCreatures "Start Combat")] [text "Start Combat"]

{-| A button for stopping combat. -}
stopCombatButton : Html M.Msg
stopCombatButton = button [onClick (M.SendCommand T.StopCombat)] [text "Stop Combat"]

{-| A form for creating a creature. -}
createCreatureDialog : M.Model -> T.App -> M.PendingCreature -> Html M.Msg
createCreatureDialog model app {id, name, class} =
  let disabledButton = button [disabled True] [text "Create Creature"]
      createCreatureButton =
        case (id, name, class) of
          (Just id, Just name, Just class) ->
            let cc = T.CreatureCreation id name class {x= 0, y= 0, z=0} ""
            in button [onClick (M.CreateCreature cc)] [text "Create Creature"]
          _ -> disabledButton
      cancelCreationButton = button [onClick M.CancelCreatingCreature] [text "Cancel Creation"]
  in vbox
    [ input [type_ "text", placeholder "id", onInput M.SetCreatureId ] []
    , input [type_ "text", placeholder "name", onInput M.SetCreatureName ] []
    , select [onInput M.SetCreatureClass]
             <| [option [value ""] [text "Select a Class"]]
                ++ (List.map (\className -> option [value className] [text className])
                             (Dict.keys app.current_game.classes))
    , hbox [createCreatureButton, cancelCreationButton]
    ]

