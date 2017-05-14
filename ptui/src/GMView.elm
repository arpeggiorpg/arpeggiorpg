module GMView exposing (gmView)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy as Lazy
import Maybe.Extra as MaybeEx

import Model as M
import Types as T
import Grid
import Elements exposing (..)
import FolderView
import CommonView exposing (popUpMenu)

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

{-| Top-level GM view. -}
gmView : M.Model -> Html M.Msg
gmView model =
  case model.app of
    Just app ->
      CommonView.viewGame model app (makeUI model app)
    Nothing -> vbox [text "No app yet. Maybe reload."
                    , hbox [text "Last error:", pre [] [text model.error]]]

makeUI : M.Model -> T.App -> CommonView.UI
makeUI model app =
  let (map, mapModeControls) = mapView model app in
  { mapView = map
  , mapModeControls = mapModeControls
  , defaultTab = "Campaign"
  , sideBar =
        [ ("Campaign", (\() -> campaignView model app))
        , ("Combat", (\() -> combatView model app))
        , ("Players", (\() -> playersView model app))
        , ("History", (\() -> Lazy.lazy historyView app))
        , ("Saved Games", (\() -> savedGameView model app))
        ]
  , extraOverlays = [bottomActionBar app]
  , modal = checkModal model app
  }

savedGameView : M.Model -> T.App -> Html M.Msg
savedGameView model app =
  let
    loadButton =
      button [onClick (M.GetSavedGames (M.SetModal << M.ModalLoadGame))] [text "Load Game"]
    saveButton =
      button [onClick (M.GetSavedGames (\ns -> M.SetModal (M.ModalSaveGame {existing=ns, newGame=""})))]
             [text "Save Game"]
  in hbox [loadButton, saveButton]

campaignView : M.Model -> T.App -> Html M.Msg
campaignView model app =
  vabox [s [S.height (S.pct 100)]]
        [ Lazy.lazy2 FolderView.campaignFolder model.folderState app
        , secondaryFocusView model app]

secondaryFocusView : M.Model -> T.App -> Html M.Msg
secondaryFocusView model app =
  case model.secondaryFocus of
    M.Focus2None -> text ""
    M.Focus2Creature path cid ->
      case T.getCreature app.current_game cid of
        Just creature ->
          -- TODO: * move functionality from All Creatures here (add to combat?)
          let item = {key=T.FolderCreature cid, path=path, prettyName=creature.name}
          in console model app item (creatureConsole model app creature)
        Nothing -> text "Creature Disappeared"
    M.Focus2Note path origName note ->
      let item = {key=T.FolderNote origName, path=path, prettyName=origName}
      in console model app item (noteConsole model app path origName note)
    M.Focus2Map path mapID ->
      case M.getMapNamed mapID app of
        Just map ->
          let item = {key=T.FolderMap mapID, path=path, prettyName=map.name}
          in console model app item (mapConsole model app path mapID)
        Nothing -> text "Map Disappeared"
    M.Focus2Scene path sceneID ->
      case T.getScene app sceneID of
        Just scene ->
          let item = {key=T.FolderScene sceneID, path=path, prettyName=scene.name}
          in console model app item (sceneConsole model app scene)
        Nothing -> text "Scene Disappeared"

console : M.Model -> T.App -> M.FolderItem -> Html M.Msg -> Html M.Msg
console model app {key, path, prettyName} content =
  let
    menu = popUpMenu model "console-" (toString key) threeDots threeDots
                     menuItems
    menuItems = [ (text "Delete", deleteItem)
                , (text "Move", M.SetModal (M.MoveFolderItem {src=path, item=key, dst=[]}))
                ]
    deleteItem =
      case key of
        T.FolderScene k -> M.SendCommand (T.DeleteScene k)
        T.FolderNote name -> M.SendCommand (T.DeleteNote path name)
        T.FolderCreature cid -> M.SendCommand (T.DeleteCreature cid)
        T.FolderMap mid -> M.SendCommand (T.DeleteMap mid)
        T.FolderSubfolder name -> M.SendCommand (T.DeleteFolder (path ++ [name]))
  in
    vabox [s [S.marginTop (S.em 1), S.height (S.pct 100)]]
      [ hbox [text prettyName, text " in ", renderFolderPath path, menu]
      , content ]

creatureConsole : M.Model -> T.App -> T.Creature -> Html M.Msg
creatureConsole model app creature =
  let editCreatureLink =
        ( text "Edit Creature"
        , M.SetModal (M.ModalEditCreature {cid=creature.id, name=creature.name, note=creature.note, portrait_url=creature.portrait_url}))
      menu =
        div [s [S.position S.absolute, S.top (S.px 0), S.right (S.px 0)]]
            [popUpMenu model "creature-console-menu" creature.id gear gearBox [editCreatureLink]]
  in
  div [s [S.position S.relative]]
    [ CommonView.creatureCard [noteBox model creature, menu] app creature ]

sceneConsole : M.Model -> T.App -> T.Scene -> Html M.Msg
sceneConsole model app scene =
  let
    gotCreatures cids =
      let
        selectedCreatures = Dict.fromList (List.map (\c -> (c, {x=0, y=0, z=0})) cids)
        newCreatures =
          Dict.merge
            (\_ _ acc -> acc)
            (\cid l r acc -> Dict.insert cid l acc)
            (\cid pos acc -> Dict.insert cid (pos, T.AllPlayers) acc)
            scene.creatures selectedCreatures
            Dict.empty
        newScene = {scene | creatures = newCreatures}
      in M.SendCommand (T.EditScene newScene)
    selectCreatures = M.SetModal
      <| M.SelectCreaturesFromCampaign
          {cb=gotCreatures, reason="Set Creatures in Scene", selectedCreatures=Dict.keys scene.creatures}
    moveAPlayer pid = M.SendCommand (T.SetPlayerScene pid (Just scene.id))
    moveAllPlayers = M.Batch (List.map moveAPlayer (Dict.keys app.players))
  in vbox
    [ hbox [strong [] [text "Scene:"], text scene.name]
    , button [onClick moveAllPlayers] [text "Move all Players to this Scene"]
    , case app.current_game.current_combat of
        Just combat ->
          if combat.scene == scene.id
          then hbox [ text "There is a combat happening in this scene!"
                    , button [onClick (M.SelectView "right-side-bar" "Combat")] [text "View Combat"]]
          else text ""
        Nothing -> startCombatButton model app
    , sceneChallenges model app scene
    , hbox [ strong [] [text "Creatures"]
           , clickableIcon [onClick selectCreatures] "more_horiz"
           ]
    , div [s [S.marginLeft (S.em 1)]] [terseCreaturesList model app scene]
    ]

challengeCreaturesAndShowResults : T.AttrCheck -> List T.CreatureID -> M.Msg
challengeCreaturesAndShowResults attrCheck cids =
  let challenge cid = M.SendCommandCB (T.AttributeCheck cid attrCheck) (always M.ShowGameLogs)
  in M.Batch (List.map challenge cids)

sceneChallenges : M.Model -> T.App -> T.Scene -> Html M.Msg
sceneChallenges model app scene =
  let
    challengeCreatures description skillCheck = M.SetModal <|
      M.ModalSimpleSelectCreatures
        { cb = challengeCreaturesAndShowResults skillCheck
        , title = description
        , selected = []
        , from=Dict.keys scene.creatures}
    renderCheck (description, skillCheck) =
      let
        attrChecksWithoutThis = Dict.remove description scene.attribute_checks
        sceneWithoutThis = {scene | attribute_checks = attrChecksWithoutThis}
        menuItems = [(text "Delete", M.SendCommand (T.EditScene sceneWithoutThis))]
      in
      habox
        [s [S.justifyContent S.spaceBetween]]
        [ text description
        , renderAttributeRequirement skillCheck
        , button [onClick <| challengeCreatures description skillCheck] [text "Challenge!"]
        , popUpMenu model "scene-challenge" description threeDots threeDots menuItems
        ]
    premades =
      if (Dict.size scene.attribute_checks) > 0
      then vbox (List.map renderCheck (Dict.toList scene.attribute_checks))
      else text ""
    adHocButton =
      button
        [onClick (M.SetModal <| M.ModalAdHocChallenge {scene=scene.id, description="Ad-Hoc", check={attr="", target=T.Unskilled, reliable=False}})]
        [text "Ad-Hoc Challenge"]
    createNewButton =
      button
        [onClick (M.SetModal <| M.ModalCreateNewChallenge {scene=scene.id, description="", check={attr="", target=T.Unskilled, reliable=False}})]
        [text "Create New"]
  in
    vbox
      [ strong [] [text "Challenges"]
      , div [s [S.marginLeft (S.em 1)]] [premades, hbox [createNewButton, adHocButton]]
      ]

renderAttributeRequirement : T.AttrCheck -> Html M.Msg
renderAttributeRequirement check =
  hbox [attrIcon check.attr, strong [] [text (toString check.target)]
       , text <| if check.reliable then "(reliable)" else ""]

attrIcon attrid =
  case attrid of
    "finesse" -> text "🐈"
    "strength" -> text "💪"
    "magic" -> text "🔮"
    "perception" -> text "👁️"
    x -> text attrid

renderRoll num = hbox [text "🎲", text (toString num)]


terseCreatureLine : T.App -> T.Creature -> Html M.Msg
terseCreatureLine app creature =
  let
    inCombat = T.isCreatureInCombat app.current_game creature.id
    inCombatIcon = if inCombat then text "⚔️" else text ""
  in
    hbox [ CommonView.classIcon creature, strong [] [text creature.name]
         , CommonView.hpBubble creature, CommonView.nrgBubble creature
         , inCombatIcon ]

terseCreaturesList : M.Model -> T.App -> T.Scene -> Html M.Msg
terseCreaturesList model app scene =
  let
    creatureLine creature =
      let
        abs = CommonView.creatureAbilities app.current_game scene.id True creature
        inCombat = T.isCreatureInCombat app.current_game creature.id
        combatRelated =
          case app.current_game.current_combat of
            Just combat ->
              if inCombat
              then [(text "Remove from Combat", M.SendCommand (T.RemoveCreatureFromCombat creature.id))]
              else [(text "Add to Combat", M.SendCommand (T.AddCreatureToCombat creature.id))]
            Nothing -> []
        modSceneCreature f =
          let newCreatures =
                case Dict.get creature.id scene.creatures of
                  Just x -> Dict.insert creature.id (f x) scene.creatures
                  Nothing -> scene.creatures
          in { scene | creatures = newCreatures }
        showToPlayersMsg = M.SendCommand (T.EditScene (modSceneCreature (\(pos, _) -> (pos, T.AllPlayers))))
        hideFromPlayersMsg = M.SendCommand (T.EditScene (modSceneCreature (\(pos, _) -> (pos, T.GMOnly))))
        visibilityRelated =
          case Dict.get creature.id scene.creatures of
            Just (_, T.GMOnly) -> [(text "Show to Players", showToPlayersMsg)]
            Just (_, T.AllPlayers) -> [(text "Hide from Players", hideFromPlayersMsg)]
            Nothing -> []
        extraItems = []
          -- TODO: We can't reasonably add "View Creature" here because we don't know the creature's
          -- path!
          -- [(text "View Creature", M.SetSecondaryFocus (M.Focus2Creature [] creature.id))]
        menuItems = extraItems ++ visibilityRelated ++ combatRelated ++ abs
      in
        hbox [ terseCreatureLine app creature
             , popUpMenu model "terse-creature-abilities" creature.id threeDots threeDots menuItems]
  in vbox (List.map creatureLine (T.getCreatures app.current_game (Dict.keys scene.creatures)))

noteConsole : M.Model -> T.App -> T.FolderPath -> String -> T.Note -> Html M.Msg
noteConsole model app path origName note =
  let noteMsg newNote = M.SetSecondaryFocus (M.Focus2Note path origName newNote)
      saveButton =
        case T.getFolder app path of
          Just (T.Folder folder) ->
            case Dict.get origName folder.data.notes of
              Just appNote ->
                if appNote /= note
                then button [onClick (M.SendCommand (T.EditNote path origName note))] [text "Save"]
                else text ""
              Nothing -> button [onClick (M.SendCommand (T.CreateNote path note))] [text "Create"]
          Nothing -> text "This folder has disappeared!?"
  in vabox [s [S.height (S.pct 100)]]
    [ hbox
        [ text (String.join "/" path)
        , text "/"
        , input [ type_ "text", defaultValue origName
                , onInput (\name -> noteMsg {note | name = name})]
                []
        , saveButton
        ]
    , textarea [s [S.height (S.vh 100)], onInput (\c -> noteMsg {note | content = c}), defaultValue note.content] []
    ]

createFolderInPath : M.Model -> T.App -> M.CreatingFolder -> Html M.Msg
createFolderInPath model app {parent, child} =
  let
    updateName newName = M.SetModal (M.CreateFolder {parent=parent, child=newName})
    msgs = M.Batch [M.SetModal M.NoModal, M.SendCommand (T.CreateFolder (parent ++ [child]))]
  in
    vbox [ hbox [text "Creating a folder in ", renderFolderPath parent]
         , input [placeholder "Folder Name", onInput updateName] []
         , button [onClick msgs] [text "Create"]]

sceneButton : String -> Html M.Msg
sceneButton sceneName = button [onClick (M.SetFocus (M.FocusScene sceneName))] [text sceneName]

bottomActionBar : T.App -> Html M.Msg
bottomActionBar app =
  case app.current_game.current_combat of
    Nothing -> text ""
    Just combat -> CommonView.mainActionBar app combat

moveAnywhereToggle : M.Model -> Html M.Msg
moveAnywhereToggle model =
  case model.moving of
    Just {ooc_creature} ->
      case ooc_creature of
        Just x ->
          hbox [ text "Allow movement anywhere: "
               , input [type_ "checkbox", checked model.moveAnywhere, onClick M.ToggleMoveAnywhere] []]
        Nothing -> text ""
    Nothing -> text "Why is this being called?"

mapConsole : M.Model -> T.App -> T.FolderPath -> T.MapID -> Html M.Msg
mapConsole model app path mapID =
  case M.getMapNamed mapID app of
    Nothing -> text ("Map not found: " ++ mapID)
    Just gameMap ->
      vbox
        [ strong [] [text gameMap.name]
        , case model.focus of
            M.EditingMap path gridData ->
              editingMapConsole model app gameMap path gridData
            _ -> button [onClick (M.SetFocus (M.EditingMap path {map=gameMap, paintStyle=M.PaintTerrain}))] [text "Edit this Map"]
        ]

editingMapConsole : M.Model -> T.App -> T.Map -> T.FolderPath -> M.GridData -> Html M.Msg
editingMapConsole model app origMap path gridData =
  let
    updateName name =
      let map = gridData.map
          newGrid = {gridData | map = {map | name = name}}
      in M.SetFocus (M.EditingMap path newGrid)
    toggleSpecial checked =
      let style = if checked then M.PaintSpecial {color="", note="", vis=T.AllPlayers} else M.PaintTerrain
      in M.SetFocus (M.EditingMap path {gridData | paintStyle = style})
    updateSpecial f =
      let newStyle =
            case gridData.paintStyle of
              M.PaintSpecial rec -> M.PaintSpecial (f rec)
              x -> x
      in M.SetFocus (M.EditingMap path {gridData | paintStyle = newStyle})
    updateSpecialColor color = updateSpecial (\special -> {special | color = color})
    updateSpecialNote note = updateSpecial (\special -> {special | note = note})
    updateSpecialVis isChecked =
      let vis = if isChecked then T.GMOnly else T.AllPlayers
      in updateSpecial (\special -> {special | vis = vis})
    saveMap = M.Batch [M.SetFocus (M.PreviewMap gridData.map.id), M.SendCommand (T.EditMap gridData.map)]
  in
    vbox
      [ button [onClick (M.SetFocus (M.PreviewMap gridData.map.id))] [text "Cancel Editing Map"]
      , hbox
        [ input [type_ "text", placeholder "map name", defaultValue origMap.name, onInput updateName] []
        , button [onClick saveMap] [text "Save"]]
      , hbox [ label [] [text "Draw Special Squares"]
             , input [type_ "checkbox", onCheck toggleSpecial] []]
      , case gridData.paintStyle of
            M.PaintSpecial _ ->
              hbox
                [ input [type_ "text", placeholder "color", onInput updateSpecialColor] []
                , input [type_ "text", placeholder "note", onInput updateSpecialNote] []
                , label [] [text "GM Only"], input [type_ "checkbox", onCheck updateSpecialVis] []
                ]
            _ -> text ""
      ]

createMapDialog : M.Model -> T.App -> M.CreatingMap -> Html M.Msg
createMapDialog model app cm =
  let updateName name = M.SetModal (M.CreateMap {cm | name=name})
      create = M.Batch [ M.SendCommand (T.CreateMap cm.path {name=cm.name, terrain=[]})
                       , M.SetModal M.NoModal ]
  in
    vbox
      [ input [type_ "text", placeholder "Map Name", onInput updateName] []
      , button [onClick create] [text "Create"]
      ]

moveFolderItemDialog : M.Model -> T.App -> M.MovingFolderItem -> Html M.Msg
moveFolderItemDialog model app {src, item, dst} =
  let
    select path = M.SetModal (M.MoveFolderItem {src=src, item=item, dst=path})
    submit = M.Batch [ M.SendCommand (T.MoveFolderItem src item dst)
                      , M.SetModal M.NoModal
                      ]
  in
    vbox
      [ FolderView.selectFolder model.folderState app select
      , button [onClick submit] [text "Select"]
      ]

renameFolderDialog : M.Model -> T.App -> M.RenamingFolder -> Html M.Msg
renameFolderDialog model app {path, newName} =
  let
    changeName name = M.SetModal (M.RenameFolder {path=path, newName=name})
    submit = M.Batch [M.SetModal M.NoModal, M.SendCommand (T.RenameFolder path newName)]
  in
  vbox [ hbox [dtext "Renaming", renderFolderPath path]
       , input [value newName, onInput changeName] []
       , button [onClick submit] [text "Submit"]]

selectCreaturesFromCampaignDialog : M.Model -> T.App -> M.SelectingCreatures -> Html M.Msg
selectCreaturesFromCampaignDialog model app {reason, selectedCreatures, cb} =
  let
    select cid =
      Debug.log ("Selecting cid "++ cid) <|
      M.SetModal (M.SelectCreaturesFromCampaign {reason=reason, cb=cb, selectedCreatures=selectedCreatures ++ [cid]})
    unselect cid =
      Debug.log ("DEselecting cid "++ cid) <|
      M.SetModal (M.SelectCreaturesFromCampaign {reason=reason, cb=cb, selectedCreatures=List.filter (\c -> c /= cid) selectedCreatures})
    submit = M.Batch [M.SetModal M.NoModal, cb selectedCreatures]
  in
    vbox [ strong [] [text reason]
         , FolderView.selectCreatures model.folderState app select unselect selectedCreatures
         , button [onClick submit] [text "Submit"]
         ]

saveGameDialog : M.Model -> T.App -> M.SavingGame -> Html M.Msg
saveGameDialog model app {existing, newGame} =
  let entry = input [type_ "text", value newGame, placeholder "New Game Name", onInput updateName] []
      selectors = List.map (\name -> button [onClick (updateName name)] [text name]) existing
      updateName name = M.SetModal (M.ModalSaveGame {existing=existing, newGame=name})
      saveMsg = M.Batch [M.SaveGame newGame, M.SetModal M.NoModal]
      saveButton = button [onClick saveMsg] [text "Save Game!"]
  in vbox (entry :: selectors ++ [saveButton])

loadGameDialog : M.Model -> T.App -> List String -> Html M.Msg
loadGameDialog model app names =
  let loadButton name = button [onClick (M.Batch [M.SetModal M.NoModal, M.LoadGame name])]
                               [text name]
  in vbox (List.map loadButton names)

editCreatureDialog : M.Model -> T.App -> M.EditingCreature -> Html M.Msg
editCreatureDialog model app editing =
  case T.getCreature app.current_game editing.cid of
    Just creature ->
      let
        update f inp = M.SetModal (M.ModalEditCreature (f inp))
        submitMsg = M.SendCommand (T.EditCreature {creature | note = editing.note, portrait_url = editing.portrait_url, name=editing.name})
        entry p d f = input [type_ "text", placeholder p, defaultValue d, onInput (update f)] []
      in vbox
        [ entry "Name" creature.name (\n -> {editing | name=n})
        , entry "Note" creature.note (\n -> {editing | note=n})
        , entry "Portrait URL" creature.portrait_url (\u -> {editing | portrait_url=u})
        , button [onClick (M.Batch [submitMsg, M.SetModal M.NoModal])] [text "Submit"]]
    Nothing -> text "Creature not found"

showGameLogsDialog : M.Model -> T.App -> List T.GameLog -> Html M.Msg
showGameLogsDialog model app logs =
  vbox (List.map (renderGameLog app) logs)

createNewChallengeDialog : M.Model -> T.App -> M.SceneChallenge -> Html M.Msg
createNewChallengeDialog model app sc =
  case T.getScene app sc.scene of
    Nothing -> text "Scene not found"
    Just scene ->
      let
        newAttrChecks = Dict.insert sc.description sc.check scene.attribute_checks
        newScene = {scene | attribute_checks = newAttrChecks}
        saveScene = M.Batch [ M.SetModal M.NoModal, M.SendCommand (T.EditScene newScene)]
      in
        vbox
          [ challengeEditor M.ModalCreateNewChallenge model app sc
          , button [ disabled (sc.check.attr == "" || sc.description == "")
                   , onClick saveScene]
                   [text "Save Challenge"]
          ]

adHocChallengeDialog : M.Model -> T.App -> M.SceneChallenge -> Html M.Msg
adHocChallengeDialog model app sc =
  case T.getScene app sc.scene of
    Nothing -> text "Scene not found"
    Just scene ->
      let
        challengeCreatures =
          challengeCreaturesAndShowResults
            {reliable=sc.check.reliable, attr=sc.check.attr, target=sc.check.target}
            (Dict.keys scene.creatures)
      in
        vbox
          [ challengeEditor M.ModalAdHocChallenge model app sc
          , button [disabled (sc.check.attr == ""), onClick challengeCreatures] [text "Challenge!"]
          ]

challengeEditor : (M.SceneChallenge -> M.Modal) -> M.Model -> T.App -> M.SceneChallenge -> Html M.Msg
challengeEditor modalConstructor model app ahc =
  let scene = T.getScene app ahc.scene
      updateSC f inp = M.SetModal (modalConstructor (f ahc inp))
      updateCheck f inp = updateSC (\sc inp -> {sc | check = f sc.check inp}) inp
      updateDifficulty value =
        let newDiff =
              case value of
                "Inept" -> T.Inept
                "Unskilled" -> T.Unskilled
                "Skilled" -> T.Skilled
                "Expert" -> T.Expert
                _ -> Debug.crash "sorry"
        in updateCheck (\check inp -> { check | target = inp}) newDiff
  in
    case scene of
      Nothing -> text "Scene not found!"
      Just scene ->
        vbox
          [ input [ type_ "text", defaultValue ahc.description
                  , onInput <| updateSC (\sc inp -> {sc | description = inp})]
                  []
          , select [onInput <| updateCheck (\ahc inp -> {ahc | attr = inp})]
              [ option [value ""] [text "Select an Attribute"]
              -- TODO: generalize attributes, get rid of these hard-coded ones
              , option [value "strength"] [text "Strength"]
              , option [value "finesse"] [text "Finesse"]
              , option [value "magic"] [text "Magic"]
              , option [value "perception"] [text "Perception"]
              ]
          , select [onInput updateDifficulty]
              [ option [value "Inept"] [text "Inept"]
              , option [value "Unskilled"] [text "Unskilled"]
              , option [value "Skilled"] [text "Skilled"]
              , option [value "Expert"] [text "Expert"]
              ]
          , hbox
              [ input [ type_ "checkbox"
                      , onCheck (updateCheck (\ahc checked -> {ahc | reliable = checked}))
                      ] []
              , text "Reliable check? (if a creature has the skill level, they will automatically succeed)"
              ]
          ]

{-| Check for any GM-specific modals that should be rendered. -}
checkModal : M.Model -> T.App -> Maybe (Html M.Msg)
checkModal model app =
  let
    game = app.current_game
    generalModal =
      case model.modal of
        M.CreateFolder creating -> Just (createFolderInPath model app creating)
        M.CreateCreature pending -> Just (createCreatureDialog model app pending)
        M.CreateScene cs -> Just (createSceneDialog model app cs)
        M.CreateMap cm -> Just (createMapDialog model app cm)
        M.MoveFolderItem mfi -> Just (moveFolderItemDialog model app mfi)
        M.RenameFolder rf -> Just (renameFolderDialog model app rf)
        M.SelectCreaturesFromCampaign sc -> Just (selectCreaturesFromCampaignDialog model app sc)
        M.ModalSaveGame sg -> Just (saveGameDialog model app sg)
        M.ModalLoadGame lg -> Just (loadGameDialog model app lg)
        M.ModalEditCreature ce -> Just (editCreatureDialog model app ce)
        M.SelectOrderedCreatures soc -> Just (selectOrderedCreaturesDialog model app soc)
        M.ModalSimpleSelectCreatures sc -> Just (simpleSelectCreaturesDialog model app sc)
        M.ModalShowGameLogs logs -> Just (showGameLogsDialog model app logs)
        M.ModalAdHocChallenge sc -> Just (adHocChallengeDialog model app sc)
        M.ModalCreateNewChallenge sc -> Just (createNewChallengeDialog model app sc)
        M.NoModal -> Nothing
    cancelableModal html =
      vbox [html, button [onClick (M.SetModal M.NoModal)] [text "Cancel"]]
  in Maybe.map cancelableModal generalModal |> MaybeEx.orElse (CommonView.checkModal model app)

{-| A view that allows selecting creatures in a particular order and calling a callback when done.
-}
simpleSelectCreaturesDialog : M.Model -> T.App -> M.SimpleSelectingCreatures -> Html M.Msg
simpleSelectCreaturesDialog model app {from, selected, cb, title} =
  let modModal f =
        M.SetModal <|
          M.ModalSimpleSelectCreatures <| f {from=from, selected=selected, cb=cb, title=title}
      toggleCreature creature checked =
        if checked
        then modModal (\soc -> { soc | selected = creature.id :: soc.selected})
        else modModal (\soc -> { soc | selected = List.filter (\c -> c /= creature.id) soc.selected})
      checkBox creature =
        input [ type_ "checkbox", onCheck <| toggleCreature creature]
               [text "Add"]
      selectableCreature creature =
        habox [s [S.width (S.px 500)]]
              [checkBox creature, terseCreatureLine app creature, noteBox model creature]
      selectableCreatureItems =
        vbox <| List.map selectableCreature (T.getCreatures app.current_game from)
      done = M.Batch [cb selected, M.SetModal M.NoModal]
      doneSelectingButton = button [onClick done] [text title]
      cancelButton = button [onClick (M.SetModal M.NoModal)] [text "Cancel"]
  in vbox <|
    [h3 [] [text <| "Select Creatures to " ++ title]
    , habox [s [S.width (S.px 1000), S.justifyContent S.spaceBetween]]
            [selectableCreatureItems]
    , hbox [doneSelectingButton, cancelButton]]


{-| A view that allows selecting creatures in a particular order and calling a callback when done.
-}
selectOrderedCreaturesDialog : M.Model -> T.App -> M.SelectingOrderedCreatures -> Html M.Msg
selectOrderedCreaturesDialog model app {from, selected, cb, title} =
  let modModal f = M.SetModal (M.SelectOrderedCreatures (f {from=from, selected=selected, cb=cb, title=title}))
      selectButton creature =
        button [ onClick (modModal (\soc -> {soc | selected = Dict.insert creature.id 0 soc.selected}))]
               [text "Add"]
      unselectButton cid =
        button [ onClick (modModal (\soc -> {soc | selected = Dict.remove cid soc.selected})) ]
               [text "Remove"]
      selectableCreature creature =
        habox [s [S.width (S.px 500)]]
              [selectButton creature, terseCreatureLine app creature, noteBox model creature]
      selectableCreatureItems =
        vbox <| List.map selectableCreature (T.getCreatures app.current_game from)
      setInit cid newInitStr =
        let newInit =
              case String.toInt newInitStr of
                Ok num -> num
                Err _ -> if newInitStr == "" then 0 else Dict.get cid selected |> Maybe.withDefault 0
        in modModal (\soc -> {soc | selected = Dict.insert cid newInit soc.selected})
      initBox creature init =
        input [type_ "text", value (if isNaN (toFloat init) then "-" else if init == 0 then "" else (toString init)), onInput (setInit creature.id)] []
      selectedCreatureItem (cid, init) =
        case T.getCreature app.current_game cid of
          Just creature -> habox
            [s [S.width (S.px 500)]]
            [CommonView.creatureIcon app creature, initBox creature init, unselectButton creature.id]
          Nothing -> text ""
      selectedCreatureItems =
        vbox <| List.map selectedCreatureItem (Dict.toList selected)
      finishedCreatures = List.sortBy (\(cid, init) -> -init) (Dict.toList selected)
      done = M.Batch [cb (Dict.keys selected), M.SetModal M.NoModal]
      doneSelectingButton = button [onClick done] [text title]
      cancelButton = button [onClick (M.SetModal M.NoModal)] [text "Cancel"]
  in vbox <|
    [h3 [] [text <| "Select Creatures to " ++ title]
    , habox [s [S.width (S.px 1000), S.justifyContent S.spaceBetween]]
            [selectableCreatureItems, selectedCreatureItems]
    , hbox [doneSelectingButton, cancelButton]]

{-| Figure out which map to render based on the focus. -}
mapView : M.Model -> T.App -> (Html M.Msg, Html M.Msg)
mapView model app =
  case model.focus of
    M.EditingMap path gridData ->
      ( editMap model app path gridData
      , text "Edit this map!")
    M.PreviewMap name ->
      ( Grid.terrainMap model (M.tryGetMapNamed name app) []
      , text "Previewing Map")
    M.FocusScene name ->
      case Dict.get name app.current_game.scenes of
        Just scene -> sceneMap model app scene
        Nothing -> (text "", text "Scene does not exist")
    M.NoFocus -> (text "", text "No Focus")

editMap : M.Model -> T.App -> T.FolderPath -> M.GridData -> Html M.Msg
editMap model app path gridData =
  Grid.editMap model gridData.map []

sceneMap : M.Model -> T.App -> T.Scene -> (Html M.Msg, Html M.Msg)
sceneMap model app scene =
  let game = app.current_game
      currentMap = M.tryGetMapNamed scene.map app
      currentCombatCreature = Maybe.map (\com -> (T.combatCreature game com).id) game.current_combat
      enableMovement mapc =
        { mapc | highlight = if (Just mapc.creature.id) == currentCombatCreature then Just M.Current else Nothing
               , clickable = Just (M.GetMovementOptions scene.id)}
      vCreatures = (CommonView.visibleCreatures app.current_game scene)
      defaultMap () =
        ( Grid.terrainMap model currentMap (List.map enableMovement vCreatures)
        , text "Click a creature to move")
  in
    (CommonView.movementMap model app scene currentMap vCreatures
      |> Maybe.map (\g -> (g, CommonView.movementControls [moveAnywhereToggle model] model)))
    |> MaybeEx.or (CommonView.targetMap model app scene currentMap vCreatures)
    |> MaybeEx.unpack defaultMap identity

{-| An area for writing terse notes about a Creature. Intended to be passed as the "extras" argument
    to creatureCard. -}
noteBox : M.Model -> T.Creature -> Html M.Msg
noteBox model creature =
  let note = Maybe.withDefault creature.note (Dict.get creature.id model.creatureNotes)
      submitMsg = M.SendCommand (T.EditCreature {creature | note = note})
      inp = input [s [S.width (S.px 300)], type_ "text", defaultValue creature.note
                  , onInput (M.SetCreatureNote creature.id)]
                  []
      saveButton =
        if creature.note /= note
        then button [onClick submitMsg] [text "Save Note"]
        else text ""
  in hbox <| [inp, saveButton]

{-| A button for removing a creature from combat. -}
disengageButton : T.Creature -> Html M.Msg
disengageButton creature =
  button [onClick (M.SendCommand (T.RemoveCreatureFromCombat creature.id))] [text ("Disengage " ++ creature.name)]

{-| Render combat if we're in combat, or a Start Combat button if not -}
combatView : M.Model -> T.App -> Html M.Msg
combatView model app =
  case app.current_game.current_combat of
    Just com -> inCombatView model app com
    Nothing -> startCombatButton model app

{-| The content of what's rendered when we're actually in combat. -}
inCombatView : M.Model -> T.App -> T.Combat -> Html M.Msg
inCombatView model app combat =
  let game = app.current_game
      creatures = T.getCreatures game combat.creatures.data
      disengageButtons = hbox (List.map disengageButton creatures)
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
          [ hbox [strong [] [text "Scene: "], sceneButton combat.scene]
          , CommonView.combatantList extraGutter extraCreatureCard app combat
          , stopCombatButton
          , disengageButtons]
  in combatView

{-| A button for starting combat. -}
startCombatButton : M.Model -> T.App -> Html M.Msg
startCombatButton model app =
  case model.focus of
    M.FocusScene sceneName  ->
      let gotCreatures cids = M.SendCommand (T.StartCombat sceneName cids)
          sceneCreatures =
            case Dict.get sceneName app.current_game.scenes of
              Just scene -> Dict.keys scene.creatures
              Nothing -> []
      in button [onClick (M.SetModal (M.SelectOrderedCreatures {from = sceneCreatures, selected =Dict.empty, cb = gotCreatures, title = "Start Combat"}))] [text "Start Combat"]
    _ -> button [disabled True] [text "Select a Scene to Start Combat"]

{-| A button for stopping combat. -}
stopCombatButton : Html M.Msg
stopCombatButton = button [onClick (M.SendCommand T.StopCombat)] [text "Stop Combat"]

{-| A form for creating a scene. -}
createSceneDialog : M.Model -> T.App -> M.CreatingScene -> Html M.Msg
createSceneDialog model app creating =
  let ready = creating.scene.name /= "" && creating.scene.map /= ""
      update : (T.SceneCreation -> String -> T.SceneCreation) -> String -> M.Msg
      update f inp = M.SetModal (M.CreateScene {path = creating.path, scene = f creating.scene inp})
  in
    vbox
      [ h3 [] [text "Create a Scene"]
      , hbox [text "Name:", input [type_ "text", placeholder "Name", onInput (update (\sc inp -> {sc | name = inp}))] []]
      , text "Map:"
      , FolderView.selectMap model.folderState app (update (\sc inp -> {sc | map = inp}))
      , button
          [ onClick
              (M.Batch [ M.SendCommand (T.CreateScene creating.path creating.scene)
                       , M.SetModal M.NoModal])
          , disabled (not ready)] [text "Create"]
      ]

{-| A form for creating a creature. -}
createCreatureDialog : M.Model -> T.App -> M.PendingCreature -> Html M.Msg
createCreatureDialog model app {name, class, path} =
  let disabledButton = button [disabled True] [text "Create Creature"]
      createCreatureButton =
        case (name, class) of
          (Just name, Just class) ->
            let cc = T.CreatureCreation name class ""
                msg = M.Batch [M.SendCommand (T.CreateCreature path cc), M.SetModal M.NoModal]
            in button [onClick msg] [text "Create Creature"]
          _ -> disabledButton
      updatePendingClass : String -> M.Msg
      updatePendingClass class = M.SetModal (M.CreateCreature {name = name, class = Just class, path = path})
      updatePendingName name = M.SetModal (M.CreateCreature {name = Just name, class = class, path = path})
  in vbox
    [ hbox [text "Creating Creature in ", renderFolderPath path]
    , input [type_ "text", placeholder "name", onInput updatePendingName ] []
    , select [onInput updatePendingClass]
             <| [option [value ""] [text "Select a Class"]]
                ++ (List.map (\className -> option [value className] [text className])
                             (Dict.keys app.current_game.classes))
    , createCreatureButton
    ]

{-| Show all registered players and which creatures they've been granted -}
playersView : M.Model -> T.App -> Html M.Msg
playersView model app =
  let gotCreatures pid cids = M.SendCommand (T.GiveCreaturesToPlayer pid cids)
      allCreatures = Dict.keys app.current_game.creatures
      selectCreatures pid = M.SetModal (M.SelectCreaturesFromCampaign {selectedCreatures=[], cb=gotCreatures pid, reason="Grant Creatures to " ++ pid})
      grantCreatures player = (text "Grant Creatures", selectCreatures player.player_id)
      sceneName =
        case model.focus of
          M.FocusScene name -> Just name
          _ -> Nothing
      sceneButtonText =
        sceneName |> Maybe.map (\name -> "Move Player to " ++ name) |> Maybe.withDefault "Remove player from scene"
      activateScene player = (text sceneButtonText, M.SendCommand (T.SetPlayerScene player.player_id sceneName))
      menu player =
        [popUpMenu model "PlayerListOptions" player.player_id gear gearBox
                   [grantCreatures player, activateScene player]]
  in CommonView.playerList app menu app.players

{-| Show a list of all events that have happened in the game. -}
historyView : T.App -> Html M.Msg
historyView app =
  let snapIdx = (Array.length app.snapshots) - 1
      _ = Debug.log "[EXPENSIVE:historyView]" ()
      items =
        case Array.get snapIdx app.snapshots of
          Just (_, items) -> Array.toList items
          Nothing -> []
  in vbox <| List.reverse (List.indexedMap (historyItem app snapIdx) items)


renderFolderPath : T.FolderPath -> Html M.Msg
renderFolderPath path =
  hbox [ icon [] "folder"
       , if List.isEmpty path then text "Campaign Root" else text (T.folderPathToString path)]

-- just a quick hack which isn't good enough. need to properly align all the log data.
hsbox : List (Html M.Msg) -> Html M.Msg
hsbox = habox [s [S.justifyContent S.spaceBetween]]

renderGameLog : T.App -> T.GameLog -> Html M.Msg
renderGameLog app log =
  let cname cid = dtext (T.creatureName app cid |> Maybe.withDefault cid)
      creatureWithSkill cid attrid =
        let creature = T.getCreature app.current_game cid
            skill = creature |> Maybe.andThen (\c -> Dict.get attrid c.attributes)
            skillText = Maybe.map toString skill |> Maybe.withDefault "Unknown"
        in hbox [cname cid, dtext <| "(" ++ skillText ++ ")"]
  in case log of
  T.GLCreateFolder path -> hsbox [dtext "Created folder", renderFolderPath path]
  T.GLRenameFolder path newName -> hsbox [dtext "Renamed Folder", renderFolderPath path, dtext newName]
  T.GLDeleteFolder path -> hsbox [dtext "Deleted Folder", renderFolderPath path]
  T.GLMoveFolderItem src item dst -> hsbox [dtext "Moved Folder Item", renderFolderPath src, dtext (toString item), renderFolderPath dst]
  T.GLCreateNote path note -> hsbox [dtext "Created Note", dtext note.name]
  T.GLEditNote path name note -> hsbox [dtext "Edited Note", renderFolderPath path, dtext name]
  T.GLDeleteNote path name -> hsbox [dtext "Deleted Note", renderFolderPath path, dtext name]
  T.GLCreateScene path scene -> hsbox [dtext "Created Scene", dtext scene.name, renderFolderPath path]
  T.GLEditScene scene -> hsbox [dtext "Edited Scene", dtext scene.name]
  T.GLDeleteScene sid -> hsbox [dtext "Deleted Scene", dtext sid]
  T.GLCreateMap path map -> hsbox [dtext "Created Map", dtext map.name, renderFolderPath path]
  T.GLEditMap map -> hsbox [dtext "Edited Map", dtext map.name]
  T.GLDeleteMap mid -> hsbox [dtext "Deleted Map", dtext mid]
  T.GLCreateCreature path creature -> hsbox [dtext "Created creature", dtext creature.name, renderFolderPath path]
  T.GLEditCreature creature -> hsbox [dtext "Edited Creature", dtext creature.name]
  T.GLDeleteCreature cid -> hsbox [dtext "Deleted creature", cname cid]
  T.GLStartCombat scene combatants -> hsbox <| [dtext "Started Combat in scene", dtext scene] ++ List.map dtext combatants
  T.GLStopCombat -> dtext "Stopped combat"
  T.GLAddCreatureToCombat cid -> hsbox [cname cid, dtext "Added Creature to Combat"]
  T.GLRemoveCreatureFromCombat cid -> hsbox [dtext "Removed creature from Combat: ", cname cid]
  T.GLCreatureLog cid cl -> hsbox [cname cid, historyCreatureLog cl]
  T.GLCombatLog cl -> historyCombatLog cl
  T.GLRollback si li -> hsbox [dtext "Rolled back. Snapshot: ", dtext (toString si), dtext " Log: ", dtext (toString li)]
  T.GLPathCreature scene cid pts -> hsbox [dtext "Pathed creature in scene", dtext scene, cname cid, dtext (maybePos pts)]
  T.GLSetCreaturePos scene cid pt -> hsbox [dtext "Ported creature in scene", dtext scene, cname cid, dtext (renderPt3 pt)]
  T.GLAttributeCheckResult cid check roll success ->
    hsbox [ dtext "Random Challenge Result"
          , creatureWithSkill cid check.attr
          , hbox [dtext "Difficulty:", renderAttributeRequirement check]
          , renderRoll roll
          , dtext (if success then "Success" else "Failure")]

historyItem : T.App -> Int -> Int -> T.GameLog -> Html M.Msg
historyItem app snapIdx logIdx log =
  let logItem = renderGameLog app log
  in hsbox [logItem, button [onClick (M.SendCommand (T.Rollback snapIdx logIdx))] [icon [] "history"]]

historyCombatLog : T.CombatLog -> Html M.Msg
historyCombatLog cl = case cl of
  T.ComLEndTurn cid -> hsbox [dtext cid, dtext "Ended Turn"]
  T.ComLChangeCreatureInitiative cid newPos -> hsbox [dtext cid, dtext "Changed initiative to", dtext <| toString newPos]
  T.ComLConsumeMovement distance -> hsbox [dtext "Used movement", dtext (toString distance)]

renderPt3 : T.Point3 -> String
renderPt3 {x, y, z} = toString x ++ "," ++ toString y

maybePos : List T.Point3 -> String
maybePos path =
  case List.head (List.reverse path) of
    Just {x, y, z} -> toString x ++ "," ++ toString y
    Nothing -> "nowhere"

historyCreatureLog : T.CreatureLog -> Html M.Msg
historyCreatureLog cl = case cl of
  T.CLDamage dmg dice -> hsbox [dtext <| "Took damage", dtext <| toString dmg, dtext <| " Dice: ", renderDice dice]
  T.CLHeal dmg dice -> hsbox [dtext <| "Healed: " ++ toString dmg, dtext <| "Dice: ", renderDice dice]
  T.CLGenerateEnergy nrg -> dtext <| "Regenerated energy: " ++ toString nrg
  T.CLReduceEnergy nrg -> dtext <| "Lost energy: " ++ toString nrg
  T.CLApplyCondition conid duration con -> dtext <| "Got condition: " ++ toString con
  T.CLRemoveCondition conid -> dtext <| "Lost condition: " ++ toString conid
  T.CLSetPos pos -> dtext <| "Moved  to " ++ toString pos
  T.CLDecrementConditionRemaining conID -> dtext <| "Tick condition: " ++ toString conID
  T.CLSetNote note -> hsbox [dtext "Set note to", dtext note]

renderDice : List Int -> Html M.Msg
renderDice dice = dtext <| String.join ", " (List.map toString dice)
