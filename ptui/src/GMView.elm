module GMView exposing (gmView)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
  { mapView = mapView model app
  , sideBar =
      CommonView.tabbedView "right-side-bar" "Campaign" model
        [ ("Campaign", always (campaignView model app))
        , ("Combat", always (combatView model app))
        , ("Players", always (playersView model app))
        , ("History", always (historyView app))
        ]
  , extraMovementOptions = [moveAnywhereToggle model]
  , extraOverlays = [bottomActionBar app]
  , modal = checkModal model app
  }

campaignView : M.Model -> T.App -> Html M.Msg
campaignView model app = vbox [FolderView.campaignFolder model app, secondaryFocusView model app]

secondaryFocusView : M.Model -> T.App -> Html M.Msg
secondaryFocusView model app =
  case model.secondaryFocus of
    M.Focus2None -> text ""
    M.Focus2Creature path cid ->
      case T.getCreature app.current_game cid of
        Just creature ->
          -- TODO: * move functionality from All Creatures here (add to combat?)
          let item = {key=T.FolderCreature cid, path=path, prettyName=creature.name}
          in console model app item (CommonView.creatureCard [noteBox model creature] app creature)
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
    vabox [s [S.marginTop (S.em 1)]]
      [ hbox [text prettyName, text " in ", renderFolderPath path, menu]
      , content ]

sceneConsole : M.Model -> T.App -> T.Scene -> Html M.Msg
sceneConsole model app scene =
  let
    gotCreatures cids =
      let
        selectedCreatures = Dict.fromList (List.map (\c -> (c, {x=0, y=0, z=0})) cids)
        newCreatures = Dict.merge (\_ _ r -> r) (\cid l r res -> Dict.insert cid l res) Dict.insert
                                  scene.creatures selectedCreatures
                                  Dict.empty
        newScene = {scene | creatures = newCreatures}
      in M.SendCommand (T.EditScene newScene)
    selectCreatures = M.SetModal
      <| M.SelectCreaturesFromCampaign
          {cb=gotCreatures, reason="Set Creatures in Scene", selectedCreatures=Dict.keys scene.creatures}
  in vbox
    [ hbox [strong [] [text "Scene:"], text scene.name]
    , case app.current_game.current_combat of
        Just combat ->
          if combat.scene == scene.id
          then hbox [ text "There is a combat happening in this scene!"
                    , button [onClick (M.SelectView "right-side-bar" "Combat")] [text "View Combat"]]
          else text ""
        Nothing -> startCombatButton model app
    , hbox [ strong [] [text "Creatures:"]
           , clickableIcon [onClick selectCreatures] "more_horiz"
           ]
    , terseCreaturesList model app scene (Dict.keys scene.creatures)
    ]

terseCreaturesList : M.Model -> T.App -> T.Scene -> List T.CreatureID -> Html M.Msg
terseCreaturesList model app scene cids =
  let
    creatureLine creature =
      let
        abs = CommonView.creatureAbilities app.current_game scene.id True creature
        inCombat =
          case app.current_game.current_combat of
            Just combat -> List.member creature.id combat.creatures.data
            Nothing -> False
        combatRelated =
          case app.current_game.current_combat of
            Just combat ->
              if inCombat
              then [(text "Remove from Combat", M.SendCommand (T.RemoveCreatureFromCombat creature.id))]
              else [(text "Add to Combat", M.SendCommand (T.AddCreatureToCombat creature.id))]
            Nothing -> []
        extraItems = []
          -- TODO: We can't reasonably add "View Creature" here because we don't know the creature's
          -- path!
          -- [(text "View Creature", M.SetSecondaryFocus (M.Focus2Creature [] creature.id))]
        menuItems = extraItems ++ combatRelated ++ abs
        inCombatIcon = if inCombat then text "⚔️" else text ""
      in
        hbox [ CommonView.classIcon creature, strong [] [text creature.name]
             , CommonView.hpBubble creature, CommonView.nrgBubble creature
             , inCombatIcon
             , popUpMenu model "terse-creature-abilities" creature.id threeDots threeDots menuItems]
  in vbox (List.map creatureLine (T.getCreatures app.current_game cids))

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
  in vbox
    [ hbox
        [ text (String.join "/" path)
        , text "/"
        , input [ type_ "text", value note.name
                , onInput (\name -> noteMsg {note | name = name})]
                []
        , saveButton
        ]
    , textarea [onInput (\c -> noteMsg {note | content = c}), value note.content] []
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
sceneButton sceneName = button [onClick (M.SetFocus (M.Scene sceneName))] [text sceneName]

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
  let map = M.getMapNamed mapID app
  in
    case map of
      Nothing -> text ("Map not found: " ++ mapID)
      Just map ->
        vbox [strong [] [text map.name]
             , 
        case model.focus of
          M.EditingMap path map ->
            let updateName name = M.SetFocus (M.EditingMap path {map | name = name})
                saveMap = M.Batch [M.SetFocus (M.PreviewMap map.id), M.SendCommand (T.EditMap map)]
            in
              vbox
                [ button [onClick (M.SetFocus M.NoFocus)] [text "Cancel Editing Map"]
                , hbox
                  [ input [type_ "text", placeholder "map name", value map.name, onInput updateName] []
                  , button [onClick saveMap] [text "Save"]
                  ]
                ]
          _ -> button [onClick (M.SetFocus (M.EditingMap path map))] [text "Edit this Map"]
        ]

createMapDialog : M.Model -> T.App -> M.CreatingMap -> Html M.Msg
createMapDialog model app cm =
  let updateName name = M.SetModal (M.CreateMap {cm | name=name})
      create = M.Batch [ M.SendCommand (T.CreateMap cm.path {name=cm.name, terrain=[]})
                       , M.SetModal M.NoModal
                       ]
  in 
    vbox
      [ input [type_ "text", value cm.name, onInput updateName] []
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
      [ FolderView.selectFolder model app select
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
         , FolderView.selectCreatures model app select unselect selectedCreatures
         , button [onClick submit] [text "Submit"]
         ]

{-| Check for any GM-specific modals that should be rendered. -}
checkModal : M.Model -> T.App -> Maybe (Html M.Msg)
checkModal model app =
  let
    game = app.current_game
    selectingCreatures =
      Maybe.map (\(selectable, selected, cb, name) -> selectCreaturesView model app selectable selected cb name)
        model.selectingCreatures
    generalModal =
      case model.modal of
        M.CreateFolder creating -> Just (createFolderInPath model app creating)
        M.CreateCreature pending -> Just (createCreatureDialog model app pending)
        M.CreateScene cs -> Just (createSceneDialog model app cs)
        M.CreateMap cm -> Just (createMapDialog model app cm)
        M.MoveFolderItem mfi -> Just (moveFolderItemDialog model app mfi)
        M.RenameFolder rf -> Just (renameFolderDialog model app rf)
        M.SelectCreaturesFromCampaign sc -> Just (selectCreaturesFromCampaignDialog model app sc)
        M.NoModal -> Nothing
    cancelableModal html =
      vbox [html, button [onClick (M.SetModal M.NoModal)] [text "Cancel"]]
  in selectingCreatures
      |> MaybeEx.orElse (Maybe.map cancelableModal generalModal)
      |> MaybeEx.orElse (CommonView.checkModal model app)

{-| A view that allows selecting creatures in a particular order and calling a callback when done.
-}
-- NOTE: I'm sure at some point we'll want to move this to CommonView -- we just need to make sure
-- that only the GM gets the noteBox in the creatureCard.
selectCreaturesView : M.Model -> T.App
                    -> List T.CreatureID -> List T.CreatureID -> M.GotCreatures
                    -> String
                    -> Html M.Msg
selectCreaturesView model app selectableCreatures selectedCreatures callback commandName =
  let selectButton creature =
        button [ onClick (M.ToggleSelectedCreature creature.id)
               , s [S.height (S.px 100), S.width (S.px 100)]]
               [text "Add"]
      unselectButton cid =
        button [ onClick (M.ToggleSelectedCreature cid)
               , s [S.height (S.px 100), S.width (S.px 100)]]
               [text "Remove"]
      selectableCreature creature =
        habox
          [s [S.width (S.px 500)]]
          [selectButton creature, CommonView.creatureCard [noteBox model creature] app creature]
      selectableCreatureItems =
        vbox <| List.map selectableCreature (T.getCreatures app.current_game selectableCreatures)
      selectedCreatureItem creature =
        habox
          [s [S.width (S.px 500)]]
          [CommonView.creatureCard [noteBox model creature] app creature, unselectButton creature.id]
      selectedCreatureItems =
        vbox <| List.map selectedCreatureItem (T.getCreatures app.current_game selectedCreatures)
      doneSelectingButton = button [onClick M.DoneSelectingCreatures] [text commandName]
      cancelButton = button [onClick M.CancelSelectingCreatures] [text "Cancel"]
  in vbox <|
    [h3 [] [text <| "Select Creatures to " ++ commandName]
    , habox [s [S.width (S.px 1000), S.justifyContent S.spaceBetween]]
            [selectableCreatureItems, selectedCreatureItems]
    , hbox [doneSelectingButton, cancelButton]]

{-| Figure out which map to render based on the focus. -}
mapView : M.Model -> T.App -> Html M.Msg
mapView model app =
  case model.focus of
    M.EditingMap _ map -> Grid.editMap model map.terrain []
    M.PreviewMap name -> Grid.terrainMap model (M.tryGetMapNamed name app).terrain []
    M.Scene name ->
      case Dict.get name app.current_game.scenes of
        Just scene -> sceneMap model app scene
        Nothing -> text ""
    M.NoFocus -> text ""

sceneMap : M.Model -> T.App -> T.Scene -> Html M.Msg
sceneMap model app scene =
  let game = app.current_game
      movementGrid msg mvmtReq creature =
        case Dict.get creature.id scene.creatures of
          Just pos ->
            Grid.movementMap model msg mvmtReq model.moveAnywhere (M.getMap model).terrain pos vCreatures
          Nothing -> text "Moving Creature is not in this scene"
      pathOrPort =
        if model.moveAnywhere
        then M.SetCreaturePos scene.id
        else M.PathCreature scene.id
      movementMap =
        case (game.current_combat, model.moving) of
          (Nothing, Just mvmtReq) ->
            Maybe.map (\creature -> movementGrid (pathOrPort creature.id) mvmtReq creature)
                      mvmtReq.ooc_creature
          (Just combat, Just mvmtReq) ->
            let (creature, moveMessage) =
                  case mvmtReq.ooc_creature of
                    Just creature -> (creature, pathOrPort creature.id)
                    Nothing -> (T.combatCreature game combat, M.PathCurrentCombatCreature)
            in Just <| movementGrid moveMessage mvmtReq creature
          _ -> Nothing
      currentCombatCreature = Maybe.map (\com -> (T.combatCreature game com).id) game.current_combat
      modifyMapCreature mapc =
        { mapc | highlight = (Just mapc.creature.id) == currentCombatCreature
               , movable = Just (M.GetMovementOptions scene.id)}
      vCreatures = List.map modifyMapCreature (CommonView.visibleCreatures app.current_game scene)
      defaultMap () = Grid.terrainMap model (M.tryGetMapNamed scene.map app).terrain vCreatures
  in movementMap |> MaybeEx.unpack defaultMap identity

{-| An area for writing notes about a Creature. Intended to be passed as the "extras" argument to 
creatureCard. -}
noteBox : M.Model -> T.Creature -> Html M.Msg
noteBox model creature = 
  let note = Maybe.withDefault creature.note (Dict.get creature.id model.creatureNotes)
      inp = input [type_ "text", value note, onInput (M.SetCreatureNote creature.id)] []
      saveButton =
        if creature.note /= note
        then button [onClick (M.SendCommand (T.SetCreatureNote creature.id note))] [text "Save Note"]
        else text ""
  in hbox <| [inp, saveButton]

{-| A button for removing a creature from combat. -}
disengageButton : T.Creature -> Html M.Msg
disengageButton creature =
  button [onClick (M.SendCommand (T.RemoveCreatureFromCombat creature.id))] [text ("Disengage " ++ creature.name)]

mapSelectorMenu : String -> M.Model -> T.App -> (String -> M.Msg) -> Html M.Msg
mapSelectorMenu defaultSelection model app action =
  let isCurrent mapID = mapID == defaultSelection
  in select [onInput action]
    <| [option [value ""] [text "Select a Map"]]
       ++ (List.map (\mapID -> option [value mapID, selected (isCurrent mapID)] [text mapID]) (Dict.keys app.current_game.maps))

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
    M.Scene sceneName  ->
      let gotCreatures cids = M.SendCommand (T.StartCombat sceneName cids)
          sceneCreatures =
            case Dict.get sceneName app.current_game.scenes of
              Just scene -> Dict.keys scene.creatures
              Nothing -> []
      in button [onClick (M.SelectCreatures sceneCreatures gotCreatures "Start Combat")] [text "Start Combat"]
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
      , FolderView.selectMap model app (update (\sc inp -> {sc | map = inp}))
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
      selectCreatures pid = M.SelectCreatures allCreatures (gotCreatures pid) ("Grant Creatures to " ++ pid)
      grantCreatures player = (text "Grant Creatures", selectCreatures player.player_id)
      sceneName =
        case model.focus of
          M.Scene name -> Just name
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
      items =
        case Array.get snapIdx app.snapshots of
          Just (_, items) -> Array.toList items
          Nothing -> []
  in vbox <| List.reverse (List.indexedMap (historyItem snapIdx) items)


renderFolderPath : T.FolderPath -> Html M.Msg
renderFolderPath path =
  hbox [ icon [] "folder"
       , if List.isEmpty path then text "Campaign Root" else text (T.folderPathToString path)]


-- just a quick hack which isn't good enough. need to properly align all the log data.
hsbox : List (Html M.Msg) -> Html M.Msg
hsbox = habox [s [S.justifyContent S.spaceBetween]]

historyItem : Int -> Int -> T.GameLog -> Html M.Msg
historyItem snapIdx logIdx log =
  let logItem = case log of
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
    T.GLCreateCreature path creature -> hsbox [dtext "Created creature", dtext creature.id, renderFolderPath path]
    T.GLDeleteCreature cid -> hsbox [dtext "Deleted creature", dtext cid]
    T.GLStartCombat scene combatants -> hsbox <| [dtext "Started Combat in scene", dtext scene] ++ List.map dtext combatants
    T.GLStopCombat -> dtext "Stopped combat"
    T.GLAddCreatureToCombat cid -> hsbox [dtext cid, dtext "Added Creature to Combat"]
    T.GLRemoveCreatureFromCombat cid -> hsbox [dtext "Removed creature from Combat: ", dtext cid]
    T.GLCreatureLog cid cl -> hsbox [dtext cid, historyCreatureLog cl]
    T.GLCombatLog cl -> historyCombatLog cl
    T.GLRollback si li -> hsbox [dtext "Rolled back. Snapshot: ", dtext (toString si), dtext " Log: ", dtext (toString li)]
    T.GLPathCreature scene cid pts -> hsbox [dtext "Pathed creature in scene", dtext scene, dtext cid, dtext (maybePos pts)]
    T.GLSetCreaturePos scene cid pt -> hsbox [dtext "Ported creature in scene", dtext scene, dtext cid, dtext (renderPt3 pt)]
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
