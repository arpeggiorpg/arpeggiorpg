module FolderView exposing (campaignFolder, selectFolder, selectCreatures, selectMap)

import Dict
import Set
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Css as S

import Model as M
import Types as T
import CommonView
import Elements exposing (..)

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

campaignFolder : M.FolderState -> T.App -> Html M.Msg
campaignFolder fstate app =
  let
    _ = Debug.log "[EXPENSIVE:campaignFolder]" ()
    cfg =
      { mutable = True
      , showScenes = True
      , showCreatures = True
      , showNotes = True
      , showMaps = True
      , allowFocus = True
      , contentControls = \_ _ -> text ""
      }
  in baseCampaignView fstate app cfg

selectCreatures : M.FolderState -> T.App -> (T.CreatureID -> M.Msg) -> (T.CreatureID -> M.Msg) -> List T.CreatureID -> Html M.Msg
selectCreatures fstate app addCreature remCreature preselected =
  let
    toggleCheck cid isChecked = if isChecked then addCreature cid else remCreature cid
    extraCheckbox _ itemId =
      case itemId of
        Just (T.FolderCreature cid) ->
          let isChecked = List.member cid preselected
          in input [type_ "checkbox", checked isChecked, onCheck (toggleCheck cid)] []
        _ -> text ""
    cfg = { showNothing | showCreatures = True, contentControls = extraCheckbox}
  in baseCampaignView fstate app cfg

selectFolder : M.FolderState -> T.App -> (T.FolderPath -> M.Msg) -> Html M.Msg
selectFolder fstate app msg =
  let
    select path _ =
      input [ type_ "radio", name "select-folder"
            , onClick (msg path) ]
            [text "Select"]
    cfg = { showNothing | contentControls = select }
  in
    baseCampaignView fstate app cfg

selectMap : M.FolderState -> T.App -> (T.MapID -> M.Msg) -> Html M.Msg
selectMap fstate app cb =
  let
    control path item =
      case item of
        Just (T.FolderMap mid) -> input [ type_ "radio", name "select-map", onClick (cb mid) ]
                                        []
        _ -> text ""
    cfg = { showNothing | showMaps = True, contentControls = control }
  in baseCampaignView fstate app cfg


type alias FolderViewConfig =
  { mutable: Bool
  , showScenes : Bool
  , showCreatures : Bool
  , showNotes : Bool
  , showMaps : Bool
  , allowFocus : Bool
  , contentControls : T.FolderPath -> Maybe T.FolderItemID -> Html M.Msg
  }

showNothing : FolderViewConfig
showNothing =
  { mutable = False
  , showScenes = False
  , showCreatures = False
  , showNotes = False
  , showMaps = False
  , allowFocus = False
  , contentControls = \_ _ -> text ""
  }

baseCampaignView : M.FolderState -> T.App -> FolderViewConfig -> Html M.Msg
baseCampaignView fstate app cfg =
  let content = folderView fstate app cfg [] app.current_game.campaign
      extra = cfg.contentControls [] Nothing
      menu = if cfg.mutable then folderMenu fstate [] else text ""
  in vbox [ hbox [icon [] "folder_open", extra, text "Campaign", menu]
          , div [s [S.marginLeft (S.em 1)]] [content]]

folderLine : FolderViewConfig -> T.FolderPath -> Maybe T.FolderItemID -> M.Msg -> String -> String -> Html M.Msg
folderLine cfg path mItem msg iconName entryName =
  baseFolderLine cfg path mItem (if cfg.allowFocus then (Just msg) else Nothing) iconName entryName

baseFolderLine : FolderViewConfig -> T.FolderPath -> Maybe T.FolderItemID -> Maybe M.Msg -> String -> String -> Html M.Msg
baseFolderLine cfg path mItem mmsg iconName entryName =
  let attrs = case mmsg of
                Just msg -> [clickable, onClick msg]
                Nothing -> []
      extra = cfg.contentControls path mItem
  in hbox [icon attrs iconName, extra, div attrs [text entryName]]


folderMenu : M.FolderState -> T.FolderPath -> Html M.Msg
folderMenu fstate path =
  let
    moveFolder =
      case T.folderPathBaseName path of
        Just basename ->
          [( hbox [icon [] "trending_flat", dtext "Move Folder"]
           , M.SetModal (M.MoveFolderItem {src=T.folderPathParent path, item=T.FolderSubfolder basename, dst=[]})
           )]
        Nothing -> []
    deleteFolder =
      if path /= []
      then [( hbox [icon [] "delete", dtext "Delete Folder"], M.SendCommand (T.DeleteFolder path))]
      else []
    renameFolder =
      if path /= []
      then [(hbox [icon [] "text_format", dtext "Rename Folder"], M.SetModal (M.RenameFolder {path=path, newName="New Name"}))]
      else []
    menuItems =
      [ ( hbox [icon [] "casino", dtext "Create Scene"]
        , M.SetModal (M.CreateScene {path = path, scene = T.SceneCreation "" ""}))
      , ( hbox [icon [] "map", dtext "Create Map"]
        , M.SetModal (M.CreateMap {path = path, name = "New Map"}))
      , ( hbox [icon [] "contacts", dtext "Create Creature"]
        , M.SetModal (M.CreateCreature {path = path, name = Nothing, class = Nothing}))
      , ( hbox [icon [] "note", dtext "Create Note"]
        , M.SetSecondaryFocus (M.Focus2Note path "New Note" {name="New Note", content=""}))
      , ( hbox [icon [] "folder", dtext "Create Folder"]
        , M.SetModal (M.CreateFolder {parent = path, child = ""}))
      ] ++  moveFolder ++ renameFolder ++ deleteFolder
  in
    CommonView.popUpMenu_ M.ToggleFolderCollapsed fstate "create-item-in-folder" (T.folderPathToString path)
      (icon [] "more_horiz") (icon [] "more_horiz")
      menuItems

folderSubEntries : M.FolderState -> T.App -> FolderViewConfig -> T.FolderPath -> T.Folder -> Html M.Msg
folderSubEntries fstate app cfg path (T.Folder folder) =
  let
    viewChild (folderName, childFolder) =
      let childPath = T.folderPathChild path folderName
          key = "folder-" ++ (T.folderPathToString childPath)
          isShown = Dict.get key fstate |> Maybe.withDefault False
          iconName = if isShown then "folder_open" else "folder"
          menu = if cfg.mutable && isShown then (folderMenu fstate childPath) else text ""
      in
        vbox [ hbox [baseFolderLine cfg childPath Nothing (Just <| M.ToggleFolderCollapsed key) iconName folderName, menu]
              , if isShown
                then div [s [S.marginLeft (S.em 1)]] [folderView fstate app cfg childPath childFolder]
                else text ""
              ]
  in vbox (List.map viewChild (Dict.toList folder.children))

folderView : M.FolderState -> T.App -> FolderViewConfig -> T.FolderPath -> T.Folder -> Html M.Msg
folderView fstate app cfg path (T.Folder folder) =
  let
    viewCreature creature =
      folderLine cfg path (Just (T.FolderCreature creature.id)) (M.SetSecondaryFocus (M.Focus2Creature path creature.id)) "contacts" creature.name
    viewScene sceneID =
      let scene = T.getScene app sceneID
          msg = M.Batch [ M.SetFocus (M.FocusScene sceneID)
                        , M.SetSecondaryFocus (M.Focus2Scene path sceneID)]
      in
        case scene of
          Just scene -> folderLine cfg path (Just (T.FolderScene sceneID)) msg "casino" scene.name
          Nothing -> text ("Invalid scene in folder: " ++ sceneID)
    viewNote (noteName, note) =
      folderLine cfg path (Just (T.FolderNote noteName)) (M.SetSecondaryFocus (M.Focus2Note path noteName note)) "note" noteName
    viewMap mapID =
      let map = M.getMapNamed mapID app
          msg = M.Batch [ M.SetFocus (M.PreviewMap mapID)
                        , M.SetSecondaryFocus (M.Focus2Map path mapID)]
      in case map of
           Just map -> folderLine cfg path (Just (T.FolderMap mapID)) msg "map" map.name
           Nothing -> text ("Invalid map in folder: " ++ mapID)
    scenes =
      if cfg.showScenes then vbox (List.map viewScene (Set.toList folder.data.scenes))
      else text ""
    creatures =
      if cfg.showCreatures
      then vbox (List.map viewCreature (T.getCreatures app.current_game (Set.toList folder.data.creatures)))
      else text ""
    notes =
      if cfg.showNotes then vbox (List.map viewNote (Dict.toList folder.data.notes))
      else text ""
    maps =
      if cfg.showMaps then vbox (List.map viewMap (Set.toList folder.data.maps))
      else text ""
    subfolders = folderSubEntries fstate app cfg path (T.Folder folder)
  in vbox [ scenes, maps, creatures, notes, subfolders]
