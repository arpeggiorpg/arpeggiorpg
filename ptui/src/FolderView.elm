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

campaignFolder : M.Model -> T.App -> Html M.Msg
campaignFolder model app =
  let
    cfg =
      { mutable = True
      , showScenes = True
      , showCreatures = True
      , showNotes = True
      , showMaps = True
      , allowFocus = True
      , contentControls = \_ _ -> text ""
      }
  in baseCampaignView model app cfg

selectCreatures : M.Model -> T.App -> (T.CreatureID -> M.Msg) -> (T.CreatureID -> M.Msg) -> List T.CreatureID -> Html M.Msg
selectCreatures model app addCreature remCreature preselected =
  let
    toggleCheck cid isChecked = if isChecked then addCreature cid else remCreature cid
    extraCheckbox _ itemId =
      case itemId of
        Just (T.FolderCreature cid) ->
          let isChecked = List.member cid preselected
          in input [type_ "checkbox", checked isChecked, onCheck (toggleCheck cid)] []
        _ -> text ""
    cfg = { showNothing | showCreatures = True, contentControls = extraCheckbox}
  in baseCampaignView model app cfg

selectFolder : M.Model -> T.App -> (T.FolderPath -> M.Msg) -> Html M.Msg
selectFolder model app msg =
  let
    select path _ =
      input [ type_ "radio", name "select-folder"
            , onClick (msg path) ]
            [text "Select"]
    cfg = { showNothing | contentControls = select }
  in
    baseCampaignView model app cfg

selectMap : M.Model -> T.App -> (T.MapID -> M.Msg) -> Html M.Msg
selectMap model app cb =
  let
    control path item =
      case item of
        Just (T.FolderMap mid) -> input [ type_ "radio", name "select-map", onClick (cb mid) ]
                                        []
        _ -> text ""
    cfg = { showNothing | showMaps = True, contentControls = control }
  in baseCampaignView model app cfg


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

baseCampaignView : M.Model -> T.App -> FolderViewConfig -> Html M.Msg
baseCampaignView model app cfg =
  rootFolder (cfg.contentControls [] Nothing) (folderView model app cfg [] app.current_game.campaign)

rootFolder : Html M.Msg -> Html M.Msg -> Html M.Msg
rootFolder extra content = 
  vbox [ hbox [icon [] "folder_open", extra, text "Campaign"]
       , div [s [S.marginLeft (S.em 1)]] [content]]

folderLine : FolderViewConfig -> T.FolderPath -> Maybe T.FolderItemID -> M.Msg -> String -> String -> Html M.Msg
folderLine cfg path mItem msg iconName entryName =
  let
    realMsg = if cfg.allowFocus then msg else M.NoMsg
    click = if cfg.allowFocus then clickable else s []
    extra = cfg.contentControls path mItem
  in
    hbox [icon [click, onClick realMsg] iconName, extra, div [click, onClick realMsg] [text entryName]]

folderSubEntries : M.Model -> T.App -> FolderViewConfig -> T.FolderPath -> T.Folder -> Html M.Msg
folderSubEntries model app cfg path (T.Folder folder) =
  let
    viewChild (folderName, childFolder) =
      let childPath = path ++ [folderName]
          key = "folder-" ++ String.join "/" childPath
          isShown = Dict.get key model.collapsed |> Maybe.withDefault False
          iconName = if isShown then "folder_open" else "folder"
      in
        vbox [ hbox [folderLine cfg childPath Nothing (M.ToggleCollapsed key) iconName folderName]
              , if isShown
                then div [s [S.marginLeft (S.em 1)]] [folderView model app cfg childPath childFolder]
                else text ""
              ]
  in vbox (List.map viewChild (Dict.toList folder.children))

folderView : M.Model -> T.App -> FolderViewConfig -> T.FolderPath -> T.Folder -> Html M.Msg
folderView model app cfg path (T.Folder folder) =
  let
    viewCreature creature =
      folderLine cfg path (Just (T.FolderCreature creature.id)) (M.SetSecondaryFocus (M.Focus2Creature path creature.id)) "contacts" creature.name
    viewScene sceneID =
      let scene = T.getScene app sceneID
          msg = M.Batch [M.SetFocus (M.Scene sceneID), M.SetSecondaryFocus (M.Focus2Scene path sceneID)]
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
    children = folderSubEntries model app cfg path (T.Folder folder)
    moveFolder =
      case T.folderPathBaseName path of
        Just basename ->
          [( hbox [icon[] "trending_flat", dtext "Move Folder"]
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
    addMenuItems =
      [ ( hbox [icon [] "casino", dtext "Create Scene"]
        , M.SetModal (M.CreateScene {path = path, scene = T.SceneCreation "" "" Dict.empty}))
      , ( hbox [icon [] "map", dtext "Create Map"]
        , M.SetModal (M.CreateMap {path = path, name = "New Map"}))
      , ( hbox [icon [] "contacts", dtext "Create Creature"]
        , M.SetModal (M.CreateCreature {path = path, name = Nothing, class = Nothing}))
      , ( hbox [icon [] "note", dtext "Create Note"]
        , M.SetSecondaryFocus (M.Focus2Note path "New Note" {name="New Note", content=""}))
      , ( hbox [icon [] "folder", dtext "Create Folder"]
        , M.SetModal (M.CreateFolder {parent = path, child = ""}))
      ] ++  moveFolder ++ renameFolder ++ deleteFolder
    addMenu =
      CommonView.popUpMenu model "create-item-in-folder" (T.folderPathToString path)
        (icon [] "more_horiz")
        (icon [] "more_horiz")
        addMenuItems
    menu = if cfg.mutable then [addMenu] else []
  in vbox <| menu ++ [ scenes, maps, creatures, notes, children]
