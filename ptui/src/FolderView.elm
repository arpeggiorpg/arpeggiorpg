module FolderView exposing (campaignFolder, selectFolders)

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
campaignFolder model app = rootFolder (folderView model app [] app.current_game.campaign)

rootFolder content = 
  vbox [ hbox [icon [] "folder_open", text "Campaign (yes this campaign)"]
       , div [s [S.marginLeft (S.em 1)]] [content]]

selectFolders : M.Model -> T.App -> Html M.Msg
selectFolders model app = rootFolder (folderOnlyView model app [] app.current_game.campaign)

folderOnlyView : M.Model -> T.App -> T.FolderPath -> T.Folder -> Html M.Msg
folderOnlyView model app path folder =
  folderSubEntries model app path folder folderOnlyView
  
folderLine msg iconName entryName = habox [clickable, onClick msg] [icon [] iconName, text entryName]

folderSubEntries model app path (T.Folder folder) recurse =
  let
    viewChild (folderName, childFolder) =
      let childPath = path ++ [folderName]
          key = "folder-" ++ String.join "/" childPath
          isShown = Dict.get key model.collapsed |> Maybe.withDefault False
          iconName = if isShown then "folder_open" else "folder"
      in
        vbox [ hbox [folderLine (M.ToggleCollapsed key) iconName folderName]
              , if isShown
                then div [s [S.marginLeft (S.em 1)]] [recurse model app childPath childFolder]
                else text ""
              ]
  in vbox (List.map viewChild (Dict.toList folder.children))


folderView : M.Model -> T.App -> T.FolderPath -> T.Folder -> Html M.Msg
folderView model app path (T.Folder folder) =
  let
    viewCreature creature =
      folderLine (M.SetSecondaryFocus (M.Focus2Creature path creature.id)) "contacts" creature.name
    viewScene sceneID =
      let scene = T.getScene app sceneID
          msg = M.Batch [M.SetFocus (M.Scene sceneID), M.SetSecondaryFocus (M.Focus2Scene path sceneID)]
      in
        case scene of
          Just scene -> folderLine msg "casino" scene.name
          Nothing -> text ("Invalid scene in folder: " ++ sceneID)
    viewNote (noteName, note) =
      folderLine (M.SetSecondaryFocus (M.Focus2Note path noteName note)) "note" noteName
    viewMap mapID =
      let map = M.getMapNamed mapID app
          msg = M.Batch [ M.SetFocus (M.PreviewMap mapID)
                        , M.SetSecondaryFocus (M.Focus2Map path mapID)]
      in case map of
           Just map -> folderLine msg "map" map.name
           Nothing -> text ("Invalid map in folder: " ++ mapID)
    scenes = vbox (List.map viewScene (Set.toList folder.data.scenes))
    creatures =
      vbox (List.map viewCreature (T.getCreatures app.current_game (Set.toList folder.data.creatures)))
    notes = vbox (List.map viewNote (Dict.toList folder.data.notes))
    maps = vbox (List.map viewMap (Set.toList folder.data.maps))
    children = folderSubEntries model app path (T.Folder folder) folderView
    deleteFolder =
      if path /= []
      then [( hbox [icon [] "delete", dtext "Delete Folder"], M.SendCommand (T.DeleteFolder path))]
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
      ] ++ deleteFolder
    addMenu =
      CommonView.popUpMenu model "create-item-in-folder" (T.folderPathToString path)
        (icon [] "more_horiz")
        (icon [] "more_horiz")
        addMenuItems
  in vbox [ hbox [addMenu], scenes, maps, creatures, notes, children]
