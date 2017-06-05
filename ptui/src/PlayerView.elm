module PlayerView exposing (playerView)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeEx

import Model as M
import Types as T
import Grid
import Elements exposing (..)

import CommonView

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

{-| Top-level player view. -}
playerView : M.Model -> Html M.Msg
playerView model =
  case model.app of
    Just app ->
      case model.playerID of
        Just playerID ->
          if T.playerIsRegistered app playerID
          then CommonView.viewGame model app (makeUI model app playerID)
          else registerForm model app
        Nothing -> registerForm model app
    Nothing -> vbox [text "No app yet. Maybe reload.", hbox [text "Last error:", pre [] [text model.error]]]

{-| Show a form where the player can type their name to register. -}
registerForm : M.Model -> T.App -> Html M.Msg
registerForm model app =
  let playerButton pid =
        button [s [S.width (S.px 250), S.height (S.px 100)]
               , onClick (M.SetPlayerID pid)]
               [text pid]
  in 
  vbox <|
    [ hbox [ input [type_ "text", placeholder "player ID", onInput M.SetPlayerID
         , s [S.width (S.px 500), S.height (S.px 100)] ] []
         , button [onClick M.RegisterPlayer] [text "Register Player"]]
    , text "Or select an existing player:"
    , habox [s [S.flexWrap S.wrap]] (List.map playerButton (Dict.keys app.players))
    ]

makeUI : M.Model -> T.App -> String -> CommonView.UI
makeUI model app playerID =
  let
    myCreatures = T.getPlayerCreatures app playerID
    (map, mapModeControls) = mapView model app myCreatures
  in
    { mapView = map
    , mapModeControls = mapModeControls
    , defaultTab = "My Creatures"
    , tabs =
          [ ("My Creatures", (\() -> myCreaturesView model app myCreatures))
          , ("Combat", (\() -> combatView model app myCreatures))
          , ("Notes", (\() -> playerNote model app playerID))]
    , modal = CommonView.checkModal model app
    , bottomBar = bottomActionBar app myCreatures
    , extra = []
    }

playerNote : M.Model -> T.App -> String -> Html M.Msg
playerNote model app playerID =
  let path = ["Players", playerID] in
  case T.getFolder app path of
    Just (T.Folder folder) ->
      let
        scratchNote =
          model.scratchNote
          |> Maybe.withDefault (Dict.get "Scratch" folder.data.notes |> Maybe.map .content |> Maybe.withDefault "Enter notes here")
        update newNote = M.UpdateScratchNote newNote.content
      in CommonView.noteEditor model app update path "Scratch" {name="Scratch", content=scratchNote} False
    _ -> text ("Note editing disabled until your GM creates the folder " ++ T.folderPathToString path)


bottomActionBar : T.App -> List T.Creature -> Maybe (Html M.Msg)
bottomActionBar app myCreatures =
  app.current_game.current_combat |>
    Maybe.andThen (\combat ->
      if List.member (T.combatCreature app.current_game combat) myCreatures
      then Just (CommonView.mainActionBar app combat)
      else Nothing
    )      

{-| A navigator for my creatures which aren't in combat. -}
myCreaturesView : M.Model -> T.App -> List T.Creature -> Html M.Msg
myCreaturesView model app creatures =
  let game = app.current_game
  in vbox (List.map (myCreatureEntry model app) creatures)

{-| A creature card plus some UI relevant for when they are out-of-combat. -}
myCreatureEntry : M.Model -> T.App -> T.Creature -> Html M.Msg
myCreatureEntry model app creature =
  vbox
    [ CommonView.creatureCard [] app creature
    , case app.current_game.current_combat of
        Nothing -> hbox (CommonView.oocActionBar model app.current_game creature)
        Just _ -> text ""
    ]

{-| Figure out which map should be rendered and render it. -}
mapView : M.Model -> T.App -> List T.Creature -> (Html M.Msg, Html M.Msg)
mapView model app myCreatures =
  case model.focus of
    M.FocusScene name ->
      case Dict.get name app.current_game.scenes of
        Just scene -> sceneMap model app scene myCreatures
        Nothing -> (text "", text "")
    _ -> (text "", text "Waiting for the GM to put you into a scene.")

sceneMap : M.Model -> T.App -> T.Scene -> List T.Creature -> (Html M.Msg, Html M.Msg)
sceneMap model app scene myCreatures =
  let game = app.current_game
      currentMap = filterMapSpecials <| M.tryGetMapNamed scene.map app
      currentCombatCreature = Maybe.map (\com -> (T.combatCreature game com).id) game.current_combat
      creatureIsMine creature = List.any (\myC -> myC.id == creature.id) myCreatures
      modifyMapCreature mapc =
        let highlight = if (Just mapc.creature.id) == currentCombatCreature then Just M.Current else Nothing
            clickable =
              case game.current_combat of
                Just combat ->
                  if creatureIsMine mapc.creature && Just mapc.creature.id == currentCombatCreature
                  then Just (always M.GetCombatMovementOptions)
                  else Nothing
                Nothing -> if creatureIsMine mapc.creature then Just (M.GetMovementOptions scene.id) else Nothing
        in { mapc | highlight = highlight
                  , clickable = clickable}
      vCreatures = List.map modifyMapCreature (visibleCreatures game scene)
      defaultMap () = ( Grid.terrainMap model currentMap vCreatures
                      , text "Click a creature to move it.")
  in
    (CommonView.movementMap model app scene currentMap vCreatures
      |> Maybe.map (\g -> (g, CommonView.movementControls [] model)))
    |> MaybeEx.or (CommonView.targetMap model app scene currentMap vCreatures)
    |> MaybeEx.unpack defaultMap identity

visibleCreatures game scene =
  let mod mapc = if mapc.visible then (Just mapc) else Nothing
  in List.filterMap mod (CommonView.visibleCreatures game scene)

filterMapSpecials : T.Map -> T.Map
filterMapSpecials map =
  let onlyShowPlayerSpecials pt (color, note, vis) =
        case vis of T.AllPlayers -> True
                    _ -> False
  in { map | specials = Dict.filter onlyShowPlayerSpecials map.specials}

{-| Show all creatures in combat, with an action bar when it's my turn. -}
combatView : M.Model -> T.App -> List T.Creature -> Html M.Msg
combatView model app myCreatures =
  case app.current_game.current_combat of
    Just combat -> inCombatView model app combat myCreatures
    Nothing -> text "There is no current combat."

inCombatView : M.Model -> T.App -> T.Combat -> List T.Creature -> Html M.Msg
inCombatView model app combat myCreatures =
  CommonView.combatantList (always << always << always []) (always []) app combat
