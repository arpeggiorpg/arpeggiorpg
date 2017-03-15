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
          then CommonView.viewGame model app (makeUI model app (T.getPlayerCreatures app playerID))
          else registerForm model
        Nothing -> registerForm model
    Nothing -> vbox [text "No app yet. Maybe reload.", hbox [text "Last error:", pre [] [text model.error]]]

{-| Show a form where the player can type their name to register. -}
registerForm : M.Model -> Html M.Msg
registerForm model =
  hbox [ input [type_ "text", placeholder "player ID", onInput M.SetPlayerID
       , s [S.width (S.px 500), S.height (S.px 100)] ] []
       , button [onClick M.RegisterPlayer] [text "Register Player"]]

makeUI : M.Model -> T.App -> List T.Creature -> CommonView.UI
makeUI model app myCreatures =
  { mapView = mapView model app myCreatures
  , sideBar =
      CommonView.tabbedView "right-side-bar" "My Creatures" model
        [ ("My Creatures", (always <| myCreaturesView model app myCreatures))
        , ("Combat", (always <| combatView model app myCreatures))]
  , extraMovementOptions = []
  , modal = CommonView.checkModal model app
  , extraOverlays = [bottomActionBar app myCreatures]
  }

bottomActionBar : T.App -> List T.Creature -> Html M.Msg
bottomActionBar app myCreatures =
  case app.current_game.current_combat of
    Nothing -> text ""
    Just combat ->
      if List.member (T.combatCreature app.current_game combat) myCreatures
      then CommonView.mainActionBar app combat
      else text ""

{-| A navigator for my creatures which aren't in combat. -}
myCreaturesView : M.Model -> T.App -> List T.Creature -> Html M.Msg
myCreaturesView model app creatures =
  let game = app.current_game
  in
    CommonView.collapsible "My Creatures" model
      <| vbox (List.map (myCreatureEntry model app) creatures)

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
mapView : M.Model -> T.App -> List T.Creature -> Html M.Msg
mapView model app myCreatures =
  let game = app.current_game
      movementGrid scene msg mvmtReq creature =
        case Dict.get creature.id scene.creatures of
          Just pos -> Grid.movementMap model msg mvmtReq False (M.getMap model) pos vCreatures
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
      currentCombatCreature = Maybe.map (\com -> (T.combatCreature com).id) game.current_combat
      creatureIsMine creature = List.any (\myC -> myC.id == creature.id) myCreatures
      modifyMapCreature mapc =
        let highlight = (Just mapc.creature.id) == currentCombatCreature
            movable =
              case game.current_combat of
                Just combat ->
                  if creatureIsMine mapc.creature && Just mapc.creature.id == currentCombatCreature
                  then Just (always M.GetCombatMovementOptions)
                  else Nothing
                Nothing -> if creatureIsMine mapc.creature then Just M.GetMovementOptions else Nothing
        in { mapc | highlight = highlight
                  , movable = movable}
      vCreatures = List.map modifyMapCreature (CommonView.visibleCreatures model app.current_game)
      defaultMap () = Grid.terrainMap model model.currentMap vCreatures
  in movementMap
      |> MaybeEx.unpack defaultMap identity

{-| Show all creatures in combat, with an action bar when it's my turn. -}
combatView : M.Model -> T.App -> List T.Creature -> Html M.Msg
combatView model app myCreatures =
  case app.current_game.current_combat of
    Just combat -> CommonView.collapsible "Combat" model <| inCombatView model app combat myCreatures
    Nothing -> text ""

inCombatView : M.Model -> T.App -> T.Combat -> List T.Creature -> Html M.Msg
inCombatView model app combat myCreatures =
  let game = app.current_game
      currentCreature = T.combatCreature combat
      bar = if List.member currentCreature myCreatures
            then sdiv [s [S.width (S.px 100)]] [strong [] [text currentCreature.name]]
            else hbox [text "Current creature:", text currentCreature.id]
      combatantList =
        CommonView.combatantList (always << always []) (always []) app combat
  in vbox <| [bar] ++ [combatantList]
