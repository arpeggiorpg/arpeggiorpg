module PlayerView exposing (playerView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeEx

import Model as M
import Types as T
import Grid
import Elements exposing (..)

import CommonView exposing (..)

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

{-| Top-level player view. -}
playerView : M.Model -> Html M.Msg
playerView model = vbox
  [ case model.app of
      Just app ->
        let vGame = case model.playerID of
              Just playerID ->
                if T.playerIsRegistered app playerID
                then viewGame model app (T.getPlayerCreatures app playerID)
                else registerForm model
              Nothing -> registerForm model
        in vbox [vGame, playerList (always []) app.players]
      Nothing -> vbox [text "No app yet. Maybe reload.", hbox [text "Last error:", pre [] [text model.error]]]
  ]

{-| Show a form where the player can type their name to register. -}
registerForm : M.Model -> Html M.Msg
registerForm model =
  hbox [ input [type_ "text", placeholder "player ID", onInput M.SetPlayerID ] []
       , button [onClick M.RegisterPlayer] [text "Register Player"]]

{-| Show a Player's-eye-view of a game, in the context of a set of controlled characters. -}
viewGame : M.Model -> T.App -> List T.Creature -> Html M.Msg
viewGame model app myCreatures = 
  div
    -- TODO: I should maybe move the "vh" to a div up all the way to the very top of the tree.
    [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98)]]
    <| 
    [ overlay (S.px 0)  (S.px 0) [S.height (S.pct 100)]
        [mapView model app myCreatures]
    , overlay (S.px 0)  (S.px 0) [S.width (S.px 80)]
        [CommonView.mapControls]
    , overlay (S.px 0) (S.px 160) [S.width (S.px 325)]
        [CommonView.collapsible "Players" model <| CommonView.playerList (always []) app.players]
    , overlayRight (S.px 0) (S.px 0) [S.width (S.px 325)]
        [ myCreaturesView model app myCreatures
        , combatView model app myCreatures]
    , CommonView.errorBox model
    ]
    ++ CommonView.movementControls [] model
    ++ modalView model app

{-| Check if any modals should be rendered and render them. -}
modalView : M.Model -> T.App -> List (Html M.Msg)
modalView model app =
  checkModal model app
    |> Maybe.map CommonView.modalOverlay
    |> Maybe.withDefault []

{-| A navigator for my creatures which aren't in combat. -}
myCreaturesView : M.Model -> T.App -> List T.Creature -> Html M.Msg
myCreaturesView model app creatures =
  let game = app.current_game
  in
    CommonView.collapsible "My Creatures" model
      <| vbox (List.map (myCreatureEntry model game) creatures)

{-| A creature card plus some UI relevant for when they are out-of-combat. -}
myCreatureEntry : M.Model -> T.Game -> T.Creature -> Html M.Msg
myCreatureEntry model game creature =
  vbox
    [ CommonView.creatureCard [] model creature
    , case game.current_combat of
        Nothing -> hbox (CommonView.oocActionBar game creature)
        Just _ -> text ""
    ]

{-| Figure out which map should be rendered and render it. -}
mapView : M.Model -> T.App -> List T.Creature -> Html M.Msg
mapView model app myCreatures =
  let game = app.current_game
      movementGrid msg mvmtReq creature =
        Grid.movementMap model msg mvmtReq False model.currentMap creature vCreatures
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
    Just combat -> collapsible "Combat" model <| inCombatView model app combat myCreatures
    Nothing -> text ""

inCombatView : M.Model -> T.App -> T.Combat -> List T.Creature -> Html M.Msg
inCombatView model app combat myCreatures =
  let game = app.current_game
      currentCreature = T.combatCreature combat
      bar = if List.member currentCreature myCreatures
            then habox [s [S.flexWrap S.wrap]]
                      [ div [s [S.width (S.px 100)]] [strong [] [text currentCreature.name]]
                      , combatActionBar game combat currentCreature
                      ]
            else hbox [text "Current creature:", text currentCreature.id]
      combatantList =
        CommonView.combatantList (always << always []) (always []) model game combat
  in vbox <| [bar] ++ [combatantList]
