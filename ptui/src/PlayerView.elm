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

import CommonView exposing (..)

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

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
      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

registerForm : M.Model -> Html M.Msg
registerForm model =
  hbox [ input [type_ "text", placeholder "player ID", onInput M.SetPlayerID ] []
       , button [onClick M.RegisterPlayer] [text "Register Player"]]

viewGame : M.Model -> T.App -> List T.Creature -> Html M.Msg
viewGame model app creatures = 
  div
    -- TODO: I should maybe move the "vh" to a div up all the way to the very top of the tree.
    [s [S.position S.relative, S.width (S.pct 100), S.height (S.vh 98)]]
    <| 
    [ overlay (S.px 0)  (S.px 0) [S.height (S.pct 100)]
        [mapView model app]
    , overlay (S.px 0)  (S.px 0) [S.width (S.px 80)]
        [CommonView.mapControls]
    , overlay (S.px 0) (S.px 160) [S.width (S.px 325)]
        [CommonView.collapsible "Players" model <| playersView app]
    , overlayRight (S.px 0) (S.px 0) [S.width (S.px 325)]
        [ myCreaturesView model app creatures
        , combatView model app creatures]
    ]
    ++ CommonView.movementControls [] model
    ++ modalView model app

playersView : T.App -> Html M.Msg
playersView app = CommonView.playerList (always []) app.players

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

mapView : M.Model -> T.App -> Html M.Msg
mapView model app =
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
      modifyMapCreature mapc =
        let highlight = (Just mapc.creature.id) == currentCombatCreature
        in { mapc | highlight = highlight
                  -- TODO: 1. only move out-of-combat for non-current creatures; when it's MY creature
                  --       2. only move current combat creature when it's MY creature
                  , movable = Just M.GetMovementOptions}
      vCreatures = List.map modifyMapCreature (CommonView.visibleCreatures model app.current_game)
      defaultMap () = Grid.terrainMap model model.currentMap vCreatures
  in movementMap
      |> MaybeEx.unpack defaultMap identity

  -- [ case model.moving of
  --     Just movementRequest ->
  --       let {max_distance, movement_options, ooc_creature} = movementRequest in
  --       case ooc_creature of
  --         Just oocMovingCreature ->
  --           if List.member oocMovingCreature creatures
  --           then
  --             -- one of my characters is moving; render the movement map
  --             Grid.movementMap model (M.PathCreature oocMovingCreature.id) movementRequest
  --                              False model.currentMap oocMovingCreature (visibleCreatures model game)
  --           else
  --             playerGrid model game creatures
  --         Nothing -> -- we're moving in-combat.
  --           case Maybe.map T.combatCreature game.current_combat of
  --               Just creature ->
  --                 let highlightMover mapc =
  --                       if creature.id == mapc.creature.id
  --                       then {mapc | highlight = True}
  --                       else mapc
  --                     vCreatures = List.map highlightMover (visibleCreatures model game)
  --                 in Grid.movementMap model M.PathCurrentCombatCreature movementRequest
  --                                  False model.currentMap creature vCreatures
  --               Nothing -> playerGrid model game creatures
  --     Nothing -> playerGrid model game creatures
  -- ]
  -- -- TODO: a read-only "creatures nearby" list without details

combatView : M.Model -> T.App -> List T.Creature -> Html M.Msg
combatView model app creatures =
  let game = app.current_game
  in
    case game.current_combat of
      Just combat ->
        let currentCreature = T.combatCreature combat
            bar = if List.member currentCreature creatures
                  then habox [s [S.flexWrap S.wrap]]
                            [ div [s [S.width (S.px 100)]] [strong [] [text currentCreature.name]]
                            , combatActionBar game combat currentCreature
                            ]
                  else hbox [text "Current creature:", text currentCreature.id]
            initiativeList = combatantList False model game combat
        in collapsible "Combat" model <| vbox <| [bar] ++ [initiativeList]
      Nothing -> text ""

-- -- render the grid and some OOC movement buttons for the given creatures
-- playerGrid : M.Model -> T.Game -> List T.Creature -> Html M.Msg
-- playerGrid model game myCreatures =
--   let moveOOCButton creature =
--         button [ onClick (M.GetMovementOptions creature)
--                , disabled (not creature.can_move)]
--                [text "Move"]
--       bar creature =
--         if T.isCreatureInCombat game creature.id
--         then Nothing
--         else Just <| hbox <| [creatureCard [] model creature, moveOOCButton creature] ++ (oocActionBar game creature)
--       oocBars = List.filterMap bar myCreatures
--       ooc = if (List.length oocBars) > 0
--             then [vbox <| [h4 [] [text "Out Of Combat"]] ++ oocBars]
--             else []
--       comUI =
--         case game.current_combat of
--           Just combat -> [h4 [] [text "Combat"], playerCombatArea model game combat myCreatures]
--           Nothing -> [text "No Combat"]
--       movable mapc =
--         -- Don't allow click-to-move when ANY creatures are in combat.
--         -- TODO: maybe allow click-to-move for out-of-combat creatures
--         case game.current_combat of
--           Just com ->
--             if List.member (T.combatCreature com) myCreatures && mapc.creature == T.combatCreature com
--             then Just (\_ -> M.GetCombatMovementOptions)
--             else Nothing
--           Nothing -> Just M.GetMovementOptions
--       currentCreature = Maybe.map (\com -> (T.combatCreature com).id) game.current_combat
--       modifyMapCreature mapc =
--         let highlight = (Just mapc.creature.id) == currentCreature
--         in { mapc | movable = (movable mapc), highlight = highlight}
--       vCreatures = List.map modifyMapCreature (visibleCreatures model game)
--   in
--     hbox <| [Grid.terrainMap model model.currentMap vCreatures
--             , vabox [s [S.width (S.px 350)]] (comUI ++ ooc ++ targetSel)]

combatantList : Bool -> M.Model -> T.Game -> T.Combat -> Html M.Msg
combatantList isGmView model game combat =
  vbox (List.map (combatantEntry isGmView model game combat) (List.indexedMap (,) combat.creatures.data))

combatantEntry : Bool -> M.Model -> T.Game -> T.Combat -> (Int, T.Creature) -> Html M.Msg
combatantEntry isGmView model game combat (idx, creature) = hbox <|
  let marker = if combat.creatures.cursor == idx
               then [ datext [s [S.width (S.px 25)]] "▶️️" ]
               else []
      initiativeArrows =
        if isGmView
        then [ button [ onClick (M.SendCommand (T.ChangeCreatureInitiative creature.id (idx - 1)))
                      , disabled (idx == 0)]
                      [text "⬆️️"]
             , button [ onClick (M.SendCommand (T.ChangeCreatureInitiative creature.id (idx + 1)))
                      , disabled (idx == (List.length combat.creatures.data) - 1)]
                      [text "⬇️️"]
             ]
        else []
      gutter = [vabox [s [(S.width (S.px 25))]] <| marker ++ initiativeArrows]
  in gutter ++ [ creatureCard [] model creature ]
