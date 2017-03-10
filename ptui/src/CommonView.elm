module CommonView exposing
  ( visibleCreatures, creatureCard, oocActionBar, combatActionBar, mapControls
  , movementControls, modalOverlay, checkModal
  , combatantList, collapsible, playerList)

import Dict
import Set

import Css as S
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeEx

import Model as M
import Types as T
import Grid

import Elements exposing (..)

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

visibleCreatures : M.Model -> T.Game -> List Grid.MapCreature
visibleCreatures model game =
  let mapInfo creature =
    Maybe.map (\class -> {creature = creature, highlight = False, movable = Nothing, class = class})
              (Dict.get creature.class game.classes)
  in
    case game.current_combat of
      Just combat ->
        if model.showOOC
        then List.filterMap mapInfo <| combat.creatures.data ++ (Dict.values game.creatures)
        else List.filterMap mapInfo combat.creatures.data
      Nothing -> (List.filterMap mapInfo (Dict.values game.creatures))

creatureCard : List (Html M.Msg) -> M.Model -> T.Creature -> Html M.Msg
creatureCard extras model creature =
  let cellStyles color =
        [s [ plainBorder
           , S.backgroundColor color
           , S.borderRadius (S.px 10)
           , S.padding (S.px 3)]]
  in
    vabox
      [s [plainBorder, S.width (S.px 300), S.height (S.px 100), S.borderRadius (S.px 10), S.padding (S.px 3)]]
      <| 
      [ hbox [strong [] [text creature.name ], classIcon creature]
      , hbox
          [ div (cellStyles (S.rgb 144 238 144))
                [text <| (toString creature.cur_health) ++ "/" ++ (toString creature.max_health)]
          , div (cellStyles (S.rgb 0 255 255))
                [text <| (toString creature.cur_energy) ++ "/" ++ (toString creature.max_energy)]
          , div (cellStyles (S.rgb 255 255 255))
                [text <| (toString creature.pos.x) ++ ", " ++ (toString creature.pos.y)]
          ]
      -- , hbox [ div (cellStyles (S.rgb 255 255 255)) [text "💪 10"]
      --        , div (cellStyles (S.rgb 255 255 255)) [text "🛡️ 10"]
      --        , div (cellStyles (S.rgb 255 255 255)) [text "🏃 10"]]
      , hbox (List.map conditionIcon (Dict.values creature.conditions))
      ] ++ extras

classIcon : T.Creature -> Html M.Msg
classIcon creature =
  case creature.class of
    "cleric" -> text "💉"
    "rogue" -> text "🗡️"
    "ranger" -> text "🏹"
    "creature" -> text "🏃"
    _ -> text ""

conditionIcon : T.AppliedCondition -> Html M.Msg
conditionIcon ac = case ac.condition of 
  T.RecurringEffect eff -> text (toString eff)
  T.Dead -> text "💀"
  T.Incapacitated -> text "😞"
  T.AddDamageBuff dmg -> text "😈"
  T.DoubleMaxMovement -> text "🏃"
  T.ActivateAbility abid -> hbox [text <| "ACTIVE: " ++ abid, durationEl ac.remaining]

durationEl : T.ConditionDuration -> Html M.Msg
durationEl condDur = case condDur of
  T.Interminate -> text "∞"
  T.Duration n -> text <| "(" ++ toString n ++ ")"


baseActionBar : Bool -> T.Game -> T.Creature -> List (Html M.Msg)
baseActionBar inCombat game creature =
  let abinfo abstatus =
        Maybe.andThen (\ability -> if ability.usable_ooc || inCombat then Just (abstatus.ability_id, ability) else Nothing)
                      (Dict.get abstatus.ability_id game.abilities)
      abilities = List.filterMap abinfo creature.abilities
  in (List.map (abilityButton creature) abilities)

{-| An action bar for characters who are not in combat. -}
oocActionBar : T.Game -> T.Creature -> List (Html M.Msg)
oocActionBar = baseActionBar False

combatActionBar : T.Game -> T.Combat -> T.Creature -> Html M.Msg
combatActionBar game combat creature =
  habox [s [S.flexWrap S.wrap]] ([doneButton creature] ++ [moveButton combat creature] ++  baseActionBar True game creature)

abilityButton : T.Creature -> (T.AbilityID, T.Ability) -> Html M.Msg
abilityButton creature (abid, ability) =
  actionButton
    [ onClick (M.SelectAbility creature.id abid)
    , disabled (not creature.can_act)]
    [text ability.name]

actionButton : List (Attribute msg) -> List (Html msg) -> Html msg
actionButton attrs children =
  button ([s [S.height (S.px 50), S.width (S.px 100)]] ++ attrs) children

doneButton : T.Creature -> Html M.Msg
doneButton creature =
  actionButton [onClick (M.SendCommand T.Done)] [text "Done"]

moveButton : T.Combat -> T.Creature -> Html M.Msg
moveButton combat creature =
  let movement_left = creature.speed - combat.movement_used
  in actionButton [ onClick M.GetCombatMovementOptions
            , disabled (not creature.can_move) ]
            [text (String.join "" ["Move (", toString (movement_left // 100), ")"])]

{-| The map controls: panning and zooming buttons
-}
mapControls : Html M.Msg
mapControls =
  vabox
    [s [ S.backgroundColor (S.rgb 230 230 230)]]
    [ hbox [ sqButton 40 [onClick (M.MapZoom M.Out)] [text "-"]
           , sqButton 40 [onClick (M.MapZoom M.In)] [text "+"]
           ]
    , vbox
        [ sqButton 40 [s [S.alignSelf S.center], onClick (M.MapPan M.Up)] [text "^"]
        , hbox
            [ sqButton 40 [s [S.flexGrow (S.int 1)], onClick (M.MapPan M.Left)] [text "<"]
            , sqButton 40 [s [S.flexGrow (S.int 1)], onClick (M.MapPan M.Right)] [text ">"]
            ]
        , sqButton 40 [s [S.alignSelf S.center], onClick (M.MapPan M.Down)] [text "v"]
        ]
    ]

combatantList : (Int -> T.Creature -> List (Html M.Msg)) -> (T.Creature -> List (Html M.Msg)) -> M.Model -> T.Game -> T.Combat -> Html M.Msg
combatantList extraGutter extraCreatureCard model game combat =
  vbox (List.map (combatantEntry extraGutter extraCreatureCard model game combat) (List.indexedMap (,) combat.creatures.data))

combatantEntry : (Int -> T.Creature -> List (Html M.Msg)) -> (T.Creature -> List (Html M.Msg)) -> M.Model -> T.Game -> T.Combat -> (Int, T.Creature) -> Html M.Msg
combatantEntry extraGutter extraCreatureCard model game combat (idx, creature) = hbox <|
  let marker = if combat.creatures.cursor == idx
               then [ datext [s [S.width (S.px 25)]] "▶️️" ]
               else []
      gutter = [vabox [s [(S.width (S.px 25))]] <| marker ++ extraGutter idx creature]
  in gutter ++ [ creatureCard (extraCreatureCard creature) model creature ]

targetSelector : M.Model -> T.Game -> (T.AbilityID -> T.DecidedTarget -> M.Msg) -> String -> Html M.Msg
targetSelector model game msgConstructor abid =
  let creatures = List.filterMap (T.findCreature game) (T.potentialCreatureTargets model.potentialTargets)
  in hbox <|
    [ case (Dict.get abid game.abilities) of
        Just ability -> case ability.target of
          T.Melee -> creatureTargetSelector (msgConstructor abid) T.DecidedMelee creatures
          T.Range distance -> creatureTargetSelector (msgConstructor abid) T.DecidedRange creatures
          T.Actor -> sqButton 100 [onClick (msgConstructor abid T.DecidedActor)] [text "Use on Self"]
        Nothing -> text "Sorry, that ability was not found. Please reload."
    , sqButton 100 [onClick M.CancelAbility] [text "Cancel ability"]
    ]

creatureTargetSelector : (T.DecidedTarget -> M.Msg) -> (T.CreatureID -> T.DecidedTarget) -> List T.Creature -> Html M.Msg
creatureTargetSelector msgConstructor targetConstructor creatures = vbox <|
  let targetCreatureButton c = sqButton 100 [onClick (msgConstructor (targetConstructor c.id))] [text c.name]
  in List.map targetCreatureButton creatures

collapsible : String -> M.Model -> Html M.Msg -> Html M.Msg
collapsible header model content =
  let isCollapsed = Dict.get header model.collapsed |> Maybe.withDefault False
      arrow = if isCollapsed then "▶" else "▼"
  in vbox <|
    [ hr [s [S.width (S.pct 100)]] []
    , hbox [strong [] [text header], button [onClick (M.ToggleCollapsed header)] [text arrow]]
    , hr [s [S.width (S.pct 100)]] []
     ] ++ if isCollapsed then [] else [content]

playerList : (T.PlayerID -> List (Html M.Msg)) -> Dict.Dict T.PlayerID (Set.Set T.CreatureID) -> Html M.Msg
playerList extra players =
  let playerEntry (pid, cids) =
        habox [s [S.justifyContent S.spaceBetween]] <|
          [strong [] [text pid]] ++ (List.map (\id -> div [] [text id]) (Set.toList cids)) ++ extra pid
  in vbox (List.map playerEntry (Dict.toList players))


movementControls : List (Html M.Msg) -> M.Model -> List (Html M.Msg)
movementControls extras model =
  case model.moving of
    Just _ ->
      [ div [s [ S.position S.absolute
                , S.left (S.pct 50)
                , S.transform (S.translate (S.pct -50))
                , plainBorder
                , S.backgroundColor (S.rgb 255 255 255)]]
        <| [ button [onClick M.CancelMovement] [text "Cancel Movement"]] ++ extras]
    Nothing -> []

{-| Render a modal dialog which disables use of all other UI. -}
modalOverlay : Html M.Msg -> List (Html M.Msg)
modalOverlay content =
  let box =
        div [s [ S.position S.absolute
                , S.left (S.pct 50)
                , S.top (S.pct 50)
                , S.transform (S.translate2 (S.pct -50) (S.pct -50))
                , plainBorder
                , S.backgroundColor (S.rgb 255 255 255)]]
            [content]
      cover =
        div [s [S.position S.absolute
                , S.width (S.vw 100)
                , S.height (S.vh 100)
                , S.backgroundColor (S.rgba 0 0 0 0.5)]]
            []
  in [cover, box]

{-| Check for any modals that both GMs and Players may need to render. -}
checkModal : M.Model -> T.App -> Maybe (Html M.Msg)
checkModal model app =
  let
    game = app.current_game
    selectingTargets =
      -- TODO: target selection should be done on the map
      case model.selectedAbility of
        Just (cid, abid) ->
          if T.isCreatureInCombat game cid
          then Just (targetSelector model game M.CombatAct abid)
          else Nothing
        Nothing -> Nothing
  in selectingTargets
