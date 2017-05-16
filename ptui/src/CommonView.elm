module CommonView exposing
  ( visibleCreatures, creatureCard, creatureIcon, creatureAbilities, oocActionBar
  , hpBubble, nrgBubble
  , movementMap, movementControls
  , checkModal
  , classIcon
  , combatantList, collapsible, playerList, errorBox
  , mainActionBar, tabbedView, viewGame, UI, popUpMenu, popUpMenu_, targetMap)

import Dict
import Set

import Css as S
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model as M
import Types as T
import Grid

import Elements exposing (..)

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button


visibleCreatures : T.Game -> T.Scene -> List M.MapCreature
visibleCreatures game scene =
  let mapInfo creature =
        Dict.get creature.class game.classes
          |> Maybe.andThen (\class ->
              Dict.get creature.id scene.creatures
                |> Maybe.map (\(pos, vis) ->
                  { creature = creature, highlight = Nothing, clickable = Nothing, class = class, pos = pos
                  , visible = vis == T.AllPlayers}))
      creatures = T.getCreatures game (Dict.keys scene.creatures)
  in
    List.filterMap mapInfo creatures


bubbleStyle : S.ColorValue compatible -> List (Attribute msg)
bubbleStyle color =
        [s [ plainBorder
           , S.backgroundColor color
           , S.borderRadius (S.px 10)
           , S.padding (S.px 3)]]


hpBubble : T.Creature -> Html M.Msg
hpBubble creature =
  sdiv (bubbleStyle (S.rgb 144 238 144))
       [text <| (toString creature.cur_health) ++ "/" ++ (toString creature.max_health)]

nrgBubble : T.Creature -> Html M.Msg
nrgBubble creature =
  sdiv (bubbleStyle (S.rgb 0 255 255))
       [text <| (toString creature.cur_energy) ++ "/" ++ (toString creature.max_energy)]

creatureCard : List (Html M.Msg) -> T.App -> T.Creature -> Html M.Msg
creatureCard extras app creature =
    vabox
      [s [ plainBorder
         , S.width (S.px 300), S.height (S.px 100), S.borderRadius (S.px 10), S.padding (S.px 3)
         , S.position S.relative]]
      <|
      [ hbox [strong [] [text creature.name ], classIcon creature]
      , hbox
          [ creatureIcon app creature
          , hpBubble creature
          , nrgBubble creature
          ]
      , hbox (List.map conditionIcon (Dict.values creature.conditions))
      ] ++ extras

classIcon : T.Creature -> Html M.Msg
classIcon creature =
  case creature.class of
    "cleric" -> text "💉"
    "rogue" -> text "🗡️"
    "ranger" -> text "🏹"
    "creature" -> text "🏃"
    "baddie" -> text "👹"
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

creatureAbilities : T.Game -> T.SceneID -> Bool -> T.Creature -> List (Html M.Msg, M.Msg)
creatureAbilities game sceneID inCombat creature =
  let
    abinfo abstatus =
        Maybe.andThen (\ability -> if ability.usable_ooc || inCombat then Just (abstatus.ability_id, ability) else Nothing)
                      (Dict.get abstatus.ability_id game.abilities)
    abilities = List.filterMap abinfo creature.abilities
    abilityMsg abid ability =
      case ability.target of
        T.Actor -> if inCombat then M.CombatAct abid T.TargetedActor
                               else M.ActCreature sceneID creature.id abid T.TargetedActor
        _ -> M.SelectAbility { scene=sceneID, creature=creature.id, ability=abid
                             , potentialTargets=Nothing, chosenPoint=Nothing }
    toResultTuple (abid, ability) = (text ability.name, abilityMsg abid ability)
  in
    List.map toResultTuple abilities

baseActionBar : T.SceneID -> Bool -> T.Game -> T.Creature -> List (Html M.Msg)
baseActionBar sceneID inCombat game creature =
  let abilityButton (text, msg) = actionButton [onClick msg, disabled (not creature.can_act)] [text]
  in List.map abilityButton (creatureAbilities game sceneID inCombat creature)

{-| An action bar for characters who are not in combat. -}
oocActionBar : M.Model -> T.Game -> T.Creature -> List (Html M.Msg)
oocActionBar model game creature =
  case model.focus of
    M.FocusScene name -> baseActionBar name False game creature
    _ -> [dtext "Can't render an action bar without a scene!"]

combatActionBar : T.Game -> T.Combat -> T.Creature -> Html M.Msg
combatActionBar game combat creature =
  let sceneName = combat.scene
  in
    habox [s [S.flexWrap S.wrap]]
      (  [doneButton creature]
      ++ [moveButton combat creature]
      ++ baseActionBar sceneName True game creature
      )

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

combatantList : (Int -> T.Creature -> List (Html M.Msg)) -> (T.Creature -> List (Html M.Msg)) -> T.App -> T.Combat -> Html M.Msg
combatantList extraGutter extraCreatureCard app combat =
  let creatures = T.getCreatures app.current_game combat.creatures.data
  in vbox (List.map (combatantEntry extraGutter extraCreatureCard app combat) (List.indexedMap (,) creatures))

combatantEntry : (Int -> T.Creature -> List (Html M.Msg)) -> (T.Creature -> List (Html M.Msg)) -> T.App -> T.Combat -> (Int, T.Creature) -> Html M.Msg
combatantEntry extraGutter extraCreatureCard app combat (idx, creature) = hbox <|
  let marker = if combat.creatures.cursor == idx
               then [ datext [s [S.width (S.px 25)]] "▶️️" ]
               else []
      gutter = [vabox [s [(S.width (S.px 25))]] <| marker ++ extraGutter idx creature]
  in gutter ++ [ creatureCard (extraCreatureCard creature) app creature ]

collapsible : String -> M.Model -> Html M.Msg -> Html M.Msg
collapsible header model content =
  let isCollapsed = Dict.get header model.collapsed |> Maybe.withDefault False
      arrow = if isCollapsed then "▶" else "▼"
  in vbox <|
    [ hline
    , hbox [strong [] [text header], button [onClick (M.ToggleCollapsed header)] [text arrow]]
    , hline
     ] ++ if isCollapsed then [] else [content]

playerList : T.App -> (T.Player -> List (Html M.Msg)) -> Dict.Dict T.PlayerID T.Player -> Html M.Msg
playerList app extra players =
  let header =
        hbox [ div [s [S.width (S.px 150)]] [strong [] [text "Player"]]
             , div [s [S.width (S.px 150)]] [strong [] [text "Characters"]]]
      playerEntry player =
        let creatures = T.getCreatures app.current_game (Set.toList player.creatures)
        in
          habox [s [S.width (S.px 450)]]
             <| [ div [s [S.width (S.px 150)]] [strong [] [text player.player_id]]
                  , vabox [s [S.width (S.px 150)]] (List.map (\c -> sdiv [] [text c.name]) creatures)
                  ] ++ extra player
  in vabox [] <| header :: (List.map playerEntry (Dict.values players))


mapModeControlsOverlay : Html M.Msg -> Html M.Msg
mapModeControlsOverlay content =
  sdiv [s [ S.position S.fixed
                , S.left (S.pct 50)
                , S.transform (S.translate (S.pct -50))
                , S.top (S.px 0)
                , plainBorder
                , S.backgroundColor (S.rgb 255 255 255)]]
       [content]

movementControls : List (Html M.Msg) -> M.Model -> Html M.Msg
movementControls extras model =
  case model.moving of
    Just _ ->
      vbox <| [ button [s [S.height (S.px 50), S.width (S.px 150)], onClick M.CancelMovement]
                       [text "Cancel Movement"]
              ] ++ extras
    Nothing -> text ""

movementMap : M.Model -> T.App -> T.Scene -> T.Map -> List M.MapCreature -> Maybe (Html M.Msg)
movementMap model app scene map vCreatures =
  let
    movementGrid msg mvmtReq creature =
      case Dict.get creature.id scene.creatures of
        Just (pos, _) ->
          Grid.movementMap model msg mvmtReq model.moveAnywhere map pos vCreatures
        Nothing -> text "Moving Creature is not in this scene"
    pathOrPort = if model.moveAnywhere then M.SetCreaturePos scene.id else M.PathCreature scene.id
  in
    case (app.current_game.current_combat, model.moving) of
      (Nothing, Just mvmtReq) ->
        Maybe.map (\creature -> movementGrid (pathOrPort creature.id) mvmtReq creature)
                  mvmtReq.ooc_creature
      (Just combat, Just mvmtReq) ->
        let (creature, moveMessage) =
              case mvmtReq.ooc_creature of
                Just creature -> (creature, pathOrPort creature.id)
                Nothing -> (T.combatCreature app.current_game combat, M.PathCurrentCombatCreature)
        in Just <| movementGrid moveMessage mvmtReq creature
      _ -> Nothing

{-| Render a modal dialog which disables use of all other UI. -}
modalOverlay : Html M.Msg -> List (Html M.Msg)
modalOverlay content =
  let box =
        sdiv [s [ S.position S.fixed
                , S.left (S.pct 50)
                , S.top (S.pct 50)
                , S.transform (S.translate2 (S.pct -50) (S.pct -50))
                , S.minWidth (S.pct 50)
                , S.minHeight (S.pct 50)
                , plainBorder
                , S.backgroundColor (S.rgb 255 255 255)]]
            [content]
      cover =
        sdiv [s [ S.position S.fixed
                , S.left (S.px 0), S.top (S.px 0)
                , S.width (S.pct 100)
                , S.height (S.pct 100)
                , S.backgroundColor (S.rgba 0 0 0 0.5)]]
            []
  in [cover, box]

{-| Check for any modals that both GMs and Players may need to render. -}
checkModal : M.Model -> T.App -> Maybe (Html M.Msg)
checkModal model app = if model.error /= "" then Just (errorBox model) else Nothing

errorBox : M.Model -> Html M.Msg
errorBox model =
  if model.error == "" then text "" else
  vbox [text "Error", button [onClick M.ClearError] [text "OK"], pre [] [text model.error]]

creatureIcon : T.App -> T.Creature -> Html M.Msg
creatureIcon app creature =
  let creatureColor =
        case Dict.get creature.class app.current_game.classes of
          Just class -> class.color
          Nothing -> "red"
  in
    if creature.portrait_url /= ""
    then
      img (stdStyle ++ [src creature.portrait_url
          , s [S.width (S.px 50), S.height (S.px 50), S.borderRadius (S.px 10), plainBorder]])
          []
    else
      sdiv [s [ S.width (S.px 50), S.height (S.px 50), S.borderRadius (S.px 10), plainBorder]
          , style [("background-color", creatureColor)] ]
          [text creature.name]

{-| An action bar that renders at the bottom of the screen for the current combat creature. -}
mainActionBar : T.App -> T.Combat -> Html M.Msg
mainActionBar app combat =
  let creature = T.combatCreature app.current_game combat
  in hbox [creatureIcon app creature, combatActionBar app.current_game combat creature]

tabbedView : String -> String -> M.Model -> List (String, () -> Html M.Msg, Maybe M.Msg) -> Html M.Msg
tabbedView category defaultView model things =
  let
    header = habox [s [S.justifyContent S.spaceBetween]] (List.map headerButton things)
    buttonText name = if name == selectedView then strong [] [text name] else text name
    headerButton (name, _, msg) =
      button [ s [S.height (S.px 50)], onClick (M.SelectView category name msg)]
             [buttonText name]
    selectedView = Dict.get category model.selectedViews |> Maybe.withDefault defaultView
    renderBody (name, renderer, _) = if name == selectedView then Just renderer else Nothing
    body =
      case List.filterMap renderBody things of
        [f] -> f ()
        _ -> text "Select a view"
  in vabox [s [S.height (S.pct 100)]] [header, body]

{-| All the parts we need to render the UI, allowing either Player or GM-specific view things to
be plugged in. -}
type alias UI =
  { mapView: Html M.Msg
  , mapModeControls: Html M.Msg
  , defaultTab: String
  , sideBar: List (String, () -> Html M.Msg, Maybe M.Msg)
  , modal: Maybe (Html M.Msg)
  , bottomBar: Maybe (Html M.Msg)
  }

{-| Top-level UI for an App. -}
viewGame : M.Model -> T.App -> UI -> Html M.Msg
viewGame model app ui =
  if model.windowSize.width >= 880 then
    div [s <| [S.position S.relative, S.width (S.pct 100), S.height (S.vh 100)]]
        <|
        [ overlay (S.px 0) (S.px 0) [S.height (S.pct 100), S.width (S.pct 100)]
            [ui.mapView]
        , overlayRight (S.px 0) (S.px 0)
            [ S.width (S.px 400)
            , S.property "height" "calc(100vh - 150px)", S.overflowY S.auto]
            [ tabbedView "right-side-bar" ui.defaultTab model ui.sideBar ]
        , mapModeControlsOverlay ui.mapModeControls
        , errorBox model
        , case ui.bottomBar of
            Nothing -> text ""
            Just bar ->
              sdiv
                [s [ S.position S.fixed
                   , S.left (S.pct 50)
                   , S.bottom (S.px 0)
                   , S.transform (S.translate (S.pct -50))
                   , plainBorder
                   , S.backgroundColor (S.rgb 255 255 255)]]
                [ bar ]
        ]
        ++ (ui.modal |> Maybe.map modalOverlay |> Maybe.withDefault [])
  else
    -- Portrait-mode mobile view!
    -- TODO: show the mapModeControls somewhere
    -- TODO: fix panzoom on mobile map
    -- TODO: render the bottomBar
    if model.error /= "" then errorBox model
    else
      case ui.modal of
        Just m -> m
        Nothing ->
          let
            mapView = vabox [s [S.height (S.pct 100)]]
                            [ habox [s [S.justifyContent S.spaceAround]]
                                    [ui.mapModeControls]
                            , ui.mapView]
            scale = toString <| toFloat (model.windowSize.width - 25) / 325.0
            scaleTab : (String, () -> Html M.Msg, Maybe M.Msg) -> (String, () -> Html M.Msg, Maybe M.Msg)
            scaleTab (name, f, m) =
              ( name
              , (\() -> div [s [S.overflowY S.auto, S.overflowX S.hidden, S.height (S.pct 100)]]
                            [ div [style [ ("transform-origin", "top left")
                                        , ("transform", "scale(" ++ scale ++ ")")
                                        ]
                                  ]
                                  [f ()]
                            ]
                )
              , m)
            scaledSideBar = List.map scaleTab ui.sideBar
            tabs = scaledSideBar ++ [("Map", (\() -> mapView), Just M.GridInitializePanZoom)]
          in
            vabox [s [S.height (S.pct 100), S.width (S.pct 100)]]
                  [ tabbedView "right-side-bar" ui.defaultTab model tabs
                  , habox [s [S.justifyContent S.spaceAround]]
                          [ui.bottomBar |> Maybe.withDefault (text "")]]
                

targetMap : M.Model -> T.App -> T.Scene -> T.Map -> List M.MapCreature
         -> Maybe (Html M.Msg, Html M.Msg)
targetMap model app scene map vCreatures =
  let
    standardInfoBox = vbox [ text "Select Targets!"
                           , button [onClick M.CancelAbility] [text "Cancel Ability"]]
    makeMap {creature, ability, chosenPoint} targets =
      let activateAbility =
            if T.isCreatureInCombat app.current_game creature
            then M.CombatAct
            else M.ActCreature scene.id creature
      in case targets of
        T.PTCreatureIDs cids ->
          let
            enableTargeting mapc =
              if List.member mapc.creature.id cids
              then
                let fullMsg = .id >> T.TargetedCreature >> activateAbility ability
                in {mapc | clickable = Just fullMsg, highlight = Just M.Targetable}
              else {mapc | clickable = Nothing}
            targetable = List.map enableTargeting vCreatures
          in ( Grid.terrainMap model map targetable , standardInfoBox )
        T.PTPoints pts ->
          -- Either we're still selecting a point, or we have selected a point and we want to
          -- confirm with the user whether it's okay to use the ability on the affected creatures.
          case chosenPoint of
            Nothing ->
              ( Grid.tileTargetingMap model M.SelectVolumeTarget map pts vCreatures , standardInfoBox )
            Just (targetedPoint, cids) ->
              let
                highlightAffected mapc =
                  if List.member mapc.creature.id cids then
                    {mapc | highlight = Just M.Affected}
                  else mapc
                highlightedTargets = List.map highlightAffected vCreatures
                confirmAbilityBox =
                  vbox [ text "Use ability? The highlighted creatures will be affected."
                       , button [onClick <| activateAbility ability (T.TargetedPoint targetedPoint)]
                                [text "Do it live!"]
                       , button [onClick M.CancelAbility] [text "Never mind!"]]
              in ( Grid.tileTargetingMap model M.SelectVolumeTarget map pts highlightedTargets
                 , confirmAbilityBox )
  in model.selectingAbility |> Maybe.andThen (\sa -> Maybe.map (makeMap sa) sa.potentialTargets)

popUpMenu : M.Model -> String -> String -> Html M.Msg -> Html M.Msg -> List (Html M.Msg, M.Msg) -> Html M.Msg
popUpMenu model = popUpMenu_ M.ToggleCollapsed model.collapsed

popUpMenu_ : (String -> M.Msg) -> Dict.Dict String Bool -> String -> String -> Html M.Msg -> Html M.Msg -> List (Html M.Msg, M.Msg) -> Html M.Msg
popUpMenu_ collapser collapsed prefix key clicker clickerClicked items =
  let realKey = prefix ++ "-" ++ key
      isClicked = Dict.get realKey collapsed |> Maybe.withDefault False
      renderItem (html, msg) = habox [clickable, onClick (M.Batch [msg, collapser realKey])] [html]
      openMenu =
        vabox
          [s [ S.boxShadow5 (S.px 5) (S.px 5) (S.px 2) (S.px -2) (S.rgb 128 128 128)
             , plainBorder, S.backgroundColor (S.rgb 255 255 255)
             , S.width (S.px 150)
             , S.position S.absolute
             , S.top (S.em 2)
             , S.zIndex (S.int 1)
             ]
          ]
          (List.map renderItem items)
      maybeMenu = if isClicked then openMenu else text ""
      header = div [clickable, onClick (collapser realKey)]
                   [if isClicked then clickerClicked else clicker]
  in div [s [S.position S.relative]] [header, maybeMenu]
