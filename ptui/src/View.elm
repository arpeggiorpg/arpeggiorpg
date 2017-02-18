module View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set

import Model as M
import Update as U
import Grid
import Elements exposing (..)

import Css as S


s = S.asPairs >> Html.Attributes.style

refreshButton : Html U.Msg
refreshButton = button [ onClick U.MorePlease ] [ text "Refresh From Server" ]

gmView : M.Model -> Html U.Msg
gmView model = vbox
  [ h2 [] [ text "P&T" ]
  , refreshButton
  , case model.app of
      Just app -> gmViewGame model app
      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

gmViewGame : M.Model -> M.App -> Html U.Msg
gmViewGame model app =
  let game = app.current_game in
  case (game.current_combat, model.moving) of
    -- Movement takes over the whole UI:
    (Nothing, Just mvmtReq) ->
      case mvmtReq.ooc_creature of
        Just creature -> Grid.movementMap (U.MoveCreature creature.id) mvmtReq model.currentMap creature (Dict.values game.creatures)
        Nothing -> -- There's a combat movement request but no combat! It'd be nice if this were impossible to represent
                   fullUI model app game
    (Just combat, Just mvmtReq) ->
      let (creature, moveMessage, visibleCreatures) =
          case mvmtReq.ooc_creature of
            Just creature -> (creature, U.MoveCreature creature.id, (Dict.values game.creatures))
            Nothing -> (M.combatCreature combat, U.CombatMove, combat.creatures.data)
      in Grid.movementMap moveMessage mvmtReq model.currentMap creature visibleCreatures
    _ -> fullUI model app game

fullUI : M.Model -> M.App -> M.Game -> Html U.Msg
fullUI model app game =
  if model.editingMap
  then Grid.editMap model.currentMap (visibleCreatures game)
  else
  hbox
    [ vbox <| [ h3 [] [text "Creatures"]
            , inactiveList model game
           ] ++ (case model.selectedAbility of
            Just (cid, abid) -> if M.isCreatureOOC game cid
                                then [targetSelector model game (U.ActCreature cid) abid]
                                else []
            Nothing -> []
           ) ++ [ extPlayerList (\pid -> [button [onClick (U.GiveCreaturesToPlayer pid)] [text "Grant Selected Creatures"]]) app.players
          , history app
          ]
    , vbox [hbox [editMapButton, mapSelector game], Grid.terrainMap True model.currentMap (visibleCreatures game)]
    , case game.current_combat of
        Just combat -> combatArea model game combat
        Nothing -> startCombatButton
    ]


extPlayerList : (M.PlayerID -> List (Html U.Msg)) -> Dict.Dict M.PlayerID (Set.Set M.CreatureID) -> Html U.Msg
extPlayerList ext players =
  let playerEntry (pid, cids) =
        hbox ([strong [] [text pid], hbox (List.map text (Set.toList cids))] ++ ext pid)
  in vbox <| [h3 [] [text "Players"]] ++ (List.map playerEntry (Dict.toList players))

playerList : Dict.Dict M.PlayerID (Set.Set M.CreatureID) -> Html U.Msg
playerList = extPlayerList (always [])

editMapButton : Html U.Msg
editMapButton = button [onClick U.StartEditingMap] [text "Edit this map"]

visibleCreatures : M.Game -> List M.Creature
visibleCreatures game =
  case game.current_combat of
    Just combat -> combat.creatures.data
    Nothing -> Dict.values game.creatures

playerView : M.Model -> Html U.Msg
playerView model = vbox
  [ h2 [] [ text "P&T" ]
  , refreshButton
  , case model.app of
      Just app ->
        let vGame = case model.playerID of
              Just playerID ->
                if M.playerIsRegistered app playerID
                then playerViewGame model app (M.getPlayerCreatures app playerID)
                else registerForm model
              Nothing -> registerForm model
        in vbox [vGame, playerList app.players]
      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

registerForm : M.Model -> Html U.Msg
registerForm model =
  hbox [ input [type_ "text", placeholder "player ID", onInput U.SetPlayerID ] []
       , button [onClick U.RegisterPlayer] [text "Register Player"]]

playerViewGame : M.Model -> M.App -> List M.Creature -> Html U.Msg
playerViewGame model app creatures = 
  let game = app.current_game in
  hbox
  [ case model.moving of
      Just movementRequest ->
        let {max_distance, movement_options, ooc_creature} = movementRequest in
        case ooc_creature of
          Just oocMovingCreature ->
            if List.member oocMovingCreature creatures
            then
              -- one of my characters is moving; render the movement map
              Grid.movementMap (U.MoveCreature oocMovingCreature.id) movementRequest
                               model.currentMap oocMovingCreature (visibleCreatures game)
            else
              -- someone else is moving (TODO: render a non-interactive movement map to show what they're doing)
              playerGrid model game creatures
          Nothing -> -- we're moving in-combat.
            let currentCreature = Maybe.map M.combatCreature game.current_combat
            in case currentCreature of
                Just creature -> Grid.movementMap U.CombatMove movementRequest model.currentMap creature (visibleCreatures game)
                Nothing -> playerGrid model game creatures
      Nothing -> playerGrid model game creatures
  ,
    case game.current_combat of
      Just combat ->
        let currentCreature = M.combatCreature combat
            bar = if List.member currentCreature creatures
                  then hbox [ strong [] [text currentCreature.id]
                            , actionBar game combat currentCreature]
                  else hbox [text "Current creature:", text currentCreature.id]
            selector = case model.selectedAbility of
                          Just (cid, abid) -> 
                                if M.isCreatureInCombat game cid
                                then [targetSelector model game U.CombatAct abid]
                                else []
                          Nothing -> []
            initiativeList = combatantList game combat
        in vbox <| [bar] ++ selector ++ [initiativeList]
      Nothing -> text "No Combat"
  ]
  -- TODO: a read-only "creatures nearby" list without details

-- render the grid and some OOC movement buttons for the given creatures
playerGrid : M.Model -> M.Game -> List M.Creature -> Html U.Msg
playerGrid model game myCreatures =
  let bar creature =
        if M.isCreatureInCombat game creature.id
        then Nothing
        else Just <| hbox <| [text creature.id, creatureStats creature, moveOOCButton creature] ++ (oocActionBar game creature)
      bars = List.filterMap bar myCreatures
      targetSel =
        case model.selectedAbility of
          Just (cid, abid) ->
            case M.listFind (\c -> c.id == cid) myCreatures of
              Just _ -> [targetSelector model game (U.ActCreature cid) abid]
              Nothing -> []
          Nothing -> []
  in
    vbox <| bars ++ targetSel ++ [Grid.terrainMap False model.currentMap (visibleCreatures game)]
  
mapSelector : M.Game -> Html U.Msg
mapSelector game = vbox <|
  let mapSelectorItem name = button [onClick (U.SelectMap name)] [text name]
  in (List.map mapSelectorItem (Dict.keys game.maps))

inactiveList : M.Model -> M.Game -> Html U.Msg
inactiveList model game = div []
  [ div [] (List.map (inactiveEntry game model.selectedCreatures) (Dict.values game.creatures))
  , createCreatureForm model game
  ]

createCreatureForm : M.Model -> M.Game -> Html U.Msg
createCreatureForm model game = div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , select [onInput U.PendingCreatureClass ]
             (List.map (\className -> option [value className] [text className])
                       (Dict.keys game.classes))
    , createCreatureButton model
    ]

createCreatureButton : M.Model -> Html U.Msg
createCreatureButton model =
  case (model.pendingCreatureId, model.pendingCreatureName, model.pendingCreatureClass) of
    (Just id, Just name, Just class) ->
      let cc = M.CreatureCreation id name class {x= 0, y= 0, z=0}
      in button [ onClick (U.CreateCreature cc) ] [text "Create Creature!"]
    _ -> button [disabled True] [text "Create Creature!"]

inactiveEntry : M.Game -> Set.Set String -> M.Creature -> Html U.Msg
inactiveEntry game pendingCreatures creature = vbox <|
  [hbox
    [ creatureCard creature
    , case game.current_combat of
        Just _ -> engageButton creature
        Nothing ->
          input [ type_ "checkbox"
          , checked (Set.member creature.id pendingCreatures)
          , onClick (U.ToggleSelectedCreature creature.id)] []
    , deleteCreatureButton creature
    ], hbox (oocActionBar game creature)]

history : M.App -> Html U.Msg
history app = 
  let items =
        case Array.get ((Array.length app.snapshots) - 1) app.snapshots of
          Just (_, items) -> List.reverse items
          Nothing -> []
  in vbox
  ([ h3 [] [text "History"] ] ++ (List.map historyItem items))

historyItem : M.GameLog -> Html U.Msg
historyItem i = case i of
  M.GLSelectMap name ->  hbox [ text "Selected Map", text name]
  M.GLEditMap name _ -> hbox [text "Edited Map", text name]
  M.GLCreateCreature creature -> hbox [text "Created creature", text creature.id]
  M.GLRemoveCreature cid -> hbox [text "Deleted creature", text cid]
  M.GLStartCombat combatants -> hbox [text "Started Combat", text (String.join ", " combatants)]
  M.GLStopCombat -> text "Stopped combat"
  M.GLAddCreatureToCombat cid -> hbox [text cid, text "Added Creature to Combat"]
  M.GLRemoveCreatureFromCombat cid -> text <| "Removed creature from Combat: " ++ cid
  M.GLCreatureLog cid cl -> hbox [text cid, historyCreatureLog cl]
  M.GLCombatLog cl -> historyCombatLog cl

historyCombatLog : M.CombatLog -> Html U.Msg
historyCombatLog cl = case cl of
  M.ComLCreatureLog cid creatureLog -> hbox [text cid, historyCreatureLog creatureLog]
  M.ComLEndTurn cid -> hbox [text cid, text "Ended Turn"]

historyCreatureLog : M.CreatureLog -> Html U.Msg
historyCreatureLog cl = case cl of
  M.CLDamage dmg -> text <| "Took damage: " ++ toString dmg
  M.CLHeal dmg -> text <| "Healed: " ++ toString dmg
  M.CLGenerateEnergy nrg -> text <| "Regenerated energy: " ++ toString nrg
  M.CLReduceEnergy nrg -> text <| "Lost energy: " ++ toString nrg
  M.CLApplyCondition conid duration con -> text <| "Got condition: " ++ toString con
  M.CLRemoveCondition conid -> text <| "Lost condition: " ++ toString conid
  M.CLPathCreature {path, distance} -> text <| "Moved " ++ toString (toFloat distance / 100) ++ " to " ++ maybePos path
  M.CLDecrementConditionRemaining conID -> text <| "Tick condition: " ++ toString conID

maybePos : List M.Point3 -> String
maybePos path =
  case (List.head (List.reverse path)) of
    Just {x, y, z} -> toString x ++ "," ++ toString y
    Nothing -> "nowhere"

combatArea : M.Model -> M.Game -> M.Combat -> Html U.Msg
combatArea model game combat =
  let bar = actionBar game combat (M.combatCreature combat)
      disengageButtons = hbox (List.map disengageButton combat.creatures.data)
      abar = vbox [ bar, combatantList game combat, stopCombatButton, disengageButtons]
  in case model.selectedAbility of
    Just (cid, abid) ->
      if M.isCreatureInCombat game cid
      then targetSelector model game U.CombatAct abid
      else abar
    Nothing -> abar

targetSelector : M.Model -> M.Game -> (M.AbilityID -> M.DecidedTarget -> U.Msg) -> String -> Html U.Msg
targetSelector model game msgConstructor abid =
  let creatures = List.filterMap (M.findCreature game) (M.potentialCreatureTargets model.potentialTargets)
  in hbox <| 
    [ case (Dict.get abid game.abilities) of
        Just ability -> case ability.target of
          M.Melee -> creatureTargetSelector (msgConstructor abid) M.DecidedMelee creatures
          M.Range distance -> creatureTargetSelector (msgConstructor abid) M.DecidedRange creatures
        Nothing -> text "Sorry, that ability was not found. Please reload."
    , button [onClick U.CancelAbility] [text "Cancel ability"]
    ]

creatureTargetSelector : (M.DecidedTarget -> U.Msg) -> (M.CreatureID -> M.DecidedTarget) -> List M.Creature -> Html U.Msg
creatureTargetSelector msgConstructor targetConstructor creatures = vbox <|
  let targetCreatureButton c = button [onClick (msgConstructor (targetConstructor c.id))] [text c.name]
  in List.map targetCreatureButton creatures

stopCombatButton : Html U.Msg
stopCombatButton = button [onClick U.StopCombat] [text "Stop Combat"]
startCombatButton : Html U.Msg
startCombatButton = button [onClick U.StartCombat] [text "Start Combat"]

combatantList : M.Game -> M.Combat -> Html U.Msg
combatantList game combat =
  vbox (List.map (combatantEntry game combat) (List.indexedMap (,) combat.creatures.data))

engageButton : M.Creature -> Html U.Msg
engageButton creature =
  button [onClick (U.AddToCombat creature.id)] [text "Engage"]

combatantEntry : M.Game -> M.Combat -> (Int, M.Creature) -> Html U.Msg
combatantEntry game combat (idx, creature) = hbox <|
  let marker = if combat.creatures.cursor == idx
               then [datext [s [S.width (S.px 25)]] "-->"]
               else [div [s [S.width (S.px 25)]] []]
  in marker ++ [ creatureCard creature ]

oocActionBar : M.Game -> M.Creature -> List (Html U.Msg)
oocActionBar game creature =
  let abilities =
        case (Dict.get creature.class game.classes) of
          Just x -> x.abilities -- TODO: get ability *name*
          Nothing -> []
  in (List.map (actionButton creature) abilities)

actionBar : M.Game -> M.Combat -> M.Creature -> Html U.Msg
actionBar game combat creature =
  hbox (oocActionBar game creature ++ [moveButton combat creature] ++ [doneButton creature])

actionButton : M.Creature -> String -> Html U.Msg
actionButton creature abid =
  button
    [ onClick (U.SelectAbility creature.id abid)
    , disabled (not creature.can_act)]
    [text abid]

creatureCard : M.Creature -> Html U.Msg
creatureCard creature =
  let cellStyles color =
        [s [ plainBorder
           , S.backgroundColor color
           , S.borderRadius (S.px 10)
           , S.padding (S.px 3)]]
  in vabox [s [plainBorder, S.width (S.px 300), S.height (S.px 100), S.borderRadius (S.px 10), S.padding (S.px 3)]]
    [ hbox [strong [] [text creature.name ], classIcon creature]
    , hbox [ div (cellStyles (S.rgb 144 238 144))
              [text <| (toString creature.cur_health) ++ "/" ++ (toString creature.max_health)]
           , div (cellStyles (S.rgb 0 255 255))
              [text <| (toString creature.cur_energy) ++ "/" ++ (toString creature.max_energy)]
           , div (cellStyles (S.rgb 255 255 255))
              [text <| (toString creature.pos.x) ++ ", " ++ (toString creature.pos.y)]
           ]
    , habox [style [("width", "50px")]] (List.map creatureConds (Dict.values creature.conditions))
    ]

classIcon : M.Creature -> Html U.Msg
classIcon creature =
  case creature.class of
    "cleric" -> text "💉"
    "rogue" -> text "🗡️"
    "ranger" -> text "🏹"
    _ -> text "?"


creatureStats : M.Creature -> Html U.Msg
creatureStats creature = 
  habox [style [("width", "400px")]]
    [ div [style [("width", "150px")]] [strong [] [text creature.name]]
    , div [style [("width", "75px")]] [text "HP ", text (toString creature.cur_health)]
    , div [style [("width", "100px")]] [text "NRG ", text (toString creature.cur_energy)]
    , div [style [("width", "75px")]] [text "POS ", text <| (toString creature.pos.x) ++ "/" ++ (toString creature.pos.y)]
    ]

creatureConds : M.AppliedCondition -> Html U.Msg
creatureConds ac = case ac.condition of 
  M.RecurringEffect eff -> text (toString eff)
  M.Dead -> text "💀"
  M.Incapacitated -> text "😞"
  M.AddDamageBuff dmg -> text "😈"

disengageButton : M.Creature -> Html U.Msg
disengageButton creature =
  button [onClick (U.RemoveFromCombat creature.id)] [text ("Disengage " ++ creature.id)]

deleteCreatureButton : M.Creature -> Html U.Msg
deleteCreatureButton creature =
  button [onClick (U.RemoveFromGame creature.id)] [text "Delete"]

moveOOCButton : M.Creature -> Html U.Msg
moveOOCButton creature =
  button [ onClick (U.GetMovementOptions creature)
         , disabled (not creature.can_move)]
         [text "Move"]

doneButton : M.Creature -> Html U.Msg
doneButton creature =
  button [onClick U.TurnDone] [text "Done"]

moveButton : M.Combat -> M.Creature -> Html U.Msg
moveButton combat creature =
  let movement_left = creature.speed - combat.movement_used
  in button [ onClick (U.RequestMove <| M.MovementRequest movement_left combat.movement_options Nothing)
            , disabled (not creature.can_move) ]
            [text (String.join "" ["Move (", toString movement_left, ")"])]
