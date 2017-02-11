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


view : M.Model -> Html U.Msg
view model = vbox
  [ h2 [] [ text "P&T" ]
  , button [ onClick U.MorePlease ] [ text "Refresh From Server" ]
  , case model.app of Just app -> viewGame model app
                      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

viewGame : M.Model -> M.App -> Html U.Msg
viewGame model app =
  let game = app.current_game in
  -- Dispatch to gmView or playerView based on who's using this UI
  case model.controlledCreatures of
    Nothing -> gmView model app game
    Just creatureIds ->
      let f cid =
            maybeOr (Maybe.andThen (\combat -> getCreatureInCombat combat cid) game.current_combat)
                    (getCreatureOOC game cid)
          creatures = List.filterMap f creatureIds
      in playerView model app game creatures

maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2 =
  case (m1, m2) of
    (Just x, _) -> Just x
    (_, Just x) -> Just x
    _ -> Nothing

listFind : (a -> Bool) -> List a -> Maybe a
listFind f l = List.head (List.filter f l)

getCreatureInCombat : M.Combat -> M.CreatureID -> Maybe M.Creature
getCreatureInCombat combat cid = listFind (\c -> c.id == cid) combat.creatures.data

getCreatureOOC : M.Game -> M.CreatureID -> Maybe M.Creature
getCreatureOOC game cid = Dict.get cid game.creatures

gmView : M.Model -> M.App -> M.Game -> Html U.Msg
gmView model app game =
  case (game.current_combat, model.moving) of
    -- Movement takes over the whole UI:
    (Nothing, Just mvmtReq) ->
      case mvmtReq.ooc_creature of
        Just creature -> Grid.movementMap (U.MoveOutOfCombat creature.id) mvmtReq model.currentMap creature (Dict.values game.creatures)
        Nothing -> -- There's a combat movement request but no combat! It'd be nice if this were impossible to represent
                   fullUI model app game
    (Just combat, Just mvmtReq) ->
      let (creature, moveMessage, visibleCreatures) =
          case mvmtReq.ooc_creature of
            Just creature -> (creature, U.MoveOutOfCombat creature.id, (Dict.values game.creatures))
            Nothing -> (M.combatCreature combat, U.Move, combat.creatures.data)
      in Grid.movementMap moveMessage mvmtReq model.currentMap creature visibleCreatures
    _ -> fullUI model app game

fullUI : M.Model -> M.App -> M.Game -> Html U.Msg
fullUI model app game =
  if model.editingMap
     then Grid.editMap model.currentMap (visibleCreatures game)
     else 
      hbox
        [ vbox [ h3 [] [text "Creatures"]
               , inactiveList model game.current_combat model.selectedCreatures game.creatures
               , history app
               ]
        , vbox [editMapButton, Grid.terrainMap model.currentMap (visibleCreatures game)]
        , case game.current_combat of
            Just combat -> combatArea model game combat
            Nothing -> startCombatButton
        , mapSelector game
        ]

editMapButton : Html U.Msg
editMapButton = button [onClick U.StartEditingMap] [text "Edit this map"]

visibleCreatures : M.Game -> List M.Creature
visibleCreatures game =
  case game.current_combat of
    Just combat -> combat.creatures.data
    Nothing -> Dict.values game.creatures

playerView : M.Model -> M.App -> M.Game -> List M.Creature -> Html U.Msg
playerView model app game creatures = hbox
  [ case model.moving of
      Just movementRequest ->
        let {max_distance, movement_options, ooc_creature} = movementRequest in
        case ooc_creature of
          Just oocMovingCreature ->
            if List.member oocMovingCreature creatures
            then
              -- one of my characters is moving; render the movement map
              Grid.movementMap (U.MoveOutOfCombat oocMovingCreature.id) movementRequest
                               model.currentMap oocMovingCreature (visibleCreatures game)
            else
              -- someone else is moving (TODO: render a non-interactive movement map to show what they're doing)
              playerGrid model game creatures
          Nothing -> -- we're moving in-combat.
            let currentCreature = Maybe.map M.combatCreature game.current_combat
            in case currentCreature of
                Just creature -> Grid.movementMap U.Move movementRequest model.currentMap creature (visibleCreatures game)
                Nothing -> playerGrid model game creatures
      Nothing -> playerGrid model game creatures
  ,
    case game.current_combat of
      Just combat ->
        let currentCreature = M.combatCreature combat
            bar = if List.member currentCreature creatures
                  then hbox [strong [] [text currentCreature.id]
                            , actionBar game combat currentCreature]
                  else hbox [text "Current creature:", text currentCreature.id]
            selector = case model.selectedAbility of
                          Just abid -> targetSelector model game combat abid
                          Nothing -> div [] []
            initiativeList = combatantList game combat
        in vbox [bar, selector, initiativeList]
      Nothing -> text "No Combat"
  ]
  -- TODO: a read-only "creatures nearby" list without details

-- render the grid and some OOC movement buttons for the given creatures
playerGrid : M.Model -> M.Game -> List M.Creature -> Html U.Msg
playerGrid model game creatures =
  let buttonForCreature creature =
        if playerInCombat game creature
        then Nothing
        else Just <| hbox [text creature.id, moveOOCButton creature]
      movementButtons = List.filterMap buttonForCreature creatures
  in
    vbox <| movementButtons ++ [Grid.terrainMap model.currentMap (visibleCreatures game)]
  

playerInCombat : M.Game -> M.Creature -> Bool
playerInCombat game cid =
  case game.current_combat of
    Nothing -> False
    Just combat -> List.member cid combat.creatures.data

mapSelector : M.Game -> Html U.Msg
mapSelector game = vbox <|
  let mapSelectorItem name = button [onClick (U.SelectMap name)] [text name]
  in (List.map mapSelectorItem (Dict.keys game.maps))

inactiveList : M.Model -> Maybe M.Combat -> Set.Set String -> Dict.Dict String M.Creature -> Html U.Msg
inactiveList model mCombat pendingCreatures creatures = div []
  [ div [] (List.map (inactiveEntry mCombat pendingCreatures) (Dict.values creatures))
  , createCreatureForm model
  , controlCreaturesButton
  ]

createCreatureForm : M.Model -> Html U.Msg
createCreatureForm model = div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , input [type_ "text", placeholder "class (rogue/ranger/cleric)", onInput U.PendingCreatureClass ] []
    , createCreatureButton model
    ]

controlCreaturesButton : Html U.Msg
controlCreaturesButton = button [ onClick U.TakeOverCreatures ] [text "Control Selected Creatures"]

createCreatureButton : M.Model -> Html U.Msg
createCreatureButton model =
  case (model.pendingCreatureId, model.pendingCreatureName, model.pendingCreatureClass) of
    (Just id, Just name, Just class) ->
      let cc = M.CreatureCreation id name class {x= 0, y= 0, z=0}
      in button [ onClick (U.CreateCreature cc) ] [text "Create Creature!"]
    _ -> button [disabled True] [text "Create Creature!"]

inactiveEntry : Maybe M.Combat -> Set.Set String -> M.Creature -> Html U.Msg
inactiveEntry mCombat pendingCreatures creature = hbox
  [ creatureStats creature
  , case mCombat of Just _ -> engageButton creature
                    Nothing ->
                      input [ type_ "checkbox"
                      , checked (Set.member creature.id pendingCreatures)
                      , onClick (U.ToggleSelectedCreature creature.id)] []
  , deleteCreatureButton creature
  , moveOOCButton creature]

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

maybePos : List M.Point3 -> String
maybePos path =
  case (List.head (List.reverse path)) of
    Just {x, y, z} -> toString x ++ "," ++ toString y
    Nothing -> "nowhere"

combatArea : M.Model -> M.Game -> M.Combat -> Html U.Msg
combatArea model game combat = case model.selectedAbility of
  Just abid -> targetSelector model game combat abid
  Nothing ->
    let bar = actionBar game combat (M.combatCreature combat)
        disengageButtons = hbox (List.map disengageButton combat.creatures.data)
    in vbox [ bar, combatantList game combat, stopCombatButton, disengageButtons]

targetSelector : M.Model -> M.Game -> M.Combat -> String -> Html U.Msg
targetSelector model game combat abid = case (Dict.get abid game.abilities) of
  Just ability -> case ability.target of
    M.Melee -> creatureTargetSelector abid M.DecidedMelee combat
    M.Range distance -> creatureTargetSelector abid M.DecidedRange combat
  Nothing -> text "Sorry, that ability was not found. Please reload."

creatureTargetSelector : M.AbilityID -> (M.CreatureID -> M.DecidedTarget) -> M.Combat -> Html U.Msg
creatureTargetSelector abid con combat = vbox <|
  let targetCreatureButton c = button [onClick (U.Act abid (con c.id))] [text c.name]
  in (List.map targetCreatureButton combat.creatures.data)

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
  let marker = if combat.creatures.cursor == idx then [text "-->"] else []
  in marker ++ [ creatureStats creature ]

actionBar : M.Game -> M.Combat -> M.Creature -> Html U.Msg
actionBar game combat creature =
  let abilities =
        case (Dict.get creature.class game.classes) of
          Just x -> x.abilities
          Nothing -> []
  in hbox (  (doneButton creature)
          :: (moveButton combat creature)
          :: (List.map actionButton abilities))

actionButton : String -> Html U.Msg
actionButton abid = button [onClick (U.SelectAbility abid)] [text abid]

creatureStats : M.Creature -> Html U.Msg
creatureStats creature = 
  hbox [ strong [] [text  creature.name]
       , text "HP: ", text (toString creature.cur_health)
       , text "Energy: ", text (toString creature.cur_energy)
       , text "Pos: ", text <| (toString creature.pos.x) ++ "/" ++ (toString creature.pos.y)
       ]

disengageButton : M.Creature -> Html U.Msg
disengageButton creature =
  button [onClick (U.RemoveFromCombat creature.id)] [text ("Disengage " ++ creature.id)]

deleteCreatureButton : M.Creature -> Html U.Msg
deleteCreatureButton creature =
  button [onClick (U.RemoveFromGame creature.id)] [text "Delete"]

moveOOCButton : M.Creature -> Html U.Msg
moveOOCButton creature =
  button [onClick (U.GetMovementOptions creature)]
         [text "Move"]

doneButton : M.Creature -> Html U.Msg
doneButton creature =
  button [onClick U.TurnDone] [text "Done"]

moveButton : M.Combat -> M.Creature -> Html U.Msg
moveButton combat creature =
  let movement_left = creature.speed - combat.movement_used
  in button [onClick (U.RequestMove <| M.MovementRequest movement_left combat.movement_options Nothing)]
            [text (String.join "" ["Move (", toString movement_left, ")"])]
