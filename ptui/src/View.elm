module View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set

import Model as M
import Types as T
import Grid
import Update as U
import Elements exposing (..)
import GMView

-- TODO: delete these once we refactor player code out
import CommonView exposing (..)

import Css as S

s = Elements.s -- to disambiguate `s`, which Html also exports
button = Elements.button

gmView : M.Model -> Html M.Msg
gmView model = vbox
  [ case model.app of
      Just app -> GMView.viewGame model app
      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

gmViewGame_ : M.Model -> T.App -> Html M.Msg
gmViewGame_ model app =
  let game = app.current_game
      center el = div [s [S.displayFlex, S.justifyContent S.spaceAround]] [el]
      movementGrid msg mvmtReq creature =
        let vCreatures = visibleCreatures model game
        in vbox
          [ hbox [ text "Allow movement anywhere: ", input [type_ "checkbox", checked model.moveAnywhere, onClick M.ToggleMoveAnywhere] []]
          , Grid.movementMap model msg mvmtReq model.moveAnywhere model.currentMap creature vCreatures
          ]
  in
  case (game.current_combat, model.moving) of
    (Nothing, Just mvmtReq) ->
      case mvmtReq.ooc_creature of
        Just creature -> center <| movementGrid (M.PathCreature creature.id) mvmtReq creature
        Nothing -> -- There's a combat movement request but no combat! It'd be nice if this were impossible to represent
                   fullUI model app game
    (Just combat, Just mvmtReq) ->
      let (creature, moveMessage) =
          case mvmtReq.ooc_creature of
            Just creature -> (creature, M.PathCreature creature.id)
            Nothing -> (T.combatCreature combat, M.PathCurrentCombatCreature)
      in center <| movementGrid moveMessage mvmtReq creature
    _ -> fullUI model app game

fullUI : M.Model -> T.App -> T.Game -> Html M.Msg
fullUI model app game =
  if model.editingMap
  then Grid.editMap model model.currentMap (visibleCreatures model game)
  else
    let combatHTML = div [] []
        targetSelectorHTML =
          case model.selectedAbility of
            Just (cid, abid) -> if T.isCreatureOOC game cid
                                then [targetSelector model game (M.ActCreature cid) abid]
                                else []
            Nothing -> []
        currentCombatCreature = Maybe.map (\com -> (T.combatCreature com).id) game.current_combat
        modifyMapCreature mapc =
          let highlight = (Just mapc.creature.id) == currentCombatCreature
          in { mapc | highlight = highlight
                    , movable = Just M.GetMovementOptions}
        vCreatures = List.map modifyMapCreature (visibleCreatures model game)
        mapHTML =
          vbox [ hbox [editMapButton, mapSelector game, oocToggler model]
               , Grid.terrainMap model (movementGhost model) model.currentMap vCreatures
               ]
        sideBarHTML =
          vabox [s [S.width (S.px 500)]] <|
            [ combatHTML
            , h3 [] [text "Creatures"]
            , inactiveList model game
            ] ++ targetSelectorHTML ++ [playerControlList app, history app]
    in
      habox [s [S.justifyContent S.spaceAround]] [mapHTML, sideBarHTML]

movementGhost model =
  case model.showingMovement of
    M.ShowingMovement soFar rest -> List.head (List.reverse soFar)
    _ -> Nothing

playerControlList app =
  let gotCreatures pid cids = U.message (M.SendCommand (T.GiveCreaturesToPlayer pid cids))
      selectCreatures pid = M.SelectCreatures (gotCreatures pid) ("Grant Creatures to " ++ pid)
      playerButton pid = [button [onClick (selectCreatures pid)] [text "Grant Creatures"]]
  in extPlayerList playerButton app.players

extPlayerList : (T.PlayerID -> List (Html M.Msg)) -> Dict.Dict T.PlayerID (Set.Set T.CreatureID) -> Html M.Msg
extPlayerList ext players =
  let playerEntry (pid, cids) =
        hbox ([strong [] [text pid], hbox (List.map text (Set.toList cids))] ++ ext pid)
  in vbox <| [h3 [] [text "Players"]] ++ (List.map playerEntry (Dict.toList players))

playerList : Dict.Dict T.PlayerID (Set.Set T.CreatureID) -> Html M.Msg
playerList = extPlayerList (always [])

oocToggler : M.Model -> Html M.Msg
oocToggler model =
  hbox [text "Show Out-of-Combat creatures: "
       , input [type_ "checkbox", checked model.showOOC, onClick M.ToggleShowOOC] []
       ]

editMapButton : Html M.Msg
editMapButton = button [onClick M.StartEditingMap] [text "Edit this map"]

playerView : M.Model -> Html M.Msg
playerView model = vbox
  [ case model.app of
      Just app ->
        let vGame = case model.playerID of
              Just playerID ->
                if T.playerIsRegistered app playerID
                then playerViewGame model app (T.getPlayerCreatures app playerID)
                else registerForm model
              Nothing -> registerForm model
        in vbox [vGame, playerList app.players]
      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

registerForm : M.Model -> Html M.Msg
registerForm model =
  hbox [ input [type_ "text", placeholder "player ID", onInput M.SetPlayerID ] []
       , button [onClick M.RegisterPlayer] [text "Register Player"]]

playerViewGame : M.Model -> T.App -> List T.Creature -> Html M.Msg
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
              Grid.movementMap model (M.PathCreature oocMovingCreature.id) movementRequest
                               False model.currentMap oocMovingCreature (visibleCreatures model game)
            else
              -- someone else is moving (TODO: render a non-interactive movement map to show what they're doing)
              playerGrid model game creatures
          Nothing -> -- we're moving in-combat.
            case Maybe.map T.combatCreature game.current_combat of
                Just creature ->
                  let highlightMover mapc =
                        if creature.id == mapc.creature.id
                        then {mapc | highlight = True}
                        else mapc
                      vCreatures = List.map highlightMover (visibleCreatures model game)
                  in Grid.movementMap model M.PathCurrentCombatCreature movementRequest
                                   False model.currentMap creature vCreatures
                Nothing -> playerGrid model game creatures
      Nothing -> playerGrid model game creatures
  ]
  -- TODO: a read-only "creatures nearby" list without details

playerCombatArea : M.Model -> T.Game -> T.Combat -> List T.Creature -> Html M.Msg
playerCombatArea model game combat creatures =
  let currentCreature = T.combatCreature combat
      bar = if List.member currentCreature creatures
            then habox [s [S.flexWrap S.wrap]]
                       [ div [s [S.width (S.px 100)]] [strong [] [text currentCreature.name]]
                       , combatActionBar game combat currentCreature
                       ]
            else hbox [text "Current creature:", text currentCreature.id]
      selector = case model.selectedAbility of
                    Just (cid, abid) -> 
                          if T.isCreatureInCombat game cid
                          then [targetSelector model game M.CombatAct abid]
                          else []
                    Nothing -> []
      initiativeList = combatantList False model game combat
  in vbox <| [bar] ++ selector ++ [initiativeList]


-- render the grid and some OOC movement buttons for the given creatures
playerGrid : M.Model -> T.Game -> List T.Creature -> Html M.Msg
playerGrid model game myCreatures =
  let moveOOCButton creature =
        button [ onClick (M.GetMovementOptions creature)
               , disabled (not creature.can_move)]
               [text "Move"]
      bar creature =
        if T.isCreatureInCombat game creature.id
        then Nothing
        else Just <| hbox <| [creatureCard [] model creature, moveOOCButton creature] ++ (oocActionBar game creature)
      oocBars = List.filterMap bar myCreatures
      ooc = if (List.length oocBars) > 0
            then [vbox <| [h4 [] [text "Out Of Combat"]] ++ oocBars]
            else []
      targetSel =
        case model.selectedAbility of
          Just (cid, abid) ->
            case T.listFind (\c -> c.id == cid) myCreatures of
              Just _ -> [targetSelector model game (M.ActCreature cid) abid]
              Nothing -> []
          Nothing -> []
      comUI =
        case game.current_combat of
          Just combat -> [h4 [] [text "Combat"], playerCombatArea model game combat myCreatures]
          Nothing -> [text "No Combat"]
      movable mapc =
        -- Don't allow click-to-move when ANY creatures are in combat.
        -- TODO: maybe allow click-to-move for out-of-combat creatures
        case game.current_combat of
          Just com ->
            if List.member (T.combatCreature com) myCreatures && mapc.creature == T.combatCreature com
            then Just (\_ -> M.GetCombatMovementOptions)
            else Nothing
          Nothing -> Just M.GetMovementOptions
      currentCreature = Maybe.map (\com -> (T.combatCreature com).id) game.current_combat
      modifyMapCreature mapc =
        let highlight = (Just mapc.creature.id) == currentCreature
        in { mapc | movable = (movable mapc), highlight = highlight}
      vCreatures = List.map modifyMapCreature (visibleCreatures model game)
  in
    hbox <| [Grid.terrainMap model (movementGhost model) model.currentMap vCreatures
            , vabox [s [S.width (S.px 350)]] (comUI ++ ooc ++ targetSel)]

mapSelector : T.Game -> Html M.Msg
mapSelector game = vbox <|
  let mapSelectorItem name = button [onClick (M.SendCommand (T.SelectMap name))] [text name]
  in (List.map mapSelectorItem (Dict.keys game.maps))

inactiveList : M.Model -> T.Game -> Html M.Msg
inactiveList model game = div []
  [ div [] (List.map (inactiveEntry model game) (Dict.values game.creatures))
  , createCreatureForm model game
  ]

createCreatureForm : M.Model -> T.Game -> Html M.Msg
createCreatureForm model game = div []
    [ input [type_ "text", placeholder "id", onInput M.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput M.PendingCreatureName ] []
    , select [onInput M.PendingCreatureClass]
             <| [option [value ""] [text "Select a Class"]]
                ++ (List.map (\className -> option [value className] [text className])
                             (Dict.keys game.classes))
    , createCreatureButton model
    ]

createCreatureButton : M.Model -> Html M.Msg
createCreatureButton model =
  case (model.pendingCreatureId, model.pendingCreatureName, model.pendingCreatureClass) of
    (Just id, Just name, Just class) ->
      let cc = T.CreatureCreation id name class {x= 0, y= 0, z=0} ""
      in button [onClick (M.SendCommand (T.CreateCreature cc))] [text "Create Creature!"]
    _ -> button [disabled True] [text "Create Creature!"]

inactiveEntry : M.Model -> T.Game -> T.Creature -> Html M.Msg
inactiveEntry model game creature = vbox <|
  [hbox <|
    [ creatureCard [noteBox model creature] model creature
    ] ++ case game.current_combat of
        Just _ -> [engageButton creature]
        Nothing -> []
    ++ [
      deleteCreatureButton creature
    ], hbox (oocActionBar game creature)]

history : T.App -> Html M.Msg
history app = 
  let snapIdx = (Array.length app.snapshots) - 1
      items =
        case Array.get snapIdx app.snapshots of
          Just (_, items) -> items
          Nothing -> []
  in vbox
  ([ h3 [] [text "History"] ] ++ List.reverse (List.indexedMap (historyItem snapIdx) items))

historyItem : Int -> Int -> T.GameLog -> Html M.Msg
historyItem snapIdx logIdx log =
  let logItem = case log of
    T.GLSelectMap name ->  hbox [ text "Selected Map", text name]
    T.GLEditMap name _ -> hbox [text "Edited Map", text name]
    T.GLCreateCreature creature -> hbox [text "Created creature", text creature.id]
    T.GLRemoveCreature cid -> hbox [text "Deleted creature", text cid]
    T.GLStartCombat combatants -> hbox [text "Started Combat", text (String.join ", " combatants)]
    T.GLStopCombat -> text "Stopped combat"
    T.GLAddCreatureToCombat cid -> hbox [text cid, text "Added Creature to Combat"]
    T.GLRemoveCreatureFromCombat cid -> text <| "Removed creature from Combat: " ++ cid
    T.GLCreatureLog cid cl -> hbox [text cid, historyCreatureLog cl]
    T.GLCombatLog cl -> historyCombatLog cl
    T.GLRollback si li -> hbox [text "Rolled back. Snapshot: ", text (toString si), text " Log: ", text (toString li)]
  in hbox [logItem, button [onClick (M.SendCommand (T.Rollback snapIdx logIdx))] [text "Rollback BEFORE here"]]

historyCombatLog : T.CombatLog -> Html M.Msg
historyCombatLog cl = case cl of
  T.ComLCreatureLog cid creatureLog -> hbox [text cid, historyCreatureLog creatureLog]
  T.ComLEndTurn cid -> hbox [text cid, text "Ended Turn"]
  T.ComLPathCurrentCreature pts -> hbox [text <| "Moved to " ++ maybePos pts]
  T.ComLChangeCreatureInitiative cid newPos -> hbox [text cid, text "Changed initiative to", text <| toString newPos]

maybePos : List T.Point3 -> String
maybePos path =
  case List.head (List.reverse path) of
    Just {x, y, z} -> toString x ++ "," ++ toString y
    Nothing -> "nowhere"

historyCreatureLog : T.CreatureLog -> Html M.Msg
historyCreatureLog cl = case cl of
  T.CLDamage dmg dice -> hbox [text <| "Took damage", text <| toString dmg, text <| " Dice: ", renderDice dice]
  T.CLHeal dmg dice -> hbox [text <| "Healed: " ++ toString dmg, text <| "Dice: ", renderDice dice]
  T.CLGenerateEnergy nrg -> text <| "Regenerated energy: " ++ toString nrg
  T.CLReduceEnergy nrg -> text <| "Lost energy: " ++ toString nrg
  T.CLApplyCondition conid duration con -> text <| "Got condition: " ++ toString con
  T.CLRemoveCondition conid -> text <| "Lost condition: " ++ toString conid
  T.CLSetPos pos -> text <| "Moved  to " ++ toString pos
  T.CLDecrementConditionRemaining conID -> text <| "Tick condition: " ++ toString conID
  T.CLSetNote note -> hbox [text "Set note to", text note]

renderDice : List Int -> Html M.Msg
renderDice dice = text <| String.join ", " (List.map toString dice)

combatantList : Bool -> M.Model -> T.Game -> T.Combat -> Html M.Msg
combatantList isGmView model game combat =
  vbox (List.map (combatantEntry isGmView model game combat) (List.indexedMap (,) combat.creatures.data))

engageButton : T.Creature -> Html M.Msg
engageButton creature =
  button [onClick (M.SendCommand (T.AddCreatureToCombat creature.id))] [text "Engage"]

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

noteBox : M.Model -> T.Creature -> Html M.Msg
noteBox model creature = 
  let note = Maybe.withDefault creature.note (Dict.get creature.id model.creatureNotes)
      inp = input [type_ "text", value note, onInput (M.SetCreatureNote creature.id)] []
      saveButton =
        if creature.note /= note
        then [button [onClick (M.SendCommand (T.SetCreatureNote creature.id note))] [text "Save Note"]]
        else []
  in hbox <| [inp] ++ saveButton

disengageButton : T.Creature -> Html M.Msg
disengageButton creature =
  button [onClick (M.SendCommand (T.RemoveCreatureFromCombat creature.id))] [text ("Disengage " ++ creature.id)]

deleteCreatureButton : T.Creature -> Html M.Msg
deleteCreatureButton creature =
  button [onClick (M.SendCommand (T.RemoveCreature creature.id))] [text "Delete"]

