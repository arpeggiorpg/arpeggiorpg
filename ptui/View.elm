module View exposing (..)

import Debug exposing (log)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE
import Set

import Model as M
import Update as U
import MouseEvent exposing (onMouseClick)


view : M.Model -> Html U.Msg
view model = vbox
  [ h2 [] [ text "P&T" ]
  , button [ onClick U.MorePlease ] [ text "Refresh From Server" ]
  , case model.app of Just app -> viewGame model app.current_game
                      Nothing -> text "No app yet. Maybe reload."
  , hbox [text "Last error:", pre [] [text model.error]]
  ]

viewGame : M.Model -> M.Game -> Html U.Msg
viewGame model game = hbox 
  [ vbox [ h3 [] [text "Creatures"]
         , inactiveList game.current_combat model.pendingCombatCreatures game.creatures
         ]
  , case game.current_combat of
      Just combat -> combatGrid model.moving (model.currentMap |> Maybe.andThen (flip Dict.get game.maps)) combat
      Nothing -> text "Enter combat to see a cool combat grid here!"
  , case game.current_combat of
      Just combat -> combatArea model game combat
      Nothing -> startCombatButton
  , mapSelector game
  ]


mapSelector : M.Game -> Html U.Msg
mapSelector game = vbox <|
  let mapSelectorItem name = button [onClick (U.SelectMap name)] [text name]
  in (List.map mapSelectorItem (Dict.keys game.maps))

inactiveList : Maybe M.Combat -> Set.Set String -> Dict.Dict String M.Creature -> Html U.Msg
inactiveList mCombat pendingCreatures creatures = div []
  [ div [] (List.map (inactiveEntry mCombat pendingCreatures) (Dict.values creatures))
  , createCreatureForm
  ]

createCreatureForm : Html U.Msg
createCreatureForm = div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , input [type_ "text", placeholder "class (rogue/ranger/cleric)", onInput U.PendingCreatureAbilitySet ] []
    , button [ onClick U.PostCreateCreature ] [ text "Create Creature!" ]
    ]

inactiveEntry : Maybe M.Combat -> Set.Set String -> M.Creature -> Html U.Msg
inactiveEntry mCombat pendingCreatures creature = hbox
  [ creatureStats creature
  , case mCombat of Just _ -> engageButton creature
                    Nothing ->
                      input [ type_ "checkbox"
                      , checked (Set.member creature.id pendingCreatures)
                      , onClick (U.ToggleSelectedCreature creature.id)] []
  , deleteCreatureButton creature]


-- The Grid

-- Convert Point3 coordinates to on-screen corodinates.
-- Point3 coordinates are in METERS, and Distance calculation is done in CENTIMETERS.
-- These functions make 1 pixel = 1 decimeter
px x = (toString x) ++ "px"
metersToPx m = m * 10
metersToPxPx = px << metersToPx
cmToPx cm = cm // 10
cmToPxPx = px << cmToPx
coord c = 250 + metersToPx c
coordPx = px << coord

combatGrid : Maybe M.MovementRequest -> Maybe M.Map -> M.Combat -> Html U.Msg
combatGrid moving maybeMap combat =
  let creatureEls = (List.map gridCreature combat.creatures.data)
      terrainEls = case maybeMap of
        Just m -> (List.map gridTerrain m)
        Nothing -> []
      movementCirc =
        case moving of
          Just {origin, max_distance} -> [movementCircle origin max_distance]
          Nothing -> []
  in div [style [("border", "2px solid black"), ("position", "relative"), ("width", metersToPxPx 50), ("height", metersToPxPx 50)]]
         (movementCirc ++ terrainEls ++ creatureEls)

clickedMove : M.Point3 -> Int -> MouseEvent.MouseEvent -> U.Msg
clickedMove origin radius me = log (toString me) <|
  let offsetX = (me.clientPos.x - radius) * 10
      offsetY = (me.clientPos.y - radius) * 10
  in (U.Move {x=(origin.x + offsetX) // 100, y=(origin.y + offsetY) // 100, z=0})

centerPositionedBox x y attrs content =
  div [style [ ("position", "absolute")
             , ("left", x)
             , ("top", y)]]
      [div (attrs ++ [style [("position", "relative"), ("margin-left", "-50%"), ("margin-top", "-50%")]])
           content]

movementCircle origin max_distance =
  let radius = cmToPx max_distance
  in centerPositionedBox (coordPx origin.x) (coordPx origin.y)
       [ style [ ("width", px (radius * 2))
               , ("height", px (radius * 2))
               , ("border-radius", px radius)
               , ("background", "lightgreen")
               ]
       , onMouseClick (clickedMove origin radius)
       ]
       []

gridCreature : M.Creature -> Html U.Msg
gridCreature creature = centerPositionedBox (coordPx creature.pos.x) (coordPx creature.pos.y)
  [style [("background-color", "cyan"), ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  [text creature.id]

gridTerrain : M.Point3 -> Html a
gridTerrain pt = centerPositionedBox (coordPx pt.x) (coordPx pt.y)
  [style [("background-color", "grey") , ("width", metersToPxPx 1), ("height", metersToPxPx 1)]]
  []

-- End grid insanity

combatArea : M.Model -> M.Game -> M.Combat -> Html U.Msg
combatArea model game combat = case model.selectedAbility of
  Just abid -> targetSelector model game combat abid
  Nothing -> combatantList game combat

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
stopCombatButton = button [onClick U.PostStopCombat] [text "Stop Combat"]
startCombatButton : Html U.Msg
startCombatButton = button [onClick U.PostStartCombat] [text "Start Combat"]

combatantList : M.Game -> M.Combat -> Html U.Msg
combatantList game { creatures, movement_used } = div []
  [ h3 [] [text "Combat!"]
  , vbox (List.map (combatantEntry game movement_used creatures.cursor) (List.indexedMap (,) creatures.data))
  , stopCombatButton
  ]

engageButton : M.Creature -> Html U.Msg
engageButton creature =
  button [onClick (U.AddToCombat creature.id)] [text "Engage"]

combatantEntry : M.Game -> M.Distance -> Int -> (Int, M.Creature) -> Html U.Msg
combatantEntry game movement_used cursor (idx, creature) = hbox
  [ if cursor == idx then actionBar movement_used game.ability_sets creature
    else div [] []
  , creatureStats creature
  , disengageButton creature
  ]

actionBar : M.Distance -> Dict.Dict String (List String) -> M.Creature -> Html U.Msg
actionBar movement_used abilitySets creature =
  let abilitySet =
        case (Dict.get creature.ability_set abilitySets) of
          Just x -> x
          Nothing -> []
  in hbox (  (doneButton creature)
          :: (moveButton movement_used creature)
          :: (List.map actionButton abilitySet))

actionButton : String -> Html U.Msg
actionButton abid = button [onClick (U.SelectAbility abid)] [text abid]

creatureStats : M.Creature -> Html U.Msg
creatureStats creature = 
  hbox [ text "Name: ", text creature.name
       , text "HP: ", text (toString creature.cur_health)
       , text "Energy: ", text (toString creature.cur_energy)
       , text "Pos: ", text <| (toString creature.pos.x) ++ "/" ++ (toString creature.pos.y)
       ]

disengageButton : M.Creature -> Html U.Msg
disengageButton creature =
  button [onClick (U.RemoveFromCombat creature.id)] [text "Disengage"]

deleteCreatureButton : M.Creature -> Html U.Msg
deleteCreatureButton creature =
  button [onClick (U.RemoveFromGame creature.id)] [text "Delete"]

doneButton : M.Creature -> Html U.Msg
doneButton creature =
  button [onClick U.TurnDone] [text "Done"]

moveButton : M.Distance -> M.Creature -> Html U.Msg
moveButton movement_used creature =
  let movement_left = creature.speed - movement_used
  in button [onClick (U.RequestMove <| M.MovementRequest creature.id creature.pos movement_left)]
            [text (String.join "" ["Move (", toString movement_left, ")"])]

hbox : List (Html a) -> Html a
hbox els = div [style [("display", "flex"), ("width", "100%")] ]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)

vbox : List (Html a) -> Html a
vbox els = div [style [("display", "flex"), ("flex-direction", "column"), ("width", "100%")]]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)
