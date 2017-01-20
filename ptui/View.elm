module View exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE
import Set

import Model as M
import Update as U

view : M.Model -> Html U.Msg
view model =
    div []
        [ h2 [] [ text "P&T" ]
        , button [ onClick U.MorePlease ] [ text "More Please!" ]
        , case model.app of Just app -> viewApp model app
                            Nothing -> div [] [text "No app yet. Maybe reload."]
        , div [] [text "Last error:", pre [] [text model.error]]
        , div [] [text "Last Response:", pre [] [text (JE.encode 4 model.lastResponse)]]
        ]

viewApp : M.Model -> M.App -> Html U.Msg
viewApp model app = div []
  [ hbox [ div [] [ h3 [] [text "Creatures"]
                  , inactiveList
                        app.current_game.current_combat
                        model.pendingCombatCreatures
                        app.current_game.creatures
                  ]
         , case app.current_game.current_combat of
             Just combat -> div [] [combatantList combat]
             Nothing -> startCombatButton
         ]
  , pre [] [ text (toString app)]
  ]

inactiveList : Maybe M.Combat -> Set.Set String -> Dict.Dict String M.Creature -> Html U.Msg
inactiveList mCombat pendingCreatures creatures = div []
  [ div [] (List.map (inactiveEntry mCombat pendingCreatures) (Dict.values creatures))
  , div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , button [ onClick U.PostCreateCreature ] [ text "Create Creature!" ]
    ]
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

stopCombatButton : Html U.Msg
stopCombatButton = button [onClick U.PostStopCombat] [text "Stop Combat"]
startCombatButton : Html U.Msg
startCombatButton = button [onClick U.PostStartCombat] [text "Start Combat"]

combatantList : M.Combat -> Html U.Msg
combatantList { creatures, movement_used } = div []
  [ h3 [] [text "Combat!"]
  , div [] [text "Current movement used:", text (toString movement_used)]
  , vbox (List.map (combatantEntry creatures.cursor) (List.indexedMap (,) creatures.data))
  , stopCombatButton
  ]

engageButton : M.Creature -> Html U.Msg
engageButton creature =
  button [onClick (U.AddToCombat creature.id)] [text "Engage"]

combatantEntry : Int -> (Int, M.Creature) -> Html U.Msg
combatantEntry cursor (idx, creature) = hbox
  [ if cursor == idx then actionBar creature
    else div [] []
  , creatureStats creature
  , disengageButton creature
  ]

creatureStats : M.Creature -> Html U.Msg
creatureStats creature = 
  hbox [div [] [text "Name: ", text creature.name]
         , div [] [text "HP: ", text (toString creature.cur_health)]
         , div [] [text "Energy: ", text (toString creature.cur_energy)]
         ]

disengageButton : M.Creature -> Html U.Msg
disengageButton creature =
  button [onClick (U.RemoveFromCombat creature.id)] [ text "Disengage" ]

deleteCreatureButton : M.Creature -> Html U.Msg
deleteCreatureButton creature =
  button [onClick (U.RemoveFromGame creature.id)] [text "Delete"]

actionBar : M.Creature -> Html U.Msg
actionBar creature = hbox [doneButton creature]

doneButton : M.Creature -> Html U.Msg
doneButton creature =
  button [onClick U.TurnDone] [text "Done"]

hbox : List (Html a) -> Html a
hbox els = div [style [("display", "flex"), ("width", "100%")] ]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)

vbox : List (Html a) -> Html a
vbox els = div [ style [("display", "flex"), ("flex-direction", "column"), ("width", "100%")]]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)
