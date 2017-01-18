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
        , div [] [text "Last error:", text model.error]
        , div [] [text "Last Response:", text (JE.encode 4 model.lastResponse)]
        ]

viewApp : M.Model -> M.App -> Html U.Msg
viewApp model app = div []
  [ hbox [ div [] [ h3 [] [text "Creatures"]
                  , renderPendingCreatures app.current_game.current_combat model.pendingCombatCreatures app.current_game.creatures
                  ]
         , case app.current_game.current_combat of
             Just combat -> div [] [renderCombat combat]
             Nothing -> renderStartCombat
         ]
  , div [] [ text (toString app)]
  ]


renderPendingCreatures : Maybe M.Combat -> Set.Set String -> Dict.Dict String M.Creature -> Html U.Msg
renderPendingCreatures mCombat pendingCreatures creatures = div []
  [ div [] (List.map (renderPendingCreature mCombat pendingCreatures) (Dict.values creatures))
  , div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , button [ onClick U.PostCreateCreature ] [ text "Create Creature!" ]
    ]
  ]

renderPendingCreature : Maybe M.Combat -> Set.Set String -> M.Creature -> Html U.Msg
renderPendingCreature mCombat pendingCreatures creature = hbox
  [ renderCreature creature
  , case mCombat of Just _ -> renderAddToCombat creature
                    Nothing ->
                      input [ type_ "checkbox"
                      , checked (Set.member creature.id pendingCreatures)
                      , onClick (U.ToggleSelectedCreature creature.id)] []]

renderStopCombat : Html U.Msg
renderStopCombat = button [onClick U.PostStopCombat] [text "Stop Combat"]
renderStartCombat : Html U.Msg
renderStartCombat = button [onClick U.PostStartCombat] [text "Start Combat"]

renderCombat : M.Combat -> Html U.Msg
renderCombat { creatures, movement_used } = div []
  [ h3 [] [text "Combat!"]
  , div [] [text "Current movement used:", text (toString movement_used)]
  , vbox (List.map (renderCreatureInCombat creatures.cursor) (List.indexedMap (,) creatures.data))
  , renderStopCombat
  ]

renderAddToCombat : M.Creature -> Html U.Msg
renderAddToCombat creature =
  button [onClick (U.AddToCombat creature.id)] [text "Engage"]

renderCreatureInCombat : Int -> (Int, M.Creature) -> Html U.Msg
renderCreatureInCombat cursor (idx, creature) = hbox
  [ if cursor == idx then div [] [text "*"]
    else div [] []
  , renderCreature creature
  ]

renderCreature : M.Creature -> Html U.Msg
renderCreature creature = 
  hbox [div [] [text "Name: ", text creature.name]
         , div [] [text "HP: ", text (toString creature.cur_health)]
         , div [] [text "Energy: ", text (toString creature.cur_energy)]
         ]

hbox : List (Html a) -> Html a
hbox els = div [style [("display", "flex"), ("width", "100%")] ]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)

vbox : List (Html a) -> Html a
vbox els = div [ style [("display", "flex"), ("flex-direction", "column"), ("width", "100%")]]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)
