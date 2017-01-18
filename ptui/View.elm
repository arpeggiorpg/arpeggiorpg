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
  [ div []
    [ input [type_ "text", placeholder "id", onInput U.PendingCreatureId ] []
    , input [type_ "text", placeholder "name", onInput U.PendingCreatureName ] []
    , button [ onClick U.PostCreateCreature ] [ text "Create Creature!" ]
    ]
  , h3 [] [text "Creatures"]
  , div [] [ renderCreatures model.pendingCombatCreatures app.current_game.creatures ]
  , case app.current_game.current_combat of
      Just combat -> div [] [renderStopCombat, renderCombat combat]
      Nothing -> renderStartCombat
  , div [] [ text (toString app)]
  ]

renderCreatures : Set.Set String -> Dict.Dict String M.Creature -> Html U.Msg
renderCreatures pendingCreatures creatures = div []
  (List.map (renderCreature pendingCreatures) (Dict.values creatures))

renderCreature : Set.Set String -> M.Creature -> Html U.Msg
renderCreature pendingCreatures creature = div []
  [ text creature.name
  , input [ type_ "checkbox"
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
  , div [] (List.map (renderCreatureInCombat creatures.cursor) (List.indexedMap (,) creatures.data))
  ]

renderCreatureInCombat : Int -> (Int, M.Creature) -> Html U.Msg
renderCreatureInCombat cursor (idx, creature) = div []
  [ div [] [text "Name: ", text creature.name]
  , div [] [text "HP: ", text (toString creature.cur_health)]
  , if cursor == idx then div [] [text "THIS M'S TURN!"]
    else div [] []
  ]
