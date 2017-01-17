module Model exposing (..)

import Dict exposing (Dict(..))
import Maybe exposing (withDefault)
import Json.Decode as JSON
import Json.Decode.Pipeline as P
import Json.Helpers as JH



defaultModel : Model
defaultModel =
  Model
    Nothing
    (PendingCreature Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing [])
    "No current error!"

type alias Model =
  { app : Maybe App
  , pendingCreature : PendingCreature
  , error: String
  }

type alias PendingCreature =
  { id : Maybe String
  , name : Maybe String
  , speed: Maybe Int
  , max_energy: Maybe Int
  , cur_energy: Maybe Int
  , abilities: List AbilityStatus
  , max_health: Maybe Int
  , cur_health: Maybe Int
  , pos: Maybe Point3
  , conditions: List AppliedCondition
  }

type alias Point3 = {x: Int, y: Int, z: Int}
point3Decoder = JSON.map3 Point3 JSON.int JSON.int JSON.int

type alias App = { current_game : Game }

appDecoder = JSON.map App (JSON.field "current_game" gameDecoder)

type alias Game =
    { current_combat : Maybe Combat
    , abilities : Dict String Ability
    }

gameDecoder =
  JSON.map2 Game
    (JSON.field "current_combat" (JSON.maybe combatDecoder))
    (JSON.field "abilities" (JSON.dict abilityDecoder))

type alias Combat =
  { creatures: List Creature
  , movementUsed: Int
  }

combatDecoder =
  JSON.map2 Combat
    (JSON.field "creatures" (JSON.list creatureDecoder))
    (JSON.field "movement_used" JSON.int)

type alias Creature =
  { id: String
  , name: String
  , speed: Int
  , max_energy: Int
  , cur_energy: Int
  , max_health: Int
  , cur_health: Int
  , pos: Point3
  , abilities: List AbilityStatus
  , conditions: List AppliedCondition
}

creatureDecoder =
  P.decode Creature
    |> P.required "id" JSON.string
    |> P.required "name" JSON.string
    |> P.required "speed" JSON.int
    |> P.required "max_energy" JSON.int
    |> P.required "cur_energy" JSON.int
    |> P.required "max_health" JSON.int
    |> P.required "cur_health" JSON.int
    |> P.required "pos" point3Decoder
    |> P.required "abilities" (JSON.list abilityStatusDecoder)
    |> P.required "conditions" (JSON.list appliedConditionDecoder)

finalizePending : PendingCreature -> Maybe Creature
finalizePending {id, name, speed, max_energy, cur_energy, abilities, max_health, cur_health, pos, conditions } =
  case (id, name) of
    (Just id, Just name) ->
      Just { id = id
           , name = name
           , speed = withDefault 10 speed
           , max_energy = withDefault 10 max_energy
           , cur_energy = withDefault 10 cur_energy
           , max_health = withDefault 10 max_health
           , cur_health = withDefault 10 cur_health
           , pos = withDefault {x=0, y=0, z=0} pos
           , abilities = abilities
           , conditions = conditions }
    _ -> Nothing

type alias Ability = { name : String }

abilityDecoder = JSON.map Ability (JSON.field "name" JSON.string)

type alias AbilityStatus = { ability_id: String, cooldown: Int }

abilityStatusDecoder = JSON.map2 AbilityStatus
                                 (JSON.field "ability_id" JSON.string)
                                 (JSON.field "cooldown" JSON.int)

type alias AppliedCondition =
  { id: Int
  , remaining: ConditionDuration
  , condition: Condition
  }

appliedConditionDecoder =
  JSON.map3 AppliedCondition
    (JSON.field "id" JSON.int)
    (JSON.field "remaining" conditionDurationDecoder)
    (JSON.field "condition" conditionDecoder)

type ConditionDuration
  = Interminate
  | Duration Int

conditionDurationDecoder =
  JSON.oneOf
    [ JH.decodeSumUnaries "Interminate"
        (Dict.fromList [("Interminate", Interminate)])
    , JH.decodeSumObjectWithSingleField "ConditionDuration"
        (Dict.fromList [("ConditionDuration", JSON.map Duration JSON.int)])
    ]

type Condition
  = RecurringEffect Effect
  | Dead
  | Incapacitated
  | AddDamageBuff Int

conditionDecoder =
  JSON.oneOf
    [ JH.decodeSumUnaries "Condition Nullary"
      (Dict.fromList [ ("Dead", Dead)
                     , ("Incapacitated", Incapacitated)])
    , JH.decodeSumObjectWithSingleField "Condition Unary"
      (Dict.fromList [ ("RecurringEffect", JSON.map RecurringEffect effectDecoder)
                     , ("AddDamageBuff", JSON.map AddDamageBuff JSON.int)])]

type Effect
  = ApplyCondition ConditionDuration Int
  | Heal Int
  | Damage Int
  | MultiEffect (List Effect)
  | GenerateEnergy Int

effectDecoder =
  JH.decodeSumObjectWithSingleField "Effect"
    (Dict.fromList
      [ ("Heal", JSON.map Heal JSON.int)
      , ("Damage", JSON.map Damage JSON.int)
      , ("GenerateEnergy", JSON.map GenerateEnergy JSON.int)
      , ("ApplyCondition", JSON.map2 ApplyCondition
                                     conditionDurationDecoder JSON.int)])
