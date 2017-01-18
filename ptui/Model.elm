module Model exposing (..)

import Dict exposing (Dict(..))
import Maybe exposing (withDefault)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as P
import Json.Helpers as JH
import Set

defaultModel : Model
defaultModel =
  Model
    Nothing
    (PendingCreature Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing [])
    Set.empty
    "No current error!"

type alias Model =
  { app : Maybe App
  , pendingCreature : PendingCreature
  , pendingCombatCreatures : Set.Set String
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

point3Decoder = JD.map3 Point3 (JD.index 0 JD.int) (JD.index 1 JD.int) (JD.index 2 JD.int)
point3Encoder {x, y, z} =  JE.list [JE.int x, JE.int y, JE.int z]

type alias App = { current_game : Game }

appDecoder = JD.map App (JD.field "current_game" gameDecoder)

type alias Game =
  { current_combat : Maybe Combat
  , abilities : Dict String Ability
  , creatures : Dict String Creature
  }

gameDecoder =
  JD.map3 Game
    (JD.field "current_combat" (JD.maybe combatDecoder))
    (JD.field "abilities" (JD.dict abilityDecoder))
    (JD.field "creatures" (JD.dict creatureDecoder))

type alias Combat =
  { creatures: CursorList Creature
  , movementUsed: Int
  }

combatDecoder : JD.Decoder Combat
combatDecoder =
  JD.map2 Combat
    (JD.field "creatures" (cursorListDecoder creatureDecoder))
    (JD.field "movement_used" JD.int)

type alias CursorList a = {
  cursor: Int,
  data: List a
}

cursorListDecoder : JD.Decoder a -> JD.Decoder (CursorList a)
cursorListDecoder elDecoder =
  JD.map2 CursorList
    (JD.field "cursor" JD.int)
    (JD.field "data" (JD.list elDecoder))

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
    |> P.required "id" JD.string
    |> P.required "name" JD.string
    |> P.required "speed" JD.int
    |> P.required "max_energy" JD.int
    |> P.required "cur_energy" JD.int
    |> P.required "max_health" JD.int
    |> P.required "cur_health" JD.int
    |> P.required "pos" point3Decoder
    |> P.required "abilities" (JD.list abilityStatusDecoder)
    |> P.required "conditions" (JD.list appliedConditionDecoder)

creatureEncoder { id, name, speed, max_energy, cur_energy, max_health
                , cur_health, pos, abilities, conditions} =
  JE.object
    [ ("id", JE.string id)
    , ("name", JE.string name)
    , ("speed", JE.int speed)
    , ("pos", point3Encoder pos)
    , ("max_energy", JE.int max_energy)
    , ("cur_energy", JE.int cur_energy)
    , ("max_health", JE.int max_health)
    , ("cur_health", JE.int cur_health)
    , ("abilities", JE.list (List.map abilityStatusEncoder abilities))
    , ("conditions", JE.list (List.map appliedConditionEncoder conditions))
    ]

type alias Ability = { name : String }

abilityDecoder = JD.map Ability (JD.field "name" JD.string)

type alias AbilityStatus = { ability_id: String, cooldown: Int }

abilityStatusDecoder = JD.map2 AbilityStatus
                                 (JD.field "ability_id" JD.string)
                                 (JD.field "cooldown" JD.int)
abilityStatusEncoder {ability_id, cooldown} =
  JE.object [ ("ability_id", JE.string ability_id)
            , ("cooldown", JE.int cooldown)
            ]

type alias AppliedCondition =
  { id: Int
  , remaining: ConditionDuration
  , condition: Condition
  }

appliedConditionDecoder =
  JD.map3 AppliedCondition
    (JD.field "id" JD.int)
    (JD.field "remaining" conditionDurationDecoder)
    (JD.field "condition" conditionDecoder)

appliedConditionEncoder {id, remaining, condition} =
  JE.object [ ("id", JE.int id)
            , ("remaining", conditionDurationEncoder remaining)
            , ("condition", conditionEncoder condition)]

type ConditionDuration
  = Interminate
  | Duration Int

conditionDurationDecoder =
  JD.oneOf
    [ JH.decodeSumUnaries "Interminate"
        (Dict.fromList [("Interminate", Interminate)])
    , JH.decodeSumObjectWithSingleField "ConditionDuration"
        (Dict.fromList [("ConditionDuration", JD.map Duration JD.int)])
    ]
conditionDurationEncoder cd =
  case cd of
    Interminate -> JE.string "Interminate"
    Duration num -> JE.object [("Duration", JE.int num)]

type Condition
  = RecurringEffect Effect
  | Dead
  | Incapacitated
  | AddDamageBuff Int

conditionDecoder =
  JD.oneOf
    [ JH.decodeSumUnaries "Condition Nullary"
      (Dict.fromList [ ("Dead", Dead)
                     , ("Incapacitated", Incapacitated)])
    , JH.decodeSumObjectWithSingleField "Condition Unary"
      (Dict.fromList [ ("RecurringEffect", JD.map RecurringEffect lazyEffectDecoder)
                     , ("AddDamageBuff", JD.map AddDamageBuff JD.int)])]

conditionEncoder condition =
  case condition of
    Dead -> JE.string "Dead"
    Incapacitated -> JE.string "Incapacitated"
    AddDamageBuff num -> JE.object [("AddDamageBuff", JE.int num)]
    RecurringEffect eff -> JE.object [("RecurringEffect", effectEncoder eff)]

type Effect
  = ApplyCondition ConditionDuration Condition
  | Heal Int
  | Damage Int
  | MultiEffect (List Effect)
  | GenerateEnergy Int

lazyEffectDecoder = JD.lazy (\_ -> effectDecoder)

effectDecoder =
  JH.decodeSumObjectWithSingleField "Effect"
    (Dict.fromList
      [ ("Heal", JD.map Heal JD.int)
      , ("Damage", JD.map Damage JD.int)
      , ("GenerateEnergy", JD.map GenerateEnergy JD.int)
      , ("ApplyCondition", JD.map2 ApplyCondition
                                     (JD.index 0 conditionDurationDecoder)
                                     (JD.index 1 conditionDecoder))
      , ("MultiEffect", JD.map MultiEffect (JD.list lazyEffectDecoder))
      ])
effectEncoder eff =
  case eff of
    Heal num -> JE.object [("Heal", JE.int num)]
    Damage num -> JE.object [("Damage", JE.int num)]
    GenerateEnergy num -> JE.object [("GenerateEnergy", JE.int num)]
    MultiEffect effs -> JE.object [("MultiEffect", JE.list (List.map effectEncoder effs))]
    ApplyCondition cd cond -> JE.object [("ApplyCondition", JE.list [ conditionDurationEncoder cd
                                                                    , conditionEncoder cond])]


-- "Input", or GameCommand and inward
type GameCommand
  = StartCombat (List String)
  | StopCombat
  -- | Act AbilityID DecidedTarget
  | Move Point3
  | CreateCreature Creature
  | RemoveCreature String
  | AddCreatureToCombat String
  | RemoveCreatureFromCombat String
  -- RetrieveFromInventory(ThingID)
  -- StowInInventory(ThingID)
  | Done

gameCommandEncoder gc =
  case gc of
    StartCombat cids -> JE.object [("StartCombat", JE.list (List.map JE.string cids))]
    CreateCreature creature -> JE.object [("CreateCreature", creatureEncoder creature)]
    _ -> JE.string "undefined"

-- pure functions on model

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
