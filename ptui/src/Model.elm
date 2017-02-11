module Model exposing (..)

import Array
import Dict exposing (Dict(..))
import Maybe exposing (withDefault)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as P
import Json.Helpers as JH
import Set


type alias CreatureID = String
type alias AbilityID = String
type alias MapName = String
type alias Distance = Int

defaultModel : Model
defaultModel =
    { app = Nothing
    , selectedAbility = Nothing
    , pendingCreatureId = Nothing
    , pendingCreatureName = Nothing
    , pendingCreatureClass = Nothing
    , selectedCreatures = Set.empty
    , moving = Nothing
    , error = "No current error!"
    , saveMapName = "" -- this could be inside of editingMap sumtype
    , editingMap = False
    , currentMap = [{x=0, y=0, z=0}]
    , controlledCreatures = Nothing
    , potentialTargets = []
  }

type alias MovementRequest = {
  max_distance: Distance,
  movement_options: List Point3,
  -- This field is a Just when we're performing out-of-combat movement.
  -- It's Nothing for in-combat movement, because in-combat movement is only for the current
  -- creature.
  ooc_creature: Maybe Creature
}


type alias Model =
  { app : Maybe App
  , pendingCreatureId : Maybe CreatureID
  , pendingCreatureName : Maybe String
  , pendingCreatureClass : Maybe String
  , selectedAbility : Maybe AbilityID
  -- Creatures which have been selected for combat
  , selectedCreatures : Set.Set CreatureID
  , error: String
  , moving: Maybe MovementRequest
  , saveMapName: String
  , currentMap : Map
  , editingMap : Bool
  , controlledCreatures : Maybe (List CreatureID)
  , potentialTargets: List PotentialTarget
  }

type ControlledCreatures
  = GM
  | Creatures (List CreatureID)

type alias CreatureCreation =
  { id : CreatureID
  , name : String
  , class: String
  , pos: Point3
  }

creatureCreationDecoder = JD.map4 CreatureCreation
  (JD.field "id" JD.string)
  (JD.field "name" JD.string)
  (JD.field "class" JD.string)
  (JD.field "pos" point3Decoder)

creatureCreationEncoder cc = JE.object
  [ ("id", JE.string cc.id)
  , ("name", JE.string cc.name)
  , ("class", JE.string cc.class)
  , ("pos", point3Encoder cc.pos)]

type alias Point3 = {x: Int, y: Int, z: Int}

point3Decoder = JD.map3 Point3 (JD.index 0 JD.int) (JD.index 1 JD.int) (JD.index 2 JD.int)
point3Encoder {x, y, z} =  JE.list [JE.int x, JE.int y, JE.int z]


type PotentialTarget
  = PTCreatureID CreatureID
  | PTPoint Point3

potentialTargetDecoder = sumDecoder "PotentialTarget"
  []
  [ ("CreatureID", JD.map PTCreatureID JD.string)
  , ("Point", JD.map PTPoint point3Decoder)
  ]

type alias App =
  { current_game : Game
  , snapshots : Array.Array (Game, (List GameLog))
  }

appDecoder = JD.map2 App
  (JD.field "current_game" gameDecoder)
  (JD.field "snapshots" (JD.array (JD.map2 (,) (JD.index 0 gameDecoder) (JD.index 1 <| JD.list gameLogDecoder))))

type GameLog
  = GLSelectMap MapName
  | GLEditMap MapName Map
  | GLCombatLog CombatLog
  | GLCreatureLog CreatureID CreatureLog
  | GLStartCombat (List CreatureID)
  | GLStopCombat
  | GLCreateCreature Creature
  | GLRemoveCreature CreatureID
  | GLAddCreatureToCombat CreatureID
  | GLRemoveCreatureFromCombat CreatureID

gameLogDecoder = sumDecoder "GameLog"
  [("StopCombat", GLStopCombat)]
  [ ("SelectMap", JD.map GLSelectMap JD.string)
  , ("CombatLog", JD.map GLCombatLog combatLogDecoder)
  , ("CreatureLog", JD.map2 GLCreatureLog (JD.index 0 JD.string) (JD.index 1 creatureLogDecoder))
  , ("StartCombat", JD.map GLStartCombat (JD.list JD.string))
  , ("CreateCreature", (JD.map GLCreateCreature creatureDecoder))
  , ("RemoveCreature", (JD.map GLRemoveCreature JD.string))
  , ("AddCreatureToCombat", (JD.map GLAddCreatureToCombat JD.string))
  , ("RemoveCreatureFromCombat", (JD.map GLRemoveCreatureFromCombat JD.string))
  , ("EditMap", (JD.map2 GLEditMap (JD.index 0 JD.string) (JD.index 1 mapDecoder)))
  ]

type CombatLog
  = ComLCreatureLog CreatureID CreatureLog
  | ComLEndTurn CreatureID

combatLogDecoder = sumDecoder "CombatLog"
  []
  [ ("CreatureLog", JD.map2 ComLCreatureLog (JD.index 0 JD.string) (JD.index 1 creatureLogDecoder))
  , ("EndTurn", JD.map ComLEndTurn JD.string)]

type CreatureLog
  = CLDamage Int
  | CLHeal Int
  | CLGenerateEnergy Int
  | CLReduceEnergy Int
  | CLApplyCondition Int ConditionDuration Condition
  | CLRemoveCondition Int
  | CLPathCreature PathAndDistance

type alias PathAndDistance = {path: List Point3, distance: Distance}

pathAndDistanceDecoder = JD.map2 PathAndDistance
  (JD.field "path" (JD.list point3Decoder))
  (JD.field "distance" JD.int)

creatureLogDecoder = sumDecoder "CreatureLog"
  []
  [ ("Damage", JD.map CLDamage JD.int)
  , ("Heal", JD.map CLHeal JD.int)
  , ("GenerateEnergy", JD.map CLGenerateEnergy JD.int)
  , ("ReduceEnergy", JD.map CLReduceEnergy JD.int)
  , ("ApplyCondition", JD.map3 CLApplyCondition JD.int conditionDurationDecoder conditionDecoder)
  , ("RemoveCondition", JD.map CLRemoveCondition JD.int)
  , ("PathCreature", JD.map CLPathCreature pathAndDistanceDecoder)
  ]

type alias Game =
  { current_combat : Maybe Combat
  , abilities : Dict AbilityID Ability
  , classes : Dict String Class
  , creatures : Dict CreatureID Creature
  , maps: Dict MapName Map
  , current_map: Maybe MapName
  }

gameDecoder : JD.Decoder Game
gameDecoder =
  P.decode Game
    |> P.required "current_combat" (JD.maybe combatDecoder)
    |> P.required "abilities" (JD.dict abilityDecoder)
    |> P.required "classes" (JD.dict classDecoder)
    |> P.required "creatures" (JD.dict creatureDecoder)
    |> P.required "maps" (JD.dict mapDecoder)
    |> P.required "current_map" (JD.maybe JD.string)

type alias Map = List Point3

mapDecoder = JD.list point3Decoder
mapEncoder t = JE.list (List.map point3Encoder t)

type alias Class =
  { abilities: List AbilityID
  , conditions: List Condition
  }

classDecoder : JD.Decoder Class
classDecoder = JD.map2 Class
  (JD.field "abilities" (JD.list JD.string))
  (JD.field "conditions" (JD.list conditionDecoder))

type alias Combat =
  { creatures: CursorList Creature
  , movement_used: Int
  , movement_options: List Point3
  }

combatDecoder : JD.Decoder Combat
combatDecoder =
  JD.map3 Combat
    (JD.field "creatures" (cursorListDecoder creatureDecoder))
    (JD.field "movement_used" JD.int)
    (JD.field "movement_options" (JD.list point3Decoder))

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
  { id: CreatureID
  , name: String
  , speed: Int
  , max_energy: Int
  , cur_energy: Int
  , max_health: Int
  , cur_health: Int
  , pos: Point3
  , abilities: List AbilityStatus
  , class: String
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
    |> P.required "class" JD.string
    |> P.required "conditions" (JD.list appliedConditionDecoder)

creatureEncoder { id, name, speed, max_energy, cur_energy, max_health
                , cur_health, pos, abilities, class, conditions} =
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
    , ("class", JE.string class)
    , ("conditions", JE.list (List.map appliedConditionEncoder conditions))
    ]

type alias Ability =
  { name : String
  , target: TargetSpec
  }

abilityDecoder = JD.map2 Ability
  (JD.field "name" JD.string)
  (JD.field "target" targetSpecDecoder)

type TargetSpec
  = Melee
  | Range Distance -- distance in centimeters
    -- CircleWithinRange(Distance, u8), // radius
    -- Cone(Distance, u8), // radians of angle of cone (should this be steradians? is it the same?)
    -- Line(Distance),
    -- LineToFirstHit(),

targetSpecDecoder = sumDecoder "TargetSpec"
  [("Melee", Melee)]
  [("Range", JD.map Range JD.int)]

type DecidedTarget
  = DecidedMelee CreatureID
  | DecidedRange CreatureID

decidedTargetEncoder dt = case dt of
  DecidedMelee cid -> JE.object [("Melee", JE.string cid)]
  DecidedRange cid -> JE.object [("Range", JE.string cid)]

type alias AbilityStatus = { ability_id: AbilityID, cooldown: Int }

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

conditionDurationDecoder = sumDecoder "ConditionDuration"
  [("Interminate", Interminate)]
  [("ConditionDuration", JD.map Duration JD.int)]
conditionDurationEncoder cd =
  case cd of
    Interminate -> JE.string "Interminate"
    Duration num -> JE.object [("Duration", JE.int num)]

type Condition
  = RecurringEffect Effect
  | Dead
  | Incapacitated
  | AddDamageBuff Int

conditionDecoder = sumDecoder "Condition"
  [ ("Dead", Dead)
  , ("Incapacitated", Incapacitated)]
  [ ("RecurringEffect", JD.map RecurringEffect lazyEffectDecoder)
  , ("AddDamageBuff", JD.map AddDamageBuff JD.int)]

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
                                     (JD.index 1 (JD.lazy (\_ -> conditionDecoder))))
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
  = SelectMap MapName
  | EditMap MapName Map
  | StartCombat (List CreatureID)
  | StopCombat
  | Act AbilityID DecidedTarget
  | Move Point3
  | MoveOutOfCombat CreatureID Point3
  | CreateCreature CreatureCreation
  | RemoveCreature CreatureID
  | AddCreatureToCombat CreatureID
  | RemoveCreatureFromCombat CreatureID
  -- RetrieveFromInventory(ThingID)
  -- StowInInventory(ThingID)
  | Done

gameCommandEncoder : GameCommand -> JE.Value
gameCommandEncoder gc =
  case gc of
    StartCombat cids -> JE.object [("StartCombat", JE.list (List.map JE.string cids))]
    CreateCreature creature -> JE.object [("CreateCreature", creatureCreationEncoder creature)]
    RemoveCreature cid -> JE.object [("RemoveCreature", JE.string cid)]
    StopCombat -> JE.string "StopCombat"
    Move pt -> JE.object [("Move", point3Encoder pt)]
    MoveOutOfCombat cid pt -> JE.object [("MoveOutOfCombat", JE.list [JE.string cid, point3Encoder pt])]
    AddCreatureToCombat cid -> JE.object [("AddCreatureToCombat", JE.string cid)]
    RemoveCreatureFromCombat cid -> JE.object [("RemoveCreatureFromCombat", JE.string cid)]
    Act abid dtarget -> JE.object [("Act", JE.list [JE.string abid, decidedTargetEncoder dtarget])]
    Done -> JE.string "Done"
    SelectMap name -> JE.object [("SelectMap", JE.string name)]
    EditMap name terrain -> JE.object [("EditMap", JE.list [JE.string name, mapEncoder terrain])]



type RustResult
  = RustOk JD.Value
  | RustErr JD.Value

rustResultDecoder = sumDecoder "RustResult"
  []
  [ ("Ok", JD.map RustOk JD.value)
  , ("Err", JD.map RustErr JD.value) ]


-- UTILS

-- A decoder for Serde-style enums. Nullary constructors are bare strings and all others are
-- `{"ConstructorName": ...}`
sumDecoder : String -> List (String, a) -> List (String, JD.Decoder a) -> JD.Decoder a
sumDecoder name nullaryList singleFieldList = JD.oneOf
  [ JH.decodeSumUnaries name
      (Dict.fromList nullaryList)
  , JH.decodeSumObjectWithSingleField name
      (Dict.fromList singleFieldList)
  ]

listFind : (a -> Bool) -> List a -> Maybe a
listFind f l = List.head (List.filter f l)

maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr m1 m2 =
  case (m1, m2) of
    (Just x, _) -> Just x
    (_, Just x) -> Just x
    _ -> Nothing

-- MODEL FUNCTIONS

distance : Point3 -> Point3 -> Distance
distance a b =
  round <| sqrt (toFloat <| (a.x - b.x)^2 + (a.y - b.y)^2 + (a.z - b.z)^2)

getMap : Model -> Map
getMap model =
  case model.app of
    Just app -> let mapName = (Maybe.withDefault "empty" app.current_game.current_map)
                in (Maybe.withDefault [] (Dict.get mapName app.current_game.maps))
    Nothing -> []

--- Get the current creature in combat.
combatCreature : Combat -> Creature
combatCreature combat = case List.head (List.drop combat.creatures.cursor combat.creatures.data) of
  Just c -> c
  Nothing -> Debug.crash "Creature CursorList was invalid"

--- Find a creature whether it's in combat or not.
findCreature : Game -> CreatureID -> Maybe Creature
findCreature game cid =
  maybeOr (Maybe.andThen (\combat -> getCreatureInCombat combat cid) game.current_combat)
          (getCreatureOOC game cid)

getCreatureInCombat : Combat -> CreatureID -> Maybe Creature
getCreatureInCombat combat cid = listFind (\c -> c.id == cid) combat.creatures.data

getCreatureOOC : Game -> CreatureID -> Maybe Creature
getCreatureOOC game cid = Dict.get cid game.creatures

potentialCreatureTargets : List PotentialTarget -> List CreatureID
potentialCreatureTargets pts =
  let f pt =
        case pt of
          PTCreatureID cid -> Just cid
          PTPoint _ -> Nothing
  in List.filterMap f pts
