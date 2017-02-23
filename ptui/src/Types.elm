module Types exposing (..)

import Array
import Dict exposing (Dict(..))
import Maybe exposing (withDefault)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as P
import Json.Helpers as JH
import Set


type alias PlayerID = String
type alias CreatureID = String
type alias AbilityID = String
type alias MapName = String
type alias Distance = Int

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
  , players : Dict PlayerID (Set.Set CreatureID)
  }

appDecoder = JD.map3 App
  (JD.field "current_game" gameDecoder)
  (JD.field "snapshots" (JD.array (JD.map2 (,) (JD.index 0 gameDecoder) (JD.index 1 <| JD.list gameLogDecoder))))
  (JD.field "players" (JD.dict (JD.map Set.fromList (JD.list JD.string))))

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
  | GLRollback Int Int

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
  , ("Rollback", (JD.map2 GLRollback (JD.index 0 JD.int) (JD.index 1 JD.int)))
  ]

type CombatLog
  = ComLCreatureLog CreatureID CreatureLog
  | ComLEndTurn CreatureID
  | ComLPathCurrentCreature (List Point3)
  | ComLChangeCreatureInitiative CreatureID Int

combatLogDecoder = sumDecoder "CombatLog"
  []
  [ ("CreatureLog", JD.map2 ComLCreatureLog (JD.index 0 JD.string) (JD.index 1 creatureLogDecoder))
  , ("EndTurn", JD.map ComLEndTurn JD.string)
  , ("PathCurrentCreature", JD.map ComLPathCurrentCreature (JD.list point3Decoder))
  , ("ChangeCreatureInitiative", JD.map2 ComLChangeCreatureInitiative (JD.index 0 JD.string) (JD.index 1 JD.int))
  ]

type CreatureLog
  = CLDamage Int (List Int)
  | CLHeal Int (List Int)
  | CLGenerateEnergy Int
  | CLReduceEnergy Int
  | CLApplyCondition Int ConditionDuration Condition
  | CLRemoveCondition Int
  | CLSetPos Point3
  | CLDecrementConditionRemaining Int
  | CLSetNote String

creatureLogDecoder = sumDecoder "CreatureLog"
  []
  [ ("Damage", JD.map2 CLDamage (JD.index 0 JD.int) (JD.index 1 (JD.list JD.int)))
  , ("Heal", JD.map2 CLHeal (JD.index 0 JD.int) (JD.index 1 (JD.list JD.int)))
  , ("GenerateEnergy", JD.map CLGenerateEnergy JD.int)
  , ("ReduceEnergy", JD.map CLReduceEnergy JD.int)
  , ("ApplyCondition", JD.map3 CLApplyCondition (JD.index 0 JD.int) (JD.index 1 conditionDurationDecoder) (JD.index 2conditionDecoder))
  , ("RemoveCondition", JD.map CLRemoveCondition JD.int)
  , ("SetPos", JD.map CLSetPos point3Decoder)
  , ("DecrementConditionRemaining", JD.map CLDecrementConditionRemaining JD.int)
  , ("SetNote", JD.map CLSetNote JD.string)
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
    |> P.required "current_combat" (JD.oneOf [JD.map Just combatDecoder, JD.null Nothing])
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
  , conditions: Dict Int AppliedCondition
  , can_act: Bool
  , can_move: Bool
  , note: String
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
    |> P.required "conditions" (JD.andThen stringKeyDictToIntKeyDict (JD.dict appliedConditionDecoder))
    |> P.required "can_act" JD.bool
    |> P.required "can_move" JD.bool
    |> P.required "note" JD.string


stringKeyDictToIntKeyDict : Dict String a -> JD.Decoder (Dict Int a)
stringKeyDictToIntKeyDict d =
  let foldFunc : String -> a -> Result String (Dict Int a) -> Result String (Dict Int a)
      foldFunc key value accumulator =
        case accumulator of
          Err x -> Err x
          Ok intMap -> 
            case (String.toInt key) of
              Ok intKey -> Ok (Dict.insert intKey value intMap)
              Err x -> Err x
      result = Dict.foldl foldFunc (Ok (Dict.fromList [])) d
  in case result of
      Err x -> JD.fail x
      Ok y -> JD.succeed y

type alias Ability =
  { name : String
  , target: TargetSpec
  }

abilityDecoder = JD.map2 Ability
  (JD.field "name" JD.string)
  (JD.field "target" targetSpecDecoder)

type TargetSpec
  = Melee
  | Range Distance
  | Actor

targetSpecDecoder = sumDecoder "TargetSpec"
  [ ("Melee", Melee)
  , ("Actor", Actor)]
  [("Range", JD.map Range JD.int)]

type DecidedTarget
  = DecidedMelee CreatureID
  | DecidedRange CreatureID
  | DecidedActor

decidedTargetEncoder dt = case dt of
  DecidedMelee cid -> JE.object [("Melee", JE.string cid)]
  DecidedRange cid -> JE.object [("Range", JE.string cid)]
  DecidedActor -> JE.string "Actor"

type alias AbilityStatus = { ability_id: AbilityID, cooldown: Int }

abilityStatusDecoder = JD.map2 AbilityStatus
                                 (JD.field "ability_id" JD.string)
                                 (JD.field "cooldown" JD.int)
abilityStatusEncoder {ability_id, cooldown} =
  JE.object [ ("ability_id", JE.string ability_id)
            , ("cooldown", JE.int cooldown)
            ]

type alias AppliedCondition =
  { remaining: ConditionDuration
  , condition: Condition
  }

appliedConditionDecoder =
  JD.map2 AppliedCondition
    (JD.field "remaining" conditionDurationDecoder)
    (JD.field "condition" conditionDecoder)

type ConditionDuration
  = Interminate
  | Duration Int

conditionDurationDecoder = sumDecoder "ConditionDuration"
  [("Interminate", Interminate)]
  [("Duration", JD.map Duration JD.int)]

conditionDurationEncoder cd =
  case cd of
    Interminate -> JE.string "Interminate"
    Duration num -> JE.object [("Duration", JE.int num)]

type Condition
  = RecurringEffect Effect
  | Dead
  | Incapacitated
  | AddDamageBuff Int
  | DoubleMaxMovement

conditionDecoder = sumDecoder "Condition"
  [ ("Dead", Dead)
  , ("Incapacitated", Incapacitated)
  , ("DoubleMaxMovement", DoubleMaxMovement)]
  [ ("RecurringEffect", JD.map RecurringEffect lazyEffectDecoder)
  , ("AddDamageBuff", JD.map AddDamageBuff JD.int)]

-- conditionEncoder condition =
--   case condition of
--     Dead -> JE.string "Dead"
--     Incapacitated -> JE.string "Incapacitated"
--     AddDamageBuff num -> JE.object [("AddDamageBuff", JE.int num)]
--     RecurringEffect eff -> JE.object [("RecurringEffect", effectEncoder eff)]
--     DoubleMaxMovement -> 

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
-- effectEncoder eff =
--   case eff of
--     Heal num -> JE.object [("Heal", JE.int num)]
--     Damage num -> JE.object [("Damage", JE.int num)]
--     GenerateEnergy num -> JE.object [("GenerateEnergy", JE.int num)]
--     MultiEffect effs -> JE.object [("MultiEffect", JE.list (List.map effectEncoder effs))]
--     ApplyCondition cd cond -> JE.object [("ApplyCondition", JE.list [ conditionDurationEncoder cd
--                                                                     , conditionEncoder cond])]


-- GameCommand represents all possible mutating commands sent to the RPI
type GameCommand
  = SelectMap MapName
  | EditMap MapName Map
  | RegisterPlayer PlayerID
  | GiveCreaturesToPlayer PlayerID (List CreatureID)
  | StartCombat (List CreatureID)
  | StopCombat
  | CombatAct AbilityID DecidedTarget
  | ActCreature CreatureID AbilityID DecidedTarget
  | PathCurrentCombatCreature Point3
  | PathCreature CreatureID Point3
  | SetCreaturePos CreatureID Point3
  | CreateCreature CreatureCreation
  | RemoveCreature CreatureID
  | AddCreatureToCombat CreatureID
  | RemoveCreatureFromCombat CreatureID
  -- RetrieveFromInventory(ThingID)
  -- StowInInventory(ThingID)
  | Done
  | Rollback Int Int
  | ChangeCreatureInitiative CreatureID Int
  | SetCreatureNote CreatureID String

gameCommandEncoder : GameCommand -> JE.Value
gameCommandEncoder gc =
  case gc of
    RegisterPlayer pid -> JE.object [("RegisterPlayer", JE.string pid)]
    GiveCreaturesToPlayer pid cids -> JE.object [("GiveCreaturesToPlayer", JE.list [JE.string pid, JE.list (List.map JE.string cids)])]
    StartCombat cids -> JE.object [("StartCombat", JE.list (List.map JE.string cids))]
    CreateCreature creature -> JE.object [("CreateCreature", creatureCreationEncoder creature)]
    RemoveCreature cid -> JE.object [("RemoveCreature", JE.string cid)]
    StopCombat -> JE.string "StopCombat"
    PathCurrentCombatCreature pt -> JE.object [("PathCurrentCombatCreature", point3Encoder pt)]
    PathCreature cid pt -> JE.object [("PathCreature", JE.list [JE.string cid, point3Encoder pt])]
    SetCreaturePos cid pt -> JE.object [("SetCreaturePos", JE.list [JE.string cid, point3Encoder pt])]
    AddCreatureToCombat cid -> JE.object [("AddCreatureToCombat", JE.string cid)]
    RemoveCreatureFromCombat cid -> JE.object [("RemoveCreatureFromCombat", JE.string cid)]
    CombatAct abid dtarget -> JE.object [("CombatAct", JE.list [JE.string abid, decidedTargetEncoder dtarget])]
    ActCreature cid abid dtarget -> JE.object [("ActCreature", JE.list [JE.string cid, JE.string abid, decidedTargetEncoder dtarget])]
    Done -> JE.string "Done"
    SelectMap name -> JE.object [("SelectMap", JE.string name)]
    EditMap name terrain -> JE.object [("EditMap", JE.list [JE.string name, mapEncoder terrain])]
    Rollback snapIdx logIdx -> JE.object [("Rollback", JE.list [JE.int snapIdx, JE.int logIdx])]
    ChangeCreatureInitiative cid newPos -> JE.object [("ChangeCreatureInitiative", JE.list [JE.string cid, JE.int newPos])]
    SetCreatureNote cid note -> JE.object [("SetCreatureNote", JE.list [JE.string cid, JE.string note])]


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

playerIsRegistered : App -> PlayerID -> Bool
playerIsRegistered app pid = Dict.member pid app.players

getPlayerCreatures : App -> PlayerID -> List Creature
getPlayerCreatures app pid =
  -- this sucks because it doesn't throw any kind of error when PID or CID aren't found
  List.filterMap (findCreature app.current_game)
                 (Set.toList (Maybe.withDefault Set.empty (Dict.get pid app.players)))

isCreatureInCombat : Game -> CreatureID -> Bool
isCreatureInCombat game cid =
  case game.current_combat of
    Nothing -> False
    Just combat ->
      case listFind (\c -> c.id == cid) combat.creatures.data of
        Just _ -> True
        Nothing -> False

isCreatureOOC : Game -> CreatureID -> Bool
isCreatureOOC game cid = Dict.member cid game.creatures

mostRecentLog : App -> Maybe GameLog
mostRecentLog app =
  let last = Array.get ((Array.length app.snapshots) - 1) app.snapshots
  in case last of
    Just (game, logs) -> List.head (List.reverse logs)
    _ -> Nothing
