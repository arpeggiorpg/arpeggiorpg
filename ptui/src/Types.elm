module Types exposing (..)

import Array
import Dict exposing (Dict(..))
import List.Extra
import Maybe exposing (withDefault)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as P
import Json.Helpers as JH
import Set


type alias Color = String
type alias SceneID = String
type alias PlayerID = String
type alias CreatureID = String
type alias AbilityID = String
type alias MapID = String
type alias Distance = Int
type alias AttrID = String

type alias CreatureCreation =
  { name : String
  , class: String
  , portrait_url: String
  }

creatureCreationEncoder : CreatureCreation -> JE.Value
creatureCreationEncoder cc = JE.object
  [ ("name", JE.string cc.name)
  , ("class", JE.string cc.class)
  , ("portrait_url", JE.string cc.portrait_url)]

type alias Point3 = {x: Int, y: Int, z: Int}
type alias Point3Tup = (Int, Int, Int)

point3Decoder : JD.Decoder Point3
point3Decoder = JD.map3 Point3 (JD.index 0 JD.int) (JD.index 1 JD.int) (JD.index 2 JD.int)
point3Encoder : Point3 -> JE.Value
point3Encoder {x, y, z} =  JE.list [JE.int x, JE.int y, JE.int z]


point3ToTup : Point3 -> Point3Tup
point3ToTup {x, y, z} = (x, y, z)

tupToPoint3 : Point3Tup -> Point3
tupToPoint3 (x, y, z) = {x=x, y=y, z=z}

point3TupEncoder : Point3Tup -> JE.Value
point3TupEncoder = tupToPoint3 >> point3Encoder

type PotentialTargets
  = PTCreatureIDs (List CreatureID)
  | PTPoints (List Point3)

potentialTargetsDecoder : JD.Decoder PotentialTargets
potentialTargetsDecoder = sumDecoder "PotentialTargets"
  []
  [ ("CreatureIDs", JD.map PTCreatureIDs (JD.list JD.string))
  , ("Points", JD.map PTPoints (JD.list point3Decoder))
  ]

type alias App =
  { current_game: Game
  , snapshots: Array.Array (GameSnapshot, (Array.Array GameLog))
  , raw_snapshots: JE.Value
  , players: Dict PlayerID Player
  }

appDecoder : JD.Decoder App
appDecoder = JD.map4 App
  (JD.field "current_game" gameDecoder)
  (JD.field "snapshots" (JD.array (JD.map2 (,) (JD.index 0 gameSnapshotDecoder) (JD.index 1 <| JD.array gameLogDecoder))))
  (JD.field "snapshots" JD.value)
  (JD.field "players" (JD.dict playerDecoder))


type alias Player =
  { player_id: PlayerID
  , scene: Maybe SceneID
  , creatures: Set.Set CreatureID}

playerDecoder : JD.Decoder Player
playerDecoder =
  JD.map3 Player
    (JD.field "player_id" JD.string)
    (JD.field "scene" (JD.oneOf [JD.map Just JD.string, JD.null Nothing]))
    (JD.field "creatures" (setDecoder JD.string))

type alias GameSnapshot = {}

gameSnapshotDecoder : JD.Decoder GameSnapshot
gameSnapshotDecoder = JD.succeed {}

type GameLog
  = GLCreateFolder FolderPath
  | GLRenameFolder FolderPath String
  | GLDeleteFolder FolderPath
  | GLMoveFolderItem FolderPath FolderItemID FolderPath
  | GLCreateNote FolderPath Note
  | GLEditNote FolderPath String Note
  | GLDeleteNote FolderPath String
  | GLCreateScene FolderPath Scene
  | GLEditScene Scene
  | GLDeleteScene SceneID
  | GLCreateMap FolderPath Map
  | GLEditMap Map
  | GLDeleteMap MapID
  | GLCombatLog CombatLog
  | GLCreatureLog CreatureID CreatureLog
  | GLStartCombat SceneID (List (CreatureID, Int))
  | GLStopCombat
  | GLCreateCreature FolderPath CreatureData
  | GLEditCreature CreatureData
  | GLDeleteCreature CreatureID
  | GLAddCreatureToCombat CreatureID
  | GLRemoveCreatureFromCombat CreatureID
  | GLRollback Int Int
  | GLPathCreature SceneID CreatureID (List Point3)
  | GLSetCreaturePos SceneID CreatureID Point3
  | GLAttributeCheckResult CreatureID AttrCheck Int Bool

gameLogDecoder : JD.Decoder GameLog
gameLogDecoder = sumDecoder "GameLog"
  [ ("StopCombat", GLStopCombat) ]
  [ ("CreateFolder", JD.map GLCreateFolder folderPathDecoder)
  , ("RenameFolder", fixedList2 GLRenameFolder folderPathDecoder JD.string)
  , ("DeleteFolder", JD.map GLDeleteFolder folderPathDecoder)
  , ("MoveFolderItem", fixedList3 GLMoveFolderItem folderPathDecoder folderItemIDDecoder folderPathDecoder)
  , ("CreateNote", fixedList2 GLCreateNote folderPathDecoder noteDecoder)
  , ("EditNote", fixedList3 GLEditNote folderPathDecoder JD.string noteDecoder)
  , ("DeleteNote", fixedList2 GLDeleteNote folderPathDecoder JD.string)
  , ("CreateScene", fixedList2 GLCreateScene folderPathDecoder sceneDecoder)
  , ("EditScene", JD.map GLEditScene sceneDecoder)
  , ("DeleteScene", JD.map GLDeleteScene JD.string)
  , ("CombatLog", JD.map GLCombatLog combatLogDecoder)
  , ("CreatureLog", fixedList2 GLCreatureLog JD.string creatureLogDecoder)
  , ("StartCombat", fixedList2 GLStartCombat JD.string initiativeListDecoder)
  , ("CreateCreature", fixedList2 GLCreateCreature folderPathDecoder creatureDataDecoder)
  , ("EditCreature", JD.map GLEditCreature creatureDataDecoder)
  , ("DeleteCreature", JD.map GLDeleteCreature JD.string)
  , ("AddCreatureToCombat", JD.map GLAddCreatureToCombat JD.string)
  , ("RemoveCreatureFromCombat", JD.map GLRemoveCreatureFromCombat JD.string)
  , ("CreateMap", fixedList2 GLCreateMap folderPathDecoder mapDecoder)
  , ("EditMap", JD.map GLEditMap mapDecoder)
  , ("DeleteMap", JD.map GLDeleteMap JD.string)
  , ("Rollback", fixedList2 GLRollback JD.int JD.int)
  , ("PathCreature", fixedList3 GLPathCreature JD.string JD.string (JD.list point3Decoder))
  , ("SetCreaturePos", fixedList3 GLSetCreaturePos JD.string JD.string point3Decoder)
  , ("AttributeCheckResult", fixedList4 GLAttributeCheckResult JD.string attrCheckDecoder JD.int JD.bool)
  ]

initiativeListDecoder : JD.Decoder (List (CreatureID, Int))
initiativeListDecoder = JD.list initiativeEntryDecoder

initiativeEntryDecoder : JD.Decoder (CreatureID, Int)
initiativeEntryDecoder = JD.map2 (,) (JD.index 0 JD.string) (JD.index 1 JD.int)


fixedList2 : (a -> b -> c) -> JD.Decoder a -> JD.Decoder b -> JD.Decoder c
fixedList2 cons d0 d1 = JD.map2 cons (JD.index 0 d0) (JD.index 1 d1)
fixedList3 : (a -> b -> c -> d) -> JD.Decoder a -> JD.Decoder b -> JD.Decoder c -> JD.Decoder d
fixedList3 cons d0 d1 d2 = JD.map3 cons (JD.index 0 d0) (JD.index 1 d1) (JD.index 2 d2)
fixedList4 : (a -> b -> c -> d -> e) -> JD.Decoder a -> JD.Decoder b -> JD.Decoder c -> JD.Decoder d -> JD.Decoder e
fixedList4 cons d0 d1 d2 d3 = JD.map4 cons (JD.index 0 d0) (JD.index 1 d1) (JD.index 2 d2) (JD.index 3 d3)
fixedList5 : (a -> b -> c -> d -> e -> f) -> JD.Decoder a -> JD.Decoder b -> JD.Decoder c -> JD.Decoder d -> JD.Decoder e -> JD.Decoder f
fixedList5 cons d0 d1 d2 d3 d4 = JD.map5 cons (JD.index 0 d0) (JD.index 1 d1) (JD.index 2 d2) (JD.index 3 d3) (JD.index 4 d4)

type CombatLog
  = ComLEndTurn CreatureID
  | ComLChangeCreatureInitiative CreatureID Int
  | ComLConsumeMovement Int
  | ComLRerollInitiative (List (CreatureID, Int))
  | ComLForceNextTurn
  | ComLForcePrevTurn

combatLogDecoder : JD.Decoder CombatLog
combatLogDecoder = sumDecoder "CombatLog"
  [("ForceNextTurn", ComLForceNextTurn)
  , ("ForcePrevTurn", ComLForcePrevTurn)]
  [ ("EndTurn", JD.map ComLEndTurn JD.string)
  , ("ChangeCreatureInitiative", JD.map2 ComLChangeCreatureInitiative (JD.index 0 JD.string) (JD.index 1 JD.int))
  , ("ConsumeMovement", JD.map ComLConsumeMovement (JD.int))
  , ("RerollInitiative", JD.map ComLRerollInitiative initiativeListDecoder)
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

creatureLogDecoder : JD.Decoder CreatureLog
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
  , maps: Dict MapID Map
  , scenes: Dict String Scene
  , campaign: Folder
  }

gameDecoder : JD.Decoder Game
gameDecoder =
  P.decode Game
    |> P.required "current_combat" (maybeDecoder combatDecoder)
    |> P.required "abilities" (JD.dict abilityDecoder)
    |> P.required "classes" (JD.dict classDecoder)
    |> P.required "creatures" (JD.dict creatureDecoder)
    |> P.required "maps" (JD.dict mapDecoder)
    |> P.required "scenes" (JD.dict sceneDecoder)
    |> P.required "campaign" folderDecoder

type Folder =
  Folder
    { data: FolderNode
    , children: Dict.Dict String Folder}

folderDecoderHelper : FolderNode -> Dict.Dict String Folder -> Folder
folderDecoderHelper d c = Folder {data = d, children = c}

folderDecoder : JD.Decoder Folder
folderDecoder =
  JD.map2 folderDecoderHelper
    (JD.field "data" folderNodeDecoder)
    (JD.field "children" <| JD.dict (JD.lazy (\_ -> folderDecoder)))


type alias FolderNode =
  { scenes: Set.Set SceneID
  , creatures: Set.Set CreatureID
  , notes: Dict.Dict String Note
  , maps: Set.Set MapID
  }

folderNodeDecoder : JD.Decoder FolderNode
folderNodeDecoder =
  JD.map4 FolderNode
    (JD.field "scenes" (setDecoder JD.string))
    (JD.field "creatures" (setDecoder JD.string))
    (JD.field "notes" (JD.dict noteDecoder))
    (JD.field "maps" (setDecoder JD.string))

type alias FolderPath = List String

folderPathDecoder : JD.Decoder FolderPath
folderPathDecoder =
  JD.string
  |> JD.andThen (\s -> case folderPathFromString s of
                         Just x -> JD.succeed x
                         Nothing -> JD.fail ("Couldn't parse path: " ++ s))

folderPathEncoder : FolderPath -> JE.Value
folderPathEncoder p = JE.string (folderPathToString p)

type alias Note =
  { name: String
  , content: String
  }

noteDecoder : JD.Decoder Note
noteDecoder = JD.map2 Note
  (JD.field "name" JD.string)
  (JD.field "content" JD.string)

noteEncoder : Note -> JE.Value
noteEncoder note = JE.object [("name", JE.string note.name), ("content", JE.string note.content)]

type alias Scene =
  { id: SceneID
  , name: String
  , map: String
  , creatures: Dict CreatureID (Point3, Visibility)
  , attribute_checks: Dict String AttrCheck
  }

sceneDecoder : JD.Decoder Scene
sceneDecoder =
  JD.map5 Scene
    (JD.field "id" JD.string)
    (JD.field "name" JD.string)
    (JD.field "map" JD.string)
    (JD.field "creatures" (JD.dict (JD.map2 (,) (JD.index 0 point3Decoder) (JD.index 1 visibilityDecoder))))
    (JD.field "attribute_checks" (JD.dict attrCheckDecoder))

sceneEncoder : Scene -> JE.Value
sceneEncoder scene =
  let
    encCreature (key, (pos, vis)) = (key, JE.list [point3Encoder pos, visibilityEncoder vis])
    encCreatures = List.map encCreature (Dict.toList scene.creatures)
    encAttrCheck (key, sc) = (key, attrCheckEncoder sc)
    encAttrChecks = List.map encAttrCheck (Dict.toList scene.attribute_checks)
  in
  JE.object
    [ ("id", JE.string scene.id)
    , ("name", JE.string scene.name)
    , ("map", JE.string scene.map)
    , ("creatures", JE.object encCreatures)
    , ("attribute_checks", JE.object encAttrChecks)
    ]

type alias AttrCheck =
  { reliable: Bool
  , attr: AttrID
  , target: SkillLevel
  }

attrCheckEncoder : AttrCheck -> JE.Value
attrCheckEncoder sc = JE.object
  [ ("reliable", JE.bool sc.reliable)
  , ("attr", JE.string sc.attr)
  , ("target", skillLevelEncoder sc.target)
  ]

attrCheckDecoder : JD.Decoder AttrCheck
attrCheckDecoder = JD.map3 AttrCheck
  (JD.field "reliable" JD.bool)
  (JD.field "attr" JD.string)
  (JD.field "target" skillLevelDecoder)

type Visibility
  = GMOnly
  | AllPlayers

visibilityEncoder : Visibility -> JE.Value
visibilityEncoder v =
  case v of
    GMOnly -> JE.string "GMOnly"
    AllPlayers -> JE.string "AllPlayers"

visibilityDecoder : JD.Decoder Visibility
visibilityDecoder = sumDecoder "Visibility"
  [ ("GMOnly", GMOnly)
  , ("AllPlayers", AllPlayers)]
  []

type alias SceneCreation =
  { name: String
  , map: String
  }

sceneCreationEncoder : SceneCreation -> JE.Value
sceneCreationEncoder scene =
  JE.object
    [ ("name", JE.string scene.name)
    , ("map", JE.string scene.map)
    ]


type alias Map =
  { id: MapID
  , name: String
  , terrain: Set.Set (Int, Int, Int)
  , specials: Dict.Dict (Int, Int, Int) (Color, String, Visibility)}

rustMap2BetterMap : RustMap -> Map
rustMap2BetterMap map =
  let specialKV (pt, color, note, vis) = (point3ToTup pt, (color, note, vis))
  in { id = map.id
  , name = map.name
  , terrain = Set.fromList (List.map point3ToTup map.terrain)
  , specials = Dict.fromList (List.map specialKV map.specials)
  }

type alias RustMap =
  { id: MapID
  , name: String
  , terrain: List Point3
  , specials: List (Point3, Color, String, Visibility)
  }

rustMapDecoder : JD.Decoder RustMap
rustMapDecoder = JD.map4 RustMap
  (JD.field "id" JD.string)
  (JD.field "name" JD.string)
  (JD.field "terrain" (JD.list point3Decoder))
  (JD.field "specials" (JD.list (tuple4 point3Decoder JD.string JD.string visibilityDecoder)))

mapDecoder : JD.Decoder Map
mapDecoder = rustMapDecoder |> JD.map rustMap2BetterMap

tuple4 : JD.Decoder a -> JD.Decoder b -> JD.Decoder c -> JD.Decoder d -> JD.Decoder (a, b, c, d)
tuple4 da db dc dd =
  JD.map4 (\a b c d -> (a, b, c, d)) (JD.index 0 da) (JD.index 1 db) (JD.index 2 dc) (JD.index 3 dd)

mapEncoder : Map -> JE.Value
mapEncoder m =
  let specialEntryEncoder (pos, (color, note, vis)) =
        JE.list [point3Encoder (tupToPoint3 pos), JE.string color, JE.string note, visibilityEncoder vis]
  in
  JE.object
    [ ("id", JE.string m.id)
    , ("name", JE.string m.name)
    , ("terrain", JE.list (List.map point3TupEncoder (Set.toList m.terrain)))
    , ("specials", JE.list (List.map specialEntryEncoder (Dict.toList m.specials)))
    ]

type alias MapCreation =
  { name: String
  , terrain: List Point3
  }

mapCreationEncoder : MapCreation -> JE.Value
mapCreationEncoder m =
  JE.object
    [ ("name", JE.string m.name)
    , ("terrain", JE.list (List.map point3Encoder m.terrain))
    ]

type alias Class =
  { abilities: List AbilityID
  , conditions: List Condition
  , color: String
  }

classDecoder : JD.Decoder Class
classDecoder = JD.map3 Class
  (JD.field "abilities" (JD.list JD.string))
  (JD.field "conditions" (JD.list conditionDecoder))
  (JD.field "color" JD.string)

type alias Combat =
  { scene: SceneID
  , creatures: CursorList (CreatureID, Int)
  , movement_used: Int
  }

combatDecoder : JD.Decoder Combat
combatDecoder =
  JD.map3 Combat
    (JD.field "scene" JD.string)
    (JD.field "creatures" (cursorListDecoder initiativeEntryDecoder))
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
  , initiative: Dice
  , max_energy: Int
  , cur_energy: Int
  , max_health: Int
  , cur_health: Int
  , abilities: Dict AbilityID AbilityStatus
  , class: String
  , conditions: Dict Int AppliedCondition
  , can_act: Bool
  , can_move: Bool
  , note: String
  , portrait_url: String
  , attributes: Dict AttrID SkillLevel
}

creatureDecoder : JD.Decoder Creature
creatureDecoder =
  P.decode Creature
    |> P.required "id" JD.string
    |> P.required "name" JD.string
    |> P.required "speed" JD.int
    |> P.required "initiative" diceDecoder
    |> P.required "max_energy" JD.int
    |> P.required "cur_energy" JD.int
    |> P.required "max_health" JD.int
    |> P.required "cur_health" JD.int
    |> P.required "abilities" (JD.dict abilityStatusDecoder)
    |> P.required "class" JD.string
    |> P.required "conditions" (JD.andThen stringKeyDictToIntKeyDict (JD.dict appliedConditionDecoder))
    |> P.required "can_act" JD.bool
    |> P.required "can_move" JD.bool
    |> P.required "note" JD.string
    |> P.required "portrait_url" JD.string
    |> P.required "attributes" (JD.dict skillLevelDecoder)

creatureEncoder : Creature -> JE.Value
creatureEncoder c =
  -- NOTE! can_act and can_move aren't included here intentionally, they are calculated properties
  -- generated when the server sends a creature to us
  JE.object
    [ ("id", JE.string c.id)
    , ("name", JE.string c.name)
    , ("speed", JE.int c.speed)
    , ("initiative", diceEncoder c.initiative)
    , ("max_energy", JE.int c.max_energy)
    , ("cur_energy", JE.int c.cur_energy)
    , ("max_health", JE.int c.max_health)
    , ("cur_health", JE.int c.cur_health)
    , ("abilities", encodeStringDict abilityStatusEncoder c.abilities)
    , ("class", JE.string c.class)
    , ("conditions", JE.object <| List.map (\(k, v) -> (toString k, appliedConditionEncoder v)) (Dict.toList c.conditions))
    , ("note", JE.string c.note)
    , ("portrait_url", JE.string c.portrait_url)
    , ("attributes", encodeStringDict skillLevelEncoder c.attributes)
    ]

encodeStringDict : (a -> JE.Value) -> Dict.Dict String a -> JE.Value
encodeStringDict vEncoder d = JE.object <| List.map (\(k, v) -> (k, vEncoder v)) (Dict.toList d)

type Dice
  = DiceExpr {num: Int, size: Int}
  | DicePlus Dice Dice
  | DiceFlat Int

exprHelper : Int -> Int -> Dice
exprHelper n s = DiceExpr { num=n, size=s}

diceDecoder : JD.Decoder Dice
diceDecoder = sumDecoder "Dice" []
  [ ("Expr", JD.map2 exprHelper (JD.field "num" JD.int) (JD.field "size" JD.int))
  , ("Plus", JD.map2 DicePlus (JD.index 0 (JD.lazy (\_ -> diceDecoder)))
                              (JD.index 1 (JD.lazy (\_ -> diceDecoder))))
  , ("Flat", JD.map DiceFlat JD.int)
  ]


diceEncoder : Dice -> JE.Value
diceEncoder dice = case dice of
  DiceExpr {num, size} ->
    JE.object [("Expr", JE.object [("num", JE.int num), ("size", JE.int size)])]
  DicePlus l r -> JE.object [("Plus", JE.list [diceEncoder l, diceEncoder r])]
  DiceFlat v -> JE.object [("Flat", JE.int v)]

-- This is separate from Creature beacuse GameLogs don't have the "calculated" properties
-- (e.g. can_act and can_move), so we can't just use Creature.
type alias CreatureData =
  { id: CreatureID
  , name: String
  , class: String
  , note: String
}

creatureDataDecoder : JD.Decoder CreatureData
creatureDataDecoder =
  P.decode CreatureData
    |> P.required "id" JD.string
    |> P.required "name" JD.string
    |> P.required "class" JD.string
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
  , usable_ooc: Bool
  }

abilityDecoder : JD.Decoder Ability
abilityDecoder = JD.map3 Ability
  (JD.field "name" JD.string)
  (JD.field "target" targetSpecDecoder)
  (JD.field "usable_ooc" JD.bool)

type TargetSpec
  = Melee
  | Range Distance
  | Actor
  | AllCreaturesInVolumeInRange {volume: Volume, range: Distance}

acivir vol r = AllCreaturesInVolumeInRange {volume=vol, range=r}

targetSpecDecoder : JD.Decoder TargetSpec
targetSpecDecoder = sumDecoder "TargetSpec"
  [ ("Melee", Melee)
  , ("Actor", Actor)]
  [ ("Range", JD.map Range JD.int)
  , ("AllCreaturesInVolumeInRange",
     JD.map2 acivir (JD.field "volume" volumeDecoder) (JD.field "range" JD.int))
  ]

type Volume
  = Sphere Distance
  -- Line(Distance),
  -- VerticalCylinder { radius: Distance, height: Distance },

volumeDecoder : JD.Decoder Volume
volumeDecoder = sumDecoder "Volume"
  []
  [("Sphere", JD.map Sphere JD.int)]

volumeEncoder : Volume -> JE.Value
volumeEncoder v = case v of
  Sphere d -> JE.object [("Sphere", JE.int d)]

type DecidedTarget
  = TargetedCreature CreatureID
  | TargetedActor
  | TargetedCreatures (List CreatureID)
  | TargetedPoint Point3

decidedTargetEncoder : DecidedTarget -> JE.Value
decidedTargetEncoder dt = case dt of
  TargetedCreature cid -> JE.object [("Creature", JE.string cid)]
  TargetedActor -> JE.string "Actor"
  TargetedCreatures cids -> JE.object [("Creatures", JE.list (List.map JE.string cids))]
  TargetedPoint pt -> JE.object [("Point", point3Encoder pt)]

type alias AbilityStatus = { ability_id: AbilityID, cooldown: Int }

abilityStatusDecoder : JD.Decoder AbilityStatus
abilityStatusDecoder = JD.map2 AbilityStatus
                                 (JD.field "ability_id" JD.string)
                                 (JD.field "cooldown" JD.int)

abilityStatusEncoder : AbilityStatus -> JE.Value
abilityStatusEncoder {ability_id, cooldown} =
  JE.object [ ("ability_id", JE.string ability_id)
            , ("cooldown", JE.int cooldown)
            ]

type alias AppliedCondition =
  { remaining: ConditionDuration
  , condition: Condition
  }

appliedConditionDecoder : JD.Decoder AppliedCondition
appliedConditionDecoder =
  JD.map2 AppliedCondition
    (JD.field "remaining" conditionDurationDecoder)
    (JD.field "condition" conditionDecoder)

appliedConditionEncoder : AppliedCondition -> JE.Value
appliedConditionEncoder ac =
  JE.object
    [ ("remaining", conditionDurationEncoder ac.remaining)
    , ("condition", conditionEncoder ac.condition)]

type ConditionDuration
  = Interminate
  | Duration Int

conditionDurationDecoder : JD.Decoder ConditionDuration
conditionDurationDecoder = sumDecoder "ConditionDuration"
  [("Interminate", Interminate)]
  [("Duration", JD.map Duration JD.int)]

conditionDurationEncoder : ConditionDuration -> JE.Value
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
  | ActivateAbility AbilityID

conditionDecoder : JD.Decoder Condition
conditionDecoder = sumDecoder "Condition"
  [ ("Dead", Dead)
  , ("Incapacitated", Incapacitated)
  , ("DoubleMaxMovement", DoubleMaxMovement)]
  [ ("RecurringEffect", JD.map RecurringEffect lazyEffectDecoder)
  , ("AddDamageBuff", JD.map AddDamageBuff JD.int)
  , ("ActivateAbility", JD.map ActivateAbility JD.string)]

conditionEncoder : Condition -> JE.Value
conditionEncoder c = case c of
  RecurringEffect eff -> JE.object [("RecurringEffect", effectEncoder eff)]
  Dead -> JE.string "Dead"
  Incapacitated -> JE.string "Incapacitated"
  AddDamageBuff buff -> JE.object [("AddDamageBuff", JE.int buff)]
  DoubleMaxMovement -> JE.string "DoubleMaxMovement"
  ActivateAbility abid -> JE.object [("ActivateAbility", JE.string abid)]


type Effect
  = ApplyCondition ConditionDuration Condition
  | Heal Int
  | Damage Int
  | MultiEffect (List Effect)
  | GenerateEnergy Int

lazyEffectDecoder : JD.Decoder Effect
lazyEffectDecoder = JD.lazy (\_ -> effectDecoder)

effectDecoder : JD.Decoder Effect
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

effectEncoder : Effect -> JE.Value
effectEncoder eff = case eff of
  ApplyCondition cd c ->
    JE.object [("ApplyCondition", JE.list [conditionDurationEncoder cd, conditionEncoder c])]
  Heal n -> JE.object [("Heal", JE.int n)]
  Damage n -> JE.object [("Damage", JE.int n)]
  MultiEffect l -> JE.object [("MultiEffect", JE.list (List.map effectEncoder l))]
  GenerateEnergy n -> JE.object [("GenerateEnergy", JE.int n)]


-- GameCommand represents all possible mutating commands sent to the RPI
type GameCommand
  = CreateFolder FolderPath
  | RenameFolder FolderPath String
  | DeleteFolder FolderPath
  | MoveFolderItem FolderPath FolderItemID FolderPath
  | CreateNote FolderPath Note
  | EditNote FolderPath String Note
  | DeleteNote FolderPath String
  | CreateScene FolderPath SceneCreation
  | EditScene Scene
  | DeleteScene SceneID
  | CreateMap FolderPath MapCreation
  | EditMap Map
  | DeleteMap MapID
  | RegisterPlayer PlayerID
  | GiveCreaturesToPlayer PlayerID (List CreatureID)
  | SetPlayerScene PlayerID (Maybe SceneID)
  | StartCombat SceneID (List CreatureID)
  | StopCombat
  | ChangeCreatureInitiative CreatureID Int
  | RerollCombatInitiative
  | ForceNextTurn
  | ForcePrevTurn
  | CombatAct AbilityID DecidedTarget
  | ActCreature SceneID CreatureID AbilityID DecidedTarget
  | PathCurrentCombatCreature Point3
  | PathCreature SceneID CreatureID Point3
  | SetCreaturePos SceneID CreatureID Point3
  | CreateCreature FolderPath CreatureCreation
  | EditCreature Creature
  | DeleteCreature CreatureID
  | AddCreatureToCombat CreatureID
  | RemoveCreatureFromCombat CreatureID
  | Done
  | Rollback Int Int
  | AttributeCheck CreatureID AttrCheck

gameCommandEncoder : GameCommand -> JE.Value
gameCommandEncoder gc =
  case gc of
    CreateFolder path ->
      JE.object [("CreateFolder", folderPathEncoder path)]
    RenameFolder path newName ->
      JE.object [("RenameFolder", JE.list [folderPathEncoder path, JE.string newName])]
    DeleteFolder path ->
      JE.object [("DeleteFolder", folderPathEncoder path)]
    MoveFolderItem src item dst ->
      JE.object [("MoveFolderItem", JE.list [folderPathEncoder src, folderItemIDEncoder item, folderPathEncoder dst])]
    CreateNote path note ->
      JE.object [("CreateNote", JE.list [folderPathEncoder path, noteEncoder note])]
    EditNote path name note ->
      JE.object [("EditNote", JE.list [folderPathEncoder path, JE.string name, noteEncoder note])]
    DeleteNote path name ->
      JE.object [("DeleteNote", JE.list [folderPathEncoder path, JE.string name])]
    CreateScene path sc ->
      JE.object [("CreateScene", JE.list [folderPathEncoder path, sceneCreationEncoder sc])]
    EditScene scene ->
      JE.object [("EditScene", sceneEncoder scene)]
    DeleteScene sid ->
      JE.object [("DeleteScene", JE.string sid)]
    RegisterPlayer pid ->
      JE.object [("RegisterPlayer", JE.string pid)]
    GiveCreaturesToPlayer pid cids ->
      JE.object [("GiveCreaturesToPlayer", JE.list [JE.string pid, JE.list (List.map JE.string cids)])]
    SetPlayerScene pid scene ->
      JE.object [("SetPlayerScene", JE.list [JE.string pid, encodeMaybe scene JE.string])]
    StartCombat scene cids ->
      JE.object [("StartCombat", JE.list [JE.string scene, JE.list (List.map JE.string cids)])]
    StopCombat ->
      JE.string "StopCombat"
    RerollCombatInitiative ->
      JE.string "RerollCombatInitiative"
    ForceNextTurn ->
      JE.string "ForceNextTurn"
    ForcePrevTurn -> JE.string "ForcePrevTurn"
    CreateCreature path creature ->
      JE.object [("CreateCreature", JE.list [folderPathEncoder path, creatureCreationEncoder creature])]
    EditCreature creature ->
      JE.object [("EditCreature", creatureEncoder creature)]
    DeleteCreature cid ->
      JE.object [("DeleteCreature", JE.string cid)]
    PathCurrentCombatCreature pt ->
      JE.object [("PathCurrentCombatCreature", point3Encoder pt)]
    PathCreature scene cid pt ->
      JE.object [("PathCreature", JE.list [JE.string scene, JE.string cid, point3Encoder pt])]
    SetCreaturePos scene cid pt ->
      JE.object [("SetCreaturePos", JE.list [JE.string scene, JE.string cid, point3Encoder pt])]
    AddCreatureToCombat cid ->
      JE.object [("AddCreatureToCombat", JE.string cid)]
    RemoveCreatureFromCombat cid ->
      JE.object [("RemoveCreatureFromCombat", JE.string cid)]
    CombatAct abid dtarget ->
      JE.object [("CombatAct", JE.list [JE.string abid, decidedTargetEncoder dtarget])]
    ActCreature scene cid abid dtarget ->
      JE.object [("ActCreature", JE.list [JE.string scene, JE.string cid, JE.string abid, decidedTargetEncoder dtarget])]
    Done ->
      JE.string "Done"
    CreateMap path creation ->
      JE.object [("CreateMap", JE.list [folderPathEncoder path, mapCreationEncoder creation])]
    EditMap map ->
      JE.object [("EditMap", mapEncoder map)]
    DeleteMap mid ->
      JE.object [("DeleteMap", JE.string mid)]
    Rollback snapIdx logIdx ->
      JE.object [("Rollback", JE.list [JE.int snapIdx, JE.int logIdx])]
    ChangeCreatureInitiative cid newPos ->
      JE.object [("ChangeCreatureInitiative", JE.list [JE.string cid, JE.int newPos])]
    AttributeCheck cid attrCheck ->
      JE.object [("AttributeCheck", JE.list [JE.string cid, attrCheckEncoder attrCheck])]


type SkillLevel
  = Inept
  | Unskilled
  | Skilled
  | Expert
  | Supernatural

skillLevelEncoder : SkillLevel -> JE.Value
skillLevelEncoder sl = case sl of
  Inept -> JE.string "Inept"
  Unskilled -> JE.string "Unskilled"
  Skilled -> JE.string "Skilled"
  Expert -> JE.string "Expert"
  Supernatural -> JE.string "Supernatural"

skillLevelDecoder : JD.Decoder SkillLevel
skillLevelDecoder = sumDecoder "SkillLevel"
  [ ("Inept", Inept)
  , ("Unskilled", Unskilled)
  , ("Skilled", Skilled)
  , ("Expert", Expert)
  , ("Supernatural", Supernatural)
  ]
  []

type FolderItemID
  = FolderScene SceneID
  | FolderCreature CreatureID
  | FolderNote String
  | FolderMap MapID
  | FolderSubfolder String

folderItemIDEncoder : FolderItemID -> JE.Value
folderItemIDEncoder item = case item of
  FolderScene sid -> JE.object [("SceneID", JE.string sid)]
  FolderCreature cid -> JE.object [("CreatureID", JE.string cid)]
  FolderNote nid -> JE.object [("NoteID", JE.string nid)]
  FolderMap mid -> JE.object [("MapID", JE.string mid)]
  FolderSubfolder name -> JE.object [("SubfolderID", JE.string name)]

folderItemIDDecoder : JD.Decoder FolderItemID
folderItemIDDecoder = sumDecoder "FolderItemID"
  []
  [ ("SceneID", JD.map FolderScene JD.string)
  , ("CreatureID", JD.map FolderCreature JD.string)
  , ("NoteID", JD.map FolderNote JD.string)
  , ("MapID", JD.map FolderMap JD.string)
  , ("SubfolderID", JD.map FolderSubfolder JD.string)
  ]

resultDecoder : JD.Decoder error -> JD.Decoder success -> JD.Decoder (Result error success)
resultDecoder errorDecoder successDecoder = sumDecoder "Result"
  []
  [ ("Ok", JD.map Ok successDecoder)
  , ("Err", JD.map Err errorDecoder)
  ]

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

encodeMaybe : Maybe a -> (a -> JE.Value) -> JE.Value
encodeMaybe mebbe encoder =
  case mebbe of
    Just x -> encoder x
    Nothing -> JE.null

maybeDecoder d = JD.oneOf [JD.map Just d, JD.null Nothing]

setDecoder : JD.Decoder comparable -> JD.Decoder (Set.Set comparable)
setDecoder el = JD.map Set.fromList (JD.list el)

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
combatCreature : Game -> Combat -> Creature
combatCreature game combat =
  case List.head (List.drop combat.creatures.cursor combat.creatures.data) of
    Just (cid, _) ->
      case getCreature game cid of
        Just c -> c
        Nothing -> Debug.crash "Combat creature wasn't found in game"
    Nothing -> Debug.crash "Creature CursorList was invalid"

getCreature : Game -> CreatureID -> Maybe Creature
getCreature game cid = Dict.get cid game.creatures

getCreatures : Game -> List CreatureID -> List Creature
getCreatures game cids = List.filterMap (getCreature game) cids

getCombatCreatures : Game -> Combat -> List (Creature, Int)
getCombatCreatures game combat =
  let
    gc (cid, init) = getCreature game cid |> Maybe.map (\c -> (c,init))
  in List.filterMap gc combat.creatures.data

creatureName : App -> CreatureID -> Maybe String
creatureName app cid = getCreature app.current_game cid |> Maybe.map (\c -> c.name)

getScene : App -> SceneID -> Maybe Scene
getScene app sid = Dict.get sid app.current_game.scenes

playerIsRegistered : App -> PlayerID -> Bool
playerIsRegistered app pid = Dict.member pid app.players

getPlayerCreatures : App -> PlayerID -> List Creature
getPlayerCreatures app pid =
  let cids =
        Dict.get pid app.players
        |> Maybe.map (\player -> player.creatures)
        |> Maybe.withDefault Set.empty
  in List.filterMap (getCreature app.current_game) (Set.toList cids)

isCreatureInCombat : Game -> CreatureID -> Bool
isCreatureInCombat game cid =
  case game.current_combat of
    Nothing -> False
    Just combat -> List.any (\(c, _) -> c == cid) combat.creatures.data

isCreatureOOC : Game -> CreatureID -> Bool
isCreatureOOC game cid = Dict.member cid game.creatures

getFolder : App -> FolderPath -> Maybe Folder
getFolder app path =
  let getChild seg mFolder =
        mFolder
        |> Maybe.andThen (\(Folder folder) -> (Dict.get seg folder.children))
  in List.foldl getChild (Just app.current_game.campaign) path

folderPathToString : FolderPath -> String
folderPathToString p =
  if List.isEmpty p then "" else ("/" ++ String.join "/" p)

folderPathFromString : String -> Maybe FolderPath
folderPathFromString ps =
  if String.isEmpty ps then Just []
  else
    if String.startsWith "/" ps
    then
      Just <| String.split "/" (String.slice 1 (String.length ps) ps)
    else Nothing

emptyMap : Map
emptyMap = {id="invalid", name="<empty>", terrain=Set.empty, specials=Dict.empty}

folderPathParent : FolderPath -> FolderPath
folderPathParent path = Maybe.withDefault [] <| List.Extra.init path

folderPathBaseName : FolderPath -> Maybe String
folderPathBaseName path = List.Extra.last path

folderPathChild : FolderPath -> String -> FolderPath
folderPathChild fp name = fp ++ [name]

getCreaturePos : CreatureID -> Scene -> Maybe Point3
getCreaturePos cid scene = Dict.get cid scene.creatures |> Maybe.map (\(p, _) -> p)
