{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module PandT.Types where

import ClassyPrelude

import Control.Lens ((^.), (^?), (^..), at, over, view, preview, makeLenses, set, firstOf, _head)
import Data.Text (Text)
import Data.Map (Map)

data Intensity = Low | Medium | High
    deriving (Show, Eq, Ord)

newtype DamageIntensity = DamageIntensity Intensity
    deriving (Show, Eq, Ord)

newtype Stamina = Stamina Intensity
    deriving (Show, Eq, Ord)

newtype Range = Range Int
    deriving (Show, Eq, Ord)

newtype Radius = Radius Int
    deriving (Show, Eq, Ord)

newtype Duration = Duration Int
    deriving (Show, Eq, Ord)

newtype Period = Period Int
    deriving (Show, Eq, Ord)

newtype CastTime = CastTime Int
    deriving (Show, Eq, Ord)

newtype Cooldown = Cooldown Int
    deriving (Show, Eq, Ord)

newtype Health = Health Int
    deriving (Show, Eq, Ord)

data Resource = Mana Int | Energy Int
    deriving (Show, Eq)

newtype Player = Player { playerName :: Text }
    deriving (Show, Ord, Eq)

type CreatureName = Text -- this should probably be a newtype

data ConditionDuration -- this could have a reasonable Ord instance
    = TimedCondition Duration
    | UnlimitedDuration
    deriving (Show, Eq)

data ConditionValue
    = RecurringEffect Period Effect
    | DamageAbsorb DamageIntensity
    | DamageBuff DamageIntensity
    | DamageDebuff DamageIntensity
    | Incapacitated
    deriving (Show, Eq)

data Condition
    = Condition
    { _conditionName :: Text
    , _conditionDuration :: ConditionDuration
    , _conditionValue :: ConditionValue
    } deriving (Show, Eq)


data Effect
    = Interrupt
    | ApplyCondition Condition
    | Heal DamageIntensity
    | Damage DamageIntensity
    | MultiEffect Effect Effect
    deriving (Show, Eq)


data TargetSystem
    = TargetCreature Range
    | TargetCircle Range Radius
    | TargetLineFromSource Range
    | TargetCone Range
    deriving (Show, Eq)


data TargetedEffect = TargetedEffect
    { _targetName :: CreatureName -- ^ Used for prompting the user for the target
    , _targetSystem :: TargetSystem
    , _targetedEffect :: Effect
    } deriving (Show, Eq)


makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff = ApplyCondition Condition
    { _conditionName=cname
    , _conditionDuration=TimedCondition (Duration cdur)
    , _conditionValue=RecurringEffect (Period 1) ceff
    }

-- Tera-style multi-healing
healTwoTargets :: [TargetedEffect]
healTwoTargets = take 2 . repeat $ TargetedEffect
        { _targetName="Heal"
        , _targetSystem=TargetCreature (Range 5)
        , _targetedEffect=Heal (DamageIntensity Medium)
        }

-- basic damage+dot attack
immolate :: TargetedEffect
immolate = TargetedEffect
    { _targetName="Immolate"
    , _targetSystem=TargetCreature (Range 5)
    , _targetedEffect=MultiEffect directDamage dot
    } where
        directDamage = Damage (DamageIntensity Medium)
        dot = makeTimedEOT "Immolation" 3 dotTick
        dotTick = Damage (DamageIntensity Low)


mistPunch :: [TargetedEffect]
mistPunch =
    [ TargetedEffect
        { _targetName="MistPunch Damage"
        , _targetSystem=TargetCreature (Range 1)
        , _targetedEffect=Damage (DamageIntensity Low)
        }
    , TargetedEffect
        { _targetName="MistPunch Heal"
        , _targetSystem=TargetCreature (Range 4)
        , _targetedEffect=Heal (DamageIntensity Low)
        }
    ]

-- ok this won't work, because the secondary effect will apply to the primary target.
-- fistsOfFury :: [TargetedEffect]
-- fistsOfFury =
--     [ TargetedEffect { _targetSystem=TargetCreature (Range 1), _targetedEffect=stunAndMediumDamage}
--     , TargetedEffect { _targetSystem=TargetCone (Range 1), _targetedEffect=stunAndLowDamage}
--     ]

stun :: Duration -> Effect
stun dur = ApplyCondition Condition
    { _conditionName = "Stunned"
    , _conditionValue = Incapacitated
    , _conditionDuration = TimedCondition dur
    }

-- but, I guess, on the other hand, we can just deal two amounts of low damage to the main target...
fistsOfFury :: [TargetedEffect]
fistsOfFury =
    [ TargetedEffect
        { _targetName="Fists of Fury Primary"
        , _targetSystem=TargetCreature (Range 1)
        , _targetedEffect=lowDamage
        }
    , TargetedEffect
        { _targetName="Fists of Fury Area"
        , _targetSystem=TargetCone (Range 1)
        , _targetedEffect=stunAndLowDamage
        }
    ]
    where
        lowDamage = Damage (DamageIntensity Medium)
        stunAndLowDamage = MultiEffect stunEff lowDamage
        stunEff = stun (Duration 1)



data Ability = Ability
    { _abilityName :: Text
    , _cost :: Resource
    , _effects :: [TargetedEffect]
    , _castTime :: CastTime
    , _cooldown :: Cooldown
    }
    deriving (Show, Eq)

makeLenses ''Ability

data Creature = Creature
    { _creatureName :: CreatureName
    , _conditions :: [Condition]
    , _resource :: Resource
    , _stamina :: Stamina
    , _health :: Health
    , _abilities :: [Ability]
    , _casting :: Maybe Ability
    }
    deriving (Show, Eq)

makeLenses ''Creature

staminaToHealth :: Stamina -> Health
staminaToHealth (Stamina High) = Health 100
staminaToHealth (Stamina Medium) = Health 50
staminaToHealth (Stamina Low) = Health 25

damageToHealthVal :: DamageIntensity -> Int
damageToHealthVal (DamageIntensity High) = 50
damageToHealthVal (DamageIntensity Medium) = 25
damageToHealthVal (DamageIntensity Low) = 10

healthMinusDamage :: Health -> DamageIntensity -> Health
healthMinusDamage (Health healthVal) dmg = Health (healthVal - (damageToHealthVal dmg))

healthPlusDamage :: Health -> DamageIntensity -> Health
healthPlusDamage (Health healthVal) dmg = Health (healthVal + (damageToHealthVal dmg))

makeCreature :: CreatureName -> Resource -> Stamina -> [Ability] -> Creature
makeCreature cname res sta creatAbilities = Creature
    { _creatureName=cname
    , _conditions=[]
    , _resource=res
    , _stamina=sta
    , _health=staminaToHealth sta
    , _abilities=creatAbilities
    , _casting=Nothing}

makeDotEffect :: Text -> Intensity -> ConditionDuration -> Period -> Effect
makeDotEffect newConditionName int dur per
    = ApplyCondition
        (Condition
            { _conditionName=newConditionName
            , _conditionValue=RecurringEffect per (Damage (DamageIntensity int))
            , _conditionDuration=dur})

bleed :: Effect
-- bleed = makeDotEffect "Bleeding" Medium (TimedCondition (Duration 2)) (Period 1)
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Low))


{-
- fists of fury:
  - effect 1: TARGETED high damage + stun
  - effect 2: CONE medium damage + stun
- Immolate:
  - effect 1: medium damage to target
  - effect 2: medium damage to SAME target

-}

stab :: Ability
stab = Ability
    { _abilityName="Stab"
    , _cost=Energy 10
    , _effects=[stabTargetedEffect]
    , _castTime = CastTime 0
    , _cooldown = Cooldown 0
    }
    where
        stabTargetedEffect =
            TargetedEffect
                { _targetName = "Stab"
                , _targetSystem = TargetCreature (Range 1)
                , _targetedEffect = stabEffect
                }
        stabEffect = MultiEffect stabDirectDamage bleed
        stabDirectDamage = Damage (DamageIntensity Medium)

creat :: Creature
creat = makeCreature "Creat" (Energy 100) (Stamina High) [stab]

applyEffect :: Creature -> Effect -> Creature
applyEffect creature effect = go effect
    where
        go (ApplyCondition condition) = over conditions (condition:) creature
        go (Damage amt) = over health (flip healthMinusDamage amt) creature
        go (Heal amt) = over health (flip healthPlusDamage amt) creature
        go (MultiEffect e1 e2) = applyEffect (applyEffect creature e1) e2

dotted :: Creature
dotted = applyEffect creat bleed

damaged :: Creature
damaged = applyEffect creat (Damage (DamageIntensity Medium))

healed :: Creature
healed = applyEffect damaged (Heal (DamageIntensity Low))

-- Workflow

data PlayerChoosingAbility
data PlayerChoosingTargets
data GMVettingAction

data GameState a where
    PlayerChoosingAbility :: GameState PlayerChoosingAbility
    PlayerChoosingTargets :: Ability -> GameState PlayerChoosingTargets
    GMVettingAction :: Ability -> [[CreatureName]] -> GameState GMVettingAction

instance Show (GameState a) where
    show PlayerChoosingAbility = "PlayerChoosingAbility"
    show (PlayerChoosingTargets ability) = "PlayerChoosingTargets " ++ show ability
    show (GMVettingAction ability targets) = "GMVettingAction " ++ show ability ++ " " ++ show targets

instance Eq (GameState a) where
    PlayerChoosingAbility == PlayerChoosingAbility = True
    (PlayerChoosingTargets ab1) == (PlayerChoosingTargets ab2) = ab1 == ab2
    (GMVettingAction ab1 targ1) == (GMVettingAction ab2 targ2) = (ab1 == ab2) && (targ1 == targ2)


data Game status = Game
    { _state :: GameState status
    , _playerCharacters :: Map Player CreatureName
    , _currentCreature :: CreatureName
    , _currentPlayer :: Player
    , _creaturesInPlay :: Map CreatureName Creature
    }
    deriving (Show, Eq)

makeLenses ''Game

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Game PlayerChoosingTargets
chooseAbility game ability =
    set state newState game
    where newState = PlayerChoosingTargets ability

chooseTargets :: Game PlayerChoosingTargets -> [[CreatureName]] -> Game GMVettingAction
chooseTargets game@(Game {_state=(PlayerChoosingTargets ability)}) creatures =
    set state newState game
    where newState = GMVettingAction ability creatures


applyAbility
    :: Game GMVettingAction
    -> Maybe (Game GMVettingAction)
applyAbility game@(Game {_state=GMVettingAction ability selections})
    = foldM appEffs game $ zip (_effects ability) selections
    where
        appEffs game (targetedEffect, creatureNames) = foldM (appEff targetedEffect) game creatureNames
        appEff :: TargetedEffect -> Game GMVettingAction -> CreatureName -> Maybe (Game GMVettingAction)
        appEff targetedEffect game creatName = do
            let applyEffect' = fmap $ flip applyEffect (_targetedEffect targetedEffect)
            return $ over (creaturesInPlay . at creatName) applyEffect' game

-- ^ XXX [Creature] should be replaced by a map of effects to lists of
-- creatures? or something...
-- chooseTargets :: Game PlayerChoosingTargets -> [Creature]
--               -> Game GMVettingAction
-- chooseTargets = undefined
--
-- vetAction :: Game GMVettingAction -> Game PlayerChoosingAbility
-- vetAction = undefined -- this is where applyAbility actually gets called
--
-- denyAction :: Game GMVettingAction -> Game PlayerChoosingTargets
-- denyAction gs = undefined

-- test data
chris :: Player
chris = Player "Chris"

jah :: Player
jah = Player "Jah"

radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab]
aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab]

myGame1 :: Game PlayerChoosingAbility
myGame1 = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr")]
    , _currentCreature="Radorg"
    , _currentPlayer=chris
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr)]
    }

myGame2 = chooseAbility myGame1 stab
myGame3 = chooseTargets myGame2 [["Aspyr"]]

(Just myGame4) = applyAbility myGame3
