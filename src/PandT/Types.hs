{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PandT.Types where

import Control.Lens ((^.), over, makeLenses, set)
import Data.Text (Text)
import Data.Foldable (foldl')
import Data.Map (Map, fromList)

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


data ConditionDuration -- this could have a reasonable Ord instance
    = TimedCondition Duration
    | UnlimitedDuration
    deriving (Show, Eq)

data ConditionValue
    = RecurringEffect Period Effect
    | DamageAbsorb DamageIntensity
    | DamageBuff DamageIntensity
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
    { _targetSystem :: TargetSystem
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
        { _targetSystem=TargetCreature (Range 5)
        , _targetedEffect=Heal (DamageIntensity Medium)
        }

-- basic damage+dot attack
immolate :: TargetedEffect
immolate = TargetedEffect
    { _targetSystem=TargetCreature (Range 5)
    , _targetedEffect=MultiEffect directDamage dot
    } where
        directDamage = Damage (DamageIntensity Medium)
        dot = makeTimedEOT "Immolation" 3 dotTick
        dotTick = Damage (DamageIntensity Low)


mistPunch :: [TargetedEffect]
mistPunch =
    [ TargetedEffect { _targetSystem=TargetCreature (Range 1), _targetedEffect=Damage (DamageIntensity Low)}
    , TargetedEffect { _targetSystem=TargetCreature (Range 4), _targetedEffect=Heal (DamageIntensity Low)}
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
    [ TargetedEffect { _targetSystem=TargetCreature (Range 1), _targetedEffect=lowDamage}
    , TargetedEffect { _targetSystem=TargetCone (Range 1), _targetedEffect=stunAndLowDamage}
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
    { _creatureName :: Text
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

makeCreature :: Text -> Resource -> Stamina -> [Ability] -> Creature
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
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Medium))

{-
- fists of fury:
  - effect 1: TARGETED high damage + stun
  - effect 2: CONE medium damage + stun
- Immolate:
  - effect 1: medium damage to target
  - effect 2: medium damage to SAME target

-}

-- stab :: Ability
-- stab = Ability
--     { _abilityName="Stab"
--     , _cost=Energy 10
--     , _effects=[Damage (DamageIntensity Medium), bleed]
--     , _target=TargetCreature (Range 1)
--     , _castTime = CastTime 0
--     , _cooldown = Cooldown 0
--     }

-- creat :: Creature
-- creat = makeCreature "Creat" (Energy 100) (Stamina High) [stab]

applyEffect :: Creature -> Effect -> Creature
applyEffect creature effect = go effect
    where
        go (ApplyCondition condition) = over conditions (condition:) creature
        go (Damage amt) = over health (flip healthMinusDamage amt) creature
        go (Heal amt) = over health (flip healthPlusDamage amt) creature

-- applyAbility :: Ability -> Creature -> Creature
-- applyAbility abil creatu
--     = foldl' applyEffect creatu (abil^.effects)

-- dotted :: Creature
-- dotted = applyEffect creat bleed
--
-- damaged :: Creature
-- damaged = applyEffect creat (Damage (DamageIntensity Medium))
--
-- healed :: Creature
-- healed = applyEffect damaged (Heal (DamageIntensity Low))

{-
abilities I want
- basic damage attack
- damage + dot
- damage + heal a target. target both distinctly!
- heal
- heal over time
- buff to damage
- debuff to damage


-}


-- Workflow

newtype Player = Player { playerName :: Text }
    deriving (Show, Ord, Eq)

-- ^ XXX Actually [Creature] is no good for GMVettingAction
data GameState
    = PlayerChoosingAbility Player Creature
    | PlayerChoosingTargets Player Creature Ability
    | GMVettingAction Player Creature Ability [Creature]
    deriving (Show, Eq)

data PlayerChoosingAbility
data PlayerChoosingTargets
data GMVettingAction

data Game status = Game
    { _state :: GameState
    , _playerCharacters :: Map Player Creature
    , _currentCreature :: Creature
    , _currentPlayer :: Player
    , _creaturesInPlay :: [Creature]
    }
    deriving (Show, Eq)

makeLenses ''Game

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Game PlayerChoosingTargets
chooseAbility game ability =
    set state newState game
    where newState = PlayerChoosingTargets
                        (_currentPlayer game)
                        (_currentCreature game)
                        ability

-- ^ XXX [Creature] should be replaced by a map of effects to lists of
-- creatures? or something...
chooseTargets :: Game PlayerChoosingTargets -> [Creature]
              -> Game GMVettingAction
chooseTargets = undefined

vetAction :: Game GMVettingAction -> Game PlayerChoosingAbility
vetAction = undefined -- this is where applyAbility actually gets called

denyAction :: Game GMVettingAction -> Game PlayerChoosingTargets
denyAction gs = undefined

-- test data
chris :: Player
chris = Player "Chris"

jah :: Player
jah = Player "Jah"

-- radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab]
-- aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab]
--
-- myGame1 :: Game PlayerChoosingAbility
-- myGame1 = Game
--     { _state=PlayerChoosingAbility chris radorg
--     , _playerCharacters=fromList [(chris, radorg), (jah, aspyr)]
--     , _currentCreature=radorg
--     , _currentPlayer=chris
--     , _creaturesInPlay=[]
--     }
--
-- myGame2 = chooseAbility myGame1 stab
