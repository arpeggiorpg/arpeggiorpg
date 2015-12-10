{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PandT.Types where

import Control.Lens
import Data.Text

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


-- a condition
-- 1. has a name
-- 2. either has a duration or is infinite
-- 3. has a summy "value":
--    a. RecurringEffect Effect Period
--    b. SomeThing
--    c. OtherThing

data ConditionDuration -- maybe add an Ord instance
    = TimedCondition Duration
    | UnlimitedDuration
    deriving (Show, Eq)

data ConditionValue
    = RecurringEffect Period Effect
    | DamageAbsorb DamageIntensity
    | DamageBuff DamageIntensity
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
    deriving (Show, Eq)


data TargetSystem
    = TargetCreature Range
    | TargetCircle Range Radius
    | TargetLineFromSource Range
    deriving (Show, Eq)


data Ability = Ability
    { _name :: Text
    , _cost :: Resource
    , _effects :: [Effect]
    , _target :: TargetSystem
    , _castTime :: CastTime
    , _cooldown :: Cooldown
    }
    deriving (Show, Eq)

makeLenses ''Ability

data Creature = Creature
    { _conditions :: [Condition]
    , _resource :: Resource
    , _stamina :: Stamina
    , _health :: Health
    , _abilities :: [Ability]
    }
    deriving (Show, Eq)

makeLenses ''Creature

_staminaToHealth :: Stamina -> Health
_staminaToHealth (Stamina High) = Health 100
_staminaToHealth (Stamina Medium) = Health 50
_staminaToHealth (Stamina Low) = Health 25

makeCreature :: Resource -> Stamina -> [Ability] -> Creature
makeCreature res sta abs = Creature
    { _conditions=[]
    , _resource=res
    , _stamina=sta
    , _health=staminaToHealth sta
    , _abilities=abs}

data Combat = Combat
    { creatures :: [Creature]
    }
    deriving (Show, Eq)


staminaToHealth :: Stamina -> Health
staminaToHealth (Stamina High) = Health 100
staminaToHealth (Stamina Medium) = Health 50
staminaToHealth (Stamina Low) = Health 25

damageToHealthVal :: DamageIntensity -> Int
damageToHealthVal (DamageIntensity High) = 50
damageToHealthVal (DamageIntensity Medium) = 25
damageToHealthVal (DamageIntensity Low) = 10

healthMinusDamage :: Health -> DamageIntensity -> Health
healthMinusDamage (Health health) dmg = Health (health - (damageToHealthVal dmg))

makeDotEffect :: Text -> Intensity -> ConditionDuration -> Effect
makeDotEffect name int dur
    = ApplyCondition
        (Condition
            { _conditionName=name
            , _conditionValue=RecurringEffect (Period 1) (Damage (DamageIntensity int))
            , _conditionDuration=dur})

immolation = makeDotEffect "Immolation" Medium (TimedCondition (Duration 3))

punch = Ability
    { _name="Punch"
    , _cost=Energy 10
    , _effects=[Damage (DamageIntensity High), immolation]
    , _target=TargetCreature (Range 1)
    , _castTime = CastTime 0
    , _cooldown = Cooldown 0
    }
creat = makeCreature (Energy 100) (Stamina High) [punch]

applyEffect :: Creature -> Effect -> Creature
applyEffect creature (ApplyCondition condition) = over conditions (condition:) creature
