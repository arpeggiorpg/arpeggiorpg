{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PandT.Types where

import Control.Lens
import Data.Text

data DamageSeverity = Low | Medium | High
    deriving (Show, Eq, Ord)

newtype Range = Range Int
    deriving (Show, Eq, Ord)

newtype Radius = Radius Int
    deriving (Show, Eq, Ord)

newtype Duration = Duration Int
    deriving (Show, Eq, Ord)

newtype CastTime = CastTime Int
    deriving (Show, Eq, Ord)

newtype Health = Health Int -- Or... hm.
    deriving (Show, Eq, Ord)

newtype Cooldown = Cooldown Int
    deriving (Show, Eq, Ord)


data Resource = Mana Int | Energy Int
    deriving (Show, Eq)


data Condition
    = DamageOverTime DamageSeverity Duration
    | DamageAbsorb DamageSeverity Duration
    | HealOverTime DamageSeverity Duration
    deriving (Show, Eq)


data Effect
    = Interrupt
    | ApplyCondition Condition
    | Heal DamageSeverity
    | Damage DamageSeverity
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

data AbilitySet = AbilitySet -- aka "class"
    { abilities :: [Ability] }
    deriving (Show, Eq)


data Creature = Creature
    { _conditions :: [Condition]
    , _resource :: Resource
    , _health :: Health
    , _abilitySets :: [AbilitySet]
    }
    deriving (Show, Eq)

makeLenses ''Creature

data Combat = Combat
    { creatures :: [Creature]
    }
    deriving (Show, Eq)


punch = Ability
    { _name="Punch"
    , _cost=Energy 10
    , _effects=[Damage High, ApplyCondition (DamageOverTime Low (Duration 3))]
    , _target=TargetCreature (Range 1)
    , _castTime = CastTime 0
    , _cooldown = Cooldown 0
    }
creat = Creature
    { _conditions=[]
    , _resource=Energy 100
    , _health=Health 100
    , _abilitySets=[AbilitySet [punch]]
    }

applyEffect :: Creature -> Effect -> Creature
applyEffect creature (ApplyCondition condition) = over conditions (condition:) creature
