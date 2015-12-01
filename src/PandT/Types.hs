{-# LANGUAGE DataKinds #-}

module PandT.Types where

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


data Ability resourceType = Ability
    { name :: Text
    , cost :: resourceType
    , effects :: [Effect]
    , target :: TargetSystem
    , castTime :: CastTime
    }
    deriving (Show, Eq)


data AbilitySet resourceType = AbilitySet -- aka "class"
    { abilities :: [Ability resourceType] }
    deriving (Show, Eq)


data Creature resourceType = Creature
    { conditions :: [Condition]
    , resource :: resourceType
    , health :: Health
    , abilitySets :: [AbilitySet resourceType]
    }
    deriving (Show, Eq)


data Mana = Mana Int
    deriving (Show, Eq, Ord)

data Energy = Energy Int
    deriving (Show, Eq, Ord)

punch = Ability
    { name="Punch"
    , cost=Energy 10
    , effects=[Damage High, ApplyCondition (DamageOverTime Low (Duration 3))]
    , target=TargetCreature (Range 1)
    , castTime = CastTime 0
    }
creat = Creature
    { conditions=[]
    , resource=Energy 100
    , health=Health 100
    , abilitySets=[AbilitySet [punch]]
    }
