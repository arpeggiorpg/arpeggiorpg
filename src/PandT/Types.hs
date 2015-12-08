{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
    { name :: Text
    , cost :: Resource
    , effects :: [Effect]
    , target :: TargetSystem
    , castTime :: CastTime
    , cooldown :: Cooldown
    }
    deriving (Show, Eq)


data AbilitySet = AbilitySet -- aka "class"
    { abilities :: [Ability] }
    deriving (Show, Eq)


data Creature = Creature
    { conditions :: [Condition]
    , resource :: Resource
    , health :: Health
    , abilitySets :: [AbilitySet]
    }
    deriving (Show, Eq)


data Combat = Combat
    { creatures :: [Creature]
    }
    deriving (Show, Eq)


punch = Ability
    { name="Punch"
    , cost=Energy 10
    , effects=[Damage High, ApplyCondition (DamageOverTime Low (Duration 3))]
    , target=TargetCreature (Range 1)
    , castTime = CastTime 0
    , cooldown = Cooldown 0
    }
creat = Creature
    { conditions=[]
    , resource=Energy 100
    , health=Health 100
    , abilitySets=[AbilitySet [punch]]
    }

applyEffect :: Creature -> Effect -> Creature
applyEffect creature (ApplyCondition condition) = creature {conditions=condition : (conditions creature)}
