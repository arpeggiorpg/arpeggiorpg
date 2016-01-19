{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PandT.Types where

import ClassyPrelude

import Control.Lens
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
    deriving (Show, Eq, Ord, Enum)

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

makePrisms ''ConditionDuration

data RecurringEffectT = RecurringEffectT Effect deriving (Show, Eq)
data DamageIncreaseT = DamageIncreaseT DamageIntensity deriving (Show, Eq)
data DamageDecreaseT = DamageDecreaseT DamageIntensity deriving (Show, Eq)
data DamageAbsorbT = DamageAbsorbT DamageIntensity deriving (Show, Eq)
data IncapacitatedT = IncapacitatedT deriving (Show, Eq)
data DeadT = DeadT deriving (Show, Eq)

data ConditionC
    = RecurringEffectC RecurringEffectT
    | DamageAbsorbC DamageAbsorbT
    | DamageIncreaseC DamageIncreaseT
    | DamageDecreaseC DamageDecreaseT
    | IncapacitatedC IncapacitatedT
    | DeadC DeadT
    deriving (Show, Eq)

pattern MkRecurringEffectC effect = RecurringEffectC (RecurringEffectT effect)
pattern MkDamageAbsorbC intensity = DamageAbsorbC (DamageAbsorbT intensity)
pattern MkDamageIncreaseC intensity = DamageIncreaseC (DamageIncreaseT intensity)
pattern MkDamageDecreaseC intensity = DamageDecreaseC (DamageDecreaseT intensity)
pattern MkIncapacitatedC = IncapacitatedC IncapacitatedT
pattern MkDeadC = DeadC DeadT

data ConditionMeta = ConditionMeta
    { _conditionMetaName :: Text
    , _conditionMetaDuration :: ConditionDuration
    } deriving (Show, Eq)

data ConditionDef = ConditionDef
    { _conditionDefMeta :: ConditionMeta
    , _conditionDefC :: ConditionC }
    deriving (Show, Eq)

pattern MkConditionDef name duration c = ConditionDef (ConditionMeta name duration) c

-- A condition at runtime: contains a value of a condition type, and the
-- necessary runtime data for that condition. This does the important job of
-- declaring types that ensure the appropriate runtime data types are associated
-- with the appropriate condition definition types.
data AppliedC
    = AppliedRecurringEffect RecurringEffectT
    | AppliedDamageAbsorb
        DamageAbsorbT
        Int -- ^ Amount of damage absorbed so far
    | AppliedDamageIncrease DamageIncreaseT
    | AppliedDamageDecrease DamageDecreaseT
    | AppliedIncapacitated IncapacitatedT
    | AppliedDead DeadT
    deriving (Show, Eq)

data AppliedCondition = AppliedCondition
    { _appliedConditionDurationLeft :: ConditionDuration
    , _appliedConditionMeta :: ConditionMeta
    , _appliedConditionC :: AppliedC }
    deriving (Show, Eq)

data Effect
    = Interrupt
    | ApplyCondition ConditionDef
    | Heal DamageIntensity
    | Damage DamageIntensity
    | MultiEffect Effect Effect
    deriving (Show, Eq)

makePrisms ''ConditionC
makePrisms ''AppliedC
makePrisms ''Effect
makeLenses ''AppliedCondition
makeLenses ''ConditionMeta
makeLenses ''ConditionDef

class HasConditionMeta a where
    conditionName :: Lens' a Text
    conditionDuration :: Lens' a ConditionDuration

instance HasConditionMeta ConditionMeta where
    conditionName = conditionMetaName
    conditionDuration = conditionMetaDuration

instance HasConditionMeta ConditionDef where
    conditionName = conditionDefMeta . conditionName
    conditionDuration = conditionDefMeta . conditionDuration

instance HasConditionMeta AppliedCondition where
    conditionName = appliedConditionMeta . conditionName
    conditionDuration = appliedConditionMeta . conditionDuration

data SingleTarget
data MultiTarget

data TargetSystem a where
    TargetCreature :: Range -> TargetSystem SingleTarget
    TargetCircle :: Range -> Radius -> TargetSystem MultiTarget
    TargetLineFromSource :: Range -> TargetSystem MultiTarget
    TargetCone :: Range -> TargetSystem MultiTarget

deriving instance Show (TargetSystem a)
deriving instance Eq (TargetSystem a)

makePrisms ''TargetSystem

data TargetedEffectP a = TargetedEffectP
    { _targetedEffectName :: Text -- ^ Used for prompting the user for the target
    , _targetedEffectSystem :: TargetSystem a
    , _targetedEffectEffect :: Effect
    } deriving (Show, Eq)

data TargetedEffect
    = SingleTargetedEffect (TargetedEffectP SingleTarget)
    | MultiTargetedEffect (TargetedEffectP MultiTarget)
    deriving (Show, Eq)

data SelectedTargetedEffect
    = SelectedMultiTargetedEffect [CreatureName] (TargetedEffectP MultiTarget)
    | SelectedSingleTargetedEffect CreatureName (TargetedEffectP SingleTarget)
    deriving (Show, Eq)

makeLenses ''TargetedEffectP
makeLenses ''TargetedEffect
makePrisms ''TargetedEffect
makeLenses ''SelectedTargetedEffect
makePrisms ''SelectedTargetedEffect


data Ability = Ability
    { _abilityName :: Text
    , _cost :: Resource
    , _abilityEffects :: [TargetedEffect]
    , _castTime :: CastTime
    , _cooldown :: Cooldown
    }
    deriving (Show, Eq)

makeLenses ''Ability

data Creature = Creature
    { _creatureName :: CreatureName
    , _conditions :: [AppliedCondition]
    , _resource :: Resource
    , _stamina :: Stamina
    , _health :: Health
    , _abilities :: [Ability]
    , _casting :: Maybe (Ability, Duration)
    }
    deriving (Show, Eq)

makeLenses ''Creature

data PlayerChoosingAbility
data PlayerChoosingTargets
data PlayerIncapacitated
data PlayerCasting
data PlayerFinishingCast
data GMVettingAction

data GameState a where
    PlayerIncapacitated :: GameState PlayerIncapacitated
    PlayerCasting :: GameState PlayerCasting
    PlayerFinishingCast :: GameState PlayerFinishingCast
    PlayerChoosingAbility :: GameState PlayerChoosingAbility
    PlayerChoosingTargets :: Ability -> GameState PlayerChoosingTargets
    GMVettingAction :: Ability -> [SelectedTargetedEffect] -> GameState GMVettingAction

deriving instance Show (GameState a)
deriving instance Eq (GameState a)
makePrisms ''GameState

data Game status = Game
    { _state :: GameState status
    , _playerCharacters :: Map Player CreatureName
    , _currentCreatureName :: CreatureName
    , _creaturesInPlay :: Map CreatureName Creature
    , _initiative :: [CreatureName]
    }
    deriving (Show, Eq)

makeLenses ''Game

class CancelCast a where
    cancelCast :: Game a -> Game PlayerChoosingAbility
    cancelCast game =
        set state PlayerChoosingAbility $
            set (currentCreature._Just.casting) Nothing game

instance CancelCast PlayerFinishingCast where
instance CancelCast PlayerCasting where

-- | A game at the start of a turn -- represents the subset of states that a
-- game can be in when a player starts their turn.
data GameStartTurn
    = GSTPlayerChoosingAbility (Game PlayerChoosingAbility)
    | GSTPlayerIncapacitated (Game PlayerIncapacitated)
    | GSTPlayerCasting (Game PlayerCasting)
    | GSTPlayerFinishingCast (Game PlayerFinishingCast)
    deriving (Show, Eq)


currentCreature :: Lens' (Game a) (Maybe Creature)
currentCreature = lens getter setter
    where
        getter game = game^.creaturesInPlay.at (game^.currentCreatureName)
        setter game value = set (creaturesInPlay.at (game^.currentCreatureName)) value game

{-
TODO: "dynamic" damage numbers here -- we may do less or more damage than what
is defined by the Effect, because of things like damage absorbs.
-}
type EffectOccurrence = [(Effect, CreatureName)]

data CombatEvent
    = AbilityStartCast CreatureName Ability
    | AbilityUsed
        { _combatEventAbilityUsed :: Ability
        , _combatEventAbilityOrigin :: CreatureName
        , _combatEventAbilityEffects :: EffectOccurrence
        }
    | RecurringEffectOccurred
        { _combatEventRecurringEventOrigin :: CreatureName
        , _combatEventRecurringEventEffect :: EffectOccurrence
        }
    | SkippedIncapacitatedCreatureTurn CreatureName
    | SkippedTurn CreatureName
    | CreatureTurnStarted CreatureName
    deriving (Show, Eq)

makeLenses ''CombatEvent
makePrisms ''CombatEvent
