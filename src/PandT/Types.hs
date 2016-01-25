{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All the PandT types.

module PandT.Types where

import PandT.Prelude

newtype Stamina = Stamina Intensity deriving (Show, Eq, Ord)
newtype Range = Range Int deriving (Show, Eq, Ord)
newtype Radius = Radius Int deriving (Show, Eq, Ord)
newtype Duration = Duration Int deriving (Show, Eq, Ord, Enum)
newtype CastTime = CastTime Int deriving (Show, Eq, Ord)
newtype Cooldown = Cooldown Int deriving (Show, Eq, Ord)
newtype Health = Health Int deriving (Show, Eq, Ord)
newtype Player = Player Text deriving (Show, Ord, Eq)

type CreatureName = Text -- XXX TODO: newype?
type DamageIntensity = Int -- XXX TODO: newtype?

data Intensity = Low | Medium | High deriving (Show, Eq, Ord)
data Resource = Mana Int | Energy Int deriving (Show, Eq)

data ConditionDuration -- this could have a reasonable Ord instance
    = TimedCondition Duration
    | UnlimitedDuration
    deriving (Show, Eq)
makePrisms ''ConditionDuration

data RecurringEffectT = RecurringEffectT Effect deriving (Show, Eq)
data DamageIncreaseT = DamageIncreaseT {_intensityIncrease :: DamageIntensity} deriving (Show, Eq)
data DamageDecreaseT = DamageDecreaseT {_intensityDecrease :: DamageIntensity} deriving (Show, Eq)
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
    { _appliedConditionOriginator :: CreatureName
    -- ^ Mechanically we only need this for RecurringEffect (so far), but informationally it's
    -- useful to show to players who caused a condition
    , _appliedConditionDurationLeft :: ConditionDuration
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

-- | Various ways in which effects can be targeted.
data TargetSystem a where
    TargetCreature :: Range -> TargetSystem SingleTarget
    TargetCircle :: Range -> Radius -> TargetSystem MultiTarget
    -- Umm, this probably needs the origin too...
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

-- | Something that a creature can do. (contrast with Effects, which are something that can *happen*
-- to a creature or the world)
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
    -- ^ Buffs and debuffs that are applied to this creature.
    , _resource :: Resource
    -- ^ What kind of resource the creature uses.
    , _stamina :: Stamina
    -- ^ The stamina determines the maximum amount of health a creature can hav.
    , _health :: Health
    -- ^ *Current* health.
    , _abilities :: [Ability]
    -- ^ All the abilities that a creature can use.
    , _casting :: Maybe (Ability, Duration)
    -- ^ The ability that a creature is "casting", if it has a cast time, and the duration
    -- *reaining* in the cast time.
    }
    deriving (Show, Eq)

makeLenses ''Creature

-- | The toplevel data type representing a game. It's parameterized by a state variable so that we
-- can safely transition from state to state -- e.g., a `Game PlayerChoosingAbility` must transition
-- to a `Game PlayerChoosingTargets` and nothing else -- so we write functions that accept and
-- return these specific Game types.
data Game status = Game
    { _state :: status
    , _playerCharacters :: Map Player CreatureName
    , _currentCreatureName :: CreatureName
    , _creaturesInPlay :: Map CreatureName Creature
    , _initiative :: [CreatureName]
    }
    deriving (Show, Eq)

makeLenses ''Game

-- | All the various states that the game can be in.
data PlayerChoosingAbility = PlayerChoosingAbility deriving (Show, Eq)
data PlayerChoosingTargets = PlayerChoosingTargets Ability deriving (Show, Eq)
data PlayerIncapacitated = PlayerIncapacitated deriving (Show, Eq)
data PlayerCasting = PlayerCasting deriving (Show, Eq)
data PlayerFinishingCast = PlayerFinishingCast deriving (Show, Eq)
data GMVettingAction = GMVettingAction Ability [SelectedTargetedEffect] deriving (Show, Eq)

-- | A game at the start of a turn -- represents the subset of states that a game can be in when a
-- player starts their turn.
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
        , _combatEventRecurringEventTarget :: CreatureName
        , _combatEventRecurringEventEffect :: EffectOccurrence
        }
    | SkippedIncapacitatedCreatureTurn CreatureName
    | SkippedTurn CreatureName
    | CreatureTurnStarted CreatureName
    | CanceledCast CreatureName Ability
    deriving (Show, Eq)

makeLenses ''CombatEvent
makePrisms ''CombatEvent
