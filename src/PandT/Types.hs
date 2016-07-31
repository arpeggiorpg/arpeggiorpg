{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All the PandT types.

module PandT.Types where

import PandT.Prelude

import Math.Geometry.Grid.Octagonal (UnboundedOctGrid)
import Math.Geometry.GridMap.Lazy (LGridMap)

newtype Stamina = Stamina Intensity deriving (Show, Eq, Ord)
newtype Range = Range Int deriving (Show, Eq, Ord)
newtype Radius = Radius Int deriving (Show, Eq, Ord)
newtype Duration = Duration Int deriving (Show, Eq, Ord, Enum)
newtype CastTime = CastTime Int deriving (Show, Eq, Ord)
newtype Cooldown = Cooldown Int deriving (Show, Eq, Ord)
newtype Health = Health {_unHealth :: Int} deriving (Show, Eq, Ord)
newtype Player = Player Text deriving (Show, Ord, Eq)

type CreatureName = Text -- XXX TODO: newtype?
type AbilityName = Text -- XXX TODO: newtype?
type DamageIntensity = Int -- XXX TODO: newtype?

data Intensity = Low | Medium | High deriving (Show, Eq, Ord)
newtype Energy = Energy {_unEnergy :: Int} deriving (Show, Eq, Ord, Enum, Num)

makeLenses ''Health
makeLenses ''Energy

staminaToHealth :: Stamina -> Health
staminaToHealth (Stamina High) = Health 10
staminaToHealth (Stamina Medium) = Health 5
staminaToHealth (Stamina Low) = Health 3

data ConditionDuration -- this could have a reasonable Ord instance
    = TimedCondition Duration
    | UnlimitedDuration
    deriving (Show, Eq)
makePrisms ''ConditionDuration


-- CONDITIONS

-- so this section of the code is kind of a mess.
-- A lot of repitition: *T types, ConditionC cases, AppliedC cases, and patterns.
-- It's also a closed data type when I think it probably ought to be open.
-- So: consider Data types a la carte.

data RecurringEffectT = RecurringEffectT Effect deriving (Show, Eq)
data DamageIncreaseT = DamageIncreaseT {_intensityIncrease :: DamageIntensity} deriving (Show, Eq)
data DamageDecreaseT = DamageDecreaseT {_intensityDecrease :: DamageIntensity} deriving (Show, Eq)
data IncomingDamageReductionT = IncomingDamageReductionT {_intensityReduce :: DamageIntensity} deriving (Show, Eq)
data IncapacitatedT = IncapacitatedT deriving (Show, Eq)
data DeadT = DeadT deriving (Show, Eq)
data ActivatedAbilityT = ActivatedAbilityT {_activatedAbilityName :: AbilityName} deriving (Show, Eq)

data ConditionC
    = RecurringEffectC RecurringEffectT
    | IncomingDamageReductionC IncomingDamageReductionT
    | DamageIncreaseC DamageIncreaseT
    | DamageDecreaseC DamageDecreaseT
    | IncapacitatedC IncapacitatedT
    | DeadC DeadT
    | ActivatedAbilityC ActivatedAbilityT
    deriving (Show, Eq)

pattern MkRecurringEffectC effect = RecurringEffectC (RecurringEffectT effect)
pattern MkIncomingDamageReductionC intensity = IncomingDamageReductionC (IncomingDamageReductionT intensity)
pattern MkDamageIncreaseC intensity = DamageIncreaseC (DamageIncreaseT intensity)
pattern MkDamageDecreaseC intensity = DamageDecreaseC (DamageDecreaseT intensity)
pattern MkIncapacitatedC = IncapacitatedC IncapacitatedT
pattern MkDeadC = DeadC DeadT
pattern MkActivatedAbilityC name = ActivatedAbilityC (ActivatedAbilityT name)

data ConditionMeta = ConditionMeta
    { _conditionMetaName :: Text
    , _conditionMetaDuration :: ConditionDuration
    } deriving (Show, Eq)

data ConditionDef = ConditionDef
    { _conditionDefMeta :: ConditionMeta
    , _conditionDefC :: ConditionC }
    deriving (Show, Eq)

pattern MkConditionDef name duration c = ConditionDef (ConditionMeta name duration) c

-- A condition at runtime: contains a value of a condition type, and the necessary runtime data for
-- that condition. This does the important job of declaring types that ensure the appropriate
-- runtime data types are associated with the appropriate condition definition types. of course,
-- right now there is no dynamic data specific to a condition, but there _could_ be...
data AppliedC
    = AppliedRecurringEffect RecurringEffectT
    | AppliedIncomingDamageReduction IncomingDamageReductionT
    | AppliedDamageIncrease DamageIncreaseT
    | AppliedDamageDecrease DamageDecreaseT
    | AppliedIncapacitated IncapacitatedT
    | AppliedDead DeadT
    | AppliedActivatedAbility ActivatedAbilityT
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
    | GenerateEnergy Energy
    | Resurrect
    deriving (Show, Eq)

makePrisms ''ConditionC
makePrisms ''AppliedC
makePrisms ''Effect
makeLenses ''AppliedCondition
makeLenses ''ConditionMeta
makeLenses ''ConditionDef
makeLenses ''ActivatedAbilityT

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


-- XXX FIXME TODO - I may be able to replace this with typeclasses. Should I?

data SingleTarget
data MultiTarget
data SelfTarget

-- | Various ways in which effects can be targeted.
data TargetSystem a where
    TargetCreature :: Range -> TargetSystem SingleTarget
    TargetCircle :: Range -> Radius -> TargetSystem MultiTarget
    -- Umm, this probably needs the origin too...
    TargetLineFromSource :: Range -> TargetSystem MultiTarget
    TargetCone :: Range -> TargetSystem MultiTarget
    TargetSelf :: TargetSystem SelfTarget

deriving instance Show (TargetSystem a)
deriving instance Eq (TargetSystem a)

makePrisms ''TargetSystem

data TargetedEffectP a = TargetedEffectP
    { _targetedEffectName :: Text -- ^ Abstract name for the target. Used for prompting.
    , _targetedEffectSystem :: TargetSystem a
    , _targetedEffectEffect :: Effect
    } deriving (Show, Eq)

data TargetedEffect
    = SingleTargetedEffect {_singleTE :: TargetedEffectP SingleTarget}
    | MultiTargetedEffect {_multiTE :: TargetedEffectP MultiTarget}
    | SelfTargetedEffect {_selfTE :: TargetedEffectP SelfTarget}
    deriving (Show, Eq)

data SelectedTargetedEffect
    = SelectedMultiTargetedEffect [CreatureName] (TargetedEffectP MultiTarget)
    | SelectedSingleTargetedEffect CreatureName (TargetedEffectP SingleTarget)
    | SelectedSelfTargetedEffect (TargetedEffectP SelfTarget)
    deriving (Show, Eq)

makeLenses ''TargetedEffectP
makeLenses ''TargetedEffect
makePrisms ''TargetedEffect
makeLenses ''SelectedTargetedEffect
makePrisms ''SelectedTargetedEffect

-- | Something that a creature can do. (contrast with Effects, which are something that can *happen*
-- to a creature or the world)
data Ability = Ability
    { _abilityName :: AbilityName
    , _cost :: Energy
    , _abilityEffects :: [TargetedEffect]
    , _castTime :: CastTime
    , _cooldown :: Cooldown
    , _abilityRequiresActivation :: Bool
    }
    deriving (Show, Eq)

makeLenses ''Ability

makeAbility :: AbilityName -> Energy -> [TargetedEffect] -> CastTime -> Cooldown -> Ability
makeAbility n e ts ct cd = Ability n e ts ct cd False

data Creature = Creature
    { _creatureName :: CreatureName
    , _conditions :: [AppliedCondition]
    -- ^ Buffs and debuffs that are applied to this creature.
    , _creatureEnergy :: Energy
    -- ^ energy that the creature has
    , _stamina :: Stamina
    -- ^ The stamina determines the maximum amount of health a creature can have.
    , _health :: Health
    -- ^ *Current* health.
    , _abilities :: [Ability]
    -- ^ All the abilities that a creature can use.
    , _casting :: Maybe (Ability, Duration, [SelectedTargetedEffect])
    -- ^ The ability that a creature is "casting", if it has a cast time, and the duration
    -- *remaining* in the cast time.
    , _cooldowns :: Map AbilityName Cooldown
    -- ^ A map of ability names to amount of time left for that ability to cool down.
    }
    deriving (Show, Eq)

makeLenses ''Creature

makeCreature :: CreatureName -> Energy -> Stamina -> [Ability] -> Creature
makeCreature cname nrg sta creatAbilities = Creature
    { _creatureName=cname
    , _conditions=[]
    , _creatureEnergy=nrg
    , _stamina=sta
    , _health=staminaToHealth sta
    , _abilities=creatAbilities
    , _casting=Nothing
    , _cooldowns=mapFromList []}

-- | mkGame -- Smart constructor for Games
-- What can this help with?
-- 1. ensure all named creatures are in play


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
    , _gameGeo :: LGridMap UnboundedOctGrid CreatureName
    -- ^ Perhaps we should parameterize the grid type but eh
    }
    deriving (Show, Eq)

makeLenses ''Game

-- | All the various states that the game can be in.
data PlayerChoosingAbility = PlayerChoosingAbility deriving (Show, Eq)
data PlayerChoosingTargets = PlayerChoosingTargets Ability deriving (Show, Eq)
data PlayerIncapacitated = PlayerIncapacitated deriving (Show, Eq)
data PlayerCasting = PlayerCasting deriving (Show, Eq)
data PlayerFinishingCast = PlayerFinishingCast deriving (Show, Eq)
data GMVetting = GMVetting deriving (Show, Eq)

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

type EffectOccurrence = [(Effect, CreatureName)]

{-
| A data structure that encodes things that have happened in combat. This is used to render a log of
events.

NOTE (Historical): Once, I considered using this data structure not just for logging, but also for the
core of the simulation loop; these values would represent deltas applied to a state, by having
functions like:

    applyAbility :: Game a -> Abilities -> [CombatEvent]

and

    applyEvents :: [CombatEvent] -> Game a -> Game a

however, this *can't* work, because the creation of certain CombatEvents are dependent upon the game
state *as modified by previous CombatEvents* -- for example, if a CombatEvent does damage, that
damage *may* then create a CreatureDied CombatEvent, if the state indicates that the target
creature's new hit points are <= 0.

Therefore you can't go from [CombatEvent] -> Game a -> Game a; an alternative design that might work
would be incrementally apply individual CombatEvents to get further CombatEvents.

TODO: we must encode the fully-calculated numbers in this data structure, and not simply link to the
Effects that are being applied, in order to support dynamic damage numbers -- we may do less or more
damage than what is defined by the Effect, because of things like damage reduction or randomization.
-}
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
