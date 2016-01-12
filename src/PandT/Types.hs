{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PandT.Types where

import ClassyPrelude

import Control.Lens ((^.), (^?), (^..), at, over, view, preview,
                     Prism',
                     mapped,
                     makeLenses, makePrisms, set, firstOf, _head,
                     _Just)
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)

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

data RecurringEffect
data DamageIncrease
data DamageDecrease
data DamageAbsorb
data Incapacitated
data Dead

data ConditionDef a where
    RecurringEffect :: Text -> ConditionDuration -> Effect -> ConditionDef RecurringEffect
    DamageIncrease :: Text -> ConditionDuration -> DamageIntensity -> ConditionDef DamageIncrease
    DamageDecrease :: Text -> ConditionDuration -> DamageIntensity -> ConditionDef DamageDecrease
    DamageAbsorb :: Text -> ConditionDuration -> DamageIntensity -> ConditionDef DamageAbsorb
    Incapacitated :: Text -> ConditionDuration -> ConditionDef Incapacitated
    Dead :: Text -> ConditionDuration -> ConditionDef Dead

deriving instance Show (ConditionDef a)
deriving instance Eq (ConditionDef a)

data ConditionCase
    = SomeRecurringEffect (ConditionDef RecurringEffect)
    | SomeDamageAbsorb (ConditionDef DamageAbsorb)
    | SomeDamageIncrease (ConditionDef DamageIncrease)
    | SomeDamageDecrease (ConditionDef DamageDecrease)
    | SomeIncapacitated (ConditionDef Incapacitated)
    | SomeDead (ConditionDef Dead)
    deriving (Show, Eq)

-- A condition at runtime: contains a condition definition and the necessary
-- runtime data for that condition. This does the important job of declaring
-- types that ensure the appropriate runtime data types are associated with the
-- appropriate condition definition types.
data AppliedCondition
    = AppliedRecurringEffect
        { _durationLeft :: ConditionDuration
        , _recurringEffectDef :: ConditionDef RecurringEffect}
    | AppliedDamageAbsorb
        { _durationLeft :: ConditionDuration
        , _damageAbsorbDef :: ConditionDef DamageAbsorb
        , _absorbed :: Int}
    | AppliedDamageIncrease
        { _durationLeft :: ConditionDuration
        , _damageIncreaseDef :: ConditionDef DamageIncrease}
    | AppliedDamageDecrease
        { _durationLeft :: ConditionDuration
        , _damageDecreaseDef :: ConditionDef DamageDecrease}
    | AppliedIncapacitated
        { _durationLeft :: ConditionDuration
        , _incapacitatedDef :: ConditionDef Incapacitated}
    | AppliedDead
        { _durationLeft :: ConditionDuration
        , _deadDef :: ConditionDef Dead}
    deriving (Show, Eq)

data Effect
    = Interrupt
    | ApplyCondition ConditionCase
    | Heal DamageIntensity
    | Damage DamageIntensity
    | MultiEffect Effect Effect

deriving instance Show Effect
deriving instance Eq Effect

makePrisms ''ConditionDef
makePrisms ''AppliedCondition
makeLenses ''AppliedCondition
makePrisms ''Effect

data TargetSystem
    = TargetCreature Range
    | TargetCircle Range Radius
    | TargetLineFromSource Range
    | TargetCone Range
    deriving (Show, Eq)

makePrisms ''TargetSystem

data TargetedEffect = TargetedEffect
    { _targetName :: CreatureName -- ^ Used for prompting the user for the target
    , _targetSystem :: TargetSystem
    , _targetedEffect :: Effect
    } deriving (Show, Eq)

makeLenses ''TargetedEffect


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
    , _conditions :: [AppliedCondition]
    , _resource :: Resource
    , _stamina :: Stamina
    , _health :: Health
    , _abilities :: [Ability]
    , _casting :: Maybe Ability
    }
    deriving (Show, Eq)

makeLenses ''Creature

data PlayerChoosingAbility
data PlayerChoosingTargets
data PlayerIncapacitated
data GMVettingAction

data GameState a where
    PlayerIncapacitated :: GameState PlayerIncapacitated
    PlayerChoosingAbility :: GameState PlayerChoosingAbility
    PlayerChoosingTargets :: Ability -> GameState PlayerChoosingTargets
    GMVettingAction :: Ability -> [[CreatureName]] -> GameState GMVettingAction

deriving instance Show (GameState a)
deriving instance Eq (GameState a)
makePrisms ''GameState

data Game status = Game
    { _state :: GameState status
    , _playerCharacters :: Map Player CreatureName
    , _currentCreature :: CreatureName
    , _creaturesInPlay :: Map CreatureName Creature
    , _initiative :: [CreatureName]
    }
    deriving (Show, Eq)

makeLenses ''Game


{-
Radix used DoubleHeal.
  - Target: Aspyr, healing: High.
  - Target: Ulsoga, healing: Medium.
-}
type EffectOccurrence = [(Effect, CreatureName)]

data CombatEvent
    = AbilityUsed
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
    deriving (Show, Eq)

makeLenses ''CombatEvent
makePrisms ''CombatEvent

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

dead :: AppliedCondition
dead = AppliedDead UnlimitedDuration $ Dead "Dead" UnlimitedDuration

checkDead :: Creature -> Creature
checkDead creat
    | elem dead (_conditions creat) = creat
    | _health creat <= Health 0 = over conditions (dead:) creat
    | otherwise = creat

applyCondition :: ConditionCase -> AppliedCondition
applyCondition (SomeRecurringEffect cdef@(RecurringEffect _ dur _)) = AppliedRecurringEffect dur cdef
applyCondition (SomeDamageIncrease cdef@(DamageIncrease _ dur _)) = AppliedDamageIncrease dur cdef
applyCondition (SomeDamageDecrease cdef@(DamageDecrease _ dur _)) = AppliedDamageDecrease dur cdef
applyCondition (SomeDamageAbsorb cdef@(DamageAbsorb _ dur _)) = AppliedDamageAbsorb dur cdef 0
applyCondition (SomeIncapacitated cdef@(Incapacitated _ dur)) = AppliedIncapacitated dur cdef
applyCondition (SomeDead cdef@(Dead _ dur)) = AppliedDead dur cdef

applyEffect :: Creature -> Effect -> Creature
applyEffect creature effect = checkDead $ go effect
    where
        go (ApplyCondition condition) = over conditions (applyCondition condition:) creature
        go (Damage amt) = over health (flip healthMinusDamage amt) creature
        go (Heal amt) = over health (flip healthPlusDamage amt) creature
        go (MultiEffect e1 e2) = applyEffect (applyEffect creature e1) e2

-- Workflow

renderState :: GameState a -> Text
renderState PlayerChoosingAbility = "PlayerChoosingAbility"
renderState (PlayerChoosingTargets ability)
    = "PlayerChoosingTargets: " ++ _abilityName ability
renderState (GMVettingAction ability targets)
    = "GMVettingAction: " ++ _abilityName ability ++ " -> " ++ tshow targets


renderCreatureStatus :: Creature -> Text
renderCreatureStatus creature =
    line
    where
        hp = tshow $ creature^.health
        conds = tshow $ creature^.conditions
        line = unwords [creature^.creatureName, hp, conds]

renderInitiative :: Game a -> Text
renderInitiative game
    = let
        currentName = (_currentCreature game)
        creature name = view (creaturesInPlay . at name) game
        pfx name = if name == currentName then "*" else " "
        statusLine name = unwords.toList $ renderCreatureStatus <$> creature name
        rend name = pfx name ++ statusLine name
    in
        unlines $ map rend (_initiative game)

render :: Game a -> Text
render game@(Game {..}) = unlines
    [ "# Game"
    , "Current creature: " ++ " (" ++ _currentCreature ++ ") "
    , renderState _state
    , renderInitiative game
    ]

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Game PlayerChoosingTargets
chooseAbility game ability = set state (PlayerChoosingTargets ability) game

chooseTargets :: Game PlayerChoosingTargets -> [[CreatureName]] -> Game GMVettingAction
chooseTargets game@(Game {_state=(PlayerChoosingTargets ability)}) creatures
    = set state (GMVettingAction ability creatures) game

applyAbility
    :: Game GMVettingAction
    -> Maybe (Game GMVettingAction, CombatEvent)
applyAbility game@(Game {_state=GMVettingAction ability selections})
    = do
        (newGame, log) <- foldM appEffs (game, []) $ zip (_effects ability) selections
        return (newGame, AbilityUsed ability (game^.currentCreature) (reverse log))
    where
        appEffs gameAndLog (targetedEffect, creatureNames) = foldM (appEff targetedEffect) gameAndLog creatureNames
        appEff :: TargetedEffect -> (Game GMVettingAction, EffectOccurrence) -> CreatureName -> Maybe (Game GMVettingAction, EffectOccurrence)
        appEff targetedEffect (game, log) creatName = do
            let effect = (_targetedEffect targetedEffect)
            let applyEffect' = fmap $ flip applyEffect effect
            let applied = over (creaturesInPlay . at creatName) applyEffect' game
            return $ (applied, ((effect, creatName):log))

traceShowMessage message obj = trace (message ++ show obj ++ ": ") obj

getNextCircular :: (Eq a, Show a) => a -> [a] -> a
getNextCircular el l = go $ splitWhen (==el) l
    where
        go [first:_, []] = first
        go [_, next:_] = next
        go _ = error "u sux"

hasCondition :: Prism' AppliedCondition a -> Creature -> Bool
hasCondition prism creature = any match (creature^.conditions)
    where match ac = maybe False (const True) (ac ^? prism)

tickCondition :: Creature -> AppliedCondition -> Creature
tickCondition
    creat
    (AppliedRecurringEffect durLeft (RecurringEffect _ _ eff))
    = applyEffect creat eff
tickCondition creat _ = creat

-- There's a bunch of re-iterating here
decrementConditions :: Creature -> Creature
decrementConditions creature = over (conditions.mapped.durationLeft._TimedCondition) pred creature

isConditionExpired :: AppliedCondition -> Bool
isConditionExpired x = False

cleanUpConditions :: Creature -> Creature
cleanUpConditions = over conditions (filter isConditionExpired)

endTurnFor :: Creature -> Creature
endTurnFor unaffected = cleanUpConditions . decrementConditions $ (foldl' tickCondition unaffected (unaffected^.conditions))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)


-- | Advance the game so it's the next creature's turn.
-- This function should not be invoked directly, since it ignores the type of
-- the input state; instead acceptAction or skipTurn should be used to advance
-- the game.
nextTurn_ :: Game a -> WriterT [CombatEvent] Maybe (Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility))
nextTurn_ game = do
    -- TODO: This is getting confusing even with as little logic as it has. Refactor!
    let previousCreatureName = game^.currentCreature
        nextCreatureName = getNextCircular previousCreatureName (game^.initiative)
    prevCreature <- lift $ game^.creaturesInPlay.at previousCreatureName
    let previousCreatureTicked = endTurnFor prevCreature
        gameWithPreviousCreatureUpdated = set (creaturesInPlay.at previousCreatureName) (Just previousCreatureTicked) game
    nextCreature <- lift $ gameWithPreviousCreatureUpdated^.creaturesInPlay.at nextCreatureName
    let nextCreatureTurn = set currentCreature nextCreatureName gameWithPreviousCreatureUpdated
    tell [CreatureTurnStarted nextCreatureName]
    if hasCondition _AppliedDead <||> hasCondition _AppliedIncapacitated $ nextCreature then
        return (Left (set state PlayerIncapacitated nextCreatureTurn))
    else
        return (Right (set state PlayerChoosingAbility nextCreatureTurn))


nextTurn :: Game a -> Maybe (Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility))
nextTurn = ignoreLog . nextTurn_

ignoreLog :: Monad m => WriterT w m a -> m a
ignoreLog writer = (runWriterT writer) >>= (return . fst)

skipTurn_ :: Game PlayerChoosingAbility -> WriterT [CombatEvent] Maybe (Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility))
skipTurn_ = nextTurn_

skipTurn = ignoreLog . skipTurn_

skipIncapacitatedPlayer_ :: Game PlayerIncapacitated -> WriterT [CombatEvent] Maybe (Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility))
skipIncapacitatedPlayer_ = nextTurn_

skipIncapacitatedPlayer = ignoreLog . skipIncapacitatedPlayer_

acceptAction_ :: Game GMVettingAction -> WriterT [CombatEvent] Maybe (Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility))
acceptAction_ game = do
    (newGame, event) <- lift $ applyAbility game
    tell [event]
    nextTurn_ newGame

acceptAction = ignoreLog . acceptAction_

denyAction :: Game GMVettingAction -> Game PlayerChoosingAbility
denyAction game =
    set state newState game
    where newState = PlayerChoosingAbility


makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff
    = ApplyCondition $ SomeRecurringEffect $
        RecurringEffect
            cname
            (TimedCondition (Duration cdur))
            ceff
