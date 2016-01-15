{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PandT.Types where

import ClassyPrelude

import Control.Lens ((^.), (^?), (^..), at, over, view, preview,
                     Getter, to,
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

currentCreature :: Getter (Game a) (Maybe Creature)
currentCreature = to (\game -> game^.creaturesInPlay.at (game^.currentCreatureName))

{-
Radix used DoubleHeal.
  - Target: Aspyr, healing: High.
  - Target: Ulsoga, healing: Medium.

TODO: "dynamic" damage numbers here -- we may do less or more damage than what
is defined by the Effect, because of things like damage absorbs.

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
        , _combatEventRecurringEventEffect :: EffectOccurrence
        }
    | SkippedIncapacitatedCreatureTurn CreatureName
    | SkippedTurn CreatureName
    | CreatureTurnStarted CreatureName
    deriving (Show, Eq)

makeLenses ''CombatEvent
makePrisms ''CombatEvent

renderConditionCase :: ConditionCase -> Text
renderConditionCase (SomeRecurringEffect (RecurringEffect name duration eff))
    = name ++ " (Recurring Effect) for " ++ (tshow duration) ++ " rounds"
renderConditionCase (SomeDamageAbsorb (DamageAbsorb name duration (DamageIntensity int)))
    = name ++ " (Damage Absorb) for " ++ (tshow duration) ++ " rounds"
renderConditionCase (SomeDamageIncrease (DamageIncrease name duration (DamageIntensity int)))
    = name ++ " (Damage Increase) for " ++ (tshow duration) ++ " rounds"
renderConditionCase (SomeDamageDecrease (DamageDecrease name duration (DamageIntensity int)))
    = name ++ " (Damage Decrease) for " ++ (tshow duration) ++ " rounds"
renderConditionCase (SomeIncapacitated (Incapacitated name duration))
    = name ++ " (Incapacitate) for " ++ (tshow duration) ++ " rounds"
renderConditionCase (SomeDead (Dead name duration))
    = name ++ " (Dead) for " ++ (tshow duration) ++ " rounds"

renderEffectOccurrence :: EffectOccurrence -> Text
renderEffectOccurrence = unlines . (map go)
    where
        go (Interrupt, cname) = "interrupting " ++ cname
        go ((Heal (DamageIntensity int)), cname) = tshow int ++ " healing to " ++ cname
        go ((Damage (DamageIntensity int)), cname) = tshow int ++ " damage to " ++ cname
        go ((MultiEffect eff1 eff2), cname) = renderEffectOccurrence [(eff1, cname), (eff2, cname)]
        go ((ApplyCondition ccase), cname) = "Applying condition " ++ (renderConditionCase ccase) ++ " to " ++ cname

renderCombatEvent :: CombatEvent -> Text
renderCombatEvent (AbilityUsed {..}) =
    _combatEventAbilityOrigin ++ " used " ++ (_combatEventAbilityUsed^.abilityName) ++ ", causing:\n"
    ++ (renderEffectOccurrence _combatEventAbilityEffects) ++ ".\n"
renderCombatEvent (CreatureTurnStarted name) = name ++ "'s turn stared."

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
renderState PlayerIncapacitated = "PlayerIncapacitated"
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
        currentName = game^.currentCreatureName
        creature name = view (creaturesInPlay . at name) game
        pfx name = if name == currentName then "*" else " "
        statusLine name = unwords.toList $ renderCreatureStatus <$> creature name
        rend name = pfx name ++ statusLine name
    in
        unlines $ map rend (_initiative game)

render :: Game a -> Text
render game@(Game {..}) = unlines
    [ "# Game"
    , "Current creature: " ++ " (" ++ _currentCreatureName ++ ") "
    , renderState _state
    , renderInitiative game
    ]

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Game PlayerChoosingTargets
chooseAbility game ability = set state (PlayerChoosingTargets ability) game

chooseTargets :: Game PlayerChoosingTargets -> [SelectedTargetedEffect] -> Game GMVettingAction
chooseTargets game@(Game {_state=(PlayerChoosingTargets ability)}) selections
    = set state (GMVettingAction ability selections) game

applyAbility
    :: Game GMVettingAction
    -> Maybe (Game GMVettingAction, CombatEvent)
applyAbility game@(Game {_state=GMVettingAction ability selections})
    = do
        (newGame, log) <- foldM appEffs (game, []) selections
        return (newGame, AbilityUsed ability (game^.currentCreatureName) (reverse log))
    where
        -- appEffs :: (Game GMVettingAction, ()) -> SelectedTargetedEffect -> Maybe (Game GMVettingAction, CombatEvent)
        appEffs gameAndLog (SelectedSingleTargetedEffect creatureName targetedEffect)
            = appEff targetedEffect gameAndLog creatureName
        appEffs gameAndLog (SelectedMultiTargetedEffect creatureNames targetedEffect)
            = foldM (appEff targetedEffect) gameAndLog creatureNames

        appEff :: TargetedEffectP a -> (Game GMVettingAction, EffectOccurrence) -> CreatureName
            -> Maybe (Game GMVettingAction, EffectOccurrence)
        appEff targetedEffect (game, log) creatName = do
            let effect = targetedEffect^.targetedEffectEffect
                applyEffect' = fmap $ flip applyEffect effect
                applied = over (creaturesInPlay . at creatName) applyEffect' game
            return (applied, ((effect, creatName):log))

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
isConditionExpired ac = maybe False (== Duration 0) (ac^.durationLeft^?_TimedCondition)

cleanUpConditions :: Creature -> Creature
cleanUpConditions = over conditions (filter (not . isConditionExpired))

endTurnFor :: Creature -> Creature
endTurnFor unaffected = cleanUpConditions . decrementConditions $ (foldl' tickCondition unaffected (unaffected^.conditions))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)


type ModifiedGame = Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility)

-- | Advance the game so it's the next creature's turn.
-- This function should not be invoked directly, since it ignores the type of
-- the input state; instead acceptAction or skipTurn should be used to advance
-- the game.
nextTurn_ :: Game a -> WriterT [CombatEvent] Maybe ModifiedGame
nextTurn_ game = do
    -- TODO: This is getting confusing even with as little logic as it has. Refactor!
    let previousCreatureName = game^.currentCreatureName
        nextCreatureName = getNextCircular previousCreatureName (game^.initiative)
    prevCreature <- lift $ game^.creaturesInPlay.at previousCreatureName
    let previousCreatureTicked = endTurnFor prevCreature
        gameWithPreviousCreatureUpdated = set (creaturesInPlay.at previousCreatureName) (Just previousCreatureTicked) game
    nextCreature <- lift $ gameWithPreviousCreatureUpdated^.creaturesInPlay.at nextCreatureName
    let nextCreatureTurn = set currentCreatureName nextCreatureName gameWithPreviousCreatureUpdated
    tell [CreatureTurnStarted nextCreatureName]
    if hasCondition _AppliedDead <||> hasCondition _AppliedIncapacitated $ nextCreature then
        return (Left (set state PlayerIncapacitated nextCreatureTurn))
    else
        return (Right (set state PlayerChoosingAbility nextCreatureTurn))


nextTurn :: Game a -> Maybe ModifiedGame
nextTurn = ignoreLog . nextTurn_

ignoreLog :: Monad m => WriterT w m a -> m a
ignoreLog writer = (runWriterT writer) >>= (return . fst)

skipTurn_ :: Game PlayerChoosingAbility -> WriterT [CombatEvent] Maybe ModifiedGame
skipTurn_ = nextTurn_

skipTurn = ignoreLog . skipTurn_

skipIncapacitatedPlayer_ :: Game PlayerIncapacitated -> WriterT [CombatEvent] Maybe ModifiedGame
skipIncapacitatedPlayer_ = nextTurn_

skipIncapacitatedPlayer = ignoreLog . skipIncapacitatedPlayer_

acceptAction_ :: Game GMVettingAction -> WriterT [CombatEvent] Maybe ModifiedGame
acceptAction_ game = do
    (newGame, event) <- lift $ applyAbility game
    tell [event]
    nextTurn_ newGame

acceptAction = ignoreLog . acceptAction_

denyAction :: Game GMVettingAction -> Game PlayerChoosingAbility
denyAction game = set state PlayerChoosingAbility game


makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff
    = ApplyCondition $ SomeRecurringEffect $
        RecurringEffect
            cname
            (TimedCondition (Duration cdur))
            ceff
