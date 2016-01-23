{-# LANGUAGE RankNTypes #-}

-- | All the core simulation code for P&T. Pure functions on the data in the PandT.Types module.

module PandT.Sim where

import ClassyPrelude

import Control.Lens
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)

import PandT.Types

staminaToHealth :: Stamina -> Health
staminaToHealth (Stamina High) = Health 10
staminaToHealth (Stamina Medium) = Health 5
staminaToHealth (Stamina Low) = Health 3

decreaseHealth :: Health -> DamageIntensity -> Health
decreaseHealth (Health healthVal) dmg = Health (healthVal -  dmg)

increaseHealth :: Health -> DamageIntensity -> Health
increaseHealth (Health healthVal) dmg = Health (healthVal + dmg)

makeCreature :: CreatureName -> Resource -> Stamina -> [Ability] -> Creature
makeCreature cname res sta creatAbilities = Creature
    { _creatureName=cname
    , _conditions=[]
    , _resource=res
    , _stamina=sta
    , _health=staminaToHealth sta
    , _abilities=creatAbilities
    , _casting=Nothing}


deadDef :: ConditionDef
deadDef = MkConditionDef "Dead" UnlimitedDuration MkDeadC

appliedDead :: AppliedCondition
appliedDead = applyCondition deadDef

checkDead :: Creature -> Creature
checkDead creat
    | hasCondition _AppliedDead creat = creat
    | _health creat <= Health 0 = over conditions (appliedDead:) creat
    | otherwise = creat

applyConditionC :: ConditionC -> AppliedC
applyConditionC (RecurringEffectC reff) = AppliedRecurringEffect reff
applyConditionC (DamageIncreaseC di) = AppliedDamageIncrease di
applyConditionC (DamageDecreaseC dd) = AppliedDamageDecrease dd
applyConditionC (DamageAbsorbC da) = AppliedDamageAbsorb da 0
applyConditionC (IncapacitatedC i) = AppliedIncapacitated i
applyConditionC (DeadC d) = AppliedDead d

applyCondition :: ConditionDef -> AppliedCondition
applyCondition cd = AppliedCondition (cd^.conditionDuration) (cd^.conditionDefMeta) (applyConditionC (cd^.conditionDefC))

applyEffect :: Creature -> Effect -> Creature
applyEffect creature effect = checkDead $ go effect
    where
        go Interrupt = set casting Nothing creature
        go (ApplyCondition cdef) = over conditions (applyCondition cdef:) creature
        go (Damage amt) = over health (flip decreaseHealth amt) creature
        go (Heal amt) = over health (flip increaseHealth amt) creature
        go (MultiEffect e1 e2) = applyEffect (applyEffect creature e1) e2

-- Workflow

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Game PlayerChoosingTargets
chooseAbility game ability = set state (PlayerChoosingTargets ability) game

chooseTargets :: Game PlayerChoosingTargets -> [SelectedTargetedEffect] -> Game GMVettingAction
chooseTargets game@(Game {_state=(PlayerChoosingTargets ability)}) selections
    = set state (GMVettingAction ability selections) game

applyAbility :: Game GMVettingAction -> Maybe (Game GMVettingAction, CombatEvent)
applyAbility game@(Game {_state=GMVettingAction ability _}) =
    case ability^.castTime of
        (CastTime 0) -> applyAbilityEffects game
        (CastTime timeLeft) -> do
            let currentCreatureLens = creaturesInPlay.at (game^.currentCreatureName)
                newGame = (set (currentCreatureLens._Just.casting) (Just (ability, (Duration timeLeft))) game)
            return (newGame, AbilityStartCast (game^.currentCreatureName) ability)

applyAbilityEffects
    :: Game GMVettingAction
    -> Maybe (Game GMVettingAction, CombatEvent)
applyAbilityEffects game@(Game {_state=GMVettingAction ability selections})
    = do
        (newGame, combatLog) <- foldM appEffs (game, []) selections
        return (newGame, AbilityUsed ability (game^.currentCreatureName) (reverse combatLog))
    where
        appEffs :: (Game GMVettingAction, EffectOccurrence) -> SelectedTargetedEffect
                -> Maybe (Game GMVettingAction, EffectOccurrence)
        appEffs gameAndLog (SelectedSingleTargetedEffect name targetedEffect)
            = appTEff targetedEffect gameAndLog name
        appEffs gameAndLog (SelectedMultiTargetedEffect creatureNames targetedEffect)
            = foldM (appTEff targetedEffect) gameAndLog creatureNames

appTEff :: TargetedEffectP a -> (Game GMVettingAction, EffectOccurrence) -> CreatureName
    -> Maybe (Game GMVettingAction, EffectOccurrence)
appTEff targetedEffect (game, combatLog) creatName = do
    let effect = targetedEffect^.targetedEffectEffect
        applyEffect' = fmap $ flip applyEffect effect
        applied = over (creaturesInPlay . at creatName) applyEffect' game
    return (applied, ((effect, creatName):combatLog))

traceShowMessage :: Show a => String -> a -> a
traceShowMessage message obj = trace (message ++ (show obj) ++ ": ") obj

getNextCircular :: (Eq a, Show a) => a -> [a] -> a
getNextCircular el l = go $ splitWhen (==el) l
    where
        go [firstEl:_, []] = firstEl
        go [_, next:_] = next
        go _ = error "u sux"

hasCondition :: Prism' AppliedC a -> Creature -> Bool
hasCondition appliedCPrism creature = any match (map _appliedConditionC (creature^.conditions))
    where match ac = maybe False (const True) (ac ^? appliedCPrism)

tickCondition :: Creature -> AppliedCondition -> Creature
tickCondition creat (AppliedCondition _ _ (AppliedRecurringEffect (RecurringEffectT eff))) = applyEffect creat eff
tickCondition creat _ = creat

-- There's a bunch of re-iterating here
decrementConditions :: Creature -> Creature
decrementConditions creature = over (conditions.mapped.appliedConditionDurationLeft._TimedCondition) pred creature

isConditionExpired :: AppliedCondition -> Bool
isConditionExpired ac = maybe False (== Duration 0) (ac^.appliedConditionDurationLeft^?_TimedCondition)

cleanUpConditions :: Creature -> Creature
cleanUpConditions = over conditions (filter (not . isConditionExpired))

endTurnFor :: Creature -> Creature
endTurnFor creature = cleanUpConditions . decrementConditions $ (foldl' tickCondition creature (creature^.conditions))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

-- | Advance the game so it's the next creature's turn.
-- This function should not be invoked directly, since it ignores the type of
-- the input state; instead acceptAction or skipTurn should be used to advance
-- the game.
nextTurn_ :: Game a -> WriterT [CombatEvent] Maybe GameStartTurn
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
    if
        | hasCondition _AppliedDead <||> hasCondition _AppliedIncapacitated $ nextCreature ->
            return (GSTPlayerIncapacitated (set state PlayerIncapacitated nextCreatureTurn))
        -- | not (isNothing (nextCreature^.casting)) ->
        | maybe False (> (Duration 0)) (nextCreature^.casting^?_Just._2) ->
            return (GSTPlayerCasting (set state PlayerCasting nextCreatureTurn))
        | (nextCreature^.casting^?_Just._2) == Just (Duration 0) ->
            return (GSTPlayerFinishingCast (set state PlayerFinishingCast nextCreatureTurn))
        | otherwise ->
            return (GSTPlayerChoosingAbility (set state PlayerChoosingAbility nextCreatureTurn))


nextTurn :: Game a -> Maybe GameStartTurn
nextTurn = ignoreLog . nextTurn_

ignoreLog :: Monad m => WriterT w m a -> m a
ignoreLog writer = (runWriterT writer) >>= (return . fst)

skipTurn_ :: Game PlayerChoosingAbility -> WriterT [CombatEvent] Maybe GameStartTurn
skipTurn_ = nextTurn_

skipTurn :: Game PlayerChoosingAbility -> Maybe GameStartTurn
skipTurn = ignoreLog . skipTurn_

skipIncapacitatedPlayer_ :: Game PlayerIncapacitated -> WriterT [CombatEvent] Maybe GameStartTurn
skipIncapacitatedPlayer_ = nextTurn_

skipIncapacitatedPlayer :: Game PlayerIncapacitated -> Maybe GameStartTurn
skipIncapacitatedPlayer = ignoreLog . skipIncapacitatedPlayer_

acceptAction_ :: Game GMVettingAction -> WriterT [CombatEvent] Maybe GameStartTurn
acceptAction_ game = do
    (newGame, event) <- lift $ applyAbility game
    tell [event]
    nextTurn_ newGame

acceptAction :: Game GMVettingAction -> Maybe GameStartTurn
acceptAction = ignoreLog . acceptAction_

denyAction :: Game GMVettingAction -> Game PlayerChoosingAbility
denyAction game = set state PlayerChoosingAbility game

class CancelCast a where
    -- cancelCast :: Game PlayerCasting -> WriterT [CombatEvent] Maybe GameStartTurn
    -- cancelCast game = do
    --     ability <- lift (game^?currentCreature._Just.casting._Just._1)
    --     tell ([CanceledCast (game^.currentCreatureName) ability])
    --     return (set
    --     nextTurn_ game

    cancelCast :: Game a -> Game PlayerChoosingAbility
    cancelCast game =
        set state PlayerChoosingAbility $
            set (currentCreature._Just.casting) Nothing game

instance CancelCast PlayerFinishingCast where
instance CancelCast PlayerCasting where

continueCasting_ :: Game PlayerCasting -> WriterT [CombatEvent] Maybe GameStartTurn
continueCasting_ = nextTurn_

finishCast :: Game PlayerFinishingCast -> [SelectedTargetedEffect] -> Maybe (Game GMVettingAction)
finishCast game selections = do
    creature <- game^.currentCreature
    (ability, _) <- creature^.casting
    return (set state (GMVettingAction ability selections) game)
