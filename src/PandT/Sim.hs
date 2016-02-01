{-# LANGUAGE RankNTypes #-}

-- | All the core simulation code for P&T. Pure functions on the data in the PandT.Types module.
-- These functions are meant to be game-agnostic, inasmuch as the core PandT.Types model is
-- game-agnostic (not very).

module PandT.Sim where

import PandT.Prelude
import PandT.Types
import qualified Data.Map as DM

import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)


staminaToHealth :: Stamina -> Health
staminaToHealth (Stamina High) = Health 10
staminaToHealth (Stamina Medium) = Health 5
staminaToHealth (Stamina Low) = Health 3

decreaseHealth :: Health -> DamageIntensity -> Health
decreaseHealth (Health healthVal) dmg = Health (healthVal -  dmg)

increaseHealth :: Health -> DamageIntensity -> Health
increaseHealth (Health healthVal) dmg = Health (healthVal + dmg)

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

deadDef :: ConditionDef
deadDef = MkConditionDef "Dead" UnlimitedDuration MkDeadC

applyDead :: CreatureName -> AppliedCondition
applyDead killerName = applyCondition killerName deadDef

checkDead :: CreatureName -> Creature -> Creature
checkDead originCreatureName creat
    | hasCondition _AppliedDead creat = creat
    | _health creat <= Health 0 = over conditions ((applyDead originCreatureName):) creat
    | otherwise = creat

applyConditionC :: ConditionC -> AppliedC
applyConditionC (RecurringEffectC reff) = AppliedRecurringEffect reff
applyConditionC (DamageIncreaseC di) = AppliedDamageIncrease di
applyConditionC (DamageDecreaseC dd) = AppliedDamageDecrease dd
applyConditionC (IncomingDamageReductionC da) = AppliedIncomingDamageReduction da
applyConditionC (IncapacitatedC i) = AppliedIncapacitated i
applyConditionC (DeadC d) = AppliedDead d

applyCondition :: CreatureName -> ConditionDef -> AppliedCondition
applyCondition originCreatureName cd = AppliedCondition originCreatureName (cd^.conditionDuration) (cd^.conditionDefMeta) (applyConditionC (cd^.conditionDefC))

applyEffect :: Creature -> Effect -> Creature -> Creature
applyEffect originCreature effect targetCreature = (checkDead originCreatureName) $ go effect
    where
        originCreatureName = (originCreature^.creatureName)
        go Interrupt = set casting Nothing targetCreature
        go Resurrect = removeConditions _AppliedDead targetCreature
        go (GenerateEnergy nrg) = over creatureEnergy (regEnergy nrg) targetCreature
        go (ApplyCondition cdef) = over conditions (applyCondition originCreatureName cdef:) targetCreature
        go (Damage amt) = applyDamage originCreature amt targetCreature
        go (Heal amt) = over health (heal targetCreature amt) targetCreature
        go (MultiEffect e1 e2) = applyEffect originCreature e2 (applyEffect originCreature e1 targetCreature)

heal :: Creature -> DamageIntensity -> Health -> Health
heal target amt oldHealth = min (increaseHealth oldHealth amt) (staminaToHealth (target^.stamina))

regEnergy :: Energy -> Energy -> Energy
regEnergy increase oldEnergy = min (oldEnergy + increase) 10

applyDamage :: Creature -> DamageIntensity -> Creature -> Creature
applyDamage originCreature amt targetCreature =
    let damageIncreases = getAppliedConditionsMatching _AppliedDamageIncrease originCreature
        damageDecreases = getAppliedConditionsMatching _AppliedDamageDecrease originCreature
        sumIncreases = sum (map _intensityIncrease damageIncreases)
        sumDecreases = sum (map _intensityDecrease damageDecreases)
        damageReductions = getAppliedConditionsMatching _AppliedIncomingDamageReduction targetCreature
        sumReductions = sum (map _intensityReduce damageReductions)
        damageDelta = sumIncreases + (negate sumDecreases) + (negate sumReductions)
        damage = amt + damageDelta
        minDamage = if damage <= 0 then 1 else damage
    in
        over health (flip decreaseHealth minDamage) targetCreature

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
        originCreature <- game^.currentCreature
        (newGame, combatLog) <- foldM (appEffs originCreature) (game, []) selections
        let newGame' = over (currentCreature._Just.creatureEnergy) (\x -> (x - (ability^.cost))) newGame
            newGame'' = case ability^.cooldown of
                            (Cooldown 0) -> newGame'
                            n -> over (currentCreature._Just.cooldowns) (insertMap (ability^.abilityName) n) newGame'
        return (newGame'', AbilityUsed ability (game^.currentCreatureName) (reverse combatLog))
    where
        appEffs :: Creature -> (Game GMVettingAction, EffectOccurrence) -> SelectedTargetedEffect
                -> Maybe (Game GMVettingAction, EffectOccurrence)
        appEffs originCreature gameAndLog (SelectedSingleTargetedEffect name targetedEffect)
            = appTEff originCreature targetedEffect gameAndLog name
        appEffs originCreature gameAndLog (SelectedMultiTargetedEffect creatureNames targetedEffect)
            = foldM (appTEff originCreature targetedEffect) gameAndLog creatureNames
        appEffs originCreature gameAndLog (SelectedSelfTargetedEffect targetedEffect)
            -- XXX FIXME TODO: I'm passing the creature name when I already have the Creature
            = foldM (appTEff originCreature targetedEffect) gameAndLog [(originCreature^.creatureName)]

appTEff :: Creature -> TargetedEffectP a -> (Game GMVettingAction, EffectOccurrence) -> CreatureName
        -> Maybe (Game GMVettingAction, EffectOccurrence)
appTEff originCreature targetedEffect (game, combatLog) creatName = do
    let effect = targetedEffect^.targetedEffectEffect
        applyEffect' = fmap (applyEffect originCreature effect)
        applied = over (creaturesInPlay . at creatName) applyEffect' game
    return (applied, ((effect, creatName):combatLog))

getNextCircular :: (Eq a, Show a) => a -> [a] -> a
getNextCircular el l = go $ splitWhen (==el) l
    where
        go [firstEl:_, []] = firstEl
        go [_, next:_] = next
        go _ = error "u sux"

getAppliedConditionsMatching :: Prism' AppliedC a -> Creature -> [a]
getAppliedConditionsMatching appliedCPrism creature =
    mapMaybe (\cond -> cond^?appliedConditionC.appliedCPrism) (creature^.conditions)

hasCondition :: Prism' AppliedC a -> Creature -> Bool
hasCondition appliedCPrism creature = (not.null) (getAppliedConditionsMatching appliedCPrism creature)

isDead :: Creature -> Bool
isDead = hasCondition _AppliedDead

removeConditions :: Prism' AppliedC a -> Creature -> Creature
removeConditions appliedCPrism creature = creature {_conditions=filter predic (creature^.conditions)}
    where
        predic cond = isNothing (cond^?appliedConditionC.appliedCPrism)

-- | tickCondition returns Maybe because it needs to look up the origin creature by name in the
-- game.
tickCondition :: Game a -> Creature -> AppliedCondition -> Maybe Creature
tickCondition game creat (AppliedCondition originCreatureName _ _ (AppliedRecurringEffect (RecurringEffectT eff))) = do
    originCreature <- game^.creaturesInPlay.at originCreatureName
    return (applyEffect originCreature eff creat)
tickCondition _ creat _ = Just creat

-- There's a bunch of re-iterating here
decrementConditions :: Creature -> Creature
decrementConditions creature = over (conditions.mapped.appliedConditionDurationLeft._TimedCondition) pred creature

isConditionExpired :: AppliedCondition -> Bool
isConditionExpired ac = maybe False (== Duration 0) (ac^?appliedConditionDurationLeft._TimedCondition)

cleanUpConditions :: Creature -> Creature
cleanUpConditions = over conditions (filter (not . isConditionExpired))

endTurnFor :: Game a -> Creature -> Maybe Creature
endTurnFor game creature = do
    conditionsTicked <- foldM (tickCondition game) creature (creature^.conditions)
    let cooldownsTicked = over cooldowns (DM.mapMaybe tickCooldown) conditionsTicked
    return (cleanUpConditions (decrementConditions cooldownsTicked))

tickCooldown :: Cooldown -> Maybe Cooldown
tickCooldown (Cooldown 0) = Nothing
tickCooldown (Cooldown n) = Just (Cooldown (n-1))

-- The workflow: functions that transition between types of Game.

usableAbilities :: Creature -> [Ability]
usableAbilities c = filter (offCooldown c <&&> enoughEnergy c) (c^.abilities)

offCooldown :: Creature -> Ability -> Bool
offCooldown c ab = notMember (ab^.abilityName) (c^.cooldowns)

enoughEnergy :: Creature -> Ability -> Bool
enoughEnergy c ab = (ab^.cost) <= (c^.creatureEnergy)

data ChooseAbilityError
    = InconsistentError -- ^ ok this is dumb but it's basically when our creature name lookups fail
    | NotEnoughEnergyError
    | AbilityOnCooldownError
    deriving (Show, Eq)

chooseAbility :: Game PlayerChoosingAbility -> Ability
              -> Either ChooseAbilityError (Game PlayerChoosingTargets)
chooseAbility game ability =
    case game^.currentCreature of
        Nothing -> Left InconsistentError
        Just cc -> do
            if
                | not (enoughEnergy cc ability) -> Left NotEnoughEnergyError
                | not (offCooldown cc ability) -> Left AbilityOnCooldownError
                | otherwise -> return (set state (PlayerChoosingTargets ability) game)

chooseTargets :: Game PlayerChoosingTargets -> [SelectedTargetedEffect] -> Game GMVettingAction
chooseTargets game@(Game {_state=(PlayerChoosingTargets ability)}) selections
    = set state (GMVettingAction ability selections) game

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
    previousCreatureTicked <- lift (endTurnFor game prevCreature)
    let gameWithPreviousCreatureUpdated = set (creaturesInPlay.at previousCreatureName) (Just previousCreatureTicked) game
    nextCreature <- lift $ gameWithPreviousCreatureUpdated^.creaturesInPlay.at nextCreatureName
    let nextCreatureTurn = set currentCreatureName nextCreatureName gameWithPreviousCreatureUpdated
    tell [CreatureTurnStarted nextCreatureName]
    if
        | hasCondition _AppliedDead <||> hasCondition _AppliedIncapacitated $ nextCreature ->
            return (GSTPlayerIncapacitated (set state PlayerIncapacitated nextCreatureTurn))
        | maybe False (> (Duration 0)) (nextCreature^?casting._Just._2) ->
            return (GSTPlayerCasting (set state PlayerCasting nextCreatureTurn))
        | (nextCreature^?casting._Just._2) == Just (Duration 0) ->
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
