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

data RecurringEffect = RecurringEffectT Effect deriving (Show, Eq)
data DamageIncrease = DamageIncreaseT DamageIntensity deriving (Show, Eq)
data DamageDecrease = DamageDecreaseT DamageIntensity deriving (Show, Eq)
data DamageAbsorb = DamageAbsorbT DamageIntensity deriving (Show, Eq)
data Incapacitated = IncapacitatedT deriving (Show, Eq)
data Dead = DeadT deriving (Show, Eq)

data ConditionC
    = RecurringEffectC RecurringEffect
    | DamageAbsorbC DamageAbsorb
    | DamageIncreaseC DamageIncrease
    | DamageDecreaseC DamageDecrease
    | IncapacitatedC Incapacitated
    | DeadC Dead
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

pattern MkConditionDef name duration c =
    ConditionDef (ConditionMeta name duration) c

-- A condition at runtime: contains a value of a condition type, and the
-- necessary runtime data for that condition. This does the important job of
-- declaring types that ensure the appropriate runtime data types are associated
-- with the appropriate condition definition types.
data AppliedC
    = AppliedRecurringEffect RecurringEffect
    | AppliedDamageAbsorb DamageAbsorb Int
    | AppliedDamageIncrease DamageIncrease
    | AppliedDamageDecrease DamageDecrease
    | AppliedIncapacitated Incapacitated
    | AppliedDead Dead
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

deriving instance Show Effect
deriving instance Eq Effect

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

currentCreature :: Lens' (Game a) (Maybe Creature)
currentCreature = lens getter setter
    where
        getter game = game^.creaturesInPlay.at (game^.currentCreatureName)
        setter game value = set (creaturesInPlay.at (game^.currentCreatureName)) value game


{-
Radix used DoubleHeal.
  - Target: Aspyr, healing: High.
  - Target: Ulsoga, healing: Medium.

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

renderConditionDef :: ConditionDef -> Text
renderConditionDef (ConditionDef meta condc) =
    meta^.conditionName ++ " (" ++ (conditionCaseName condc) ++ ") for " ++ (tshow (meta^.conditionDuration)) ++ " rounds"

conditionCaseName :: ConditionC -> Text
conditionCaseName = n
    where
        n (RecurringEffectC _) = "Recurring Effect"
        n (DamageAbsorbC _) = "Damage Absorb"
        n (DamageIncreaseC _) = "Damage Increase"
        n (DamageDecreaseC _) = "Damage Decrease"
        n (IncapacitatedC _)  = "Incapacitation"
        n (DeadC _) = "Death"

renderEffectOccurrence :: EffectOccurrence -> Text
renderEffectOccurrence = unlines . (map go)
    where
        go (Interrupt, cname) = "interrupting " ++ cname
        go ((Heal (DamageIntensity int)), cname) = tshow int ++ " healing to " ++ cname
        go ((Damage (DamageIntensity int)), cname) = tshow int ++ " damage to " ++ cname
        go ((MultiEffect eff1 eff2), cname) = renderEffectOccurrence [(eff1, cname), (eff2, cname)]
        go ((ApplyCondition cdef), cname) = "Applying condition " ++ (renderConditionDef cdef) ++ " to " ++ cname

renderCombatEvent :: CombatEvent -> Text
renderCombatEvent (AbilityUsed {..}) =
    _combatEventAbilityOrigin ++ " used " ++ (_combatEventAbilityUsed^.abilityName) ++ ", causing:\n"
    ++ (renderEffectOccurrence _combatEventAbilityEffects) ++ ".\n"
renderCombatEvent (CreatureTurnStarted name) = name ++ "'s turn stared."
renderCombatEvent (AbilityStartCast cname ab) = cname ++ " started casting " ++ (ab^.abilityName) ++ "."

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
applyConditionC (RecurringEffectC re) = AppliedRecurringEffect re
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
        go (ApplyCondition cdef) = over conditions (applyCondition cdef:) creature
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
        castSumm = case creature^.casting of
            Nothing -> ""
            Just (ability, (Duration duration)) ->
                "(casting " ++ (ability^.abilityName) ++ " for " ++ (tshow duration) ++ " more rounds)"
        line = unwords [creature^.creatureName, hp, castSumm, conds]

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

applyAbility :: Game GMVettingAction -> Maybe (Game GMVettingAction, CombatEvent)
applyAbility game@(Game {_state=GMVettingAction ability selections}) =
    case ability^.castTime of
        (CastTime 0) -> applyAbilityEffects game
        (CastTime castTime) -> do
            creature <- game^.currentCreature
            let currentCreatureLens = creaturesInPlay.at (game^.currentCreatureName)
                newGame = (set (currentCreatureLens._Just.casting) (Just (ability, (Duration castTime))) game)
            return (newGame, AbilityStartCast (game^.currentCreatureName) ability)

applyAbilityEffects
    :: Game GMVettingAction
    -> Maybe (Game GMVettingAction, CombatEvent)
applyAbilityEffects game@(Game {_state=GMVettingAction ability selections})
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

hasCondition :: Prism' AppliedC a -> Creature -> Bool
hasCondition prism creature = any match (map _appliedConditionC (creature^.conditions))
    where match ac = maybe False (const True) (ac ^? prism)

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
endTurnFor unaffected = cleanUpConditions . decrementConditions $ (foldl' tickCondition unaffected (unaffected^.conditions))

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)


-- | A game at the start of a turn -- represents the subset of states that a
-- game can be in when a player starts their turn.
data GameStartTurn
    = GSTPlayerChoosingAbility (Game PlayerChoosingAbility)
    | GSTPlayerIncapacitated (Game PlayerIncapacitated)
    | GSTPlayerCasting (Game PlayerCasting)
    | GSTPlayerFinishingCast (Game PlayerFinishingCast)
    deriving (Show, Eq)


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

skipTurn = ignoreLog . skipTurn_

skipIncapacitatedPlayer_ :: Game PlayerIncapacitated -> WriterT [CombatEvent] Maybe GameStartTurn
skipIncapacitatedPlayer_ = nextTurn_

skipIncapacitatedPlayer = ignoreLog . skipIncapacitatedPlayer_

acceptAction_ :: Game GMVettingAction -> WriterT [CombatEvent] Maybe GameStartTurn
acceptAction_ game = do
    (newGame, event) <- lift $ applyAbility game
    tell [event]
    nextTurn_ newGame

acceptAction = ignoreLog . acceptAction_

denyAction :: Game GMVettingAction -> Game PlayerChoosingAbility
denyAction game = set state PlayerChoosingAbility game

class CancelCast a where
    cancelCast :: Game a -> Game PlayerChoosingAbility
    cancelCast game =
        set state PlayerChoosingAbility $
            set (currentCreature._Just.casting) Nothing game

instance CancelCast PlayerFinishingCast where
instance CancelCast PlayerCasting where


finishCast :: Game PlayerFinishingCast -> [SelectedTargetedEffect] -> Maybe (Game GMVettingAction)
finishCast game selections = do
    creature <- game^.currentCreature
    (ability, duration) <- creature^.casting
    return (set state (GMVettingAction ability selections) game)

makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff
    = ApplyCondition (
        MkConditionDef
            cname
            (TimedCondition (Duration cdur))
            (MkRecurringEffectC ceff))
