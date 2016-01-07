{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PandT.Types where

import ClassyPrelude

import Control.Lens ((^.), (^?), (^..), at, over, view, preview,
                     makeLenses, makePrisms, set, firstOf, _head,
                     _Just)
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
    deriving (Show, Eq, Ord)

newtype Period = Period Int
    deriving (Show, Eq, Ord)

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

data RecurringEffectDef = RecurringEffectDef { _recurringEffectPeriod :: Period, _recurringEffectEffect :: Effect } deriving (Show, Eq)
data DamageIncreaseDef = DamageIncreaseDef { _damageIncrease :: DamageIntensity } deriving (Show, Eq)
data DamageDecreaseDef = DamageDecreaseDef { _damageDecrease :: DamageIntensity } deriving (Show, Eq)
data DamageAbsorbDef = DamageAbsorbDef { _damageAbsorbIntensity :: DamageIntensity } deriving (Show, Eq)
data IncapacitatedDef = IncapacitatedDef deriving (Show, Eq)
data DeadDef = DeadDef deriving (Show, Eq)

data RecurringEffectData = RecurringEffectData { _durationSinceLastTick :: Duration } deriving (Show, Eq)
data DamageAbsorbData = DamageAbsorbData { _damageAbsorbed :: Int } deriving (Show, Eq)

data ConditionDef a where
    MkRecurringEffect :: Text -> ConditionDuration -> RecurringEffectDef -> ConditionDef RecurringEffectDef
    MkDamageIncrease :: Text -> ConditionDuration -> DamageIncreaseDef -> ConditionDef DamageIncreaseDef
    MkDamageDecrease :: Text -> ConditionDuration -> DamageDecreaseDef -> ConditionDef DamageDecreaseDef
    MkDamageAbsorb :: Text -> ConditionDuration -> DamageAbsorbDef -> ConditionDef DamageAbsorbDef
    MkIncapacitated :: Text -> ConditionDuration -> ConditionDef IncapacitatedDef
    MkDead :: Text -> ConditionDuration -> ConditionDef DeadDef

deriving instance Show (ConditionDef a)
deriving instance Eq (ConditionDef a)

data ConditionCase
    = SomeRecurringEffect (ConditionDef RecurringEffectDef)
    | SomeDamageAbsorb (ConditionDef DamageAbsorbDef)
    | SomeDamageIncrease (ConditionDef DamageIncreaseDef)
    | SomeDamageDecrease (ConditionDef DamageDecreaseDef)
    | SomeIncapacitated (ConditionDef IncapacitatedDef)
    | SomeDead (ConditionDef DeadDef)
    deriving (Show, Eq)

-- A condition at runtime: contains a condition definition and the necessary
-- runtime data for that condition. This does the important job of declaring
-- types that ensure the appropriate runtime data types are associated with the
-- appropriate condition definition types.
data AppliedCondition
    = AppliedRecurringEffect ConditionDuration (ConditionDef RecurringEffectDef) RecurringEffectData
    | AppliedDamageAbsorb ConditionDuration (ConditionDef DamageAbsorbDef) DamageAbsorbData
    | AppliedDamageIncrease ConditionDuration (ConditionDef DamageIncreaseDef)
    | AppliedDamageDecrease ConditionDuration (ConditionDef DamageDecreaseDef)
    | AppliedIncapacitated ConditionDuration (ConditionDef IncapacitatedDef)
    | AppliedDead ConditionDuration (ConditionDef DeadDef)
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
makeLenses ''ConditionDef
makePrisms ''AppliedCondition
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
data GMVettingAction

data GameState a where
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
    , _currentPlayer :: Player
    , _creaturesInPlay :: Map CreatureName Creature
    , _initiative :: [CreatureName]
    }
    deriving (Show, Eq)

makeLenses ''Game


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
dead = AppliedDead UnlimitedDuration $ MkDead "Dead" UnlimitedDuration

checkDead :: Creature -> Creature
checkDead creat
    | elem dead (_conditions creat) = creat
    | _health creat <= Health 0 = over conditions (dead:) creat
    | otherwise = creat

applyCondition :: ConditionCase -> AppliedCondition
applyCondition (SomeRecurringEffect cdef@(MkRecurringEffect _ dur _)) =
    let condData=RecurringEffectData {_durationSinceLastTick=(Duration 0)}
    in AppliedRecurringEffect dur cdef condData
applyCondition (SomeDamageIncrease cdef@(MkDamageIncrease _ dur _)) = AppliedDamageIncrease dur cdef
applyCondition (SomeDamageDecrease cdef@(MkDamageDecrease _ dur _)) = AppliedDamageDecrease dur cdef
applyCondition (SomeDamageAbsorb cdef@(MkDamageAbsorb _ dur _)) =
    let condData=DamageAbsorbData {_damageAbsorbed=0}
    in AppliedDamageAbsorb dur cdef condData
applyCondition (SomeIncapacitated cdef@(MkIncapacitated _ dur)) = AppliedIncapacitated dur cdef
applyCondition (SomeDead cdef@(MkDead _ dur)) = AppliedDead dur cdef

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
    , "Current player: " ++ (playerName _currentPlayer) ++ " (" ++ _currentCreature ++ ") "
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
    -> Maybe (Game GMVettingAction)
applyAbility game@(Game {_state=GMVettingAction ability selections})
    = foldM appEffs game $ zip (_effects ability) selections
    where
        appEffs game (targetedEffect, creatureNames) = foldM (appEff targetedEffect) game creatureNames
        appEff :: TargetedEffect -> Game GMVettingAction -> CreatureName -> Maybe (Game GMVettingAction)
        appEff targetedEffect game creatName = do
            let applyEffect' = fmap $ flip applyEffect (_targetedEffect targetedEffect)
            let applied = over (creaturesInPlay . at creatName) applyEffect' game
            return $ applied


getNextCircular :: Eq a => a -> [a] -> a
getNextCircular el l = go $ snd $ partition (==el) l
    where
        go (_:[]) = headEx l
        go (_:two:_) = two
        go _ = error "u sux"


nextTurn :: Game GMVettingAction -> Game PlayerChoosingAbility
nextTurn game =
    let stateUpdated = set state PlayerChoosingAbility game
        nextCreature = getNextCircular (_currentCreature stateUpdated) (keys $ _creaturesInPlay stateUpdated)
    in set currentCreature nextCreature stateUpdated


acceptAction :: Game GMVettingAction -> Maybe (Game PlayerChoosingAbility)
acceptAction game = do
    newGame <- applyAbility game
    return $ nextTurn newGame


denyAction :: Game GMVettingAction -> Game PlayerChoosingAbility
denyAction game =
    set state newState game
    where newState = PlayerChoosingAbility


makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff
    = ApplyCondition $ SomeRecurringEffect $
        MkRecurringEffect
            cname
            (TimedCondition (Duration cdur))
            (RecurringEffectDef
                { _recurringEffectPeriod=(Period 1)
                , _recurringEffectEffect=ceff})
