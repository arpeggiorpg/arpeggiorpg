{-# LANGUAGE RankNTypes, PartialTypeSignatures, NoMonomorphismRestriction #-}

import Control.Lens
import Control.Monad.Writer.Strict
import Data.Either
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit

import Math.Geometry.Grid.Octagonal (UnboundedOctGrid(..))
import Math.Geometry.GridMap.Lazy (lazyGridMapIndexed)

import PandT.Prelude
import PandT.Types
import PandT.Abilities
import PandT.Sim


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [turnTests, conditionTests, abilityTests, logTests]

logTests :: TestTree
logTests = testGroup "Log Tests"
    [ testCase "punch log" $
        punchLog @?= [ AbilityUsed punch "Radorg" [(punchEffect, "Aspyr")]
                     , CreatureTurnStarted "Aspyr"]
    ]

turnTests :: TestTree
turnTests = testGroup "Turn Tests"
    [ testCase "After Radorg acts, Aspyr goes" $
        stabAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "After Aspyr acts, Ulsoga goes" $
        (forceNextTurn stabAccepted)^.currentCreatureName @?= "Ulsoga"
    , testCase "After Ulsoga acts, Radorg goes" $
        (forceNextTurn (forceNextTurn stabAccepted))^.currentCreatureName @?= "Radorg"
    ]

conditionTests :: TestTree
conditionTests = testGroup "Condition Tests"
    [ testCase "RecurringEffect recurs on end of target's turn" $
        afterBleedTick^?creaturesInPlay.at "Aspyr"._Just.health @?= Just (Health 5)
    , testCase "Conditions end" $
        afterBleedEnd^?creaturesInPlay.at "Aspyr"._Just.conditions @?= Just []
    , testCase "Dead creature is dead" $
        isDead deadCreature @?= True
    , testCase "Damage to dead creature does not cause additional Dead condition" $
        length (getAppliedConditionsMatching _AppliedDead deadTwice) @?= 1
    , testCase "Dead creature gets a turn" $
        killAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "Incapacitated creature getsp a turn" $
        bonkAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "Incapacitated creature has conditions ticked" $
        afterBonk^?creaturesInPlay.at "Aspyr"._Just.conditions @?= Just []
    , testCase "UnlimitedDuration conditions never expire" $
        let afterSecondDeadTurn = (forceNextTurn . forceNextTurnIncap . forceNextTurn . forceNextTurn $ killAccepted)
            (Just deadAspyr) = afterSecondDeadTurn^.creaturesInPlay.at "Aspyr"
        in isDead deadAspyr @?= True
    , testCase "Timed conditions expire when duration reaches 0" $
        let afterSecondBleedTurn = (forceNextTurn . forceNextTurn . forceNextTurn . forceNextTurn $ stabAccepted)
        in afterSecondBleedTurn^?creaturesInPlay.at "Aspyr"._Just.conditions @?= Just []
    , testCase "DamageIncrease applies to direct damage" $
        blStabAccepted^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 6)
    , testCase "DamageIncrease applies to RecurringEffect damage" $
        afterBLBleedTick^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 3)
    , testCase "DamageDecrease applies to direct damage" $
        weakStabAccepted^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 8)
    , testCase "DamageDecrease applies to RecurringEffect damage" $
        afterWeakBleedTick^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 7)
    -- block reduces damage by 3, but min damage is 1
    , testCase "IncomingDamageReduction reduces incoming damage" $
        stabThroughBlockAccepted^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 9)
    , testCase "IncomingDamageReduction reduces recurring damage" $
        afterBlockedBleedTick^?creaturesInPlay.at "Radorg"._Just.health @?= Just (Health 8)
    ]

abilityTests :: TestTree
abilityTests =
    let (Just aspyrPunched) = punchAccepted^.creaturesInPlay.at "Aspyr"
        (Just aspyrStabbed) = stabAccepted^.creaturesInPlay.at "Aspyr"
    in
    testGroup "Ability Tests"
    [ testCase "Ability damage takes effect" $
        aspyrPunched^.health @?= (Health 7)
    , testCase "Ability condition in multi-effect adds condition" $
        aspyrStabbed^.conditions @?= [appliedBleed "Radorg"]
    , testCase "Ability on cooldown can't be used" $
        chooseAbility stabOnCooldown stab @?= Left AbilityOnCooldownError
    , testCase "Abilities eventually cool down" $
        isRight (chooseAbility stabOffCooldown stab) @?= True
    ]

creat, dotted, damaged, healed, deadCreature, deadTwice :: Creature
creat = makeCreature "Creat the Geat" (Energy 100) (Stamina High) [stab]
dotted = applyEffect creat bleed creat
damaged = applyEffect creat (Damage 3) creat
healed = applyEffect creat (Heal 2) damaged
deadCreature = foldl' (\tmpcreat eff -> applyEffect creat eff tmpcreat) creat (take 2 $ repeat (Damage 5))
deadTwice = applyEffect creat (Damage 2) deadCreature

bleedCondition :: ConditionDef
(Just bleedCondition) = bleed^?_ApplyCondition

appliedBleed :: CreatureName -> AppliedCondition
appliedBleed creatName = applyCondition creatName bleedCondition

simulateMove :: Game PlayerChoosingAbility -> Ability -> CreatureName
             -> (Game PlayerChoosingTargets,
                 Game GMVettingAction,
                 GameStartTurn,
                 [CombatEvent])
simulateMove game ability target =
    let (Right targeting) = chooseAbility game ability
        vetting = case ability^.abilityEffects of
            [(SingleTargetedEffect firstTEffect)] -> chooseTargets targeting [SelectedSingleTargetedEffect target firstTEffect]
            [(SelfTargetedEffect firstTEffect)] -> chooseTargets targeting [SelectedSelfTargetedEffect firstTEffect]
        (Just (accepted, log)) = runWriterT $ acceptAction_ vetting
    in (targeting, vetting, accepted, log)

chris = Player "Chris"
jah = Player "Jah"
beth = Player "Beth"

allAbilities = [stab, punch, kill, bonk, bloodlust, soothe, weaken]
radorg = makeCreature "Radorg" (Energy 100) (Stamina High) allAbilities
aspyr = makeCreature "Aspyr" (Energy 100) (Stamina High) allAbilities
ulsoga = makeCreature "Ulsoga" (Energy 100) (Stamina High) allAbilities

myGame :: Game PlayerChoosingAbility
myGame = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr"), (beth, "Ulsoga")]
    , _currentCreatureName="Radorg"
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr), ("Ulsoga", ulsoga)]
    , _initiative=["Radorg", "Aspyr", "Ulsoga"]
    , _gameGeo=lazyGridMapIndexed UnboundedOctGrid []
    }

forceNextTurn :: Game a -> Game PlayerChoosingAbility
forceNextTurn game =
    case nextTurn game of
        (Just (GSTPlayerIncapacitated g)) -> terror ("Expected player choosing ability when moving from " ++ game^.currentCreatureName ++ " to " ++ g^.currentCreatureName)
        (Just (GSTPlayerChoosingAbility g)) -> g

forceNextTurnIncap :: Game a -> Game PlayerIncapacitated
forceNextTurnIncap game =
    case nextTurn game of
        (Just (GSTPlayerIncapacitated g)) -> g
        (Just (GSTPlayerChoosingAbility g)) -> terror ("Expected incapacitated player when moving from " ++ game^.currentCreatureName ++ " to " ++ g^.currentCreatureName)

(_, _, (GSTPlayerChoosingAbility punchAccepted), punchLog) = simulateMove myGame punch "Aspyr"

(_, _, (GSTPlayerChoosingAbility stabAccepted), _) = simulateMove myGame stab "Aspyr"
afterBleedTick = forceNextTurn stabAccepted
(_, _, (GSTPlayerIncapacitated killAccepted), _) = simulateMove myGame kill "Aspyr"
stabOnCooldown = forceNextTurn afterBleedTick
afterBleedEnd = forceNextTurn (forceNextTurn stabOnCooldown)
stabOffCooldown = forceNextTurn afterBleedEnd

(_, _, (GSTPlayerIncapacitated bonkAccepted), _) = simulateMove myGame bonk "Aspyr"
afterBonk = forceNextTurn bonkAccepted

(_, _, (GSTPlayerChoosingAbility blAccepted), _) = simulateMove myGame bloodlust "Aspyr"
(_, _, (GSTPlayerChoosingAbility blStabAccepted), _) = simulateMove blAccepted stab "Radorg" -- hm! stabbing the hand that feeds you!
afterBLBleedTick = forceNextTurn (forceNextTurn blStabAccepted)

(_, _, (GSTPlayerChoosingAbility weakenAccepted), _) = simulateMove myGame weaken "Aspyr"
(_, _, (GSTPlayerChoosingAbility weakStabAccepted), _) = simulateMove weakenAccepted stab "Radorg"
afterWeakBleedTick = forceNextTurn (forceNextTurn weakStabAccepted)

(_, _, (GSTPlayerChoosingAbility blockAccepted), _) = simulateMove myGame block "unused"
(_, _, (GSTPlayerChoosingAbility stabThroughBlockAccepted), _) = simulateMove blockAccepted stab "Radorg"
afterBlockedBleedTick = forceNextTurn (forceNextTurn stabThroughBlockAccepted)
