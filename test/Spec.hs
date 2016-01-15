import Control.Lens
import Control.Monad.Writer.Strict
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import PandT.Types
import PandT.Abilities

import ClassyPrelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [turnTests, conditionTests, abilityTests, logTests]

logTests :: TestTree
logTests = testGroup "Log Tests"
    [ testCase "punch log" $
        punchLog @?= [AbilityUsed punch "Radorg" [(punchEffect, "Aspyr")]
                     ,CreatureTurnStarted "Aspyr"]
    ]

turnTests :: TestTree
turnTests = testGroup "Turn Tests"
    [ testCase "After Radorg acts, Aspyr goes" $
        stabAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "After Aspyr acts, Ulsoga goes" $
        (nextTurn stabAccepted)^?_Just._Right.currentCreatureName @?= Just "Ulsoga"
    , testCase "After Ulsoga acts, Radorg goes" $
        (nextTurn =<< (nextTurn stabAccepted)^?_Just._Right)^?_Just._Right.currentCreatureName @?= Just "Radorg"
    ]

conditionTests :: TestTree
conditionTests = testGroup "Condition Tests"
    [ testCase "RecurringEffect recurs on end of target's turn" $
        afterBleedTick^.creaturesInPlay.at "Aspyr"^?_Just.health @?= Just (Health 65)
    , testCase "Conditions end" $
        afterBleedEnd^.creaturesInPlay.at "Aspyr"^?_Just.conditions @?= Just []
    , testCase "Dead creature is dead" $
        deadCreature^.conditions @?= [dead]
    , testCase "Damage to dead creature does not cause additional Dead condition" $
        deadTwice^.conditions @?= [dead]
    , testCase "Dead creature gets a turn" $
        killAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "Incapacitated creature gets a turn" $
        bonkAccepted^.currentCreatureName @?= "Aspyr"
    , testCase "Incapacitated creature has conditions ticked" $
        afterBonk^.creaturesInPlay.at "Aspyr"^?_Just.conditions @?= Just []
    ]

abilityTests :: TestTree
abilityTests =
    let (Just aspyrPunched) = punchAccepted^.creaturesInPlay.at "Aspyr"
        (Just aspyrStabbed) = stabAccepted^.creaturesInPlay.at "Aspyr"
    in
    testGroup "Ability Tests"
    [ testCase "Ability damage takes effect" $
        aspyrPunched^.health @?= (Health 75)
    , testCase "Ability condition in multi-effect adds condition" $
        aspyrStabbed^.conditions @?= [appliedBleed]
    ]

creat = makeCreature "Creat the Geat" (Energy 100) (Stamina High) [stab]
dotted = applyEffect creat bleed
damaged = applyEffect creat (Damage (DamageIntensity Medium))
healed = applyEffect damaged (Heal (DamageIntensity Low))
deadCreature = foldl' applyEffect creat (take 2 $ repeat (Damage (DamageIntensity High)))
deadTwice = (applyEffect deadCreature (Damage (DamageIntensity Low)))


simulateMove :: Game PlayerChoosingAbility -> Ability -> CreatureName
             -> (Game PlayerChoosingTargets,
                 Game GMVettingAction,
                 Either (Game PlayerIncapacitated) (Game PlayerChoosingAbility),
                 [CombatEvent])
simulateMove game ability target =
    let targeting = chooseAbility game ability
        (SingleTargetedEffect firstTEffect) = headEx (ability^.abilityEffects)
        vetting = chooseTargets targeting [SelectedSingleTargetedEffect target firstTEffect]
        (Just (accepted, log)) = runWriterT $ acceptAction_ vetting
    in (targeting, vetting, accepted, log)

chris = Player "Chris"
jah = Player "Jah"
beth = Player "Beth"

radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab, punch, kill, bonk]
aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab, punch, kill, bonk]
ulsoga = makeCreature "Ulsoga" (Energy 100) (Stamina High) [stab, punch, kill, bonk]

myGame :: Game PlayerChoosingAbility
myGame = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr"), (beth, "Ulsoga")]
    , _currentCreatureName="Radorg"
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr), ("Ulsoga", ulsoga)]
    , _initiative=["Radorg", "Aspyr", "Ulsoga"]
    }

(punchTargeting, punchVetting, (Right punchAccepted), punchLog) = simulateMove myGame punch "Aspyr"
(stabTargeting, stabVetting, (Right stabAccepted), stabLog) = simulateMove myGame stab "Aspyr"
(Just (Right afterBleedTick)) = nextTurn =<< (nextTurn stabAccepted)^?_Just._Right
(killTargeting, killVetting, (Left killAccepted), killLog) = simulateMove myGame kill "Aspyr"
(Just (Right afterBleedEnd)) = nextTurn =<< (nextTurn afterBleedTick)^?_Just._Right
(bonkTargeting, bonkVetting, (Left bonkAccepted), bonkLog) = simulateMove myGame bonk "Aspyr"
(Just (Right afterBonk)) = nextTurn bonkAccepted
