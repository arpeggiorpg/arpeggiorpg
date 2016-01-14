{-# LANGUAGE OverloadedStrings #-}

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
        stabAccepted^.currentCreature @?= "Aspyr"
    , testCase "After Aspyr acts, Ulsoga goes" $
        (nextTurn stabAccepted)^?_Just._Right.currentCreature @?= Just "Ulsoga"
    , testCase "After Ulsoga acts, Radorg goes" $
        (nextTurn =<< (nextTurn stabAccepted)^?_Just._Right)^?_Just._Right.currentCreature @?= Just "Radorg"
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
        killAccepted^.currentCreature @?= "Aspyr"
    , testCase "Incapacitated creature gets a turn" $
        bonkAccepted^.currentCreature @?= "Aspyr"
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
        vetting = chooseTargets targeting [[target]]
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
    , _currentCreature="Radorg"
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


-- following test data still unused

-- Tera-style multi-healing
healTwoTargets :: [TargetedEffect]
healTwoTargets = take 2 . repeat $ TargetedEffect
        { _targetName="Heal"
        , _targetSystem=TargetCreature (Range 5)
        , _targetedEffect=Heal (DamageIntensity Medium)
        }

-- basic damage+dot attack
immolate :: TargetedEffect
immolate = TargetedEffect
    { _targetName="Immolate"
    , _targetSystem=TargetCreature (Range 5)
    , _targetedEffect=MultiEffect directDamage dot
    } where
        directDamage = Damage (DamageIntensity Medium)
        dot = makeTimedEOT "Immolation" 3 dotTick
        dotTick = Damage (DamageIntensity Low)


mistPunch :: [TargetedEffect]
mistPunch =
    [ TargetedEffect
        { _targetName="MistPunch Damage"
        , _targetSystem=TargetCreature (Range 1)
        , _targetedEffect=Damage (DamageIntensity Low)
        }
    , TargetedEffect
        { _targetName="MistPunch Heal"
        , _targetSystem=TargetCreature (Range 4)
        , _targetedEffect=Heal (DamageIntensity Low)
        }
    ]

-- ok this won't work, because the secondary effect will apply to the primary target.
-- fistsOfFury :: [TargetedEffect]
-- fistsOfFury =
--     [ TargetedEffect { _targetSystem=TargetCreature (Range 1), _targetedEffect=stunAndMediumDamage}
--     , TargetedEffect { _targetSystem=TargetCone (Range 1), _targetedEffect=stunAndLowDamage}
--     ]

-- but, I guess, on the other hand, we can just deal two amounts of low damage to the main target...
fistsOfFury :: [TargetedEffect]
fistsOfFury =
    [ TargetedEffect
        { _targetName="Fists of Fury Primary"
        , _targetSystem=TargetCreature (Range 1)
        , _targetedEffect=lowDamage
        }
    , TargetedEffect
        { _targetName="Fists of Fury Area"
        , _targetSystem=TargetCone (Range 1)
        , _targetedEffect=stunAndLowDamage
        }
    ]
    where
        lowDamage = Damage (DamageIntensity Medium)
        stunAndLowDamage = MultiEffect stunEff lowDamage
        stunEff = mkStun (Duration 1)

{-
durations! ticks!

fists of fury: incapacitate lasts until the beginning of the caster's next turn.

    ignoring the area-effect of this: this can be implemented as, of course, the
    Incapacitated condition on the target, along with a condition or native
    concept of "Channeling" on the caster. When ticking over to the caster, we
    can see that the channel is ending and remove the condition from the target.
    This, of course, requires some way to link the particular spell being cast
    by the caster to the particular Incapacitated condition on the target.

rain of fire: lasts for 3 rounds, burns any creature entering or starting their turn in the area effected

    Argh AE.

dancing sword: summons a sword that attacks an enemy target on the casting player's turn

    This should probably just be implemented as an ability that summons a creature.

illness: lasts five rounds, deals 1 damage per turn at the end of the target's turn

    This can be a basic-ish condition.

Burning arrow: strikes the target for damage, and then burns the target at the end of each of his turn

FoF:...

Creature.channeling :: Bool ???
Condition.channelingCreature :: CreatureName ??

or Creature.

RecurringEffect = RecurringEffect {
    effect :: Effect,
    when :: WhenEffectHappens
}

data WhenEffectHappens
    = EndOfTargetTurn
    | BeginningOfTargetTurn


-}
