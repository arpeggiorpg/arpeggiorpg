{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import PandT.Types

import ClassyPrelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [effectTests, conditionTests, abilityTests]

effectTests :: TestTree
effectTests = testGroup "Effect Tests"
    [ testCase "Dead creature is dead" $
        deadCreature^.conditions @?= [dead]
    , testCase "Damage to dead creature does not cause additional Dead condition" $
        deadTwice^.conditions @?= [dead]
    ]

conditionTests :: TestTree
conditionTests = testGroup "Condition Tests"
    [ testCase "RecurringEffect recurs on start of character's turn" $
        stabAccepted^.creaturesInPlay.at "Aspyr"^?_Just.health @?= Just (Health 50)
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

chris = Player "Chris"
jah = Player "Jah"


punch :: Ability
punch = Ability "Punch" (Energy 10) [punchTEffect] (CastTime 0) (Cooldown 0)
    where
        punchTEffect = TargetedEffect "Stab" (TargetCreature (Range 1)) punchEffect
        punchEffect = Damage (DamageIntensity Medium)


bleed :: Effect
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Low))

bleedCondition :: ConditionCase
(Just bleedCondition) = bleed^?_ApplyCondition

appliedBleed :: AppliedCondition
appliedBleed = applyCondition bleedCondition

stab :: Ability
stab = Ability
    { _abilityName="Stab"
    , _cost=Energy 10
    , _effects=[stabTargetedEffect]
    , _castTime = CastTime 0
    , _cooldown = Cooldown 0
    }
    where
        stabTargetedEffect =
            TargetedEffect
                { _targetName = "Stab"
                , _targetSystem = TargetCreature (Range 1)
                , _targetedEffect = stabEffect
                }
        stabEffect = MultiEffect stabDirectDamage bleed
        stabDirectDamage = Damage (DamageIntensity Medium)

radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab, punch]
aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab, punch]

simulateMove :: Game PlayerChoosingAbility -> Ability -> CreatureName -> (Game PlayerChoosingTargets, Game GMVettingAction, Game PlayerChoosingAbility)
simulateMove game ability target =
    let targeting = chooseAbility game ability
        vetting = chooseTargets targeting [[target]]
        (Just accepted) = acceptAction vetting
    in (targeting, vetting, accepted)

myGame :: Game PlayerChoosingAbility
myGame = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr")]
    , _currentCreature="Radorg"
    , _currentPlayer=chris
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr)]
    , _initiative=["Radorg", "Aspyr"]
    }

(punchTargeting, punchVetting, punchAccepted) = simulateMove myGame punch "Aspyr"
-- myGameDenied = denyAction punchVetting
(stabTargeting, stabVetting, stabAccepted) = simulateMove myGame stab "Aspyr"


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

stun :: Duration -> Effect
stun dur = ApplyCondition $ SomeIncapacitated $
    Incapacitated "Stunned" (TimedCondition dur)

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
        stunEff = stun (Duration 1)

{-
durations! ticks!

fists of fury: incapacitate lasts until the beginning of the caster's next turn.
rain of fire: lasts for 3 rounds, burns any creature entering or starting their turn in the area effected

-}
