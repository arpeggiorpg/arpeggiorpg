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
        myGame7^.creaturesInPlay.at "Aspyr"^?_Just.health @?= Just (Health 50)
    ]

abilityTests :: TestTree
abilityTests =
    let (Just aspyr4) = myGame4^.creaturesInPlay.at "Aspyr" in
    testGroup "Ability Tests"
    [ testCase "Ability damage takes effect" $
        aspyr4^.health @?= (Health 75)
    , testCase "Ability condition in multi-effect adds condition" $
        aspyr4^.conditions @?= [bleedCondition]
    ]

creat = makeCreature "Creat the Geat" (Energy 100) (Stamina High) [stab]
dotted = applyEffect creat bleed
damaged = applyEffect creat (Damage (DamageIntensity Medium))
healed = applyEffect damaged (Heal (DamageIntensity Low))
deadCreature = foldl' applyEffect creat (take 2 $ repeat (Damage (DamageIntensity High)))
deadTwice = (applyEffect deadCreature (Damage (DamageIntensity Low)))

chris = Player "Chris"
jah = Player "Jah"

bleed :: Effect
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Low))

bleedCondition :: Condition
(Just bleedCondition) = bleed^?_ApplyCondition

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

radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab]
aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab]

myGame1 :: Game PlayerChoosingAbility
myGame1 = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr")]
    , _currentCreature="Radorg"
    , _currentPlayer=chris
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr)]
    , _initiative=["Radorg", "Aspyr"]
    }

myGame2 = chooseAbility myGame1 stab
myGame3 = chooseTargets myGame2 [["Aspyr"]]
(Just myGame4) = acceptAction myGame3
myGameDenied = denyAction myGame3
myGame5 = chooseAbility myGame4 stab
myGame6 = chooseTargets myGame5 [["Radorg"]]
(Just myGame7) = acceptAction myGame6


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
stun dur = ApplyCondition Condition
    { _conditionName = "Stunned"
    , _conditionValue = Incapacitated
    , _conditionDuration = TimedCondition dur
    }

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
