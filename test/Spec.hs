{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import PandT.Types

import ClassyPrelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "unitTests"
    [ testCase "Dead creature is dead" $
        [dead] @=? deadCreature^.conditions
    , testCase "Damage to dead creature does not ause additional Dead condition" $
        [dead] @=? deadTwice^.conditions
    ]

creat = makeCreature "Creat the Geat" (Energy 100) (Stamina High) [stab]
dotted = applyEffect creat bleed
damaged = applyEffect creat (Damage (DamageIntensity Medium))
healed = applyEffect damaged (Heal (DamageIntensity Low))
deadCreature = foldl' applyEffect creat (take 3 $ repeat (Damage (DamageIntensity High)))
deadTwice = (applyEffect deadCreature (Damage (DamageIntensity Low)))

chris = Player "Chris"
jah = Player "Jah"

bleed :: Effect
-- bleed = makeDotEffect "Bleeding" Medium (TimedCondition (Duration 2)) (Period 1)
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Low))

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
myGame5 = denyAction myGame3
