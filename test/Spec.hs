{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import PandT.Types

import ClassyPrelude

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [effectTests, abilityTests]

effectTests :: TestTree
effectTests = testGroup "Effect Tests"
    [ testCase "Dead creature is dead" $
        deadCreature^.conditions @?= [dead]
    , testCase "Damage to dead creature does not ause additional Dead condition" $
        deadTwice^.conditions @?= [dead]
    ]

abilityTests :: TestTree
abilityTests = testGroup "Ability Tests"
    [ testCase "Ability damage takes effect" $
        myGame4^.creaturesInPlay.at "Aspyr"^?_Just.health @?= Just (Health 75)
    , testCase "Ability condition in multi-effect adds condition" $
        myGame4^.creaturesInPlay.at "Aspyr"^?_Just.conditions @?= Just [bleedCondition]
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
myGame5 = denyAction myGame3
