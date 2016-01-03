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
        [dead] @=? (applyEffect deadCreature (Damage (DamageIntensity Low)))^.conditions
    ]

dotted :: Creature
dotted = applyEffect creat bleed

damaged :: Creature
damaged = applyEffect creat (Damage (DamageIntensity Medium))

healed :: Creature
healed = applyEffect damaged (Heal (DamageIntensity Low))

deadCreature = foldl' applyEffect creat (take 3 $ repeat (Damage (DamageIntensity High)))

chris :: Player
chris = Player "Chris"

jah :: Player
jah = Player "Jah"

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
