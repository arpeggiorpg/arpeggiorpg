module PandT.Abilities where

import ClassyPrelude
import Control.Lens
import PandT.Types

punchTEffect :: TargetedEffect
punchTEffect = TargetedEffect "Stab" (TargetCreature (Range 1)) punchEffect

punchEffect :: Effect
punchEffect = Damage (DamageIntensity Medium)

punch :: Ability
punch = Ability "Punch" (Energy 10) [punchTEffect] (CastTime 0) (Cooldown 0)


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

kill :: Ability
kill = Ability "Kill" (Energy 10) [killTargetedEffect] (CastTime 0) (Cooldown 0)
    where
        killTargetedEffect = TargetedEffect "Stab" (TargetCreature (Range 1)) killEffect
        killEffect = ApplyCondition (SomeDead (Dead "Dead" UnlimitedDuration))

mkStun :: Duration -> Effect
mkStun dur = ApplyCondition $ SomeIncapacitated $
    Incapacitated "Stunned" (TimedCondition dur)

bonk :: Ability
bonk = Ability "Bonk" (Energy 10) [bonkTEffect] (CastTime 0) (Cooldown 0)
    where
        bonkTEffect = TargetedEffect "Bonk" (TargetCreature (Range 1)) bonkEffect
        bonkEffect = ApplyCondition (SomeIncapacitated (Incapacitated "Bonked" (TimedCondition (Duration 1))))
