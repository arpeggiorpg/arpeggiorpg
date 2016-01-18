module PandT.Abilities where

import ClassyPrelude
import Control.Lens
import PandT.Types

punchTEffect :: TargetedEffect
punchTEffect = SingleTargetedEffect $ TargetedEffectP "Stab" (TargetCreature (Range 1)) punchEffect

punchEffect :: Effect
punchEffect = Damage (DamageIntensity Medium)

punch :: Ability
punch = Ability "Punch" (Energy 10) [punchTEffect] (CastTime 0) (Cooldown 0)

bleed :: Effect
bleed = makeTimedEOT "Bleeding" 2 (Damage (DamageIntensity Low))

bleedCondition :: ConditionC
(Just bleedCondition) = bleed^?_ApplyCondition

appliedBleed :: AppliedCondition
appliedBleed = applyCondition bleedCondition

stab :: Ability
stab = Ability
    { _abilityName="Stab"
    , _cost=Energy 10
    , _abilityEffects=[stabTargetedEffect]
    , _castTime = CastTime 0
    , _cooldown = Cooldown 0
    }
    where
        stabTargetedEffect =
            SingleTargetedEffect $ TargetedEffectP
                { _targetedEffectName = "Stab"
                , _targetedEffectSystem = TargetCreature (Range 1)
                , _targetedEffectEffect = stabEffect
                }
        stabEffect = MultiEffect stabDirectDamage bleed
        stabDirectDamage = Damage (DamageIntensity Medium)

wrath :: Ability
wrath = Ability "Wrath" (Mana 10) [wrathTEffect] (CastTime 1) (Cooldown 0)
    where
        wrathTEffect = SingleTargetedEffect $ TargetedEffectP "Wrath" (TargetCreature (Range 1)) wrathEffect
        wrathEffect = Damage (DamageIntensity High)

kill :: Ability
kill = Ability "Kill" (Energy 10) [killTargetedEffect] (CastTime 0) (Cooldown 0)
    where
        killTargetedEffect = SingleTargetedEffect $ TargetedEffectP "Stab" (TargetCreature (Range 1)) killEffect
        killEffect = ApplyCondition (Dead "Dead" UnlimitedDuration)

mkStun :: Duration -> Effect
mkStun dur = ApplyCondition (Incapacitated "Stunned" (TimedCondition dur))

bonk :: Ability
bonk = Ability "Bonk" (Energy 10) [bonkTEffect] (CastTime 0) (Cooldown 0)
    where
        bonkTEffect = SingleTargetedEffect $ TargetedEffectP "Bonk" (TargetCreature (Range 1)) bonkEffect
        bonkEffect = ApplyCondition (Incapacitated "Bonked" (TimedCondition (Duration 1)))
