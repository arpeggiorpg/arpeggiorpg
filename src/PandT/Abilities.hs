-- | A bunch of abilities. This module should probably go in a GameDesign sub-module or something.

module PandT.Abilities where

import PandT.Prelude
import PandT.Types
import PandT.Sim

condDur :: Int -> ConditionDuration
condDur dur = TimedCondition (Duration dur)

punchTEffect :: TargetedEffect
punchTEffect = SingleTargetedEffect $ TargetedEffectP "Punch" (TargetCreature (Range 1)) (Damage 3)

punchEffect :: Effect
punchEffect = Damage 3

punch :: Ability
punch = makeAbility "Punch" (Energy 1) [punchTEffect] (CastTime 0) (Cooldown 0)

sootheTEffect :: TargetedEffect
sootheTEffect = SingleTargetedEffect $ TargetedEffectP "Soothe" (TargetCreature (Range 5)) (Heal 1)

soothe :: Ability
soothe = makeAbility "Soothe" (Energy 1) [sootheTEffect] (CastTime 0) (Cooldown 0)

bloodlustCondition :: ConditionDef
bloodlustCondition = MkConditionDef "Bloodlust" (condDur 3) (MkDamageIncreaseC 1)

bloodlustTEffect :: TargetedEffect
bloodlustTEffect = SingleTargetedEffect $ TargetedEffectP "Bloodlust" (TargetCreature (Range 5)) (ApplyCondition bloodlustCondition)

bloodlust :: Ability
bloodlust = makeAbility "Bloodlust" (Energy 1) [bloodlustTEffect] (CastTime 0) (Cooldown 0)


weakenCondition :: ConditionDef
weakenCondition = MkConditionDef "Weakened" (condDur 3) (MkDamageDecreaseC 1)

weakenTEffect :: TargetedEffect
weakenTEffect = SingleTargetedEffect $ TargetedEffectP "Weaken" (TargetCreature (Range 5)) (ApplyCondition weakenCondition)

weaken :: Ability
weaken = makeAbility "Weaken" (Energy 1) [weakenTEffect] (CastTime 0) (Cooldown 0)

block :: Ability
block = makeAbility "Block" (Energy 0) [blockTEffect] (CastTime 0) (Cooldown 4)
    -- this is hella OP
    where
        blockTEffect = SelfTargetedEffect (TargetedEffectP "(always self)" TargetSelf blockEffect)
        blockEffect = ApplyCondition (MkConditionDef "Blocking" (condDur 2) (MkIncomingDamageReductionC 3))

sacrificialStrike :: Ability
sacrificialStrike = makeAbility "Sacrificial Strike" (Energy 0) [selfTEff, targetTEff] (CastTime 0) (Cooldown 1)
    where
        selfTEff = SelfTargetedEffect (TargetedEffectP "(always self)" TargetSelf selfDamageEff)
        selfDamageEff = Damage 3
        targetTEff = SingleTargetedEffect (TargetedEffectP "Strike" (TargetCreature (Range 5)) targetDamageEff)
        targetDamageEff = Damage 5

makeTimedEOT :: Text -> Int -> Effect -> Effect
makeTimedEOT cname cdur ceff
    = ApplyCondition (MkConditionDef cname (condDur cdur) (MkRecurringEffectC ceff))

bleed :: Effect
bleed = makeTimedEOT "Bleeding" 2 (Damage 2)

stab :: Ability
stab = makeAbility "Stab" (Energy 1) [stabTargetedEffect] (CastTime 0) (Cooldown 1)
    where
        stabTargetedEffect =
            SingleTargetedEffect $ TargetedEffectP
                { _targetedEffectName = "Stab"
                , _targetedEffectSystem = TargetCreature (Range 1)
                , _targetedEffectEffect = stabEffect
                }
        stabEffect = MultiEffect stabDirectDamage bleed
        stabDirectDamage = Damage 3

wrath :: Ability
wrath = makeAbility "Wrath" (Energy 1) [wrathTEffect] (CastTime 2) (Cooldown 0)
    where
        wrathTEffect = SingleTargetedEffect $ TargetedEffectP "Wrath" (TargetCreature (Range 1)) wrathEffect
        wrathEffect = Damage 5

kill :: Ability
kill = makeAbility "Kill" (Energy 1) [killTargetedEffect] (CastTime 0) (Cooldown 100)
    where
        killTargetedEffect = SingleTargetedEffect $ TargetedEffectP "Stab" (TargetCreature (Range 1)) killEffect
        killEffect = ApplyCondition deadDef

mkStun :: Text -> ConditionDuration -> Effect
mkStun name dur = ApplyCondition (MkConditionDef name dur MkIncapacitatedC)

bonk :: Ability
bonk = makeAbility "Bonk" (Energy 1) [bonkTEffect] (CastTime 0) (Cooldown 0)
    where
        bonkTEffect = SingleTargetedEffect $ TargetedEffectP "Bonk" (TargetCreature (Range 1)) bonkEffect
        bonkEffect = mkStun "Bonked" (condDur 1)

meditate :: Ability
meditate = makeAbility "Meditate" (Energy 0) [medTEffect] (CastTime 0) (Cooldown 0)
    where
        medTEffect = SelfTargetedEffect (TargetedEffectP "(always self)" TargetSelf (GenerateEnergy 3))

rebirth :: Ability
rebirth = makeAbility "Rebirth" (Energy 5) [rebirthTEffect] (CastTime 0) (Cooldown 0)
    where
        rebirthTEffect = SingleTargetedEffect $ TargetedEffectP "Resurrect" (TargetCreature (Range 10)) Resurrect

pummel :: Ability
pummel = makeAbility "Pummel" (Energy 1) [pummelTEffect] (CastTime 0) (Cooldown 0)
    where
        pummelTEffect = SingleTargetedEffect $ TargetedEffectP "Pummel" (TargetCreature (Range 1)) Interrupt


-- A "one-two punch" set of combo abilities. Casting one enables the casting of two for a single round.
one :: Ability
one = makeAbility "One" (Energy 1) [oneTEffect, oneActivateTwo] (CastTime 0) (Cooldown 1)
    where
        oneTEffect = SingleTargetedEffect $ TargetedEffectP "Strike" (TargetCreature (Range 1)) (Damage 1)
        oneActivateTwo = SelfTargetedEffect $ TargetedEffectP "(always self)" TargetSelf (ApplyCondition canTwo)
        canTwo = MkConditionDef "Two Activated" (TimedCondition (Duration 2)) (MkActivatedAbilityC "Two")

two :: Ability
two = (makeAbility "Two" (Energy 2) [twoTEffect] (CastTime 0) (Cooldown 0)) {_abilityRequiresActivation = True}
    where
        twoTEffect = SingleTargetedEffect $ TargetedEffectP "Strike" (TargetCreature (Range 1)) (Damage 4)
