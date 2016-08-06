{-# LANGUAGE QuasiQuotes #-}

-- | Text renderers for stuff that needs rendered.

-- TODO: we should be rendering these objects as HTML, not text. The objects will be serialized over
--       an HTTP connection as JSON and GHCJS on the client side will take them to HTML.

module PandT.Render where
import           PandT.Prelude
import           PandT.Types


renderConditionDef :: ConditionDef -> Text
renderConditionDef (ConditionDef meta condc) =
    let cName = meta^.conditionName
        cType = renderConditionCaseName condc
        dur = renderConditionDuration (meta^.conditionDuration)
    in [ui|#{cName} (#{cType}, #{dur})|]

renderConditionDuration :: ConditionDuration -> Text
renderConditionDuration UnlimitedDuration = "unlimited"
renderConditionDuration (TimedCondition (Duration n)) = [ui|#{n} rounds|]

renderConditionCaseName :: ConditionC -> Text
renderConditionCaseName = n
    where
        n (RecurringEffectC _) = "Recurring Effect"
        n (IncomingDamageReductionC _) = "Incoming Damage Reduction"
        n (DamageIncreaseC _) = "Damage Increase"
        n (DamageDecreaseC _) = "Damage Decrease"
        n (IncapacitatedC _)  = "Incapacitation"
        n (DeadC _) = "Death"
        n (ActivatedAbilityC _) = "Activated Ability"

renderEffectOccurrence :: EffectOccurrence -> Text
renderEffectOccurrence = unlines . map go
    where
        go (Interrupt, cname) = [ui|- interrupted #{cname}|]
        go (Heal int, cname) = [ui|- healed #{cname} for #{int}|]
        go (Damage int, cname) = [ui|- damaged #{cname} for #{int}|]
        go (GenerateEnergy nrg, cname) = [ui|- generated #{nrg^.unEnergy} energy for #{cname}|]
        go (Resurrect, cname) = [ui|- resurrected #{cname}|]
        go (MultiEffect eff1 eff2, cname) = renderEffectOccurrence [(eff1, cname), (eff2, cname)]
        go (ApplyCondition cdef, cname) = [ui|- applied condition #{renderConditionDef cdef} to #{cname}|]

renderCombatEvent :: CombatEvent -> Text
renderCombatEvent = \case
    AbilityUsed abil source occurrences -> [ui|
        ### #{source} used #{abil^.abilityName} ###
        #{renderEffectOccurrence occurrences}|]
    CreatureTurnStarted name -> [ui|#{name}'s turn started|]
    AbilityStartCast cname ab -> [ui|#{cname} started casting #{ab^.abilityName}.|]
    RecurringEffectOccurred source target occurrences -> [ui|
        #{source}'s recurring effect ticked on #{target}, causing:
        #{renderEffectOccurrence occurrences}.|]
    SkippedIncapacitatedCreatureTurn creatName -> [ui|#{creatName} is incapacitated.|]
    SkippedTurn creatName -> [ui|#{creatName} skipped their turn.|]
    CanceledCast creatName ability -> [ui|#{creatName} stopped casting #{ability^.abilityName}|]

class RenderState a where
    renderState :: a -> Text

instance RenderState PlayerChoosingAbility where
    renderState PlayerChoosingAbility = "Choosing ability"
instance RenderState PlayerIncapacitated where
    renderState PlayerIncapacitated = "Incapacitated"
instance RenderState PlayerChoosingTargets where
    renderState (PlayerChoosingTargets ability) = [ui|Choosing targets for #{ability^.abilityName}|]
instance RenderState PlayerCasting where
    renderState PlayerCasting = "Casting"
instance RenderState PlayerFinishingCast where
    renderState PlayerFinishingCast = "Finishing cast"

renderCreatureStatus :: Creature -> Text
renderCreatureStatus creature =
    line
    where
        hp = creature^.health.unHealth
        conds = intercalate "; " (map renderAppliedCondition (creature^.conditions))
        castSumm :: Text
        castSumm = case creature^.casting of
            Nothing -> ""
            Just (ability, Duration duration, selections) ->
                [ui|(casting #{ability^.abilityName} at #{selections} for #{tshow duration} more rounds)|]
        line = [ui|#{creature^.creatureName} (#{hp} HP, #{creature^.creatureEnergy.unEnergy} NRG) #{castSumm} #{conds}|]

renderAppliedCondition :: AppliedCondition -> Text
renderAppliedCondition (AppliedCondition originCreatureName duration meta _) =
    [ui|#{originCreatureName}'s #{meta^.conditionName} (#{renderConditionDuration duration})|]

renderInitiative :: Game a -> Text
renderInitiative game
    = let
        currentName = game^.currentCreatureName
        creature name = view (creaturesInPlay . at name) game
        pfx name = if name == currentName then "*" else " "
        statusLine name = unwords.toList $ renderCreatureStatus <$> creature name
        rend name = pfx name ++ statusLine name
    in
        unlines $ map rend (_initiative game)

render :: RenderState a => Game a -> Text
render game@Game {..} = unlines
    [ [ui|# #{_currentCreatureName}'s turn|]
    , renderState _state
    , renderInitiative game
    ]
