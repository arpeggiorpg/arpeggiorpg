{-# LANGUAGE ScopedTypeVariables #-}

-- | A console-based UI for P&T. Pretty much just for testing, though it may be useful as a very
-- crude, GM-only UI at some point.

module PandT.Interaction where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Strict (runWriterT)
import System.IO (hSetBuffering, BufferMode(NoBuffering))

import PandT.Prelude
-- imports from PandT.Types and Sim are an indication of either what needs to be exposed through the
-- API, or what needs to be factored into the API code and have something simpler exposed.
import PandT.Types ( Ability(..)
                   , CombatEvent(..)
                   , Creature(..)
                   , CreatureName
                   , Energy(..)
                   , GMVettingAction(..)
                   , Game(..)
                   , GameStartTurn(..)
                   , Intensity(..)
                   , Player(..)
                   , PlayerCasting(..)
                   , PlayerChoosingAbility(..)
                   , PlayerChoosingTargets(..)
                   , PlayerFinishingCast(..)
                   , SelectedTargetedEffect(..)
                   , Stamina(..)
                   , TargetSystem(..)
                   , TargetedEffect(..)
                   , TargetedEffectP(..)
                   , abilities
                   , abilityEffects
                   , abilityName
                   , casting
                   , currentCreature
                   , makeCreature
                   )
import PandT.Abilities
import PandT.Render
import PandT.Sim ( usableAbilities
                 , chooseAbility
                 , chooseTargets
                 , acceptAction_
                 , denyAction
                 , cancelCast
                 , skipIncapacitatedPlayer
                 )
import Math.Geometry.Grid.Octagonal (UnboundedOctGrid(..))
import Math.Geometry.GridMap.Lazy (lazyGridMapIndexed)


chris, jah, beth :: Player
chris = Player "Chris"
jah = Player "Jah"
beth = Player "Beth"

allAbs :: [Ability]
allAbs = [stab, punch, kill, bonk, wrath, soothe, block, meditate, sacrificialStrike, rebirth, pummel, one, two]

radorg, aspyr, ulsoga :: Creature
radorg = makeCreature "Radorg" (Energy 1) (Stamina High) allAbs
aspyr = makeCreature "Aspyr" (Energy 10) (Stamina High) allAbs
ulsoga = makeCreature "Ulsoga" (Energy 10) (Stamina High) allAbs

myGame :: Game PlayerChoosingAbility
myGame = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr"), (beth, "Ulsoga")]
    , _currentCreatureName="Radorg"
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr), ("Ulsoga", ulsoga)]
    , _initiative=["Radorg", "Aspyr", "Ulsoga"]
    , _gameGeo=lazyGridMapIndexed UnboundedOctGrid [((0,0), "Radorg"), ((0,1), "Aspyr"), ((1,0), "Ulsoga")]
    }

lookupAbility :: Creature -> Text -> Maybe Ability
lookupAbility creature abName = find matchName (creature^.abilities)
    where matchName ab = toCaseFold (ab^.abilityName) == toCaseFold abName

promptForAbility :: Game PlayerChoosingAbility -> MaybeT IO (Game PlayerChoosingTargets)
promptForAbility game = do
    creature <- liftMaybe (game^.currentCreature)
    putStr "Abilities: "
    forM_ (usableAbilities creature) (\ab -> putStr $ (ab^.abilityName) ++ " ")
    putStr "\nEnter ability name> "
    abName <- hGetLine stdin
    case lookupAbility creature abName of
        Nothing -> do
            liftIO $ putStrLn "Not found"
            promptForAbility game
        Just ability ->
            case chooseAbility game ability of
                Left e -> do
                    liftIO (putStrLn ("Couldn't use that ability! " ++ tshow e))
                    promptForAbility game
                Right game' -> return game'

promptForTargets :: Game PlayerChoosingTargets -> MaybeT IO (Game GMVettingAction)
promptForTargets game@(Game {_state=PlayerChoosingTargets ability}) = do
    creatureNameses <- mapM promptTEffect (ability^.abilityEffects)
    return (chooseTargets game creatureNameses)

promptTEffect :: TargetedEffect -> MaybeT IO SelectedTargetedEffect
promptTEffect (SingleTargetedEffect teffect@(TargetedEffectP targetName (TargetCreature range) _)) = do
    lift $ putStr ("Single target for " ++ targetName ++ " range: " ++ (tshow range) ++ "> ")
    target <- lift $ hGetLine stdin
    -- XXX TODO: check if name is in game^.creaturesInPlay
    return (SelectedSingleTargetedEffect target teffect)
promptTEffect (MultiTargetedEffect teffect@(TargetedEffectP targetName system _)) = do
    creatureNames <- case system of
        (TargetCircle range radius) ->
            promptMultiTarget [] targetName (" Circle range: " ++ (tshow range) ++ " radius: " ++ (tshow radius))
        (TargetLineFromSource range) ->
            promptMultiTarget [] targetName (" Line range: " ++ (tshow range))
        (TargetCone range) ->
            promptMultiTarget [] targetName (" Cone range: " ++ (tshow range))
    return (SelectedMultiTargetedEffect creatureNames teffect)
promptTEffect (SelfTargetedEffect teffectp) = return (SelectedSelfTargetedEffect teffectp)

promptMultiTarget :: [CreatureName] -> Text -> Text -> MaybeT IO [CreatureName]
promptMultiTarget sofar targetName prompt = do
    lift $ putStr ("Target for " ++ targetName ++ prompt ++ " (enter DONE when done)> ")
    target <- lift $ hGetLine stdin
    if target == "DONE" then
        return sofar
    else
        promptMultiTarget (target:sofar) targetName prompt

promptForVet :: Game GMVettingAction -> MaybeT IO GameStartTurn
promptForVet game = do
    yes <- promptYesNo "GM! Is this okay?"
    if yes then do
        (nextGame, combatLog) <- liftMaybe (runWriterT (acceptAction_ game))
        displayCombatLog combatLog
        return nextGame
    else
        return (GSTPlayerChoosingAbility (denyAction game))

promptForCasting :: Game PlayerCasting -> MaybeT IO GameStartTurn
promptForCasting game = do
    castingAbName <- liftMaybe (game^?currentCreature._Just.casting._Just._1.abilityName)
    yes <- promptYesNo ("You are casting " ++ castingAbName ++ ". Would you like to continue casting?")
    if yes then do
        -- NO!!!!!
        -- XXX TODO FIXME
        error "CRAP! No! Must GM-Vet this!"
    else do
        return (GSTPlayerChoosingAbility (cancelCast game))

promptForFinishingCast :: Game PlayerFinishingCast -> MaybeT IO GameStartTurn
promptForFinishingCast game = do
    castingAbName <- liftMaybe (game^?currentCreature._Just.casting._Just._1.abilityName)
    yes <- promptYesNo ("Would you like to let your " ++ castingAbName ++ " fly?")
    if yes then do
        -- XXX TODO FIXME
        error "CRAP! GM-Vet this!"
    else do
        return (GSTPlayerChoosingAbility (cancelCast game))

promptYesNo :: Text -> MaybeT IO Bool
promptYesNo prompt = do
    putStr (prompt ++ " [enter y/n] ")

    (input :: Text) <- lift (hGetLine stdin)
    case (toCaseFold input) of
        "y" -> return True
        "n" -> return False
        _ -> promptYesNo prompt

displayCombatLog :: [CombatEvent] -> MaybeT IO ()
displayCombatLog = mapM_ (putStrLn . renderCombatEvent)

runIterationT :: Maybe GameStartTurn -> MaybeT IO GameStartTurn
runIterationT mGame = do
    game <- liftMaybe mGame
    case game of
        GSTPlayerIncapacitated incap -> do
            liftIO (putStrLn (render incap))
            liftIO (putStrLn "Skipping turn because player is incapacitated!")
            next <- liftMaybe $ skipIncapacitatedPlayer incap
            runIterationT (Just next)
        GSTPlayerChoosingAbility choosingAbility -> do
            liftIO . putStrLn $ render choosingAbility
            choosingTargets <- promptForAbility choosingAbility
            vetting <- promptForTargets choosingTargets
            vetted <- promptForVet vetting
            runIterationT (Just vetted)
        GSTPlayerCasting gameCasting -> do
            next <- promptForCasting gameCasting
            runIterationT (Just next)
        GSTPlayerFinishingCast gameFinishingCast -> do
            next <- promptForFinishingCast gameFinishingCast
            runIterationT (Just next)


runIteration :: Maybe GameStartTurn -> IO (Maybe GameStartTurn)
runIteration mGame = runMaybeT . runIterationT $ mGame

runConsoleGame :: IO ()
runConsoleGame = do
    hSetBuffering stdout NoBuffering
    runForeverM runIteration (Just . GSTPlayerChoosingAbility $ myGame)
