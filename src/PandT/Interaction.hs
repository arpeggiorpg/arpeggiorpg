module PandT.Interaction where

import ClassyPrelude

import Control.Monad.Trans.Maybe
import System.IO (hSetBuffering, BufferMode(NoBuffering))

import PandT.Types
import PandT.Abilities

chris = Player "Chris"
jah = Player "Jah"
beth = Player "Beth"

radorg = makeCreature "Radorg" (Energy 100) (Stamina High) [stab, punch, kill, bonk]
aspyr = makeCreature "Aspyr" (Mana 100) (Stamina High) [stab, punch, kill, bonk]
ulsoga = makeCreature "Ulsoga" (Energy 100) (Stamina High) [stab, punch, kill, bonk]

myGame :: Game PlayerChoosingAbility
myGame = Game
    { _state=PlayerChoosingAbility
    , _playerCharacters=mapFromList [(chris, "Radorg"), (jah, "Aspyr"), (beth, "Ulsoga")]
    , _currentCreatureName="Radorg"
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr), ("Ulsoga", ulsoga)]
    , _initiative=["Radorg", "Aspyr", "Ulsoga"]
    }

-- This must exist somewhere
runForever :: Monad m => (a -> m a) -> a -> m ()
runForever go start = go start >>= runForever go

lookupAbility :: Text -> Maybe Ability
lookupAbility abilityName = Nothing

promptForAbility :: Game PlayerChoosingAbility -> MaybeT IO (Game PlayerChoosingTargets)
promptForAbility game = do
    putStr "Enter ability name> "
    abilityName <- hGetLine stdin
    case lookupAbility abilityName of
        Nothing -> do
            liftIO $ putStrLn "Not found"
            promptForAbility game
        Just ability -> do
            return $ chooseAbility game ability

promptForTargets :: Game PlayerChoosingTargets -> MaybeT IO (Game GMVettingAction)
promptForTargets = error "prompt for targets"

promptForVet :: Game GMVettingAction -> MaybeT IO ModifiedGame
promptForVet = error "Prompt for vet"

-- why do I have to define this
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

runIterationT :: Maybe ModifiedGame -> MaybeT IO ModifiedGame
runIterationT mGame = do
    game <- liftMaybe mGame
    case game of
        Left incap -> do
            liftIO . putStrLn $ render incap
            nextTurn <- liftMaybe $ skipIncapacitatedPlayer incap
            runIterationT (Just nextTurn)
        Right choosingAbility -> do
            liftIO . putStrLn $ render choosingAbility
            choosingTargets <- promptForAbility choosingAbility
            vetting <- promptForTargets choosingTargets
            vetted <- promptForVet vetting
            runIterationT (Just vetted)


runIteration :: Maybe ModifiedGame -> IO (Maybe ModifiedGame)
runIteration mGame = runMaybeT . runIterationT $ mGame

runConsoleGame :: IO ()
runConsoleGame = do
    hSetBuffering stdout NoBuffering
    runForever runIteration (Just . Right $ myGame)
