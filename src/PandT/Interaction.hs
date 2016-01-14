module PandT.Interaction where

import ClassyPrelude

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
    , _currentCreature="Radorg"
    , _creaturesInPlay=mapFromList [("Radorg", radorg), ("Aspyr", aspyr), ("Ulsoga", ulsoga)]
    , _initiative=["Radorg", "Aspyr", "Ulsoga"]
    }

-- This must exist somewhere
runForever :: Monad m => (a -> m a) -> a -> m ()
runForever go start = go start >>= runForever go

lookupAbility :: Text -> Maybe Ability
lookupAbility = error "Lookup Ability"

promptForAbility :: Game PlayerChoosingAbility -> IO (Maybe (Game PlayerChoosingTargets))
promptForAbility game = do
    putStr "Enter ability name> "
    abilityName <- hGetLine stdin
    let mAbility = lookupAbility abilityName
    maybe (promptForAbility game) (return . Just .  (chooseAbility game)) mAbility

promptForTargets :: Game PlayerChoosingTargets -> IO (Maybe (Game GMVettingAction))
promptForTargets = error "prompt for targets"

promptForVet :: Game GMVettingAction -> IO (Maybe ModifiedGame)
promptForVet = error "Prompt for vet"

runIteration :: Maybe ModifiedGame -> IO (Maybe ModifiedGame)
runIteration mGame = do
    case mGame of
        Nothing -> error "Internal error"
        Just game ->
            case game of
                Left incap -> do
                    putStrLn $ render incap
                    maybe (error "Internal error") (runIteration . Just) (skipIncapacitatedPlayer incap)
                Right choosingAbility -> do
                    putStrLn $ render choosingAbility
                    maybeChoosingTargets <- promptForAbility choosingAbility
                    case maybeChoosingTargets of
                        Nothing -> error "Internal error"
                        Just choosingTargets -> do
                            maybeVetting <- promptForTargets choosingTargets
                            case maybeVetting of
                                Nothing -> error "Internal error"
                                Just vetting -> do
                                    maybeVetting <- promptForVet vetting
                                    runIteration maybeVetting

runConsoleGame :: IO ()
runConsoleGame = do
    hSetBuffering stdout NoBuffering
    runForever runIteration (Just . Right $ myGame)
