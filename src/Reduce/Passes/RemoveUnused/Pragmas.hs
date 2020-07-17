module Reduce.Passes.RemoveUnused.Pragmas (reduce) where

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Text as T

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    liftIO $ putStrLn "\n***Performing RemovePragmas***"
    isTestStillFresh "Pragmas"
    oldState <- get
    liftIO . putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    traverse_ tryToRemovePragma $ _pragmas oldState

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = do
    liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
    conf     <- ask
    oldState <- get

    let 
        newState = oldState & pragmas %~ filter (/= pragmaToTry) 

    liftIO (testAndUpdateStateFlex conf False True newState) >>= \case
        False -> updateStatistics "pragmas" False 0
        True  -> updateStatistics "pragmas" True (T.length (showState oldState) - T.length (showState newState)) 
        
