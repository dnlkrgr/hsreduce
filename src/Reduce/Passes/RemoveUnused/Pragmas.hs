module Reduce.Passes.RemoveUnused.Pragmas (reduce) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Text as T

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    isTestStillFresh "Pragmas"
    liftIO $ putStrLn "\n***Performing RemovePragmas***"
    oldState <- get
    liftIO . putStrLn $ "Size of old state: " ++ (show . T.length . showState $ oldState)
    traverse_ tryToRemovePragma $ _pragmas oldState

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = do
    liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
    conf     <- ask
    oldState <- get

    let 
        newState = oldState { _pragmas = filter (/= pragmaToTry) (_pragmas oldState)}

    liftIO (testAndUpdateStateFlex conf False True newState) >>= \case
        False -> put oldState { _statistics = updateStatistics False "pragmas" oldState 0 }
        True  -> 
            put oldState { 
                _statistics = updateStatistics True "pragmas" oldState (T.length (showState oldState) - T.length (showState newState)) 
                }
        
