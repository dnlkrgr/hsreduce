module Reduce.Passes.RemoveUnused.Pragmas (reduce) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Text as T

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    printInfo "Removing Pragmas"
    isTestStillFresh "Pragmas"

    oldState <- liftIO . atomically . readTVar =<< asks _tState
    traverse_ tryToRemovePragma $ _pragmas oldState

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = do
    liftIO $ putStrLn $ "trying pragma: " ++ show pragmaToTry
    conf     <- ask
    oldState <- liftIO . atomically . readTVar $ _tState conf

    let 
        newState = oldState & pragmas %~ filter (/= pragmaToTry) 

    liftIO (testAndUpdateStateFlex conf False True newState) >>= \case
        False -> liftIO $ updateStatistics conf "pragmas" False 0
        True  -> do
            liftIO $ updateStatistics conf "pragmas" True (T.length (showState oldState) - T.length (showState newState)) 
            liftIO . atomically $ writeTVar (_tState conf) newState
