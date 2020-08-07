module Passes.RemoveUnused.Pragmas (reduce) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Control.Monad.Reader

import Util.Types
import Util.Util

reduce :: R ()
reduce = do
    printInfo "Removing Pragmas"
    isTestStillFresh "Pragmas"

    oldState <- liftIO . atomically . readTVar =<< asks _tState
    forM_ (_pragmas oldState) tryToRemovePragma 

tryToRemovePragma :: Pragma -> R ()
tryToRemovePragma pragmaToTry = tryNewState "pragmas" (pragmas %~ filter (/= pragmaToTry))