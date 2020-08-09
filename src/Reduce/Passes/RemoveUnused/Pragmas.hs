module Passes.RemoveUnused.Pragmas (reduce) where

import Control.Concurrent.STM
import Lens.Micro.Platform
import Control.Monad.Reader

import Types
import Util

reduce :: R ()
reduce = do
    printInfo "Removing Pragmas"
    isTestStillFresh "Pragmas"

    conf        <- ask
    oldState    <- liftIO . atomically . readTVar $ _tState conf
    forM_ (_pragmas oldState) $ \p -> 
        liftIO . Util.tryNewState "pragmas" (pragmas %~ filter (/= p)) =<< ask