module Passes.RemoveUnused.Pragmas (reduce) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Lens.Micro.Platform
import Types
import Util

reduce :: R ()
reduce =
    do
        printInfo "Removing Pragmas"
        isTestStillFresh "Pragmas"

        ask
            >>= fmap _pragmas . liftIO . readTVarIO . _tState
            >>= mapM_
                ( \p ->
                      ask
                          >>= liftIO . tryNewState "pragmas" (pragmas %~ filter (/= p))
                )