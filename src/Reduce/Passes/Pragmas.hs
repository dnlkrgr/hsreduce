module Reduce.Passes.Pragmas (reduce) where

import Control.Concurrent.STM.Lifted
import Control.Monad.Reader
import Lens.Micro.Platform
import Util.Types
import Util.Util

reduce :: R IO ()
reduce =
    do
        printInfo "Removing Pragmas"
        isTestStillFresh "Pragmas"

        ask
            >>= fmap _pragmas . readTVarIO . _tState
            >>= mapM_ ( \p -> tryNewState "pragmas" (pragmas %~ filter (/= p)))