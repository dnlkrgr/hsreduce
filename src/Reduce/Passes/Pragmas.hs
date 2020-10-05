module Reduce.Passes.Pragmas (reduce) where

import Control.Concurrent.STM.Lifted (readTVarIO)
import Control.Monad.Reader (MonadReader (ask))
import Lens.Micro.Platform ((%~))
import Util.Types (R, RConf (_tState), RState (_pragmas), pragmas)
import Util.Util (isTestStillFresh, printInfo, tryNewState)

reduce :: R IO ()
reduce =
    do
        printInfo "Removing Pragmas"
        isTestStillFresh "Pragmas"

        ask
            >>= fmap _pragmas . readTVarIO . _tState
            >>= mapM_ (\p -> tryNewState "pragmas" (pragmas %~ filter (/= p)))