module Reduce.Passes.TemplateHaskell where

import Util.Types
import Util.Util
import Control.Concurrent.STM.Lifted
import Control.Monad.Reader

dumpSplices :: R IO ()
dumpSplices = do
    conf <- ask
    oldState <- readTVarIO $ _tState conf

    -- write current AST to file
    -- parse and typecheck file
    -- try new state, but: 
    --      1. write renamed AST to file
    --      instead of normal AST

    return ()