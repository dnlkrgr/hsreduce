module Reduce.Passes.TemplateHaskell where

import GHC hiding (Parsed, Renamed)
import Data.Maybe
import Control.Concurrent.STM.Lifted
import Control.Monad.Reader
import qualified Data.Text.IO as TIO
import Parser.Parser
import Path
import Util.Types
import Util.Util

dumpSplices :: R IO ()
dumpSplices = do
    conf <- ask
    oldState <- readTVarIO $ _tState conf

    when ("TemplateHaskell" `elem` map showExtension (_pragmas oldState))
        . withTempDir (_tempDirs conf)
        $ \tempDir -> do
            let filePath = _sourceFile conf
                absPath = tempDir </> filePath

            -- write current AST to file
            -- parse and typecheck file
            newState <- liftIO $ do
                TIO.writeFile (fromAbsFile absPath) $ showState Parsed oldState
                currentState <- parse absPath
                -- if we don't have a renamed AST, we can't do anything
                if isJust $ _typechecked currentState
                    then do
                        --      1. write renamed AST to file
                        --      instead of normal AST
                        TIO.writeFile (fromAbsFile absPath) $ showState Renamed currentState
                        -- TODO: check if it is still typecheckable; maybe it's not necessary because the test won't be interesting
                        parse absPath
                    else
                        return oldState

            -- try new state, but:
            tryNewState "dumpSplices" (const newState)