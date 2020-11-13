module Reduce.Passes.TemplateHaskell where

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

    when ("TemplateHaskell" `elem` map showExtension (_pragmas oldState)) $ do
        newState <- withTempDir (_tempDirs conf) $ \tempDir -> do
            let filePath = _sourceFile conf
                absPath = tempDir </> filePath

            -- write current AST to file
            -- parse and typecheck file
            liftIO $ do
                TIO.writeFile (fromAbsFile absPath) $ showState Parsed oldState
                parse absPath >>= \case
                    currentState@TypecheckedState{} -> do
                        TIO.writeFile (fromAbsFile absPath) $ showState Renamed currentState
                        -- TODO: check if it is still typecheckable; maybe it's not necessary because the test won't be interesting
                        parse absPath
                    other -> return other

        -- try new state, but:
        tryNewState "dumpSplices" (const newState)