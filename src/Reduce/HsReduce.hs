module HsReduce
  ( hsreduce
  )
where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import System.IO.Temp (createTempDirectory)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Parser.Parser
import Path
import System.Directory
import Util.Types
import Util.Util
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Passes.RemoveUnused.Decls   as Decls (fast, slow)
import qualified Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Passes.RemoveUnused.Parameters as Parameters (reduce)
import qualified Passes.Stubbing as Stubbing (fast, medium, slow, slowest)
import qualified Passes.DataTypes as DataTypes (inline, rmvConArgs)
import qualified Passes.Names as Names (shortenNames)
import qualified Passes.Functions as Functions (inline)

hsreduce :: Int -> FilePath -> FilePath -> (Maybe (R ())) -> IO ()
hsreduce numberOfThreads (fromJust . parseRelFile -> test) (fromJust . parseRelFile -> filePath) mAction = do
    putStrLn "*******************************************************"
    -- 1. parse the test case once at the beginning so we can work on the AST
    -- 2. record all the files in the current directory
    -- 3. record the starting time
    sourceDir           <- parseAbsDir =<< getCurrentDirectory

    let 
        fullFilePath    =  sourceDir </> filePath

    fileContent         <- TIO.readFile $ fromAbsFile fullFilePath
    beginState          <- parse True [] [] fullFilePath
    t1                  <- getCurrentTime
    tState              <- atomically $ newTVar beginState

    let 
        oldSize         =  T.length fileContent
        files           = [test, filePath]


    -- 1. create a channel
    -- 2. create as many temp dirs as we have threads
    -- 3. copy all necessary files into the temp dir
    -- 4. write the temp dir name into the channel
    tChan <- atomically newTChan
    forM_ [1 .. numberOfThreads] $ \_ ->  do
        t <- createTempDirectory (fromAbsDir sourceDir) "hsreduce"

        tempDir <- parseAbsDir t

        forM_ files $ \f -> copyFile (fromAbsFile $ sourceDir </> f) (fromAbsFile $ tempDir </> filename f)

        atomically $ writeTChan tChan tempDir

    -- recording the size diff of formatting
    print $ filename test
    print $ filename filePath

    let beginConf       = (RConf (filename test) (filename filePath) numberOfThreads tChan tState)
    updateStatistics beginConf "formatting" True (oldSize - T.length (showState beginState))

    -- run the reducing functions
    case mAction of
        Nothing -> void $ runR beginConf allActions
        Just oneAction -> void $ runR beginConf oneAction
    newState <- readTVarIO tState

    -- handling of the result and outputting useful information
    let fileName = takeWhile (/= '.') . fromAbsFile $ fullFilePath
        newSize  = T.length . showState $ newState

    putStrLn "*******************************************************"
    putStrLn "\n\nFinished."
    putStrLn $ "Old size:        " ++ show oldSize
    putStrLn $ "Reduced size:    " ++ show newSize

    TIO.writeFile (fileName ++ "_hsreduce.hs") (showState newState)

    t2 <- getCurrentTime
    
    perfStats <- mkPerformance (fromIntegral oldSize) (fromIntegral newSize) t1 t2 (fromIntegral numberOfThreads)

    appendFile "hsreduce_performance.csv" $ show perfStats
    LBS.writeFile "hsreduce_statistics.csv" . encodeDefaultOrderedByName . map snd . M.toList . _passStats $ _statistics newState


    forM_ [1 .. numberOfThreads] $ \_ -> do
        t <- atomically $ readTChan tChan
        removeDirectoryRecursive $ fromAbsDir t


allActions :: R ()
allActions =
    forM_ passes $ \pass -> do
        liftIO $ putStrLn "\n\n*** Increasing granularity ***"
        largestFixpoint pass
  -- where passes = [snail]
  where passes = [fast, medium, slow, slowest, snail]


-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
fast :: R ()
fast = do
    Imports.reduce
    Pragmas.reduce
    Exports.reduce
    Decls.fast

medium :: R ()
medium = do
    Stubbing.fast
    fast
    Decls.slow

slow :: R ()
slow = do
    Stubbing.medium
    fast 

slowest :: R ()
slowest = do
    Stubbing.slow
    fast 

snail :: R ()
snail = do
    Stubbing.slowest
    -- Names.shortenNames -- currently broken
    Parameters.reduce
    DataTypes.inline
    Functions.inline
    DataTypes.rmvConArgs
    fast

-- 1. check if the test-case is still interesting (it should be at the start of the loop!)
-- 2. set alive variable to false
-- 3. run reducing function f
-- 4. check if something interesting happened; if yes, continue
largestFixpoint :: R () -> R ()
largestFixpoint f = do
    tState <- asks _tState
    go tState 
  where
      go tState = do
          liftIO $ putStrLn "\n\n\n***NEW ITERATION***"
          isTestStillFresh "largestFixpoint"
  
          liftIO . atomically $ modifyTVar tState $ \s -> s { _isAlive = False }
          f
          b <- liftIO (fmap _isAlive . atomically $ readTVar tState) 
          when b (go tState)