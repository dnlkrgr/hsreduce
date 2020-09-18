module Reduce.Driver
    ( hsreduce,
    )
where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Word
import Parser.Parser
import Path
import Path.IO
import Util.Types
import Util.Util

hsreduce :: Word8 -> FilePath -> FilePath -> R () -> IO ()
hsreduce (fromIntegral -> numberOfThreads) test filePath allActions = do
    putStrLn "*******************************************************"
    testAbs <- resolveFile' test
    filePathAbs <- resolveFile' filePath
    -- 1. parse the test case once at the beginning so we can work on the AST
    -- 2. record all the files in the current directory
    -- 3. record the starting time

    fileContent <- TIO.readFile $ fromAbsFile filePathAbs
    beginState <- parse True filePathAbs
    t1 <- getCurrentTime
    tState <- atomically $ newTVar beginState

    let sourceDir = parent testAbs
        oldSize = T.length fileContent
    files <- listDir sourceDir

    -- 1. create a channel
    -- 2. create as many temp dirs as we have threads
    -- 3. copy all necessary files into the temp dir
    -- 4. write the temp dir name into the channel
    tChan <- atomically newTChan
    forM_ [1 .. numberOfThreads] $ \_ -> do
        tempDir <- createTempDir [absdir|/tmp|] "hsreduce"

        forM_ (fst files) $ \d -> copyDirRecur d (tempDir </> dirname d)
        forM_ (snd files) $ \f -> copyFile f (tempDir </> filename f)

        atomically $ writeTChan tChan tempDir

    -- recording the size diff of formatting for more accurate statistics
    let beginConf = (RConf (filename testAbs) (filename filePathAbs) numberOfThreads tChan tState)
    updateStatistics beginConf "formatting" 1 (T.length (showState beginState) - oldSize)

    -- run the reducing functions
    void $ runR beginConf $ largestFixpoint allActions
    newState <- readTVarIO tState

    -- handling of the result and outputting useful information
    let fileName = takeWhile (/= '.') . fromAbsFile $ filePathAbs
        newSize = T.length . showState $ newState

    putStrLn "*******************************************************"
    putStrLn "\n\nFinished."
    putStrLn $ "Old size:        " <> show oldSize
    putStrLn $ "Reduced size:    " <> show newSize

    TIO.writeFile (fileName <> "_hsreduce.hs") (showState newState)

    t2 <- getCurrentTime

    perfStats <- mkPerformance (fromIntegral oldSize) (fromIntegral newSize) t1 t2 (fromIntegral numberOfThreads)

    appendFile "hsreduce_performance.csv" $ show perfStats
    LBS.writeFile "hsreduce_statistics.csv" . encodeDefaultOrderedByName . map snd . M.toList . _passStats $ _statistics newState

    forM_ [1 .. numberOfThreads] $ \_ -> do
        t <- atomically $ readTChan tChan
        removeDirRecur t


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

            liftIO . atomically $ modifyTVar tState $ \s -> s {_isAlive = False}
            f
            liftIO (_isAlive <$> readTVarIO tState) >>= flip when (go tState)