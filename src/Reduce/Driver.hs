module Reduce.Driver
    ( hsreduce,
    )
where

import Control.Concurrent.STM.Lifted
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Csv
import Data.Text(pack)
import Data.Text.Lazy.Builder
import Data.Time
import Data.Word
import Katip
import Katip.Scribes.Handle
import Path
import Path.IO
import System.IO
import Util.Types
import Util.Util
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

hsreduce :: [R IO ()] -> Word8 -> FilePath -> FilePath -> IO ()
hsreduce allActions (fromIntegral -> numberOfThreads) test filePath = do
    testAbs <- resolveFile' test
    filePathAbs <- resolveFile' filePath
    -- 1. parse the test case once at the beginning so we can work on the AST
    -- 2. record all the files in the current directory
    -- 3. record the starting time

    fileContent <- TIO.readFile $ fromAbsFile filePathAbs
    beginState <- parse filePathAbs
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

    -- KATIP STUFF
    let mkFileHandle = openFile "hsreduce.log" WriteMode

    bracket mkFileHandle hClose $ \_ -> do
        -- handleScribe <- mkHandleScribe ColorIfTerminal fileHandle (permitItem InfoS) V2
        handleScribe <- mkHandleScribeWithFormatter myFormat ColorIfTerminal stdout (permitItem InfoS) V2
        let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "hsreduce" "devel"

        bracket mkLogEnv closeScribes $ \le -> do
            let beginConf = RConf (filename testAbs) (filename filePathAbs) numberOfThreads tChan tState mempty mempty le

            -- run the reducing functions
            runR beginConf $ do
                updateStatistics beginConf "formatting" 1 (T.length (showState beginState) - oldSize)
                mapM_ largestFixpoint allActions

                newState <- readTVarIO tState

                -- handling of the result and outputting useful information
                let fileName = takeWhile (/= '.') . fromAbsFile $ filePathAbs
                    newSize = T.length . showState $ newState

                $(logTM) InfoS "*******************************************************"
                $(logTM) InfoS "Finished."
                $(logTM) InfoS (LogStr . fromString $ "Old size:        " <> show oldSize)
                $(logTM) InfoS (LogStr . fromString $ "Reduced size:    " <> show newSize)

                liftIO $ TIO.writeFile (fileName <> "_hsreduce.hs") (showState newState)

                t2 <- liftIO getCurrentTime

                perfStats <- mkPerformance (fromIntegral oldSize) (fromIntegral newSize) t1 t2 (fromIntegral numberOfThreads)

                liftIO $ appendFile "hsreduce_performance.csv" $ show perfStats

                -- TODO: format this better
                $(logTM) InfoS (LogStr . fromString . show . map snd . M.toList . _passStats $ _statistics newState)

    forM_ [1 .. numberOfThreads] $ \_ -> do
        t <- atomically $ readTChan tChan
        removeDirRecur t


-- 1. check if the test-case is still interesting (it should be at the start of the loop!)
-- 2. set alive variable to false
-- 3. run reducing function f
-- 4. check if something interesting happened; if yes, continue
largestFixpoint :: R IO () -> R IO ()
largestFixpoint f = do
    tState <- asks _tState
    go tState
    where
        go tState = do
            -- $(logTM) InfoS "***NEW ITERATION***"
            isTestStillFresh "largestFixpoint"

            atomically $ modifyTVar tState $ \s -> s {_isAlive = False}
            f
            (_isAlive <$> readTVarIO tState) >>= flip when (go tState)


myTimeFormat :: UTCTime -> T.Text
myTimeFormat = pack . formatTime defaultTimeLocale "%H:%M:%S"

myFormat :: LogItem a => ItemFormatter a
myFormat withColor _ Item{..} =
    brackets nowStr 
    -- <> brackets (mconcat $ map fromText $ intercalateNs _itemNamespace) 
    <> brackets (fromText (renderSeverity' _itemSeverity))
    <> brackets (fromText $ "ThreadId " <> getThreadIdText _itemThread)
    -- <> mconcat ks 
    -- <> maybe mempty (brackets . fromString . locationToString) _itemLoc
    <> fromText " " <> (unLogStr _itemMessage)
  where
    nowStr = fromText (myTimeFormat _itemTime)
    renderSeverity' severity = colorBySeverity withColor severity (renderSeverity severity)