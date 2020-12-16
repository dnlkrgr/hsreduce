module Reduce.Driver
    ( hsreduce,
      hsreduce'
    )
where

import Data.Time
import Control.Applicative
import Lens.Micro.Platform
import qualified Data.Vector as DV
import Data.IORef
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Csv
import Control.Concurrent.STM.Lifted
    ( newTVar,
      newTChan,
      readTChan,
      writeTChan,
      modifyTVar,
      readTVarIO,
      atomically )
import Control.Exception ( bracket )
import Control.Monad
import Control.Monad.Reader ( asks, MonadIO(liftIO) )
import Data.Text(pack)
import Data.Text.Lazy.Builder ( fromText, fromString )
import Data.Time
    ( UTCTime, getCurrentTime, formatTime, defaultTimeLocale )
import Katip
    ( closeScribes,
      defaultScribeSettings,
      initLogEnv,
      permitItem,
      registerScribe,
      renderSeverity,
      logTM,
      Item(Item, _itemLoc, _itemNamespace, _itemTime, _itemMessage,
           _itemPayload, _itemProcess, _itemHost, _itemThread, _itemSeverity,
           _itemEnv, _itemApp),
      LogItem,
      LogStr(LogStr, unLogStr),
      Severity(InfoS, DebugS),
      ThreadIdText(getThreadIdText),
      Verbosity(V2),
      )
import Katip.Scribes.Handle
    ( mkHandleScribeWithFormatter,
      ColorStrategy(ColorIfTerminal),
      ItemFormatter,
      brackets,
      colorBySeverity )
import Path
import Path.IO
import System.IO 
import Util.Types
import Util.Util
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Util.Parser
import Data.Word

hsreduce :: [R IO ()] -> Word64 -> FilePath -> FilePath -> Bool -> Word64 -> Bool -> Maybe T.Text -> IO ()
hsreduce allActions (fromIntegral -> numberOfThreads) test sourceFile recordStatistics timeOut debug mUnusedPassName = do
            testAbs <- resolveFile' test
            filePathAbs <- resolveFile' sourceFile
            -- 1. parse the test case once at the beginning so we can work on the AST
            -- 2. record all the files in the current directory
            -- 3. record the starting time
            fileContent <- TIO.readFile $ fromAbsFile filePathAbs
            beginState <- parse filePathAbs
            hsreduce' allActions numberOfThreads testAbs filePathAbs fileContent beginState recordStatistics timeOut debug mUnusedPassName

hsreduce' :: [R IO ()] -> Word64 -> Path Abs File -> Path Abs File -> T.Text -> RState -> Bool -> Word64 -> Bool -> Maybe T.Text -> IO ()
hsreduce' allActions (fromIntegral -> numberOfThreads) testAbs filePathAbs fileContent beginState recordStatistics timeOut debug mUnusedPassName = do
    -- get old stats, fail as fast as possible
    -- TODO: check if we need to create this dir
    statDir <- liftA2 (</>) getHomeDir (pure [reldir|.hsreduce|])
    ensureDir statDir

    statFilePath <- case mUnusedPassName of
            Nothing -> pure $ statDir </> [relfile|stats.csv|]
            Just unusedPassName -> liftA2 (</>) (pure statDir) (parseRelFile $ "stats_" <> T.unpack unusedPassName <> ".csv")

    oldStats <- if recordStatistics 
        then do 
            doesFileExist statFilePath >>= \case
                False -> pure emptyStats
                True -> decodeByName <$> LBS.readFile (fromAbsFile statFilePath) >>= \case 
                    Left e -> do
                        print e
                        pure emptyStats
                    Right (_, oldStats) -> pure . Statistics $ M.fromList $ DV.toList $ DV.map stats2NamedTuple oldStats
        else pure emptyStats

    t1 <- getCurrentTime
    tState <- atomically $ newTVar beginState

    let sourceDir = parent testAbs
        oldSize = T.length fileContent
    startTokenNumber <- countTokensM $ fromAbsFile filePathAbs
    startNamesNumber <- countNamesM $ fromAbsFile filePathAbs

    -- 1. create a channel
    -- 2. create as many temp dirs as we have threads
    -- 3. copy all necessary files into the temp dir
    -- 4. write the temp dir name into the channel
    tChan <- atomically newTChan
    forM_ [1 .. numberOfThreads] $ \_ -> do
        tempDir <- createTempDir [absdir|/tmp|] "hsreduce"

        (dirs, files) <- listDir sourceDir
        forM_ dirs $ \d -> copyDirRecur d (tempDir </> dirname d)
        forM_ files $ \f -> copyFile f (tempDir </> filename f)

        atomically $ writeTChan tChan tempDir

    -- KATIP STUFF
    let mkFileHandle = openFile "hsreduce.log" WriteMode

    bracket mkFileHandle hClose $ \logFile -> do
        handleScribe <- mkHandleScribeWithFormatter myFormat ColorIfTerminal logFile (permitItem DebugS) V2
        let mkLogEnv = registerScribe "hsreduce.log" handleScribe defaultScribeSettings =<< initLogEnv "hsreduce" "devel"

        bracket mkLogEnv closeScribes $ \le -> do
            logRef <- newIORef []
            let beginConf = RConf (filename testAbs) (filename filePathAbs) numberOfThreads tChan tState mempty mempty le logRef (timeOut * 1000 * 1000) debug

            -- run the reducing functions
            runR beginConf $ do
                let newTokenNumber = countTokensHelper (_dflags beginState) $ T.unpack fileContent
                let newNamesNumber = countNames (_parsed beginState) 

                updateStatistics beginConf "formatting" 1 (fromIntegral (T.length $ showState Parsed beginState) - fromIntegral oldSize) (newTokenNumber - startTokenNumber) (newNamesNumber - startNamesNumber)
                mapM_ largestFixpoint allActions

                newState <- readTVarIO tState

                -- handling of the result and outputting useful information
                let fileName = takeWhile (/= '.') . fromAbsFile $ filePathAbs
                    newSize = T.length . showState Parsed $ newState
                let successfulInvocations = sum . map _successfulAttempts . M.elems $ newState ^. statistics . passStats
                let totalInvocations = sum . map _totalAttempts . M.elems $ newState ^. statistics . passStats
                t2 <- liftIO getCurrentTime

                let offset =
                        if utctDayTime t2 < utctDayTime t1
                        then 86401
                        else 0
                let duration = utctDayTime t2 + offset - utctDayTime t1


                printDebugInfo "*******************************************************"
                printDebugInfo "Finished."
                printDebugInfo $ "Old size:     (names)   " <> show (countNames (_parsed beginState))
                printDebugInfo $ "Reduced size: (names)   " <> show (countNames (_parsed newState))
                printDebugInfo $ "Duration:               " <> show duration
                printDebugInfo $ "Total Invocations:      " <> show totalInvocations
                printDebugInfo $ "Successful Invocations: " <> show successfulInvocations
                printDebugInfo $ "Old size:     (bytes)   " <> show oldSize
                printDebugInfo $ "Reduced size: (bytes)   " <> show newSize
                -- printDebugInfo $ "Old size:     (tokens)   " <> show (countToken (_parsed beginState))
                -- printDebugInfo $ "Reduced size: (tokens)   " <> show (countNames (_parsed newState))

                $(logTM) InfoS "*******************************************************"
                $(logTM) InfoS "Finished."
                $(logTM) InfoS (LogStr . fromString $ "Old size     (bytes):        " <> show oldSize)
                $(logTM) InfoS (LogStr . fromString $ "Reduced size (bytes):    " <> show newSize)

                liftIO $ TIO.writeFile (fileName <> "_hsreduce.hs") (showState Parsed newState)

                when recordStatistics $ do 

                    perfStats <- mkPerformance (fromIntegral oldSize) (fromIntegral newSize) t1 t2 (fromIntegral numberOfThreads) successfulInvocations totalInvocations (getTokenDiff newState beginState) (getNameDiff newState beginState)
                    liftIO $ appendFile "hsreduce_performance.csv" $ show perfStats

                    liftIO 
                        . BS.writeFile (fromAbsFile statFilePath)
                        . LBS.toStrict
                        . encodeDefaultOrderedByName
                        . map snd 
                        . M.toList 
                        $ M.unionWith (+) (oldStats ^. passStats) (newState ^. statistics . passStats)

                    liftIO 
                        . BS.writeFile (fromRelFile [relfile|./hsreduce_statistics.csv|])
                        . LBS.toStrict
                        . encodeDefaultOrderedByName
                        . map snd 
                        . M.toList 
                        $ (newState ^. statistics . passStats)

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
            printDebugInfo "***NEW ITERATION***"
            $(logTM) InfoS "***NEW ITERATION***"
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