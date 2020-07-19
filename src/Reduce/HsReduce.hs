module Reduce.HsReduce
  ( hsreduce
  )
where

import "temporary" System.IO.Temp (createTempDirectory)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity
import GHC.Conc
import Parser.Parser
import Path
import System.Directory
import System.Posix.Files
import Util.Types
import Util.Util
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Reduce.Passes.RemoveUnused.Decls   as Decls (fast, slow)
import qualified Reduce.Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Reduce.Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing (fast, medium, slow, slowest)

hsreduce :: FilePath -> FilePath -> FilePath -> IO ()
hsreduce (fromJust . parseAbsDir -> sourceDir) (fromJust . parseRelFile -> test) (fromJust . parseRelFile -> filePath) = do
    putStrLn "*******************************************************"
    -- 1. parse the test case once at the beginning so we can work on the AST
    -- 2. record all the files in the current directory
    -- 3. record the starting time
    let fullFilePath    =  sourceDir </> filePath
    fileContent         <- TIO.readFile $ fromAbsFile fullFilePath
    beginState          <- parse True [] [] fullFilePath
    let oldSize         =  T.length fileContent
    files               <- listDirectory (fromAbsDir sourceDir)
    t1                  <- utctDayTime <$> getCurrentTime
    tAST                <- atomically . newTVar $ _parsed beginState
    tAlive              <- atomically $ newTVar False
    numberOfThreads     <- getNumProcessors

    -- 1. create a channel
    -- 2. create as many temp dirs as we have threads
    -- 3. copy all necessary files into the temp dir
    -- 4. write the temp dir name into the channel
    tChan <- atomically newTChan
    forM_ [1 .. numberOfThreads] $ \_ ->  do
        t <- createTempDirectory "/tmp" "hsreduce"

        tempDir <- parseAbsDir t

        forM_ files $ \f -> do
            iterFile    <- parseRelFile f
            let oldPath = fromAbsFile $ sourceDir </> iterFile
            status      <- getFileStatus oldPath

            if isDirectory status
            then copyDirectoryRecursive normal oldPath (fromAbsFile $ tempDir </> filename iterFile)
            else copyFile oldPath (fromAbsFile $ tempDir </> filename iterFile)

        atomically $ writeTChan tChan tempDir

    -- run the reducing functions
    newState <- snd <$> runR (RConf test filePath numberOfThreads tChan tAST tAlive) beginState allActions

    -- handling of the result and outputting useful information
    let fileName = takeWhile (/= '.') . fromAbsFile $ fullFilePath
        newSize  = T.length . showState $ newState
        ratio    = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int

    putStrLn "*******************************************************"
    putStrLn $ "\n\nFinished."
    putStrLn $ "Old size:        " ++ show oldSize
    putStrLn $ "Reduced size:    " ++ show newSize
    putStrLn $ "Reduced file by: " ++ show ratio ++ "%"

    TIO.writeFile (fileName ++ "_hsreduce.hs") (showState newState)

    t2 <- utctDayTime <$> getCurrentTime
    putStrLn $ "\n\nExecution took " ++ show (flip div (10^(12 :: Integer)) . diffTimeToPicoseconds $ t2 - t1) ++ " seconds."

    putStrLn . show $ _statistics newState
    -- writeFile "hsreduce.statistics" . printStatistics $ _statistics newState

allActions :: R ()
allActions = do
    forM_ passes $ \pass -> do
        liftIO $ putStrLn "\n\n*** Increasing granularity ***"
        largestFixpoint pass
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
    fast

-- 1. check if the test-case is still interesting (it should be at the start of the loop!)
-- 2. set alive variable to false
-- 3. run reducing function f
-- 4. check if something interesting happened; if yes, continue
largestFixpoint :: R () -> R ()
largestFixpoint f = do
    tAlive <- asks _tAlive
    go tAlive
  where
      go tAlive = do
          liftIO $ putStrLn "\n\n\n***NEW ITERATION***"
          isTestStillFresh "largestFixpoint"
  
          -- modify $ \s -> s { _isAlive = False }
          liftIO . atomically $ writeTVar tAlive False
          f
          liftIO (atomically $ readTVar tAlive) >>= flip when (go tAlive)
