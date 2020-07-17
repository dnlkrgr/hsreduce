module Reduce.HsReduce
  ( hsreduce
  )
where

import Data.Maybe
import System.Posix.Files
import Path
import System.Directory
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import "temporary" System.IO.Temp (createTempDirectory)
import Data.Time
import Util.Util
import Util.Types
import qualified Reduce.Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Reduce.Passes.RemoveUnused.Decls   as Decls (fast, slow)
import qualified Reduce.Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing (fast, medium, slow, slowest, rmvUnusedParams)
import Parser.Parser
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity
import Control.Concurrent.STM


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
    let numberOfThreads = 1

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
    newState <- snd <$> runR (RConf test filePath numberOfThreads tChan tAST) beginState allActions

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
    putStrLn $ "\n\nExecution took " ++ show (flip div (60 * 10^(12 :: Integer)) . diffTimeToPicoseconds $ t2 - t1) ++ " minutes."

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
    Stubbing.rmvUnusedParams

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
largestFixpoint f =
    go
  where
      go = do
          liftIO $ putStrLn "\n\n\n***NEW ITERATION***"

          isTestStillFresh "largestFixpoint"
  
          modify $ \s -> s { _isAlive = False }
          f
  
          gets _isAlive >>= flip when go
