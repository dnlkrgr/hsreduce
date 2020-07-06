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
import qualified Reduce.Passes.RemoveUnused.Decls   as Decls (reduce)
import qualified Reduce.Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing (reduce)
import Parser.Parser
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity
import Control.Concurrent.STM


hsreduce :: Int -> FilePath -> FilePath -> FilePath -> IO ()
hsreduce numberOfThreads (fromJust . parseAbsDir -> sourceDir) (fromJust . parseRelFile -> test) (fromJust . parseRelFile -> filePath) = do
    putStrLn "*******************************************************"
    -- 1. parse the test case once at the beginning so we can work on the AST
    -- 2. record all the files in the current directory
    -- 3. record the starting time
    let fullFilePath    =  sourceDir </> filePath
    fileContent         <- TIO.readFile $ fromAbsFile fullFilePath
    beginState          <- parse True [] [] fullFilePath
    let oldSize         =  T.length fileContent
    files               <- listDirectory (fromAbsDir sourceDir)
    startTime           <- utctDayTime <$> getCurrentTime
    tAST                <- atomically . newTVar $ _parsed beginState

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
  
    putStrLn $ "Old size: " ++ show oldSize
    putStrLn $ "Reduced file size: " ++ show newSize
    putStrLn $ "Reduced file by " ++ show ratio ++ "%"
  
    TIO.writeFile (fileName ++ "_hsreduce.hs") (showState newState)
  
    endTime <- utctDayTime <$> getCurrentTime
    print $ "Execution took " ++ show (round (endTime - startTime) `div` 60 :: Int) ++ " minutes."


allActions :: R ()
allActions = do
    largestFixpoint fast
    liftIO $ putStrLn "*** Increasing granularity ***"
    largestFixpoint slow


-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
fast :: R ()
fast = do
    Imports.reduce
    Pragmas.reduce
    Exports.reduce
    Decls.reduce

slow :: R ()
slow = do
    Stubbing.reduce
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
          liftIO $ putStrLn "\n***NEW ITERATION***"

          isTestStillFresh "largestFixpoint"
  
          modify $ \s -> s { _isAlive = False }
          f
          isAlive <- gets _isAlive
  
          when isAlive go
