module Reduce.HsReduce
  ( hsreduce
  )
where

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

hsreduce :: FilePath -> FilePath -> FilePath -> IO ()
hsreduce fSourceDir fTest fFilePath  = do
    putStrLn "*******************************************************"
    sourceDir           <- parseAbsDir fSourceDir
    test                <- parseRelFile fTest
    filePath            <- parseRelFile fFilePath 
    startTime           <- utctDayTime <$> getCurrentTime
    let fullFilePath    =  sourceDir </> filePath
    fileContent         <- TIO.readFile $ fromAbsFile fullFilePath
    beginState          <- parse True [] [] fullFilePath
    let oldSize         =  T.length fileContent
    files               <- listDirectory (fromAbsDir sourceDir)
    let numberOfThreads = 4

    tchan <- atomically newTChan
    forM_ [1 .. numberOfThreads] $ \_ ->  do
        t <- createTempDirectory "/tmp" "hsreduce"

        tempDir <- parseAbsDir t

  
        forM_ files $ \f -> do
            iterFile <- parseRelFile f
            let oldPath = fromAbsFile $ sourceDir </> iterFile
            status <- getFileStatus oldPath
  
            if isDirectory status
            then copyDirectoryRecursive normal oldPath (fromAbsFile $ tempDir </> filename iterFile)
            else copyFile oldPath (fromAbsFile $ tempDir </> filename iterFile)

        atomically $ writeTChan tchan tempDir

    newState <- snd <$> runR (RConf test filePath numberOfThreads tchan) beginState allActions
  
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
    test <- asks _test
    tchan      <- asks _tempDirs
    result <- liftIO $ withTempDir tchan $ \tempDir -> runTest' (tempDir </> test) (120 * 1000 * 1000) 

    case result of 
        Uninteresting -> error "*** test is uninteresting at the start! ***"
        Interesting -> do
            liftIO $ putStrLn ":-) Test is interesting at the start"
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

-- | calculate the fixpoint, by always checking if the new module is
-- smaller than the old one
largestFixpoint :: R () -> R ()
largestFixpoint f =
    go
    where
      go = do
          liftIO $ putStrLn "\n***NEW ITERATION***"
  
          modify $ \s -> s { _isAlive = False }
          f
          isAlive <- gets _isAlive
  
          when isAlive go
