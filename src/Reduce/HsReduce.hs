module Reduce.HsReduce
  ( hsreduce,
    arst
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
import "temporary" System.IO.Temp (withTempDirectory)
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

root = "/home/daniel/workspace/hsreduce/test-cases/trying-out/"
--root = "/home/daniel/workspace/hsreduce/test-cases/ticket14779/"
arst = do
  source <- parseAbsFile $ root ++ "Trying-out.hs"
  --source <- parseAbsFile $ root ++ "Ticket14779.hs"
  test <- parseAbsFile $ root ++ "interesting.sh"
  hsreduce test source

hsreduce :: Path Abs File -> Path Abs File -> IO ()
hsreduce test filePath = do
  putStrLn "*******************************************************"

  startTime   <- utctDayTime <$> getCurrentTime
  fileContent <- TIO.readFile $ fromAbsFile filePath
  beginState  <- parse [] [] filePath
  let oldSize = T.length fileContent
      sourceDir = parent filePath
  files <- listDirectory (fromAbsDir sourceDir)
  currentDir <- getCurrentDirectory

  newState <-
    withTempDirectory
      currentDir
      "temp"
      (\t -> do
         tempDir <- parseAbsDir t
         let tempTest     = tempDir </> filename test
             tempFilePath = tempDir </> filename filePath

         forM_ files $ \f -> do
           iterFile <- parseRelFile f
           let oldPath = fromAbsFile $ sourceDir </> iterFile
           status <- getFileStatus oldPath

           if isDirectory status
              then copyDirectoryRecursive normal oldPath (fromAbsFile $ tempDir </> filename iterFile)
              else copyFile oldPath (fromAbsFile $ tempDir </> filename iterFile)

         snd <$> runR (RConf tempTest tempFilePath)
                                beginState
                                allActions
      )

  let fileName = takeWhile (/= '.') . fromAbsFile $ filePath
      newSize = T.length . T.pack $ showGhc . _parsed $ newState
      ratio =
        round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int

  debugPrint $ "Old size: " ++ show oldSize
  debugPrint $ "Reduced file size: " ++ show newSize
  putStrLn   $ "Reduced file by " ++ show ratio ++ "%"

  TIO.writeFile (fileName ++ "_hsreduce.hs") (showState newState)

  endTime <- utctDayTime <$> getCurrentTime
  print $ "Execution took " ++ show (round (endTime - startTime) `div` 60 :: Int) ++ " minutes."

allActions :: R ()
allActions = do
  runTest (120 * 1000 * 1000) >>=
    \case
      Uninteresting -> error "*** test is uninteresting at the start! ***"
      Interesting -> do
        liftIO $ putStrLn ":-) Test is interesting at the start"
        largestFixpoint allPassesOnce


-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
allPassesOnce :: R ()
allPassesOnce = do
  Imports.reduce
  Pragmas.reduce
  Exports.reduce
  Decls.reduce
  Stubbing.reduce

-- | calculate the fixpoint, by always checking if the new module is
-- smaller than the old one
largestFixpoint :: R () -> R ()
largestFixpoint f =
  go
  where
    go = do
      liftIO $ putStrLn "\n***NEW ITERATION***"

      f
      isAlive <- gets _isAlive

      when isAlive go
