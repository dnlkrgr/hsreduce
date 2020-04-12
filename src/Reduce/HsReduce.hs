module Reduce.HsReduce
  ( hsreduce,
  )
where

import System.Posix.Files
import System.FilePath.Posix
import System.Directory
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

hsreduce :: FilePath -> FilePath -> IO ()
hsreduce test filePath = do
  putStrLn "*******************************************************"
  startTime   <- utctDayTime <$> getCurrentTime
  fileContent <- TIO.readFile filePath
  beginState  <- parse [] [] filePath 
  let oldSize = T.length fileContent
      sourceDir = fst $ splitFileName filePath
  files <- listDirectory sourceDir
  print files
  newState <-
    withTempDirectory
      "."
      "temp"
      (\tempDir -> do
         let tempTest     = tempDir </> snd (splitFileName test)
             tempFilePath = tempDir </> snd (splitFileName filePath)
         forM_ files $ \iterFile -> do
           let oldPath = (sourceDir </> iterFile)
           status <- getFileStatus oldPath
           if isDirectory status
              then copyDirectoryRecursive normal oldPath (tempDir </> snd (splitFileName iterFile))
              else copyFile oldPath (tempDir </> snd (splitFileName iterFile))
         snd <$> runR (RConf tempTest tempFilePath)
                                beginState
                                allActions
      )
  let fileName = takeWhile (/= '.') filePath
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
  runTest >>=
    \case
      Uninteresting -> error "*** test is uninteresting from the start! ***"
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
  iterateFrom
  where
    iterateFrom = do
      liftIO $ putStrLn "\n***NEW ITERATION***"
      oldLength <- BS.length . TE.encodeUtf8 . T.pack . showGhc . _parsed <$> get
      f
      newLength <- BS.length . TE.encodeUtf8 . T.pack . showGhc . _parsed <$> get
      liftIO $ debugPrint $ "old length: " ++ show oldLength
      liftIO $ debugPrint $ "new length: " ++ show newLength
      liftIO $ debugPrint $ "new ormolu is this many chars shorter than the old one: " ++ show (oldLength - newLength)
      if newLength < oldLength
        then do
          liftIO $ debugPrint "taking new ormolu"
          iterateFrom
        else do
          liftIO $ debugPrint "taking old ormolu"
          return ()
