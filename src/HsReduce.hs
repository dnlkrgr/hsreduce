module HsReduce
  ( hsreduce,
  )
where

import System.FilePath.Posix
import System.Directory
import Data.Function
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as BS (length)
import qualified Data.Text as T (length, unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import "ormolu" Ormolu.Config (defaultConfig)
import "ormolu" Ormolu.Parser (parseModule)
import "ormolu" Ormolu.Printer (printModule)
import "temporary" System.IO.Temp (withTempDirectory)
import Data.Time
import Util.Util
import Util.Types
import qualified Reduce.Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Reduce.Passes.RemoveUnused.Decls as Decls (reduce)
import qualified Reduce.Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing (reduce)
import qualified Merge.HsAllInOne as HsAllInOne

hsreduce :: Bool -> FilePath -> FilePath -> IO ()
hsreduce isProject test filePath = do
  putStrLn "*******************************************************"
  startTime <- utctDayTime <$> getCurrentTime
  fileContent <- TIO.readFile filePath
  snd <$> parseModule defaultConfig filePath (T.unpack fileContent)
    >>= \case
      Left _ -> error "hsreduce: Could not parse Haskell file."
      Right oldOrmolu -> do
        let oldSize = T.length fileContent
            sourceDir = fst $ splitFileName filePath
        files <- 
          if isProject 
            then HsAllInOne.recListDirectory sourceDir
            else return [test, filePath]
        newOrmolu <-
          withTempDirectory
            "."
            "temp"
            (\tempDir -> do
               let tempTest     = tempDir </> snd (splitFileName test)
                   tempFilePath = tempDir </> snd (splitFileName filePath)
               forM_ files $ \iterFile -> copyFile iterFile (tempDir </> snd (splitFileName iterFile))
               _ormolu . snd <$> runR (RConf tempTest tempFilePath) 
                                      (RState oldOrmolu) 
                                      (allActions isProject)
            )
        let fileName = takeWhile (/= '.') filePath
            newSize = T.length $ printModule newOrmolu
            ratio = 
              round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int
        debugPrint $ "Old size: " ++ show oldSize
        debugPrint $ "Reduced file size: " ++ show newSize
        putStrLn   $ "Reduced file by " ++ show ratio ++ "%"
        TIO.writeFile (fileName ++ "_hsreduce.hs") (printModule newOrmolu)
  endTime <- utctDayTime <$> getCurrentTime
  print $ "Execution took " ++ show (round (endTime - startTime) `div` 60 :: Int) ++ " minutes."

allActions :: Bool -> R ()
allActions True = do
  filePath <- asks _sourceFile
  return ()
  liftIO $ HsAllInOne.hsAllInOne filePath
  -- largestFixpoint allPassesOnce
allActions False =
  largestFixpoint allPassesOnce 


-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
allPassesOnce :: R ()
allPassesOnce = do
  oldOrmolu <- gets _ormolu
  foldM_ (&) oldOrmolu allPasses
  where
    allPasses = [Imports.reduce, Pragmas.reduce, Exports.reduce, Stubbing.reduce, Decls.reduce]

-- | calculate the fixpoint, by always checking if the new module is
-- different from the old one
largestFixpoint :: R () -> R ()
largestFixpoint f =
  iterateFrom
  where
    iterateFrom = do
      liftIO $ putStrLn "\n***NEW ITERATION***"
      oldOrmolu <- gets _ormolu
      f
      newOrmolu <- gets _ormolu
      let oldLength = BS.length . TE.encodeUtf8 $ printModule oldOrmolu
          newLength = BS.length . TE.encodeUtf8 $ printModule newOrmolu
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
