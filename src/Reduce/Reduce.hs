module Reduce.Reduce
  ( hsreduce,
  )
where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString as BS (length)
import qualified Data.Text as T (length, unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult)
import Ormolu.Printer (printModule)
import System.Directory (copyFile, listDirectory)
import System.FilePath.Posix ((</>), splitFileName)
import System.IO.Temp (withTempDirectory)
import Data.Time
import Reduce.Util
import Reduce.Types
import qualified Reduce.Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Reduce.Passes.RemoveUnused.Decls as Decls (reduce)
import qualified Reduce.Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Reduce.Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing (reduce)

hsreduce :: FilePath -> FilePath -> IO ()
hsreduce test filePath = do
  startTime <- utctDayTime <$> getCurrentTime
  fileContent <- TIO.readFile filePath
  snd <$> parseModule defaultConfig filePath (T.unpack fileContent)
    >>= \case
      Left _ -> return ()
      Right oldOrmolu -> do
        let oldSize = T.length fileContent
            sourceDir = fst $ splitFileName filePath
        files <- listDirectory sourceDir
        newOrmolu <-
          withTempDirectory
            "."
            "temp"
            ( \tempDir -> do
                let tempTest = tempDir </> snd (splitFileName test)
                    tempFilePath = tempDir </> snd (splitFileName filePath)
                -- TODO: Reduce should only copy test and the source file, nothing else! => tests should be self-contained
                forM_ files $ \iterFile -> copyFile (sourceDir </> iterFile) (tempDir </> snd (splitFileName iterFile))
                -- TODO: write tests to check if the new module is even parsable haskell
                evalStateT (largestFixpoint allPassesOnce oldOrmolu) (ReduceState tempTest tempFilePath oldOrmolu)
            )
        let fileName = takeWhile (/= '.') filePath
            newSize = T.length $ printModule newOrmolu
            ratio = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int
        debugPrint $ "Old size: " ++ show oldSize
        debugPrint $ "Reduced file size: " ++ show newSize
        putStrLn $ "Reduced file by " ++ show ratio ++ "%"
        TIO.writeFile (fileName ++ "_hsreduce.hs") (printModule newOrmolu)
  endTime <- utctDayTime <$> getCurrentTime
  print $ "Execution took " ++ show (round (endTime - startTime) `div` 60 :: Int) ++ " minutes."

-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
allPassesOnce :: OPR.ParseResult -> ReduceM OPR.ParseResult
allPassesOnce oldOrmolu =
  foldM (&) oldOrmolu allPasses
  where
    allPasses = [Imports.reduce, Pragmas.reduce, Exports.reduce, Stubbing.reduce, Decls.reduce]

-- | calculate the fixpoint, by always checking if the new module is
-- different from the old one

largestFixpoint :: (OPR.ParseResult -> ReduceM OPR.ParseResult) -> OPR.ParseResult -> ReduceM OPR.ParseResult
largestFixpoint f =
  iterateFrom
  where
    iterateFrom oldOrmolu = do
      liftIO $ putStrLn "\n***NEW ITERATION***"
      
      newOrmolu <- f oldOrmolu
      let oldLength = BS.length . TE.encodeUtf8 $ printModule oldOrmolu
          newLength = BS.length . TE.encodeUtf8 $ printModule newOrmolu
      liftIO $ debugPrint $ "old length: " ++ show oldLength
      liftIO $ debugPrint $ "new length: " ++ show newLength
      liftIO $ debugPrint $ "new ormolu is this many chars shorter than the old one: " ++ show (oldLength - newLength)
      if newLength < oldLength
        then do
          liftIO $ debugPrint "taking new ormolu"
          iterateFrom newOrmolu
        else do
          liftIO $ debugPrint "taking old ormolu"
          return oldOrmolu
