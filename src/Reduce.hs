module Reduce
  ( hsreduce,
  )
where

import Control.Monad
import qualified Data.ByteString as BS (length)
import qualified Data.Text as T (length, unpack)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult)
import Ormolu.Printer (printModule)
import qualified Passes.RemoveUnused.Decls as Decls (reduce)
import qualified Passes.RemoveUnused.Exports as Exports (reduce)
import qualified Passes.RemoveUnused.Imports as Imports (reduce)
import qualified Passes.RemoveUnused.Pragmas as Pragmas (reduce)
import qualified Passes.Stubbing as Stubbing (reduce)
import System.Directory (copyFile, listDirectory)
import System.FilePath.Posix ((</>), splitFileName)
import System.IO.Temp (withTempDirectory)
import Util

hsreduce :: FilePath -> FilePath -> IO ()
hsreduce test filePath = do
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
                -- TODO: hsreduce should only copy test and the source file, nothing else! => tests should be self-contained
                forM_ files $ \file -> copyFile (sourceDir </> file) (tempDir </> snd (splitFileName file))
                -- TODO: write tests to check if the new module is even parsable haskell
                largestFixpoint (allPassesOnce tempTest tempFilePath) oldOrmolu
            )
        let fileName = takeWhile (/= '.') filePath
            newSize = T.length $ printModule newOrmolu
            ratio = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int
        debugPrint $ "Old size: " ++ show oldSize
        debugPrint $ "Reduced file size: " ++ show newSize
        putStrLn $ "Reduced file by " ++ show ratio ++ "%"
        TIO.writeFile (fileName ++ "_hsreduce.hs") (printModule newOrmolu)

-- TODO: add information to passes (name, # successfully applied called + on a more granular level)
allPassesOnce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
allPassesOnce test filePath oldOrmolu =
  foldM (\ormolu pass -> pass test filePath ormolu) oldOrmolu allPasses
  where
    --allPasses = [Stubbing.reduce, Pragmas.reduce, Imports.reduce, Exports.reduce, Decls.reduce]
    allPasses = [Imports.reduce, Exports.reduce, Pragmas.reduce, Decls.reduce, Stubbing.reduce]

-- | calculate the fixpoint, by always checking if the new module is
-- different from the old one
largestFixpoint :: (OPR.ParseResult -> IO OPR.ParseResult) -> OPR.ParseResult -> IO OPR.ParseResult
largestFixpoint f =
  iterateFrom
  where
    iterateFrom oldOrmolu = do
      putStrLn "\n***NEW ITERATION***"
      newOrmolu <- f oldOrmolu
      let oldLength = BS.length . TE.encodeUtf8 $ printModule oldOrmolu
          newLength = BS.length . TE.encodeUtf8 $ printModule newOrmolu
      debugPrint $ "old length: " ++ show oldLength
      debugPrint $ "new length: " ++ show newLength
      debugPrint $ "new ormolu is this many chars shorter than the old one: " ++ show (oldLength - newLength)
      if newLength < oldLength
        then do
          debugPrint "taking new ormolu"
          iterateFrom newOrmolu
        else do
          debugPrint "taking old ormolu"
          return oldOrmolu
