{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Reduce (hsreduce) where

import Control.Monad (forM_, foldM)
import qualified Data.Text as T (length, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text.Encoding as TE (encodeUtf8)

import System.Directory (listDirectory, copyFile)
import System.FilePath.Posix ((</>), splitFileName)
import System.IO.Temp (withTempDirectory)
import Ormolu.Parser (parseModule)
import Ormolu.Config (defaultConfig)
import Ormolu.Parser.Result as OPR (ParseResult)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)

import Types (Interesting(..))
import Util (debug, debugPrint)
import qualified Data.ByteString as BS (length)

import qualified Passes.Stubbing as Stubbing (reduce)
import qualified Passes.RemoveUnused as RemoveUnused (reduce)

hsreduce :: FilePath -> FilePath -> IO ()
hsreduce test filePath = do
  fileContent <- TIO.readFile filePath
  let oldSize = T.length fileContent
  debugPrint $ "Original file size: " ++ show oldSize
  snd <$> parseModule defaultConfig filePath (T.unpack fileContent) >>=
    \case
      Left _ -> return ()
      Right oldOrmolu -> do
        let sourceDir =  fst $ splitFileName filePath
        debugPrint $ "source dir: " ++ sourceDir
        files <- listDirectory sourceDir
        debugPrint $ "files: " ++ show files
        newOrmolu <- withTempDirectory "." "temp" (\tempDir -> do
          let tempTest = tempDir </> snd (splitFileName test)
              tempFilePath = tempDir </> snd (splitFileName filePath)
          -- TODO: hsreduce should only copy test and the source file, nothing else! => tests should be self-contained
          forM_ files $ \file -> do
            debugPrint $ "copying file from: " ++ sourceDir </> file
            debugPrint $ "copying file to: " ++ tempDir </> snd (splitFileName file)
            copyFile (sourceDir </> file) (tempDir </> snd (splitFileName file))
          -- TODO: write tests to check if the new module is even parsable haskell
          largestFixpoint (allPassesOnce tempTest tempFilePath oldOrmolu) oldOrmolu)
        let fileName = takeWhile (/= '.') filePath
            newSize = T.length $ printModule newOrmolu
        debugPrint $ "Reduced file size: " ++ show newSize
        putStrLn $ "Reduced file by " ++ (show . round $ ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100)) ++ "%"
        TIO.writeFile (fileName ++ ".rhs") (printModule newOrmolu)

allPassesOnce :: FilePath -> FilePath -> OPR.ParseResult-> IO OPR.ParseResult
allPassesOnce test filePath oldOrmolu = foldM (\ormolu pass -> pass test filePath ormolu) oldOrmolu allPasses
  where
    allPasses = [ Stubbing.reduce ]
    -- allPasses = [ RemoveUnused.reduce ]
    -- allPasses = [ Stubbing.reduce, RemoveUnused.reduce ]

-- | calculate the fixpoint, by always checking if the new module is 
-- different from the old one
largestFixpoint :: IO OPR.ParseResult -> OPR.ParseResult-> IO OPR.ParseResult
largestFixpoint f =
  iterateFrom
  where
    iterateFrom oldOrmolu = do
      newOrmolu <- f
      let oldLength = BS.length . TE.encodeUtf8 $ printModule oldOrmolu
          newLength = BS.length . TE.encodeUtf8 $ printModule newOrmolu
      debugPrint $ "new ormolu is this many chars shorter than the old one:" ++ show (oldLength - newLength)
      if oldLength <= newLength
        then do
          debugPrint "taking old ormolu"
          return oldOrmolu
        else do
          debugPrint "taking new ormolu"
          iterateFrom newOrmolu