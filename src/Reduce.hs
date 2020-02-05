{-# LANGUAGE OverloadedStrings #-}

module Reduce (hsreduce) where

import Control.Monad (forM_, foldM)
import qualified Data.Text as T (length, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)

import System.Directory (listDirectory, copyFile)
import System.FilePath.Posix ((</>), splitFileName)
import System.IO.Temp (withTempDirectory)
import Ormolu.Parser (parseModule)
import Ormolu.Config (defaultConfig)
import Ormolu.Parser.Result as OPR (ParseResult)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)

import qualified Stubbing
import Types

hsreduce :: FilePath -> FilePath -> IO ()
hsreduce test filePath = do
  fileContent <- TIO.readFile filePath
  putStrLn $ "[debug] Original file size: " ++ show (T.length fileContent)
  (_, eitherParsedResult) <- parseModule defaultConfig filePath (T.unpack fileContent) 
  case eitherParsedResult of
    Left _ -> return ()
    Right oldOrmolu -> do
      let sourceDir =  fst $ splitFileName filePath
      putStrLn $ "[debug] source dir: " ++ sourceDir
      files <- listDirectory sourceDir
      putStrLn $ "[debug] files: " ++ show files
      newOrmolu <- withTempDirectory "." "temp" (\tempDir -> do
        let newTest = tempDir </> snd (splitFileName test)
            newFilePath = tempDir </> snd (splitFileName filePath)
        forM_ files $ \file -> do
          putStrLn $ "[debug] copying file from: " ++ sourceDir </> file
          putStrLn $ "[debug] copying file to: " ++ tempDir </> snd (splitFileName file)
          copyFile (sourceDir </> file) (tempDir </> snd (splitFileName file))
        -- TODO: write tests to check if the new module is even parsable haskell
        smallestFixpoint (allPassesOnce newTest newFilePath oldOrmolu) oldOrmolu)
      let fileName = takeWhile (/= '.') filePath
      putStrLn $ "[debug] Reduced file size: " ++ show (T.length (printModule newOrmolu))
      TIO.writeFile (fileName ++ ".rhs") (printModule newOrmolu)

allPassesOnce :: FilePath -> FilePath -> OPR.ParseResult-> IO OPR.ParseResult
allPassesOnce test filePath oldOrmolu = foldM (\ormolu pass -> pass test filePath ormolu) oldOrmolu allPasses
  where
    allPasses = [ Stubbing.reduce ]

-- | calculate the smallest fixpoint, by always checking if the new module is 
-- different from the old one
smallestFixpoint :: IO OPR.ParseResult -> OPR.ParseResult-> IO OPR.ParseResult
smallestFixpoint f =
  iterateFrom
  where
    iterateFrom oldOrmolu = do
      newOrmolu <- f
      putStrLn $ "[debug] new ormolu is this many chars shorter than old:" ++ show (T.length (printModule oldOrmolu) - T.length (printModule newOrmolu))
      if T.length (printModule oldOrmolu) <= T.length (printModule newOrmolu)
        then do
          putStrLn "[debug] taking old ormolu"
          return oldOrmolu
        else do
          putStrLn "[debug] taking new ormolu:"
          iterateFrom newOrmolu