{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T (length, pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)

import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import System.Timeout
import Ormolu.Parser
import Ormolu.Config
import Ormolu.Parser.Result as OPR
import Ormolu.Printer

-- GHC API
import DynFlags
import Platform
import Fingerprint
import Config
import HsSyn (GhcPs, HsModule, LHsDecl, hsmodDecls)
import SrcLoc (noLoc, unLoc)
import Lexer (ParseResult (PFailed, POk))
import Outputable (ppr, showSDoc)

import Stubbing
import Types

-- TODO: add tests that automatically run the test_cases and check if they're being reduced
main :: IO ()
main = do
  myArgs <- getArgs
  if length myArgs /= 2 
    then printUsage
    else do
      let [test, filePath] = myArgs
      if takeExtension test == ".sh" && takeExtension filePath == ".hs" then
        -- TODO: factor this out into function reduce that is being called with the args
        do
        fileContent <- TIO.readFile filePath
        putStrLn "Original file size:"
        print $ T.length fileContent
        (_, eitherParsedResult) <- parseModule defaultConfig filePath (T.unpack fileContent) 
        case eitherParsedResult of
          Left _ -> return ()
          Right oldOrmolu -> do
            -- TODO: write tests to check if the new module is even parsable haskell
            newOrmolu <- withTempDirectory
              "."
              "temp"
              ( \temp -> do
                  let newTest = temp </> snd (splitFileName test)
                      newFilePath = temp </> snd (splitFileName filePath)
                  copyFile test newTest
                  copyFile filePath newFilePath
                  smallestFixpoint (allPassesOnce newTest newFilePath oldOrmolu) oldOrmolu
              )
            let fileName = takeWhile (/= '.') filePath
            putStrLn "Reduced file size:"
            print $ T.length (printModule newOrmolu)
            TIO.writeFile (fileName ++ ".rhs") (printModule newOrmolu)
      else printUsage


allPassesOnce :: FilePath -> FilePath -> OPR.ParseResult-> IO OPR.ParseResult
allPassesOnce test filePath oldOrmolu = foldM (\ormolu pass -> pass test filePath ormolu) oldOrmolu allPasses
  where
    allPasses = [ Stubbing.reduce ]

-- | calculate the smallest fixpoint, by always checking if the new module is 
-- different from the old one
smallestFixpoint :: Monad m => m OPR.ParseResult -> OPR.ParseResult-> m OPR.ParseResult
smallestFixpoint f =
  iterateFrom
  where
    iterateFrom oldOrmolu = do
      newOrmolu <- f
      -- TODO: can also check if there were any interesting testcases in the reducers, if none -> stop
      if length (prettyPrintParseResult oldOrmolu) == length (prettyPrintParseResult newOrmolu)
        then return oldOrmolu
        else iterateFrom newOrmolu

printUsage :: IO ()
printUsage = 
    putStrLn . unwords $
      [ "Usage:",
        "hsreduce",
        "<test-file>",
        "<hs-source-file>"
      ]