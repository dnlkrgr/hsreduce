{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import GHC (GhcPs, HsModule, LHsDecl, hsmodDecls, unLoc)
import Lexer (ParseResult (PFailed, POk))
import Outputable (ppr, showSDoc)
import ReduceParser
import System.Directory
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath.Posix
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import System.Timeout

type Pass = HsModule GhcPs -> HsModule GhcPs

data Interesting = Interesting | Uninteresting

main :: IO ()
main = do
  args <- getArgs
  case args of
    [test, filePath] -> do
      -- TODO: check if test is shell file and the other is a haskell file
      fileContent <- TIO.readFile filePath
      case parseStuff filePath (T.unpack fileContent) of
        PFailed _ location errMsg -> putStrLn $ showSDoc baseDynFlags errMsg
        POk pstate parsedModule -> do
          let oldModule = unLoc parsedModule
              oldDeclarations = hsmodDecls oldModule
          -- TODO: write tests to check if the new module is even parsable haskell
          withTempDirectory
            "."
            "temp"
            ( \temp -> do
                let newTest = temp </> test
                    newFilePath = temp </> filePath
                copyFile test newTest
                copyFile filePath newFilePath
                smallestFixpoint (allPassesOnce newTest newFilePath oldModule) oldModule
                return ()
            )
    _ ->
      putStrLn . unwords $
        [ "Usage:",
          "hsreduce",
          "<test-file>",
          "<hs-source-file>"
        ]

-- foldM f oldModule [pass1, pass2]
-- = f (f (f oldModule pass1) pass2) pass3 ...
allPassesOnce :: FilePath -> FilePath -> HsModule GhcPs -> IO (HsModule GhcPs)
allPassesOnce test filePath oldModule = foldM (runPass test filePath) oldModule allPasses
  where
    allPasses = [id]

-- | run a pass on the old module and return the new one if it's interesting
runPass :: FilePath -> FilePath -> HsModule GhcPs -> Pass -> IO (HsModule GhcPs)
runPass test filePath oldModule pass = do
  let newModule = pass oldModule
  writeModuleToFile filePath oldModule
  interesting <- runTest test
  case interesting of
    Uninteresting -> return oldModule
    Interesting -> return newModule

-- | run the interestingness test on a timeout of 100 milli seconds
runTest :: FilePath -> IO Interesting
runTest test = do
  -- TODO: make timeout configurable with parameter
  maybeExitcode <- timeout (100 * 1000) (readProcessWithExitCode test [] "")
  case maybeExitcode of
    Nothing -> return Uninteresting
    Just (exitCode, _, _) ->
      case exitCode of
        ExitFailure _ -> return Uninteresting
        ExitSuccess -> return Interesting

-- | calculate the smallest fixpoint, by always checking if the new module is 
-- different from the old one
smallestFixpoint :: Monad m => m (HsModule GhcPs) -> HsModule GhcPs -> m (HsModule GhcPs)
smallestFixpoint f =
  iterateFrom
  where
    iterateFrom oldModule = do
      newModule <- f
      if module2String oldModule == module2String newModule
        then return oldModule
        else iterateFrom newModule

module2String :: HsModule GhcPs -> String
module2String = showSDoc baseDynFlags . ppr

writeModuleToFile :: FilePath -> HsModule GhcPs -> IO ()
writeModuleToFile filePath =
  TIO.writeFile filePath . T.pack . showSDoc baseDynFlags . ppr