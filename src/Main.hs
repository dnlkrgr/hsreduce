{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad (when)
import Control.Monad.State.Strict (MonadIO, StateT, execStateT, forM_, get, liftIO, modify', unless, State, put)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import GHC (GhcPs, HsModule, LHsDecl, hsmodDecls, unLoc)
import Lexer (ParseResult (PFailed, POk))
import Outputable (ppr, showSDoc)
import ReduceParser
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [test, haskellFilePath] -> do
      -- TODO: check if test is shell file and the other is a haskell file
      fileContent <- TIO.readFile haskellFilePath
      -- just parse once
      case parseStuff haskellFilePath (T.unpack fileContent) of
        PFailed _ location errMsg -> liftIO . putStrLn $ showSDoc baseDynFlags errMsg
        POk pstate parsedModule -> do
          let oldModule = unLoc parsedModule
              oldDeclarations = hsmodDecls oldModule
          -- TODO: write tests to check if the new module is even parsable haskell
          writeModuleToFile haskellFilePath oldModule
    _ ->
      putStrLn $
        "Usage: "
          ++ "hsreduce "
          ++ "<location-of-test-file> "
          ++ "<location-of-hs-source-file>"


writeModuleToFile :: MonadIO m => FilePath -> HsModule GhcPs -> m ()
writeModuleToFile haskellFilePath =
  liftIO . TIO.writeFile haskellFilePath . T.pack . showSDoc baseDynFlags . ppr