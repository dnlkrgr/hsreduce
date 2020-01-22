{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)

import System.Directory
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath.Posix
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import System.Timeout
import Ormolu.Parser
import Ormolu.Config
import Ormolu.Parser.Result
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

type Pass = HsModule GhcPs -> HsModule GhcPs

data Interesting = Interesting | Uninteresting

main :: IO ()
main = do
  myArgs <- getArgs
  if length myArgs /= 2 
    then printUsage
    else do
      let [test, filePath] = myArgs
      if takeExtension test == ".sh" && takeExtension filePath == ".hs" then
        do
        fileContent <- TIO.readFile filePath
        (_, eitherParsedResult) <- parseModule defaultConfig filePath (T.unpack fileContent) 
        case eitherParsedResult of
          Left _ -> return ()
          Right oldOrmolu -> do
            let oldModule = unLoc . prParsedSource $ oldOrmolu 
                oldDeclarations = hsmodDecls oldModule
            -- TODO: write tests to check if the new module is even parsable haskell
            newModule <- withTempDirectory
              "."
              "temp"
              ( \temp -> do
                  let newTest = temp </> (snd $ splitFileName test)
                      newFilePath = temp </> (snd $ splitFileName filePath)
                  copyFile test newTest
                  copyFile filePath newFilePath
                  newModule <- smallestFixpoint (allPassesOnce newTest newFilePath oldModule) oldModule
                  return newModule
              )
            let newOrmolu = oldOrmolu { prParsedSource = (noLoc newModule) }
                fileName = takeWhile (/= '.') filePath
            TIO.writeFile (fileName ++ ".rhs") (printModule newOrmolu)
      else printUsage


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

-- | run the interestingness test on a timeout of 100 milliseconds
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

printUsage :: IO ()
printUsage = 
    putStrLn . unwords $
      [ "Usage:",
        "hsreduce",
        "<test-file>",
        "<hs-source-file>"
      ]

-- dummy data needed by the GHC parser
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform = platform,
    sPlatformConstants = platformConstants,
    sProjectVersion = cProjectVersion,
    sProgramName = "ghc",
    sOpt_P_fingerprint = fingerprint0
  }
  where
    platform = Platform
      { platformWordSize = 8,
        platformOS = OSUnknown,
        platformUnregisterised = True
      }
    platformConstants = PlatformConstants
      { pc_DYNAMIC_BY_DEFAULT = False,
        pc_WORD_SIZE = 8
      }

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
