module Stubbing where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.IO as TIO (readFile, writeFile)

import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix
import System.IO.Temp
import System.Exit
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
import SrcLoc
import Lexer (ParseResult (PFailed, POk))
import Outputable (ppr, showSDoc)

import Types

-- for each rhs binding: check if its already undefined, if not: stub it
-- insert stubbed decls into all declarrations
-- run interestingness test on all modified declarations
-- return the first interesting one
-- TODO: parse "x = undefined" and save the bind of undefined to use it as a replacement for declarations

removeEach :: [a] -> [(a, [a])]
removeEach [] = []
removeEach [x] = [(x, [])]
removeEach (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- removeEach xs]

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test filePath oldOrmolu = do
    let oldLoc = prParsedSource oldOrmolu 
        oldModule = unLoc oldLoc
        allDecls = hsmodDecls oldModule
        variations = map snd $ removeEach allDecls
    return oldOrmolu

-- | take all variations and check, if there is a reduced subset that is interesting
-- | TODO: put commonly used parameters into reader
loopy :: FilePath -> FilePath -> OPR.ParseResult -> Located (HsModule GhcPs) -> HsModule GhcPs -> [[LHsDecl GhcPs]] -> IO OPR.ParseResult
loopy _ _ oldOrmolu oldLoc oldModule [] = return oldOrmolu
loopy test filePath oldOrmolu oldLoc oldModule (newDecls:rest) = do
  let newOrmolu = oldOrmolu { prParsedSource = L (getLoc oldLoc) (oldModule { hsmodDecls = newDecls }) }
  TIO.writeFile filePath .  printModule $ oldOrmolu
  interesting <- runTest test
  case interesting of
    Uninteresting -> loopy test filePath oldOrmolu oldLoc oldModule rest
    Interesting -> return newOrmolu

-- | run the interestingness test on a timeout of 100 milliseconds
runTest :: FilePath -> IO Interesting
runTest test = do
  maybeExitcode <- timeout (100 * 1000) (readProcessWithExitCode test [] "")
  case maybeExitcode of
    Nothing -> return Uninteresting
    Just (exitCode, _, _) ->
      case exitCode of
        ExitFailure _ -> return Uninteresting
        ExitSuccess -> return Interesting