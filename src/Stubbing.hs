module Stubbing (reduce) where

-- GHC API
import Ormolu.Parser.Result as OPR (ParseResult, prettyPrintParseResult, prParsedSource)
import Ormolu.Printer (printModule)

import HsSyn (GhcPs, HsModule, LHsDecl, hsmodDecls)
import Lexer (ParseResult (PFailed, POk))
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated(..), Located (..), unLoc, getLoc)

import qualified Data.Text.IO as TIO (writeFile)

import Types
import Util

-- for each rhs binding: check if its already undefined, if not: stub it
-- insert stubbed decls into all declarrations
-- run interestingness test on all modified declarations
-- return the first interesting one
-- TODO: parse "x = undefined" and save the bind of undefined to use it as a replacement for declarations

removeEach :: [a] -> [(a, [a])]
removeEach [] = []
removeEach [x] = [(x, [])]
removeEach (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- removeEach xs]

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test filePath oldOrmolu = do
  let oldLoc = prParsedSource oldOrmolu
      oldModule = unLoc oldLoc
      allDecls = hsmodDecls oldModule
      variations = removeEach allDecls
  putStrLn $ "[debug] old # of decls: " ++ show (length allDecls)
  newOrmolu <- loopy test filePath oldOrmolu oldLoc oldModule variations
  let newDecls = hsmodDecls . unLoc . prParsedSource $ newOrmolu
  putStrLn $ "[debug] new # of decls: " ++ show (length newDecls)
  return newOrmolu

-- | take all variations and check, if there is a reduced subset that is interesting
-- | TODO: put commonly used parameters into reader
loopy :: FilePath -> FilePath -> OPR.ParseResult -> Located (HsModule GhcPs) -> HsModule GhcPs -> [(LHsDecl GhcPs, [LHsDecl GhcPs])] -> IO OPR.ParseResult
loopy _ _ oldOrmolu oldLoc oldModule [] = return oldOrmolu
loopy test filePath oldOrmolu oldLoc oldModule ((removedDecl, newDecls) : rest) = do
  putStrLn $ "[debug] still **" ++ show (length rest) ++ "** decls to go"
  putStrLn $ "[debug] trying to remove decl: " ++ showSDocUnsafe (ppr removedDecl)
  let newOrmolu = oldOrmolu {prParsedSource = L (getLoc oldLoc) (oldModule {hsmodDecls = newDecls})}
  TIO.writeFile filePath . printModule $ oldOrmolu
  interesting <- runTest test
  case interesting of
    Uninteresting -> loopy test filePath oldOrmolu oldLoc oldModule rest
    Interesting -> do
      putStrLn "[debug] could delete 1 decl"
      return newOrmolu