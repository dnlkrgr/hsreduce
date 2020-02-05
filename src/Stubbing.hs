module Stubbing (reduce) where

-- GHC API

import qualified Data.Text.IO as TIO (writeFile)
-- import HsSyn (GhcPs, HsBind, HsModule, LHsDecl, hsmodDecls, HsDecl(ValD), HsBindLR(VarBind, FunBind), HsExpr, LHsExpr, MatchGroup(..), )
import HsSyn
import Lexer (ParseResult (PFailed, POk))
import Ormolu.Parser (parseModule)
import Ormolu.Config (defaultConfig)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import Types
import Util
import Data.Maybe
import Control.Monad

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test filePath oldOrmolu = do
  let oldLoc = prParsedSource oldOrmolu
      oldModule = unLoc oldLoc
      allDecls = hsmodDecls oldModule
      variations = removeEach allDecls
  newOrmolu <- tryVariations test filePath oldOrmolu oldLoc oldModule variations
  let newDecls = hsmodDecls . unLoc . prParsedSource $ newOrmolu
  return newOrmolu

-- | take all variations and check, if there is a reduced subset that is interesting
-- | TODO: put commonly used parameters into reader
-- | TODO: instead of returning: continue to modify
tryVariations :: FilePath 
              -> FilePath 
              -> OPR.ParseResult 
              -> Located (HsModule GhcPs) 
              -> HsModule GhcPs 
              -> [(LHsDecl GhcPs, [LHsDecl GhcPs])] 
              -> IO OPR.ParseResult
tryVariations _ _ oldOrmolu oldLoc oldModule [] = return oldOrmolu
tryVariations test filePath oldOrmolu oldLoc oldModule ((removedDecl, newDecls) : rest) = do
  maybeMyUndefined <- getUndefined
  case maybeMyUndefined of
    Nothing -> tryVariations test filePath oldOrmolu oldLoc oldModule rest
    Just myUndefined -> do
      let oldDeclLoc = getLoc removedDecl
          maybeModifiedDecl = stubDecl myUndefined (unLoc removedDecl)
      case maybeModifiedDecl of
        Nothing -> tryVariations test filePath oldOrmolu oldLoc oldModule rest
        Just modifiedDecl -> do
          let modifiedDecls = L oldDeclLoc modifiedDecl : newDecls
              newOrmolu = oldOrmolu {prParsedSource = L (getLoc oldLoc) (oldModule {hsmodDecls = modifiedDecls})}
          TIO.writeFile filePath . printModule $ oldOrmolu
          interesting <- runTest test
          case interesting of
            Uninteresting -> tryVariations test filePath oldOrmolu oldLoc oldModule rest
            Interesting -> do
              putStrLn "[debug] could stub 1 function binding"
              -- TODO: recurse with newOrmolu as the oldOrmolu
              return newOrmolu

-- | TODO: we should try to stub all matches / match groups
stubDecl :: GRHSs GhcPs (LHsExpr GhcPs) -> HsDecl GhcPs -> Maybe (HsDecl GhcPs)
stubDecl myUndefined (ValD x (FunBind _ funId (MG _ (L l1 (L l2 (Match _ ctxt pats _) : otherMatches)) mgOrigin) funWrapper funTick)) = 
  Just $ ValD x (FunBind NoExt funId (MG NoExt (L l1 (L l2 (Match NoExt ctxt pats myUndefined) : otherMatches)) mgOrigin) funWrapper funTick)
stubDecl _ decl = Nothing

-- getting undefined as a haskell expression 
-- TODO: find out what kind of expression `undefined` is and replace all this with 
-- just the expression
getUndefined :: IO (Maybe (GRHSs GhcPs (LHsExpr GhcPs)))
getUndefined = do
  (_, eitherUndefined) <- parseModule defaultConfig "" "x = undefined"
  case eitherUndefined of
    Left _ -> return Nothing
    Right oldOrmolu -> do
      let myDecl = unLoc $ head $ hsmodDecls $ unLoc $ prParsedSource oldOrmolu
      case myDecl of
        (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ ctxt _ grhs)]) _) _ _)) -> return $ Just grhs
        _ -> return Nothing

removeEach :: [a] -> [(a, [a])]
removeEach [] = []
removeEach [x] = [(x, [])]
removeEach (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- removeEach xs]