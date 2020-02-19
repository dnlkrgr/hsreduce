{-# LANGUAGE LambdaCase #-}

module Passes.Stubbing
  ( reduce,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.Text.IO as TIO (writeFile)
import HsSyn
import Lexer (ParseResult (PFailed, POk))
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import Types
import Util

-- | run a pass on the old module and return the new one if it's interesting
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  let allDecls = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
  runReaderT (tryAllDecls allDecls) (StubState test sourceFile oldOrmolu)

-- | take all variations and check, if there is a reduced subset that is interesting
tryAllDecls :: [LHsDecl GhcPs] -> ReaderT StubState IO OPR.ParseResult
tryAllDecls [] = do
  StubState _ _ oldOrmolu <- ask
  return oldOrmolu
tryAllDecls (declToStub@(L _ (ValD _ (FunBind _ funId (MG _ (L _ matches) _) _ _))) : rest) = do
  oldConfig@(StubState _ _ oldOrmolu) <- ask
  liftIO getUndefined
    >>= \case
      Nothing -> tryAllDecls rest
      Just myUndefined -> turnAllMatches2Undefined myUndefined declToStub matches
tryAllDecls (_ : rest) = tryAllDecls rest

-- | tries to turn all matches into undefined
turnAllMatches2Undefined ::
  GRHSs GhcPs (LHsExpr GhcPs) ->
  LHsDecl GhcPs ->
  [LMatch GhcPs (LHsExpr GhcPs)] ->
  ReaderT StubState IO OPR.ParseResult
turnAllMatches2Undefined myUndefined _ [] = asks _ormolu
turnAllMatches2Undefined myUndefined decl@(L declLoc (ValD _ (FunBind _ funId (MG _ (L mgLoc matches) mgOrigin) funWrapper funTick))) (L l matchToStub : rest) = do
  StubState test sourceFile oldOrmolu <- ask
  let parsedSource = prParsedSource oldOrmolu
      oldModule = unLoc parsedSource
      allDecls = hsmodDecls oldModule
      modifiedMatches =
        map (\(L iterLoc iterMatch) -> if iterLoc == l then match2Undefined myUndefined (L l matchToStub) else L iterLoc iterMatch) matches
  -- debugPrint "current match: "
  -- debugPrint $ showSDocUnsafe . ppr $ matchToStub
  -- debugPrint "all decls: "
  -- debug (mapM_ (liftIO . putStrLn . showSDocUnsafe . ppr)) allDecls
  let modifiedDecl = L declLoc (ValD NoExt (FunBind NoExt funId (MG NoExt (L mgLoc modifiedMatches) mgOrigin) funWrapper funTick))
      modifiedDecls = map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then modifiedDecl else L l iterDecl) allDecls
      newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
  -- debugPrint "modified decls: "
  -- debug (mapM_ (liftIO . putStrLn . showSDocUnsafe . ppr)) modifiedDecls
  writeOrmolu2FileAndTest newOrmolu >>=
    \case
      Uninteresting -> turnAllMatches2Undefined myUndefined decl rest
      Interesting ->
        local (const (StubState test sourceFile newOrmolu)) $
          turnAllMatches2Undefined myUndefined modifiedDecl rest

match2Undefined :: GRHSs GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
match2Undefined myUndefined (L l2 (Match _ ctxt pats _)) = L l2 (Match NoExt ctxt pats myUndefined)

-- getting undefined as a guarded right hand side
-- it's a function binding with only one match
getUndefined :: IO (Maybe (GRHSs GhcPs (LHsExpr GhcPs)))
getUndefined =
  snd <$> parseModule defaultConfig "" "x = undefined" >>=
    \case 
      Left _ -> return Nothing
      Right oldOrmolu ->
        case unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu of
          (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _)) -> return $ Just grhs
          _ -> return Nothing
