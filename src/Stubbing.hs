module Stubbing (reduce) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT, local)
import qualified Data.Text.IO as TIO (writeFile)
import HsSyn
import Lexer (ParseResult (PFailed, POk))
import Ormolu.Config (defaultConfig)
import Ormolu.Parser (parseModule)
import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource)
import Ormolu.Printer (printModule)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import Types (Interesting(..))
import Util (runTest)

data StubState
  = StubState
      { _test :: FilePath,
        _sourceFile :: FilePath,
        _ormolu :: OPR.ParseResult
      }

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
tryAllDecls (declToStub : rest) = do
  oldConfig@(StubState _ _ oldOrmolu) <- ask
  let parsedSource = prParsedSource oldOrmolu
      oldModule = unLoc parsedSource
      allDecls = hsmodDecls oldModule
  maybeMyUndefined <- liftIO $ getUndefined
  case maybeMyUndefined of
    Nothing -> tryAllDecls rest
    Just myUndefined -> do
      case declToStub of
        L _ (ValD _ (FunBind _ funId (MG _ (L _ matches) _) _ _)) -> do -- declToStub is FunBind
          maybeNewOrmolu <- turnAllMatches2Undefined myUndefined declToStub matches matches
          case maybeNewOrmolu of
            Nothing -> tryAllDecls rest
            Just newOrmolu -> local (const (oldConfig { _ormolu = newOrmolu})) $ tryAllDecls rest
        _ -> tryAllDecls rest


-- | tries to turn all matches into undefined
turnAllMatches2Undefined ::
  GRHSs GhcPs (LHsExpr GhcPs) ->
  LHsDecl GhcPs ->
  [LMatch GhcPs (LHsExpr GhcPs)] ->
  [LMatch GhcPs (LHsExpr GhcPs)] ->
  ReaderT StubState IO (Maybe OPR.ParseResult)
turnAllMatches2Undefined myUndefined _ _ [] = do
  StubState _ _ oldOrmolu <- ask
  return $ Just oldOrmolu
turnAllMatches2Undefined myUndefined decl matches (L l matchToStub : rest) = do
  StubState test sourceFile oldOrmolu <- ask
  let parsedSource = prParsedSource oldOrmolu
      oldModule = unLoc parsedSource
      allDecls = hsmodDecls oldModule
      modifiedMatches = map (\(L iterLoc iterMatch) -> if iterLoc == l then L l matchToStub else L iterLoc iterMatch) matches
  case decl of
    L declLoc (ValD _ (FunBind _ _ (MG _ (L _ matches) _) _ _)) -> do
      let modifiedDecls = map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then decl else L l iterDecl) allDecls
          newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
      interesting <- writeOrmolu2FileAndTest newOrmolu
      case interesting of
        Uninteresting -> turnAllMatches2Undefined myUndefined decl matches rest
        Interesting -> local (const (StubState test sourceFile newOrmolu)) $ turnAllMatches2Undefined myUndefined decl matches rest
    _ -> return Nothing

writeOrmolu2FileAndTest :: OPR.ParseResult -> ReaderT StubState IO Interesting
writeOrmolu2FileAndTest newOrmolu = do
  StubState test sourceFile oldOrmolu <- ask
  liftIO $ TIO.writeFile sourceFile . printModule $ newOrmolu
  liftIO $ runTest test

match2Undefined :: GRHSs GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
match2Undefined myUndefined (L l2 (Match _ ctxt pats _)) = L l2 (Match NoExt ctxt pats myUndefined)

-- getting undefined as a guarded right hand side
-- it's a function binding with only one match
getUndefined :: IO (Maybe (GRHSs GhcPs (LHsExpr GhcPs)))
getUndefined = do
  (_, eitherUndefined) <- parseModule defaultConfig "" "x = undefined"
  case eitherUndefined of
    Left _ -> return Nothing
    Right oldOrmolu -> do
      let myDecl = unLoc . head . hsmodDecls . unLoc $ prParsedSource oldOrmolu
      case myDecl of
        (ValD _ (FunBind _ _ (MG _ (L _ [L _ (Match _ _ _ grhs)]) _) _ _)) -> return $ Just grhs
        _ -> return Nothing

removeEach :: [a] -> [(a, [a])]
removeEach [] = []
removeEach [x] = [(x, [])]
removeEach (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- removeEach xs]