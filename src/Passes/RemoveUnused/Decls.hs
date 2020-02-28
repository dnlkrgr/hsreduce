{-# LANGUAGE LambdaCase #-}

module Passes.RemoveUnused.Decls where

-- basics

import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Debug.Trace
import HsSyn
import Ormolu.Parser.Pragma as OPP (Pragma (PragmaLanguage))
import Ormolu.Parser.Result as OPR (ParseResult, prExtensions, prParsedSource)
import Ormolu.Printer (printModule)
import Outputable (ppr, showSDocUnsafe)
import SrcLoc (GenLocated (..), Located (..), getLoc, noLoc, unLoc)
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Random
import System.Timeout
import Types
import Util

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
  putStrLn "\n***Performing RemoveUnused***"
  runGhc test sourceFile oldOrmolu >>=
    \case
      Nothing -> return oldOrmolu
      Just unusedBindingNames -> do
        let allDecls = hsmodDecls . unLoc . prParsedSource $ oldOrmolu
        IterState _ _ newOrmolu <- execStateT (traverse (tryAllDecls unusedBindingNames) allDecls) (IterState test sourceFile oldOrmolu)
        return newOrmolu

---- TODO: move common allDecls traversal in Util
tryAllDecls :: [UnusedBindingName] -> LHsDecl GhcPs -> StateT IterState IO ()
tryAllDecls unusedBindingNames (L declLoc (ValD _ (FunBind _ funId (MG _ (L _ matches) _) _ _))) = do
  oldState@(IterState test sourceFile oldOrmolu) <- get
  when (showSDocUnsafe (ppr $ unLoc funId) `elem` unusedBindingNames) $ do
    let parsedSource = prParsedSource oldOrmolu
        oldModule = unLoc parsedSource
        allDecls = hsmodDecls oldModule
        modifiedDecls = filter (\(L iterLoc iterDecl) -> iterLoc /= declLoc) allDecls
        newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
    writeOrmolu2FileAndTest newOrmolu
      >>= \case
        Interesting -> put (oldState {_ormolu = newOrmolu})
        Uninteresting -> return ()
tryAllDecls unusedBindingNames (L declLoc (TyClD _ (DataDecl _ tyId tyVars tyFixity (HsDataDefn _ nd ctxt cType kindSig constructors derivs)))) = do
  oldState@(IterState _ _ oldOrmolu) <- get
  -- BUG: it seems that constructors aren't deleted properly
  let newConstructors = filter (constructorIsUsed unusedBindingNames) constructors
      newDecl =
        TyClD NoExt (DataDecl NoExt tyId tyVars tyFixity (HsDataDefn NoExt nd ctxt cType kindSig newConstructors derivs))
      parsedSource = prParsedSource oldOrmolu
      oldModule = unLoc parsedSource
      allDecls = hsmodDecls oldModule
      modifiedDecls =
        map (\(L iterLoc iterDecl) -> if iterLoc == declLoc then L declLoc newDecl else L iterLoc iterDecl) allDecls
      newOrmolu = oldOrmolu {prParsedSource = L (getLoc parsedSource) (oldModule {hsmodDecls = modifiedDecls})}
  when (length newConstructors /= length constructors) $
    writeOrmolu2FileAndTest newOrmolu
      >>= \case
        Interesting -> do
          debugPrint $ "`length newConstructors == length constructors - 1`: " ++ show (length newConstructors == length constructors - 1)
          put (oldState {_ormolu = newOrmolu})
        Uninteresting -> return ()
tryAllDecls _ _ = return ()

constructorIsUsed :: [String] -> LConDecl GhcPs -> Bool
constructorIsUsed unusedBindingNames (L _ (ConDeclH98 _ (L _ rdrName) _ _ _ _ _)) = (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames
constructorIsUsed unusedBindingNames (L _ (ConDeclGADT _ names _ _ _ _ _ _)) =
  let result = all (\(L _ rdrName) -> (showSDocUnsafe . ppr $ rdrName) `notElem` unusedBindingNames) names
   in traceShow (if not result then "constructor " ++ unwords (map (showSDocUnsafe . ppr) names) ++ "is used: " ++ show result else "") result
