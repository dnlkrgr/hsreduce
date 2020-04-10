module Reduce.Passes.RemoveUnused.Decls (reduce) where

import Debug.Trace
import Data.Foldable
import Control.Monad.State.Strict
import qualified Data.Text as T
import Util.Types
import Util.Util
import Control.Monad.Reader
import "ghc" GHC hiding (getName)
import "ghc" Outputable
import "ghc" BasicTypes
import "ghc" TcEvidence
import "ghc" CoreSyn

-- | run ghc with -Wunused-binds -ddump-json and delete decls that are mentioned there
reduce :: R ()
reduce = do
  oldModule <- _parsed <$> get
  sourceFile <- asks _sourceFile
  liftIO $ putStrLn "\n***Removing unused declarations***"
  debugPrint $ "Size of old ormolu: " ++ (show . T.length . T.pack . lshow $ oldModule)
  let allDecls = hsmodDecls . unLoc $ oldModule
  liftIO (getGhcOutput sourceFile Binds)
    >>= \case
      Nothing ->
        traverse_ (\dcl -> traceShow ("rmvDecl" :: String) (rmvDecl dcl)
                           >>= flip unless (traceShow ("rmvBinds" :: String) $ rmvBinds Nothing dcl))
                  allDecls
      Just l -> do
        let unusedStrings = map fst l
        traverse_ (\dcl -> traceShow ("rmvUnusedDecl" :: String) (rmvUnusedDecl unusedStrings dcl)
                           >>= flip unless (traceShow ("rmvDecl" :: String) (rmvDecl dcl)
                           >>= flip unless (traceShow ("rmvBinds" :: String) $ rmvBinds (Just unusedStrings) dcl)))
                  allDecls

-- | rmv unused decls whole
-- for FunBinds: also delete the type signature
rmvUnusedDecl :: [T.Text] -> LHsDecl GhcPs -> R Bool
rmvUnusedDecl unusedStrings (L declLoc (SimplFunP funId))
  | (T.pack $ lshow funId) `elem` unusedStrings =
          changeDecls
            (filter (\(L iterLoc iterDecl) ->
              case iterDecl of
                SimplSigP tyFunId ->
                  ((`notElem` unusedStrings) . T.pack . lshow) tyFunId
                _ -> iterLoc /= declLoc))
          <$> get
    >>= testAndUpdateStateFlex False True
  | otherwise = return False
rmvUnusedDecl unusedStrings lDecl@(L _ decl) =
  if maybe False ((`elem` unusedStrings) . T.pack . oshow) . getName $ decl
    then rmvDecl lDecl
    else return False

-- | rmv decls whole
rmvDecl :: LHsDecl GhcPs -> R Bool
rmvDecl (L declLoc _) =
  changeDecls (filter (\(L iterLoc _) -> iterLoc /= declLoc)) <$> get
  >>= testAndUpdateStateFlex False True

-- | rmv unused parts of decls
rmvBinds :: Maybe [T.Text] -> LHsDecl GhcPs -> R ()
rmvBinds (Just bns) (L l dcl@(TypeSigDeclP ids swt)) = do
  liftIO $ putStrLn $ "--- Just ub, TypeSigDeclP" ++ oshow dcl
  let newFunIds = filter ((`notElem` bns) . T.pack . lshow) ids
  void $ tryNewValue (L l dcl) (TypeSigDeclX newFunIds swt)
rmvBinds _ oldDecl@(L _ (FunDeclP fid loc mtchs mo fw ft)) = do
  liftIO $ putStrLn $ "--- fundeclp" ++ oshow fid
  let nMtchs =
        filter (\(L _ (Match _ _ _ grhss)) ->
          showSDocUnsafe (pprGRHSs LambdaExpr grhss) /= "-> undefined") mtchs
  void $ tryNewValue oldDecl (FunDeclP fid loc nMtchs mo fw ft)
rmvBinds mBns ldcl@(L _ (TyClD _ _)) = do
  liftIO $ putStrLn $ "--- tycld" ++ lshow ldcl
  void $ rmvCons mBns ldcl
rmvBinds Nothing ldcl@(L _ TypeSigDeclP{}) =
  void $ reduceListOfSubelements typeSig2Ids transformTypeSig ldcl
  where
    typeSig2Ids =
      \case
        TypeSigDeclP ids _ -> ids
        _ -> []
    transformTypeSig e =
      \case
        TypeSigDeclP ids swt ->
          let newIds = filter (/= e) ids -- L l . flip TypeSigDeclX swt
          in TypeSigDeclX newIds swt
        d -> d
rmvBinds _ _ = return ()


-- TODO: apply to more TyClD types
rmvCons :: Maybe [T.Text] -> LHsDecl GhcPs -> R (LHsDecl GhcPs)
rmvCons _ = reduceListOfSubelements decl2ConsStrings delCons
  where
    delCons loc =
      \case
        (TyClD _ oDD@(DataDecl _ _ _ _ oldDataDefn)) ->
          let newCons = dd_cons oldDataDefn
          in TyClD NoExt oDD { tcdDataDefn = oldDataDefn { dd_cons = filter ((/= loc) . getLoc) newCons}}
        d -> d
    decl2ConsStrings =
      \case
        (TyClD _ (DataDecl _ _ _ _ oldDataDefn)) -> map getLoc $ dd_cons oldDataDefn
        _ -> []


-- TODO: what other decls make sense here?
getName :: HsDecl GhcPs -> Maybe (IdP GhcPs)
getName (TyClD _ d)      = Just . tcdName $ d
getName (SimplFunP funId) = Just . unLoc $ funId
getName (SimplSigP funId) = Just . unLoc $ funId
getName _ = Nothing

-- getName
pattern SimplSigP, SimplFunP :: Located (IdP GhcPs) -> HsDecl GhcPs
pattern SimplSigP lFunId <- SigD _ (TypeSig _ [lFunId] _)
pattern SimplFunP lFunId <- ValD _ (FunBind _ lFunId _ _ _)

-- simplifyDecl
pattern FunDeclP :: Located (IdP GhcPs) -> SrcSpan -> [LMatch GhcPs (LHsExpr GhcPs)] -> Origin -> HsWrapper -> [Tickish Id] -> HsDecl GhcPs
pattern FunDeclP lFunId matchesLoc funMatches mgOrigin funWrapper funTick =
  ValD NoExt (FunBind NoExt lFunId (MG NoExt (L matchesLoc funMatches) mgOrigin) funWrapper funTick)

pattern TypeSigDeclP :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclP funIds sigWctype <- (SigD _ (TypeSig _ funIds sigWctype))

pattern TypeSigDeclX :: [Located (IdP GhcPs)] -> LHsSigWcType GhcPs -> HsDecl GhcPs
pattern TypeSigDeclX funIds sigWctype = SigD NoExt (TypeSig NoExt funIds sigWctype)
