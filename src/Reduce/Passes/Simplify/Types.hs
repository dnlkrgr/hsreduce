module Reduce.Passes.Simplify.Types where

import BasicTypes
import GHC
import OccName (mkVarOcc)
import Util.Types
import Util.Util


type2Unit :: WaysToChange (HsType GhcPs)
type2Unit UnitTypeP = []
type2Unit _ = map const [UnitTypeP]

simplifyType :: WaysToChange (HsType GhcPs)
simplifyType UnitTypeP = []
simplifyType t@(ForallTypeP body) = handleSubList fType pType t <> map const [body]
simplifyType t@(QualTypeP body) = handleSubList fType pType t <> map const [body]
simplifyType (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = map const [t1]
-- simplifyType at@HsAppTy{}                   = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
simplifyType (HsAppTy _ (L l _) u@(L _ (HsTupleTy _ _ []))) = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
simplifyType (HsOpTy _ (L _ l) _ (L _ r)) = map const [l, r]
simplifyType (HsKindSig _ (L _ t) _) = map const [t]
simplifyType _ = []

pType :: HsType p -> [SrcSpan]
pType = \case
    (HsForAllTy _ bndrs _) -> map getLoc bndrs
    (HsQualTy _ ctxt _) -> map getLoc $ unLoc ctxt
    _ -> []

fType :: SrcSpan -> HsType p -> HsType p
fType loc = \case
    (HsForAllTy x bndrs body) -> HsForAllTy x (filter ((/= loc) . getLoc) bndrs) body
    (HsQualTy x ctxt body) -> HsQualTy x (filter ((/= loc) . getLoc) <$> ctxt) body
    x -> x


pattern ForallTypeP, QualTypeP :: HsType GhcPs -> HsType GhcPs
pattern ForallTypeP body <- HsForAllTy _ _ (L _ body)
pattern QualTypeP body <- HsQualTy _ _ (L _ body)