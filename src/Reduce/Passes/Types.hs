module Reduce.Passes.Types where

import BasicTypes (PromotionFlag (NotPromoted))
import GHC
    ( GenLocated (L),
      GhcPs,
      HsTupleSort (HsBoxedTuple),
      HsType
          ( HsAppTy,
            HsForAllTy,
            HsKindSig,
            HsOpTy,
            HsQualTy,
            HsTupleTy,
            HsTyVar,
            HsWildCardTy
          ),
      NoExt (NoExt),
      RdrName (Unqual),
      SrcSpan,
      getLoc,
      noLoc,
      unLoc,
    )
import OccName (mkVarOcc)
import Util.Types (Pass, WaysToChange)
import Util.Util (handleSubList, mkPass)

type2Unit :: Pass
type2Unit = mkPass "type2Unit" f
    where
        f :: WaysToChange (HsType GhcPs)
        f UnitTypeP = []
        f (HsWildCardTy _) = []
        f _ = map const [UnitTypeP]

type2WildCard :: Pass
type2WildCard = mkPass "type2WildCard" f
    where
        f :: WaysToChange (HsType GhcPs)
        f (HsWildCardTy _) = []
        f _ = map const [HsWildCardTy NoExt]

simplifyType :: Pass
simplifyType = mkPass "simplifyType" f
    where
        f :: WaysToChange (HsType GhcPs)
        f UnitTypeP = []
        f t@(ForallTypeP body) = handleSubList fType pType t <> map const [body]
        f t@(QualTypeP body) = handleSubList fType pType t <> map const [body]
        f (HsAppTy _ (L _ (HsAppTy _ _ (L _ t1))) (L _ (HsTupleTy _ _ []))) = map const [t1]
        -- f at@HsAppTy{}                   = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
        f (HsAppTy _ (L l _) u@(L _ (HsTupleTy _ _ []))) = map const [HsAppTy NoExt (L l $ HsTyVar NoExt NotPromoted (noLoc $ Unqual $ mkVarOcc "Maybe")) u]
        f (HsOpTy _ (L _ l) _ (L _ r)) = map const [l, r]
        f (HsKindSig _ (L _ t) _) = map const [t]
        f _ = []
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

pattern UnitTypeP :: HsType GhcPs
pattern UnitTypeP = HsTupleTy NoExt HsBoxedTuple []