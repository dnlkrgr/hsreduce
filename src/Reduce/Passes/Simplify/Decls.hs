module Reduce.Passes.Simplify.Decls where

import GHC
import Util.Types
import Util.Util

simplifyConDecl :: WaysToChange (ConDecl GhcPs)
simplifyConDecl gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) =
    map const [gadtDecl {con_forall = L forallLoc False}, gadtDecl {con_mb_cxt = Nothing}] <> temp gadtDecl
    where
        temp = handleSubList (\loc g -> g {con_qvars = HsQTvs NoExt (filter ((/= loc) . getLoc) (hsq_explicit $ con_qvars g))}) (map getLoc . hsq_explicit . con_qvars)
simplifyConDecl d
    | isRecCon d = handleSubList f p d -- <> [const (d { con_args = recCon2Prefix $ con_args d})]
    | otherwise = []
    where
        isRecCon =
            ( \case
                  RecCon _ -> True
                  _ -> False
            )
                . con_args
        p =
            ( \case
                  RecCon (L _ flds) -> map getLoc flds
                  _ -> []
            )
                . con_args
        f loc = \case
            XConDecl _ -> XConDecl NoExt
            c ->
                c
                    { con_args = case con_args c of
                          RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
                          a -> a
                    }

rmvFunDeps :: WaysToChange (HsDecl GhcPs)
rmvFunDeps = handleSubList f p
  where
    p (TyClD _ d@ClassDecl{}) = map getLoc $ tcdFDs d 
    p _ = []

    f loc (TyClD _ d@ClassDecl{}) = TyClD NoExt $ d { tcdFDs = filter ((/= loc) . getLoc) $ tcdFDs d }
    f _ t = t