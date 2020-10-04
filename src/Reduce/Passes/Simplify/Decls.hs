module Reduce.Passes.Simplify.Decls where

import GHC hiding (Pass)
import Util.Types
import Util.Util

simplifyConDecl :: Pass
simplifyConDecl = mkPass "simplifyConDecl" f
    where
        f :: WaysToChange (ConDecl GhcPs)
        f gadtDecl@(ConDeclGADT _ _ (L forallLoc _) _ _ _ _ _) =
            map const [gadtDecl {con_forall = L forallLoc False}, gadtDecl {con_mb_cxt = Nothing}] <> temp gadtDecl
            where
                temp = handleSubList (\loc g -> g {con_qvars = HsQTvs NoExt (filter ((/= loc) . getLoc) (hsq_explicit $ con_qvars g))}) (map getLoc . hsq_explicit . con_qvars)
        f d
            | isRecCon d = handleSubList g p d -- <> [const (d { con_args = recCon2Prefix $ con_args d})]
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
                g loc = \case
                    XConDecl _ -> XConDecl NoExt
                    c ->
                        c
                            { con_args = case con_args c of
                                  RecCon (L l flds) -> RecCon . L l $ filter ((/= loc) . getLoc) flds
                                  a -> a
                            }