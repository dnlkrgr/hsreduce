module Reduce.Passes.Extensions.TypeFamilies where

import GHC
import Util.Types
import Util.Util

rmvEquations :: WaysToChange (HsDecl GhcPs)
rmvEquations = handleSubList f p
  where
    p (TyClD _ FamDecl{ tcdFam = d }) = case fdInfo d of
        ClosedTypeFamily (Just equations) -> map getLoc equations
        _ -> []
    p _ = []

    f loc (TyClD _ t@FamDecl{ tcdFam = d }) = case fdInfo d of
        ClosedTypeFamily (Just equations) -> 
            TyClD NoExt $ t { tcdFam = d { fdInfo = ClosedTypeFamily . Just $ filter ((/= loc) . getLoc) equations }}
        _ -> TyClD NoExt t
    f _ t = t