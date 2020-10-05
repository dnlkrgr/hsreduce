module Reduce.Passes.Pat where

import GHC (GhcPs, NoExt (NoExt), Pat (WildPat))
import Util.Types (Pass, WaysToChange)
import Util.Util (mkPass)

pat2Wildcard :: Pass
pat2Wildcard = mkPass "pat2Wildcard" f
    where
        f :: WaysToChange (Pat GhcPs)
        f WildPat {} = []
        f _ = [const (WildPat NoExt)]