module Reduce.Passes.Pat where

import GHC hiding (Pass)
import Util.Types
import Util.Util

pat2Wildcard :: Pass
pat2Wildcard = mkPass "pat2Wildcard" f
    where
        f :: WaysToChange (Pat GhcPs)
        f WildPat {} = []
        f _ = [const (WildPat NoExt)]