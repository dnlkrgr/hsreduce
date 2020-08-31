module Reduce.Passes.Simplify.Pat where

import GHC
import Util.Types

pat2Wildcard :: WaysToChange (Pat GhcPs)
pat2Wildcard WildPat {} = []
pat2Wildcard _ = [const (WildPat NoExt)]