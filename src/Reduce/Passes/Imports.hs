module Reduce.Passes.Imports where

import GHC hiding (Pass)
import Util.Types (Pass, WaysToChange)
import Util.Util (handleSubList, mkPass)

unqualImport :: Pass
unqualImport = mkPass "unqualImport" f
    where
        f :: WaysToChange (ImportDecl GhcPs)
        f (ImportDecl _ srcText name pkgQual src safe _ implicit as _) =
            [const (ImportDecl NoExtField srcText name pkgQual src safe NotQualified implicit as Nothing)]
        f _ = []

rmvImports :: Pass
rmvImports = mkPass "rmvImports" f
    where
        f :: WaysToChange (HsModule GhcPs)
        f = handleSubList g (map getLoc . hsmodImports)
            where
                g loc m = m {hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m)}
