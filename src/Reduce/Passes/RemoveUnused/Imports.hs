module Passes.RemoveUnused.Imports (reduce) where

import GHC
import Types
import Util

reduce :: R ()
reduce = do
    runPass "rmvImports" rmvImports
    runPass "unqualImport" unqualImport

unqualImport :: WaysToChange (ImportDecl GhcPs)
unqualImport (ImportDecl _ srcText name pkgQual src safe _ implicit as _) =
    [const (ImportDecl NoExt srcText name pkgQual src safe False implicit as Nothing)]
unqualImport _ = []

rmvImports :: WaysToChange (HsModule GhcPs)
rmvImports = handleSubList f (map getLoc . hsmodImports)
    where
        f loc m = m {hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m)}
