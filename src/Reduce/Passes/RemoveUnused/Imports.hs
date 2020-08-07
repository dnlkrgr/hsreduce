module Passes.RemoveUnused.Imports (reduce) where

import Util.Types
import Util.Util
import GHC

reduce :: R ()
reduce = do
    printInfo "Removing Imports"
    isTestStillFresh "Imports"
    runPass "rmvImports" rmvImports
    runPass "unqualImport" unqualImport

unqualImport :: WaysToChange (ImportDecl GhcPs)
unqualImport (ImportDecl _ srcText name pkgQual src safe _ implicit as _) 
    = [const (ImportDecl NoExt srcText name pkgQual src safe False implicit as Nothing)]
unqualImport _ = []

rmvImports :: WaysToChange (HsModule GhcPs)
rmvImports = handleSubList f (map getLoc . hsmodImports) 
  where f loc m = m { hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m) }
