module Reduce.Passes.RemoveUnused.Imports (reduce) where

import Util.Types
import Util.Util
import GHC

reduce :: R ()
reduce = do
    printInfo "Removing Imports"
    isTestStillFresh "Imports"
    runPass "rmvImports" rmvImports

rmvImports :: WaysToChange (HsModule GhcPs)
rmvImports = handleSubList f (map getLoc . hsmodImports) 
  where f loc m = m { hsmodImports = filter ((/= loc) . getLoc) (hsmodImports m) }
