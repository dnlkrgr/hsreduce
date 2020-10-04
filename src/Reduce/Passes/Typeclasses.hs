module Reduce.Passes.Typeclasses (rmvTyClMethods) where

import Bag
import Data.Generics.Uniplate.Data
import GHC hiding (Pass)
import Util.Types

rmvTyClMethods :: Pass
rmvTyClMethods = AST "rmvTyClMethods" $ \ast ->
    map
        ( \sigId oldAst -> 
            transformBi (rmvNameFromSigs sigId)
            . transformBi (rmvNameFromFunbinds sigId)
            $ oldAst
            -- maybe also necessary: delete inline
            -- maybe also necessary: delete default methods
            -- maybe also necessary: delete associated types
        )
        [ sigId
          | d@ClassDecl {} :: TyClDecl GhcPs <- universeBi ast,
            ClassOpSig _ _ sigIds _ :: Sig GhcPs <- map unLoc $ tcdSigs d,
            sigId <- map unLoc sigIds
        ]

rmvNameFromFunbinds :: RdrName -> LHsBinds GhcPs -> LHsBinds GhcPs
rmvNameFromFunbinds sigId = listToBag . filter (f sigId) . bagToList
  where
      f sigName (L _ fb@FunBind{}) = unLoc (fun_id fb) /= sigName
      f _ _ = True

-- might be dangerous, because it's such a general function
rmvNameFromSigs :: RdrName -> [LSig GhcPs] -> [LSig GhcPs]
rmvNameFromSigs sigId = filter (f sigId) . map (m sigId)
  where
      f _ (L _ (ClassOpSig _ _ [] _)) = False
      f sigName (L _ (InlineSig _ otherId _)) = sigName /= unLoc otherId
      f _ _ = True

      m sigName (L l (ClassOpSig _ b methodIds t)) = L l $ ClassOpSig NoExt b (filter ((/= sigName) . unLoc) methodIds) t
      m _ s = s