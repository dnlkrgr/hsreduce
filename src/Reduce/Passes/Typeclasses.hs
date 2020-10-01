module Reduce.Passes.Typeclasses where

import Bag
import Data.Generics.Uniplate.Data
import GHC hiding (Pass)
import Util.Types

deleteTyClMethods :: Pass
deleteTyClMethods = AST "deleteTyClMethods" $ \ast ->
    map
        ( \funId oldAst -> 
            transformBi (deleteMethodFromClass funId)
            . transformBi (deleteMethodFromInstance funId)
            $ oldAst
            -- TODO: only delete methods for appropriate class name!
            -- maybe also necessary: delete inline
        )
        [ unLoc $ fun_id f
          | d@ClassDecl {} :: TyClDecl GhcPs <- universeBi ast,
            f@FunBind {} <- map unLoc . bagToList $ tcdMeths d
        ]

deleteMethodFromInstance :: RdrName -> ClsInstDecl GhcPs -> ClsInstDecl GhcPs
deleteMethodFromInstance funId d@ClsInstDecl{} = d { cid_binds = listToBag . filter (f funId) . bagToList $ cid_binds d }
  where
      f funName (L _ fb@FunBind{}) = unLoc (fun_id fb) == funName
      f _ _ = True
deleteMethodFromInstance _ d = d 

deleteMethodFromClass :: RdrName -> TyClDecl GhcPs -> TyClDecl GhcPs
deleteMethodFromClass funId d@ClassDecl{} = d { tcdMeths = listToBag . filter (f funId) . bagToList $ tcdMeths d}
  where
      f funName (L _ fb@FunBind{}) = unLoc (fun_id fb) == funName
      f _ _ = True
deleteMethodFromClass _ d = d 