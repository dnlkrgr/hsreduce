module Reduce.Passes.Typeclasses where

import Bag (bagToList, listToBag)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import qualified Data.List.NonEmpty as NE
import GHC hiding (Pass)
import Reduce.Passes.Parameters
import Util.Types (Pass (AST), WaysToChange)
import Util.Util

rmvUnusedParams :: Pass
rmvUnusedParams =
    reduce
        "Typeclasses.rmvUnusedParams"
        ( \ast ->
              [ (funId, countPatsInType t)
                | ClassOpSig _ _ [(unLoc -> funId)] (HsIB _ (unLoc -> t)) :: Sig GhcPs <- universeBi ast
              ]
        )
        getPatsLength
        ( \funId _ newLenArgs _ newI ->
              transformBi (rmvArgsFromExpr funId (NE.head newLenArgs) newI)
                  . transformBi (handleSigs funId newI)
                  . transformBi (handleFunBinds funId newI)
        )

countPatsInType :: HsType GhcPs -> Int
countPatsInType (HsFunTy _ _ (unLoc -> t)) = 1 + countPatsInType t
countPatsInType _ = 0

getPatsLength :: RdrName -> ParsedSource -> [Int]
getPatsLength name ast =
    [ countPatsInType t
      | ClassOpSig _ _ [(unLoc -> funId)] (HsIB _ (unLoc -> t)) :: Sig GhcPs <- universeBi ast,
        funId == name
    ]

handleMultiParams :: Pass
handleMultiParams =
    reduce
        "handleMultiParams"
        (\ast -> [(unLoc tcdLName, length . hsq_explicit $ tcdTyVars) | ClassDecl {..} :: TyClDecl GhcPs <- universeBi ast])
        getArgsLength
        ( \classId _ temp _ newI ->
              transformBi (rmvArgsFromClass classId newI)
                  . transformBi (rmvArgsFromInstance classId (NE.head temp) newI)
        )

rmvArgsFromClass :: RdrName -> Int -> TyClDecl GhcPs -> TyClDecl GhcPs
rmvArgsFromClass className i d@ClassDecl {..}
    | unLoc tcdLName == className = d {tcdTyVars = tcdTyVars {hsq_explicit = deleteAt i (hsq_explicit $ tcdTyVars)}}
    | otherwise = d
rmvArgsFromClass _ _ d = d

rmvArgsFromInstance :: RdrName -> Int -> Int -> ClsInstDecl GhcPs -> ClsInstDecl GhcPs
rmvArgsFromInstance className n i d@ClsInstDecl {..} =
    case cid_poly_ty of
        HsIB _ body -> d {cid_poly_ty = HsIB NoExtField $ rmvArgsFromType className n i <$> body}
        _ -> d
rmvArgsFromInstance _ _ _ d = d

-- n: total number of patterns
-- i: index of pattern we wish to remove
rmvArgsFromType :: RdrName -> Int -> Int -> HsType GhcPs -> HsType GhcPs
rmvArgsFromType conId n i e@(HsAppTy x la@(L _ a) b)
    | typeContainsTyCon conId e,
      typeFitsNumberOfParams n e,
      n == i =
        a
    | typeContainsTyCon conId e,
      typeFitsNumberOfParams n e =
        HsAppTy x (rmvArgsFromType conId (n - 1) i <$> la) b
    | otherwise = e
rmvArgsFromType _ _ _ e = e

typeFitsNumberOfParams :: Int -> HsType GhcPs -> Bool
typeFitsNumberOfParams n (HsAppTy _ l _) = typeFitsNumberOfParams (n -1) (unLoc l)
typeFitsNumberOfParams 0 _ = True
typeFitsNumberOfParams _ _ = False

typeContainsTyCon :: RdrName -> HsType GhcPs -> Bool
typeContainsTyCon tycon (HsTyVar _ _ (L _ name)) = tycon == name
typeContainsTyCon tycon (HsAppTy _ (L _ t) _) = typeContainsTyCon tycon t
typeContainsTyCon _ _ = False

getArgsLength :: RdrName -> ParsedSource -> [Int]
getArgsLength className ast = [length . hsq_explicit $ tcdTyVars d | d@ClassDecl {} :: TyClDecl GhcPs <- universeBi ast, unLoc (tcdLName d) == className]

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
        f sigName (L _ fb@FunBind {}) = unLoc (fun_id fb) /= sigName
        f _ _ = True

-- might be dangerous, because it's such a general function
rmvNameFromSigs :: RdrName -> [LSig GhcPs] -> [LSig GhcPs]
rmvNameFromSigs sigId = filter (f sigId) . map (m sigId)
    where
        f _ (L _ (ClassOpSig _ _ [] _)) = False
        f sigName (L _ (InlineSig _ otherId _)) = sigName /= unLoc otherId
        f _ _ = True
        m sigName (L l (ClassOpSig _ b methodIds t)) = L l $ ClassOpSig NoExtField b (filter ((/= sigName) . unLoc) methodIds) t
        m _ s = s

rmvFunDeps :: Pass
rmvFunDeps = mkPass "rmvFunDeps" f
    where
        f :: WaysToChange (HsDecl GhcPs)
        f = handleSubList g p
            where
                p (TyClD _ d@ClassDecl {}) = map getLoc $ tcdFDs d
                p _ = []
                g loc (TyClD _ d@ClassDecl {}) = TyClD NoExtField $ d {tcdFDs = filter ((/= loc) . getLoc) $ tcdFDs d}
                g _ t = t