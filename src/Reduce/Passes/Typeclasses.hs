module Reduce.Passes.Typeclasses (rmvFunDeps, rmvTyClMethods, handleMultiParams) where

import Bag (bagToList, listToBag)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import GHC
    ( ClsInstDecl
          ( ClsInstDecl,
            cid_binds,
            cid_datafam_insts,
            cid_ext,
            cid_overlap_mode,
            cid_poly_ty,
            cid_sigs,
            cid_tyfam_insts
          ),
      GenLocated (L),
      GhcPs,
      HsBindLR (FunBind, fun_id),
      HsDecl (TyClD),
      HsImplicitBndrs (HsIB),
      HsType (HsAppTy, HsTyVar),
      LHsBinds,
      LHsQTyVars (hsq_explicit),
      LSig,
      NoExt (NoExt),
      ParsedSource,
      RdrName,
      Sig (ClassOpSig, InlineSig),
      TyClDecl
          ( ClassDecl,
            tcdATDefs,
            tcdATs,
            tcdCExt,
            tcdCtxt,
            tcdDocs,
            tcdFDs,
            tcdFixity,
            tcdLName,
            tcdMeths,
            tcdSigs,
            tcdTyVars
          ),
      getLoc,
      unLoc,
    )
import Util.Types (Pass (AST), WaysToChange)
import Util.Util (deleteAt, handleSubList, mkPass)

-- rmvTyClParams :: Pass
-- rmvTyClParams

-- type class:
--      type class name: RdrName
--      list of type variables => get indices to be filtered out later
-- class instance decl: LHsType to iterate over; check if class name matches first

handleMultiParams :: Pass
handleMultiParams = AST "handleMultiParams" $ \ast ->
    concatMap
        ( \(classId, args) ->
              map
                  ( \i oldAst ->
                        let newArgsLength = getArgsLength classId ast
                         in if length newArgsLength == 1
                                then
                                    let nRmvdArgs = length args - head newArgsLength
                                        newI = i - nRmvdArgs
                                     in transformBi (rmvArgsFromClass classId newI)
                                            . transformBi (rmvArgsFromInstance classId (head newArgsLength) newI)
                                            $ oldAst
                                else oldAst
                  )
                  [1 .. length args]
        )
        [(unLoc $ tcdLName, map unLoc $ hsq_explicit $ tcdTyVars) | ClassDecl {..} :: TyClDecl GhcPs <- universeBi ast]

rmvArgsFromClass :: RdrName -> Int -> TyClDecl GhcPs -> TyClDecl GhcPs
rmvArgsFromClass className i d@ClassDecl {..}
    | unLoc tcdLName == className = d {tcdTyVars = tcdTyVars {hsq_explicit = deleteAt i (hsq_explicit $ tcdTyVars)}}
    | otherwise = d
rmvArgsFromClass _ _ d = d

rmvArgsFromInstance :: RdrName -> Int -> Int -> ClsInstDecl GhcPs -> ClsInstDecl GhcPs
rmvArgsFromInstance className n i d@ClsInstDecl {..} =
    case cid_poly_ty of
        HsIB _ body -> d {cid_poly_ty = HsIB NoExt $ rmvArgsFromType className n i <$> body}
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
        m sigName (L l (ClassOpSig _ b methodIds t)) = L l $ ClassOpSig NoExt b (filter ((/= sigName) . unLoc) methodIds) t
        m _ s = s

rmvFunDeps :: Pass
rmvFunDeps = mkPass "rmvFunDeps" f
    where
        f :: WaysToChange (HsDecl GhcPs)
        f = handleSubList g p
            where
                p (TyClD _ d@ClassDecl {}) = map getLoc $ tcdFDs d
                p _ = []
                g loc (TyClD _ d@ClassDecl {}) = TyClD NoExt $ d {tcdFDs = filter ((/= loc) . getLoc) $ tcdFDs d}
                g _ t = t