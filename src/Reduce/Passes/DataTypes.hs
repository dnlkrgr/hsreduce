module Reduce.Passes.DataTypes (inline, rmvConArgs) where

import Data.Generics.Uniplate.Data
import GHC hiding (Pass)
import Util.Types
import Util.Util

rmvConArgs :: Pass
rmvConArgs = AST "rmvConArgs" $ \ast ->
    concatMap
        ( \(conId, args) ->
              map
                  ( \i oldAst ->
                        let newArgsLength = getArgsLength conId ast
                         in if length newArgsLength == 1
                                then
                                    let nRmvdArgs = length args - head newArgsLength
                                        newI = i - nRmvdArgs
                                     in transformBi (rmvArgsFromExpr conId (length args) i)
                                            . transformBi (rmvArgsFromPat conId newI)
                                            . transformBi (rmvArgsFromConDecl conId newI)
                                            $ oldAst
                                else oldAst
                  )
                  [1 .. length args]
        )
        ( [(constrName, map unLoc args) | PrefixConP constrName args :: LConDecl GhcPs <- universeBi ast]
          <> [(constrName, map (unLoc . cd_fld_type . unLoc) args) | RecConP constrName args :: LConDecl GhcPs <- universeBi ast]
        )

getArgsLength :: RdrName -> ParsedSource -> [Int]
getArgsLength constrName ast = [length args | ConPatIn (L _ name) (PrefixCon args) :: Pat GhcPs <- universeBi ast, name == constrName]

pattern PrefixConP :: RdrName -> [LBangType GhcPs] -> LConDecl GhcPs
pattern PrefixConP constrName args <- L _ (ConDeclH98 _ (L _ constrName) _ _ _ (PrefixCon args) _)

pattern RecConP :: RdrName -> [LConDeclField GhcPs] -> LConDecl GhcPs
pattern RecConP constrName args <- L _ (ConDeclH98 _ (L _ constrName) _ _ _ (RecCon (L _ args)) _)

pattern TyVarP :: RdrName -> LHsType GhcPs
pattern TyVarP name <- L _ (HsTyVar _ _ (L _ name))

rmvArgsFromConDecl :: RdrName -> Int -> ConDecl GhcPs -> ConDecl GhcPs
rmvArgsFromConDecl conId i c@(ConDeclH98 x n fa tvs ctxt (PrefixCon args) doc)
    | conId == unLoc n = ConDeclH98 x n fa tvs ctxt (PrefixCon $ deleteAt i args) doc
    | otherwise = c
rmvArgsFromConDecl conId i c@(ConDeclH98 x n fa tvs ctxt (RecCon (L l args)) doc)
    | conId == unLoc n = ConDeclH98 x n fa tvs ctxt (RecCon . L l $ deleteAt i args) doc
    | otherwise = c
rmvArgsFromConDecl _ _ c = c

rmvArgsFromPat :: RdrName -> Int -> Pat GhcPs -> Pat GhcPs
rmvArgsFromPat constrName n p@(ConPatIn (L l name) (PrefixCon args))
    | constrName == name = ConPatIn (L l name) (PrefixCon $ deleteAt n args)
    | otherwise = p
rmvArgsFromPat _ _ p = p

-- ***************************************************************************
-- INLINE TYPE

-- ***************************************************************************

inline :: Pass
inline = AST "inlineType" $ \ast ->
    map
        ( \(nn, argName, mConstrName) ->
              ( case mConstrName of
                    Nothing -> id
                    Just constrName -> transformBi (inlineTypeAtPat constrName)
              )
              . transformBi (inlineTypeAtType nn argName)
        )
        ( [ (unLoc newtypeName, argName, Just constrName)
            | DataDecl _ newtypeName _ _ (HsDataDefn _ _ _ _ _ [PrefixConP constrName [TyVarP argName]] _) :: TyClDecl GhcPs <- universeBi ast
          ]
              <> [(unLoc newtypeName, argName, Nothing) | SynDecl _ newtypeName _ _ (TyVarP argName) :: TyClDecl GhcPs <- universeBi ast]
        )

inlineTypeAtType :: RdrName -> RdrName -> HsType GhcPs -> HsType GhcPs
inlineTypeAtType newtypeName argName t@(HsTyVar x p (L l tyvarName))
    | newtypeName == tyvarName = HsTyVar x p (L l argName)
    | otherwise = t
inlineTypeAtType _ _ t = t

inlineTypeAtPat :: RdrName -> Pat GhcPs -> Pat GhcPs
inlineTypeAtPat constrName p@(ConPatIn (L _ name) (PrefixCon [arg]))
    | constrName == name = unLoc arg
    | otherwise = p
inlineTypeAtPat _ p = p