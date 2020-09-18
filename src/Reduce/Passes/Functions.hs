module Reduce.Passes.Functions where

import Data.List
import GHC hiding (Pass)
import BasicTypes
import Outputable
import Data.Generics.Uniplate.Data

import Util.Util
import Util.Types

inline :: Pass
inline = AST "inlineFunctions" $ \ast -> 
    map (\(funId, lmatches) oldAst ->
                transformBi (inlineFunctionHelper funId lmatches) oldAst
        )
        [ (funId, lmatches) 
        | (fb@(FunBind _ (L _ funId) (MG _ lmatches _) _ _) :: HsBindLR GhcPs GhcPs) <- universeBi ast
        , let numOccurence  = length [() | AppP n <- universeBi ast, n == funId] 
        , numOccurence > 0
        , let fbLength      = length . (\i -> i) $ oshow fb
        , let _longestGRHS  = (\i -> i ) . head . sortOn length . map (showSDocUnsafe . pprGRHSs LambdaExpr . m_grhss . unLoc) $ unLoc lmatches
        , let lengthOfUses          = numOccurence * (length $ oshow lmatches)
        -- inlining is interesting, if rhs is greater than all uses of the function name, because function def could be deleted
        ,  fbLength >= lengthOfUses ]

pattern AppP :: RdrName -> HsExpr GhcPs
pattern AppP n <- HsApp _ (L _ (HsVar _ (L _ n))) _

inlineFunctionHelper :: RdrName -> Located [LMatch GhcPs (LHsExpr GhcPs)] -> HsExpr GhcPs -> HsExpr GhcPs
inlineFunctionHelper funName (L l2 lmatches) old@(HsApp _ (L l1 (HsVar _ (L _ n))) expr)
    | funName == n  = new
    | otherwise     = old

  where
    nPats = length . m_pats . unLoc . head $ lmatches
    nMatches = length lmatches
            -- this is obviously not the best we can do
            -- but I don't know how to handle n matches with m patterns yet
    app = \con ctxt f -> HsApp NoExt (L l1 (HsPar NoExt (noLoc (con NoExt $ MG NoExt (L l2 $ map (changeMatchContext ctxt) (f lmatches)) FromSource)))) expr
    new = case (nMatches, nPats) of
            (1, 0) -> old -- eta reduced function, how to handle multiple guards?
            (1, _) -> app HsLam LambdaExpr (take 1)
            (_, 1) -> app HsLamCase CaseAlt id
            _      -> old
inlineFunctionHelper _ _ old = old

changeMatchContext :: HsMatchContext RdrName -> LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
changeMatchContext ctxt (L l (Match _ _ p g)) = L l $ Match NoExt ctxt p g
changeMatchContext _ m = m