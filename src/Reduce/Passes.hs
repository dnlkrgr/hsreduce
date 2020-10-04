module Reduce.Passes (allActions, allPasses) where


import Util.Types
import Util.Util
import qualified Reduce.Passes.DataTypes as DataTypes (inline, rmvConArgs)
import qualified Reduce.Passes.Extensions.TypeFamilies as TypeFamilies
import qualified Reduce.Passes.Functions as Functions (inline, etaReduceMatches, rmvMatches, rmvRHSs, rmvGuards)
import qualified Reduce.Passes.Names as Names (unqualNames)
import qualified Reduce.Passes.Remove.Decls as Decls
import qualified Reduce.Passes.Remove.Exports as Exports (reduce)
import qualified Reduce.Passes.Remove.Imports as Imports
import qualified Reduce.Passes.Remove.Parameters as Parameters (reduce)
import qualified Reduce.Passes.Remove.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Simplify.Decls as Decls
import qualified Reduce.Passes.Simplify.Expr as Expr
import qualified Reduce.Passes.Simplify.Pat as Pat
import qualified Reduce.Passes.Simplify.Types as Types
import qualified Reduce.Passes.Stubbing as Stubbing
import qualified Reduce.Passes.Typeclasses as Typeclasses


allActions :: [R IO ()]
allActions = [runPass Typeclasses.handleMultiParams ]
-- allActions = pure $ mapM_ runPass [Functions.etaReduceMatches, Parameters.reduce, Functions.inline]
-- allActions = [fast, medium, slow, slowest]
-- allActions = [slowest]

fast :: R IO ()
fast = do
    mapM_ runPass 
        [ Decls.rmvSigs Nothing
        , Decls.rmvDecls Nothing
        ]

medium :: R IO ()
medium = do
    mapM_ runPass 
        [ Expr.expr2Undefined ]
    fast

slow :: R IO ()
slow = do
    mapM_ runPass 
        [ Types.type2Unit
        ]
    medium

slowest :: R IO ()
slowest = do
    mapM_ runPass
        [ Decls.simplifyDecl Nothing
        , Decls.recCon2Prefix
        , Decls.simplifyConDecl
        , Expr.filterExprSubList
        , Expr.simplifyExpr
        -- , Types.type2WildCard
        , Types.simplifyType
        , Pat.pat2Wildcard
        , Stubbing.contexts
        , Stubbing.simplifyDeriving
        , Stubbing.simplifyDerivingClause
        , Stubbing.localBinds
        , Functions.rmvRHSs
        , Functions.rmvMatches
        , Functions.rmvGuards
        , TypeFamilies.familyResultSig
        , Stubbing.tyVarBndr
        , Names.unqualNames
        , DataTypes.inline
        , DataTypes.rmvConArgs
        , Imports.unqualImport
        , TypeFamilies.apply
        , TypeFamilies.rmvEquations
        , Parameters.reduce
        , Functions.etaReduceMatches
        , Functions.inline
        , Imports.rmvImports
        , Typeclasses.rmvFunDeps
        , Typeclasses.rmvTyClMethods
        , Typeclasses.handleMultiParams
        ]
    slow
    Pragmas.reduce
    Exports.reduce


allPasses :: [Pass]
allPasses =
        [ Imports.rmvImports
        , Decls.rmvSigs Nothing
        , Decls.rmvDecls Nothing
        , Decls.simplifyDecl Nothing
        , Decls.recCon2Prefix
        , Decls.simplifyConDecl
        , Typeclasses.rmvFunDeps
        , Expr.expr2Undefined
        , Expr.filterExprSubList
        , Expr.simplifyExpr
        , Types.type2WildCard
        , Types.type2Unit
        , Types.simplifyType
        , Pat.pat2Wildcard
        , Stubbing.contexts
        , Stubbing.simplifyDeriving
        , Stubbing.simplifyDerivingClause
        , Stubbing.localBinds
        , Functions.rmvRHSs
        , Functions.rmvMatches
        , Functions.rmvGuards
        , Stubbing.tyVarBndr
        , Names.unqualNames
        , DataTypes.inline
        , DataTypes.rmvConArgs
        , Imports.unqualImport
        -- , TypeFamilies.apply
        , TypeFamilies.familyResultSig
        , TypeFamilies.rmvEquations
        , Parameters.reduce
        , Functions.inline
        ]