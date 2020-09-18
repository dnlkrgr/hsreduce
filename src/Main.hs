module Main where

import Reduce.Driver
import Merge.HsAllInOne
import Util.Types
import Options.Generic

import Util.Util
import qualified Reduce.Passes.DataTypes as DataTypes (inline, rmvConArgs)
import qualified Reduce.Passes.Extensions.TypeFamilies as TypeFamilies
import qualified Reduce.Passes.Functions as Functions (inline)
-- import qualified Reduce.Passes.Names as Names (shortenNames)
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


main :: IO ()
main = do
    unwrapRecord "hsreduce" >>= \case
        Reduce {..} -> hsreduce numberOfThreads test sourceFile allActions
        Merge {..} -> hsmerge sourceFile

allActions :: R ()
allActions = do
    Pragmas.reduce
    Exports.reduce
    mapM_ runPass passes

passes :: [Pass]
passes =
        [ Imports.rmvImports
        , Imports.unqualImport
        , Decls.rmvSigs Nothing
        , Decls.rmvDecls Nothing
        , Decls.simplifyDecl Nothing
        , Decls.recCon2Prefix
        , Decls.simplifyConDecl
        , Decls.rmvFunDeps
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
        , Stubbing.simplifyMatch
        , Stubbing.simplifyMatches
        , Stubbing.simplifyLGRHS
        , Stubbing.familyResultSig
        , Stubbing.tyVarBndr
        , Stubbing.unqualNames
        , DataTypes.inline
        , DataTypes.rmvConArgs
        , TypeFamilies.apply
        , TypeFamilies.rmvEquations
        , Parameters.reduce
        , Functions.inline
        ]