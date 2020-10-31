module Reduce.Passes (allActions, allPasses) where

import qualified Reduce.Passes.DataTypes as DataTypes (inline, rmvConArgs)
import qualified Reduce.Passes.Decls as Decls
import qualified Reduce.Passes.Exports as Exports (reduce)
import qualified Reduce.Passes.Expr as Expr
import qualified Reduce.Passes.Functions as Functions (etaReduceMatches, inline, rmvGuards, rmvMatches, rmvRHSs)
import qualified Reduce.Passes.Imports as Imports
import qualified Reduce.Passes.Names as Names (unqualNames)
import qualified Reduce.Passes.Parameters as Parameters (reduce)
import qualified Reduce.Passes.Pat as Pat
import qualified Reduce.Passes.Pragmas as Pragmas (reduce)
import qualified Reduce.Passes.Stubbing as Stubbing
import qualified Reduce.Passes.TypeFamilies as TypeFamilies
import qualified Reduce.Passes.Typeclasses as Typeclasses
import qualified Reduce.Passes.Types as Types
import Util.Types (Pass, R)
import Util.Util (runPass)

allActions :: [R IO ()]
-- allActions = map runPass [Functions.etaReduceMatches, Functions.inline, Decls.rmvDecls Nothing]
allActions = [fast, medium, slow]
-- allActions = [runPass Decls.recCon2Prefix]

fast :: R IO ()
fast = do
    mapM_
        runPass
        [ 
          TypeFamilies.apply,
          Decls.rmvSigs Nothing,
          Decls.rmvDecls Nothing
        ]

medium :: R IO ()
medium = do
    mapM_
        runPass
        [Expr.expr2Undefined]
    fast

slow :: R IO ()
slow = do
    mapM_
        runPass
        [ 
          Decls.recCon2Prefix,
          Typeclasses.rmvFunDeps,
          TypeFamilies.familyResultSig,
          Decls.rmvConstructors Nothing,
          Decls.simplifyConDecl,
          Expr.filterExprSubList,
          Expr.simplifyExpr,
          Types.simplifyType,
          Pat.pat2Wildcard,
          Stubbing.contexts,
          Stubbing.simplifyDeriving,
          Stubbing.simplifyDerivingClause,
          Stubbing.localBinds,
          Functions.rmvRHSs,
          Functions.rmvMatches,
          Functions.rmvGuards,
          Stubbing.tyVarBndr,
          Names.unqualNames,
          DataTypes.inline,
          DataTypes.rmvConArgs,
          Imports.unqualImport,
          Parameters.reduce,
          Functions.etaReduceMatches,
          Functions.inline,
          Imports.rmvImports,
          Typeclasses.rmvTyClMethods,
          Typeclasses.handleMultiParams,
          TypeFamilies.rmvEquations,
          Types.type2WildCard,
          Types.type2Unit
        ]
    Pragmas.reduce
    Exports.reduce
    medium

allPasses :: [Pass]
allPasses =
    [ 
      TypeFamilies.familyResultSig,
      TypeFamilies.rmvEquations,
      Imports.rmvImports,
      Decls.rmvSigs Nothing,
      Decls.rmvDecls Nothing,
      Decls.rmvConstructors Nothing,
      Decls.recCon2Prefix,
      Decls.simplifyConDecl,
      Typeclasses.rmvFunDeps,
      Expr.expr2Undefined,
      Expr.filterExprSubList,
      Expr.simplifyExpr,
      Types.type2WildCard,
      Types.type2Unit,
      Types.simplifyType,
      Pat.pat2Wildcard,
      Stubbing.contexts,
      Stubbing.simplifyDeriving,
      Stubbing.simplifyDerivingClause,
      Stubbing.localBinds,
      Functions.rmvRHSs,
      Functions.rmvMatches,
      Functions.rmvGuards,
      Stubbing.tyVarBndr,
      Names.unqualNames,
      DataTypes.inline,
      DataTypes.rmvConArgs,
      Imports.unqualImport,
      Parameters.reduce,
      Functions.inline
    ]