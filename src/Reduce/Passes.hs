module Reduce.Passes (allActions) where

import Reduce.Passes.TemplateHaskell as TemplateHaskell
    ( dumpSplices )
import Reduce.Passes.DataTypes as DataTypes (inline, rmvConArgs)
import Reduce.Passes.Decls as Decls
    ( splitSigs, rmvSigs, rmvDecls, rmvConstructors, simplifyConDecl )
import Reduce.Passes.Exports as Exports (reduce)
import Reduce.Passes.Expr as Expr
    ( expr2Undefined, filterExprSubList, simplifyExpr )
import Reduce.Passes.Functions as Functions (etaReduceMatches, inline, rmvGuards, rmvMatches, rmvRHSs)
import Reduce.Passes.Imports as Imports
    ( unqualImport, rmvImports )
-- import Reduce.Passes.Names as Names (unqualNames)
import Reduce.Passes.Parameters as Parameters ( rmvUnusedParams )
import Reduce.Passes.Pat as Pat ( pat2Wildcard )
import Reduce.Passes.Pragmas as Pragmas (reduce)
import Reduce.Passes.Stubbing as Stubbing
    ( contexts,
      localBinds,
      simplifyDeriving,
      simplifyDerivingClause,
      tyVarBndr )
import Reduce.Passes.TypeFamilies as TypeFamilies
    ( familyResultSig, rmvEquations, apply, rmvUnusedParams )
import Reduce.Passes.Typeclasses as Typeclasses
    ( handleMultiParams, rmvFunDeps, rmvTyClMethods, rmvUnusedParams )
import Reduce.Passes.Types as Types
    ( simplifyType, type2Unit, type2WildCard )
import Util.Types ( R ) 
import Util.Util ( runPass ) 

allActions :: [R IO ()]
allActions = [fast, medium, slow]

fast :: R IO ()
fast = do
    mapM_
        runPass
        [ TypeFamilies.apply,
          Decls.splitSigs,
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
        [ Typeclasses.rmvFunDeps,
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
        --   Names.unqualNames,
          DataTypes.inline,
          DataTypes.rmvConArgs,
          Imports.unqualImport,
          Parameters.rmvUnusedParams,
          Functions.etaReduceMatches,
          Functions.inline,
          Imports.rmvImports,
          Typeclasses.rmvTyClMethods,
          Typeclasses.handleMultiParams,
          Typeclasses.rmvUnusedParams,
          TypeFamilies.rmvEquations,
          TypeFamilies.rmvUnusedParams,
          Types.type2WildCard,
          Types.type2Unit
        ]
    Pragmas.reduce
    Exports.reduce
    TemplateHaskell.dumpSplices
    medium