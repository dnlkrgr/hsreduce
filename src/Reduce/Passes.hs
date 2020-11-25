module Reduce.Passes where

-- import Reduce.Passes.Names as Names (unqualNames)

import Control.Applicative
import Reduce.Passes.DataTypes as DataTypes (inline, rmvConArgs)
import Reduce.Passes.Decls as Decls
    ( rmvConstructors,
      rmvDecls,
      rmvSigs,
      simplifyConDecl,
      splitSigs,
    )
import Reduce.Passes.Exports as Exports (reduce)
import Reduce.Passes.Expr as Expr
import Reduce.Passes.Functions as Functions (betaReduceExprs, etaReduceMatches, inline, rmvGuards, rmvMatches, rmvRHSs)
import Reduce.Passes.Imports as Imports
    ( rmvImports,
      unqualImport,
    )
import Reduce.Passes.Parameters as Parameters (rmvUnusedParams)
import Reduce.Passes.Pat as Pat (pat2Wildcard)
import Reduce.Passes.Pragmas as Pragmas (reduce)
import Reduce.Passes.Stubbing as Stubbing
    ( contexts,
      localBinds,
      rmvDerivingClause,
      simplifyDerivingClause,
      tyVarBndr,
    )
import Reduce.Passes.TemplateHaskell as TemplateHaskell
    ( dumpSplices,
    )
import Reduce.Passes.TypeFamilies as TypeFamilies
    ( apply,
      familyResultSig,
      rmvEquations,
      rmvUnusedParams,
    )
import Reduce.Passes.Typeclasses as Typeclasses
    ( handleMultiParams,
      rmvFunDeps,
      rmvTyClMethods,
      rmvUnusedParams,
    )
import Reduce.Passes.Types as Types
    ( simplifyType,
      type2Unit,
      type2WildCard,
    )
import qualified Text.Megaparsec.Char as MC
import Util.Types
import Util.Util

passesP :: Parser [Pass]
passesP = do
    _ <- MC.char '['
    MC.space
    passes <- many passP
    MC.space
    _ <- MC.char ']'
    pure passes
    where

passP :: Parser Pass
passP = do
    MC.space
    _ <- MC.char '"'
    s <- some MC.alphaNumChar
    _ <- MC.char '"'
    MC.space
    _ <- optional $ MC.char ','
    MC.space
    case filter ((== s) . show) $ allPurePasses of
        [p] -> pure p
        _ -> error $ "Pass \"" <> s <> "\" does not exist!"

allActions :: [R IO ()]
allActions = [fast, medium, slow]

fast :: R IO ()
fast = do
    mapM_
        runPass
        [ TypeFamilies.apply,
          Decls.splitSigs,
          Decls.rmvSigs,
          Decls.rmvDecls
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
          Decls.rmvConstructors,
          Decls.simplifyConDecl,
          Expr.filterExprSubList,
          Expr.simplifyExpr,
          Types.simplifyType,
          Pat.pat2Wildcard,
          Stubbing.contexts,
          Stubbing.rmvDerivingClause,
          Stubbing.simplifyDerivingClause,
          Stubbing.localBinds,
          Functions.rmvRHSs,
          Functions.rmvMatches,
          Functions.rmvGuards,
          Stubbing.tyVarBndr,
          DataTypes.inline,
          DataTypes.rmvConArgs,
          Imports.unqualImport,
          Parameters.rmvUnusedParams,
          Functions.etaReduceMatches,
          Functions.inline,
          Functions.betaReduceExprs,
          Imports.rmvImports,
          Typeclasses.rmvTyClMethods,
          Typeclasses.handleMultiParams,
          Typeclasses.rmvUnusedParams,
          TypeFamilies.rmvEquations,
          TypeFamilies.rmvUnusedParams,
          Types.type2Unit,
          Types.type2WildCard
          --   Names.unqualNames
        ]
    Pragmas.reduce
    Exports.reduce
    TemplateHaskell.dumpSplices
    medium

allPurePasses :: [Pass]
allPurePasses =
    [ TypeFamilies.apply,
      Decls.splitSigs,
      Decls.rmvSigs,
      Decls.rmvDecls,
      Expr.expr2Undefined,
      Typeclasses.rmvFunDeps,
      TypeFamilies.familyResultSig,
      Decls.rmvConstructors,
      Decls.simplifyConDecl,
      Expr.filterExprSubList,
      Expr.simplifyExpr,
      Types.simplifyType,
      Pat.pat2Wildcard,
      Stubbing.contexts,
      Stubbing.rmvDerivingClause,
      Stubbing.simplifyDerivingClause,
      Stubbing.localBinds,
      Functions.rmvRHSs,
      Functions.rmvMatches,
      Functions.rmvGuards,
      Stubbing.tyVarBndr,
      DataTypes.inline,
      DataTypes.rmvConArgs,
      Imports.unqualImport,
      Parameters.rmvUnusedParams,
      Functions.etaReduceMatches,
      Functions.inline,
      Functions.betaReduceExprs,
      Imports.rmvImports,
      Typeclasses.rmvTyClMethods,
      Typeclasses.handleMultiParams,
      Typeclasses.rmvUnusedParams,
      TypeFamilies.rmvEquations,
      TypeFamilies.rmvUnusedParams,
      Types.type2Unit,
      Types.type2WildCard
      --   Names.unqualNames
    ]