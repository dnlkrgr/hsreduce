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


allActions :: [R IO ()]
allActions = map (mapM_ runPass) [fast, medium, slow] <> [rest]


fast :: [Pass]
fast = 
        [ TypeFamilies.apply,
          Decls.splitSigs,
          Decls.rmvSigs,
          Decls.rmvDecls,
          Decls.rmvConstructors,
          Decls.simplifyConDecl
        ]

medium :: [Pass]
medium =
    [ DataTypes.inline,
      DataTypes.rmvConArgs,
      Functions.rmvRHSs,
      Functions.rmvMatches,
      Functions.rmvGuards,
      Expr.expr2Undefined]
    <> fast

slow :: [Pass]
slow =
    [ Typeclasses.rmvFunDeps,
      TypeFamilies.familyResultSig,
      Expr.filterExprSubList,
      Expr.simplifyExpr,
      Types.simplifyType,
      Stubbing.contexts,
      Stubbing.rmvDerivingClause,
      Stubbing.simplifyDerivingClause,
      Stubbing.localBinds,
      Stubbing.tyVarBndr,
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
      Pat.pat2Wildcard,
      Types.type2Unit,
      Types.type2WildCard
      --   Names.unqualNames
    ] <> medium
    
rest :: R IO ()
rest = do
    mapM_ runPass slow
    Pragmas.reduce
    Exports.reduce
    TemplateHaskell.dumpSplices

allPurePasses :: [Pass]
allPurePasses = fast <> medium <> slow

bestOrdering :: [R IO ()]
bestOrdering = [do
    mapM_ 
      runPass 
      [rmvRHSs
      , TypeFamilies.apply
      , TypeFamilies.rmvUnusedParams
      , pat2Wildcard
      , rmvFunDeps
      , localBinds
      , betaReduceExprs
      , TypeFamilies.familyResultSig
      , Functions.inline
      , Parameters.rmvUnusedParams
      , rmvConstructors
      , unqualImport
      , rmvDerivingClause
      , DataTypes.inline
      , simplifyType
      , TypeFamilies.rmvEquations
      , rmvConArgs
      , rmvGuards
      , rmvMatches
      , simplifyDerivingClause
      , tyVarBndr
      , contexts
      , type2WildCard
      , rmvImports
      , filterExprSubList
      , etaReduceMatches
      , rmvSigs
      , handleMultiParams
      , splitSigs
      , rmvTyClMethods
      , type2Unit
      , expr2Undefined
      , simplifyConDecl
      , Typeclasses.rmvUnusedParams
      , rmvDecls
      , simplifyExpr]
    Pragmas.reduce
    Exports.reduce
    TemplateHaskell.dumpSplices ]


nestedPassesP :: Parser [[Pass]]
nestedPassesP = do
    _ <- MC.char '['
    MC.space
    passes <- many passesP
    MC.space
    _ <- MC.char ']'
    pure passes

passesP :: Parser [Pass]
passesP = do
    _ <- MC.char '['
    MC.space
    passes <- many passP
    MC.space
    _ <- MC.char ']'
    pure passes

passP :: Parser Pass
passP = do
    MC.space
    s <- some $ MC.alphaNumChar <|> MC.digitChar <|> MC.char '.' <|> MC.char ':'
    MC.space
    _ <- optional $ MC.char ','
    MC.space
    let search = filter ((== s) . show)
    case search allPurePasses of
        [p] -> pure p
        _ -> error $ "Pass \"" <> s <> "\" does not exist!"