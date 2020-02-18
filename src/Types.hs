
module Types where

import Ormolu.Parser.Result as OPR

type Pass = OPR.ParseResult -> OPR.ParseResult

data Interesting = Interesting | Uninteresting
    deriving Show

data StubState
  = StubState
      { _test :: FilePath,
        _sourceFile :: FilePath,
        _ormolu :: OPR.ParseResult
      }