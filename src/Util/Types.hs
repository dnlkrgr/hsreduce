{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson
import GHC.Generics (Generic)
import Ormolu.Parser.Result as OPR

runR :: RConf -> RState -> R a -> IO (a, RState)
runR c st (R a) = runStateT (runReaderT a c) st

newtype R a
  = R (ReaderT RConf (StateT RState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadState RState, Alternative, MonadPlus)

data RConf
  = RConf
      { _test :: !FilePath,
        _sourceFile :: !FilePath
      }

data RState
  = RState
      { _ormolu :: !OPR.ParseResult
      }

type BindingName = String

data Span
  = Span
      { file :: !String
      , startLine :: !Int
      , startCol :: !Int
      , endLine :: !Int
      , endCol :: !Int
      }
  deriving (Eq, Generic, Show)
instance FromJSON Span


data GhcOutput
  = GhcOutput
      { span :: !(Maybe Span),
        doc :: !String,
        reason :: !(Maybe String)
      }
  deriving (Eq, Generic, Show)
instance FromJSON GhcOutput

data GhcMode = Binds | Imports | Other

data Interesting = Interesting | Uninteresting
  deriving (Show)

-- arst = "{\"span\": null,\"doc\": \"[1 of 1] Compiling Main             ( AllInOne.hs, AllInOne.o )\",\"severity\": \"SevOutput\",\"reason\": null}"
-- brst = "{\"span\": {\"file\": \"AllInOne.hs\",\"startLine\": 1082,\"startCol\": 3,\"endLine\": 1082,\"endCol\": 12},\"doc\": \"Not in scope: type constructor or class \\u2018Coercible\\u2019\\nPerhaps you meant \\u2018Data.Coerce.Coercible\\u2019 (imported from Data.Coerce)\",\"severity\": \"SevError\",\"reason\": null}"
-- crst = "{\"file\": \"AllInOne.hs\",\"startLine\": 1082,\"startCol\": 3,\"endLine\": 1082,\"endCol\": 12}"
