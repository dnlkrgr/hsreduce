{-# LANGUAGE DeriveGeneric #-}

module Types where

import Ormolu.Parser.Result as OPR
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (isPrefixOf)
import HsSyn

type Pass = OPR.ParseResult -> OPR.ParseResult

data Interesting = Interesting | Uninteresting
    deriving Show

data IterState
  = IterState
      { _test :: FilePath,
        _sourceFile :: FilePath,
        _ormolu :: OPR.ParseResult
      }

type UnusedBindingName = String

newtype Span
  = Span
      { file :: String
      }
  deriving (Eq, Generic, Show)

instance FromJSON Span

data GhcOutput
  = GhcOutput
      { span :: Span,
        doc :: String,
        severity :: String,
        reason :: String
      }
  deriving (Eq, Generic, Show)

instance FromJSON GhcOutput