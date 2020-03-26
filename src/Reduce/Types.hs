{-# language GeneralizedNewtypeDeriving #-}
module Reduce.Types where

import Ormolu.Parser.Result as OPR
import Data.Aeson
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.State.Strict
import Control.Monad.Reader

runR :: RConf -> RState -> R a -> IO (a, RState)
runR c st (R a) = runStateT (runReaderT a c) st

newtype R a = 
  R (ReaderT RConf (StateT RState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadState RState)

data RConf = 
  RConf
  { _test       :: !FilePath,
    _sourceFile :: !FilePath
  }

newtype RState = 
  RState {
    _ormolu     :: OPR.ParseResult
  }

type BindingName = String

newtype Span
  = Span
      { file :: String
      }
  deriving (Eq, Generic, Show)
instance FromJSON Span

data GhcOutput
  = GhcOutput
      { span     :: !Span,
        doc      :: !String,
        severity :: !String,
        reason   :: !String
      }
  deriving (Eq, Generic, Show)
instance FromJSON GhcOutput

data GhcMode = Binds | Imports

type Pass = OPR.ParseResult -> OPR.ParseResult

data Interesting = Interesting | Uninteresting
    deriving Show