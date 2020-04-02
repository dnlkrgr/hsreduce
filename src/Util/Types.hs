{-# language GeneralizedNewtypeDeriving #-}
module Util.Types where

import Control.Applicative
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadState RState, Alternative, MonadPlus)

data RConf = 
  RConf
  { _test       :: !FilePath,
    _sourceFile :: !FilePath
  }

data RState = 
  RState {
    _ormolu     :: !OPR.ParseResult
  }

type BindingName = String

data Span
  = Span
      { file :: !String
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

data Interesting = Interesting | Uninteresting
    deriving Show