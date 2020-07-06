module Util.Types where

import Control.Concurrent.STM
import GHC.LanguageExtensions.Type
import Data.Void
import qualified Text.Megaparsec as MP
import Path
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson
import GHC.Generics (Generic)
import GHC
import Outputable

runR :: RConf -> RState -> R a -> IO (a, RState)
runR c st (R a) = runStateT (runReaderT a c) st

newtype R a = R (ReaderT RConf (StateT RState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadState RState)

data RConf = RConf
    { _test            :: Path Rel File
    , _sourceFile      :: Path Rel File
    , _numberOfThreads :: Int
    , _tempDirs        :: TChan (Path Abs Dir)
    , _tAST            :: TVar ParsedSource
    }

data RState = RState
    { _pragmas      :: [Pragma]
    , _parsed       :: ParsedSource
    , _renamed      :: Maybe RenamedSource
    , _typechecked  :: Maybe TypecheckedSource
    , _isAlive      :: Bool
    }

showState :: RState -> T.Text
showState (RState prags ps _ _ _)  =
    T.unlines
    $ map (T.pack . show) prags
    ++ [T.pack . showSDocUnsafe . ppr . unLoc $ ps]


data Span = Span
    { file      :: T.Text
    , startLine :: Int
    , startCol  :: Int
    , endLine   :: Int
    , endCol    :: Int
    }
    deriving (Eq, Generic, Show)

instance FromJSON Span


data GhcOutput = GhcOutput
    { span   :: Maybe Span
    , doc    :: T.Text
    , reason :: Maybe T.Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON GhcOutput


data Tool        = Ghc | Cabal deriving Show
data GhcMode     = Binds | Imports | Indent | MissingImport | HiddenImport | PerhapsYouMeant | NotInScope deriving (Eq, Show)
data ProjectType = Executable | Library

instance Show ProjectType where
  show Executable = "executable"
  show Library    = "library"

data Interesting = Interesting | Uninteresting
  deriving (Show)

-- TODO: maybe use another type than text for OPTION and INCLUDE
data Pragma = Language T.Text | OptionsGhc T.Text | Include T.Text
  deriving Eq


pragma2Extension :: Pragma -> Maybe Extension
pragma2Extension (Language e) = 
    case e of
        "AllowAmbiguousTypes" -> Just AllowAmbiguousTypes
        "ConstraintKinds"     -> Just ConstraintKinds
        "RankNTypes"          -> Just RankNTypes
        "TypeApplications "   -> Just TypeApplications 
        "TypeFamilies"        -> Just TypeFamilies
        "TypeInType "         -> Just TypeInType 
        "TypeOperators"       -> Just TypeOperators
        _ -> Nothing
pragma2Extension _ = Nothing 


showExtension :: Pragma -> T.Text
showExtension (Language e)   = e
showExtension (OptionsGhc _) = ""
showExtension (Include _)    = ""

instance Show Pragma where
  show (Language e)   = "{-# LANGUAGE "    ++ T.unpack e ++ " #-}"
  show (OptionsGhc o) = "{-# OPTIONS_GHC " ++ T.unpack o ++ " #-}"
  show (Include i)    = "{-# INCLUDE "     ++ T.unpack i ++ " #-}"

type Parser = MP.Parsec Void T.Text
