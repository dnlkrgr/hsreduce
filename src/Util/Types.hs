module Util.Types where

import Data.IORef
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.STM.Lifted (TChan, TVar)
import Control.Monad.Base (MonadBase (..), liftBaseDefault)
import Control.Monad.Reader
    ( MonadIO (..),
      MonadReader (local),
      MonadTrans,
      ReaderT (..),
      asks,
    )
import Control.Monad.Trans.Control
    ( ComposeSt,
      MonadBaseControl (..),
      MonadTransControl (..),
      defaultLiftBaseWith,
      defaultLiftWith,
      defaultRestoreM,
      defaultRestoreT,
    )
import Data.Aeson (FromJSON)
import Data.Csv (DefaultOrdered, ToNamedRecord)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (Day, DiffTime, UTCTime (utctDay, utctDayTime))
import Data.Void (Void)
import Data.Word (Word8)
import GHC
import GHC.LanguageExtensions.Type
    ( Extension
          ( AllowAmbiguousTypes,
            ConstraintKinds,
            RankNTypes,
            TypeApplications,
            TypeFamilies,
            TypeInType,
            TypeOperators
          ),
    )
import Katip as K
    ( Katip (..),
      KatipContext (..),
      LogContexts,
      LogEnv,
      Namespace,
    )
import Lens.Micro.Platform (makeLenses)
import Options.Generic
    ( Generic,
      ParseRecord,
      Unwrapped,
      Wrapped,
      type (:::),
      type (<?>),
    )
import Outputable hiding ((<>))
import Path (Abs, Dir, File, Path, Rel)
import qualified Text.Megaparsec as MP

data CLIOptions w
    = Reduce
          { test :: w ::: FilePath <?> "path to the interestingness test",
            sourceFile :: w ::: FilePath <?> "path to the source file",
            numberOfThreads :: w ::: Word8 <?> "how many threads you want to run concurrently"
          }
    | Merge {sourceFile :: w ::: FilePath <?> "path to the source file"}
    deriving (Generic)

data Pragma = Language T.Text | OptionsGhc T.Text | Include T.Text
    deriving (Eq)

data Performance = Performance
    { _day :: Day,
      _origSize :: Word,
      _endSize :: Word,
      _ratio :: Word,
      _startTime :: UTCTime,
      _endTime :: UTCTime,
      _duration :: DiffTime,
      _capabilities :: Word,
      _threads :: Word
    }

instance Show Performance where
    show Performance {..} =
        intercalate
            ","
            [ show _day,
              init $ show _duration,
              show _capabilities,
              show _threads,
              show _origSize,
              show _endSize,
              show _ratio
            ]
            <> "\n"

data PassStats = PassStats
    { _passName :: String,
      _successfulAttempts :: Word,
      _totalAttempts :: Word,
      _removedBytes :: Int
    }
    deriving (Generic, Show)

instance ToNamedRecord PassStats

instance DefaultOrdered PassStats

makeLenses ''PassStats

newtype Statistics = Statistics
    { _passStats :: M.Map String PassStats
    }
    deriving (Generic, Show)

makeLenses ''Statistics

emptyStats :: Statistics
emptyStats = Statistics M.empty

data RState = RState
    { _pragmas :: [Pragma],
      _parsed :: ParsedSource,
      _isAlive :: Bool,
      _statistics :: Statistics,
      _numRenamedNames :: Word,
      _typechecked :: Maybe TypecheckedModule,
      _hscEnv :: Maybe HscEnv,
      _dflags :: Maybe DynFlags
    }

makeLenses ''RState

data RConf = RConf
    { _test :: Path Rel File,
      _sourceFile :: Path Rel File,
      _numberOfThreads :: Int,
      _tempDirs :: TChan (Path Abs Dir),
      _tState :: TVar RState,
      logNamespace :: K.Namespace,
      logContext :: K.LogContexts,
      logEnv :: K.LogEnv,
      _logRef :: IORef [String]
    }

runR :: RConf -> R m a -> m a
runR conf (R a) = runReaderT a conf

newtype R m a = R {unR :: ReaderT RConf m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadTrans)

showState :: RState -> T.Text
showState (RState prags ps _ _ _ _ _ Nothing) =
    T.unlines $
        showLanguagePragmas prags
            : [T.pack . showSDocUnsafe . ppr . unLoc $ ps]
showState (RState prags ps _ _ _ _ _ (Just dflags)) =
    T.unlines $
        showLanguagePragmas prags
            : [T.pack . showSDoc dflags . ppr . unLoc $ ps]

showLanguagePragmas :: [Pragma] -> T.Text
showLanguagePragmas [] = ""
showLanguagePragmas prags = "{-# LANGUAGE " <> (T.intercalate ", " $ map showExtension prags) <> " #-}"

data Span = Span
    { file :: T.Text,
      startLine :: Int,
      startCol :: Int,
      endLine :: Int,
      endCol :: Int
    }
    deriving (Eq, Generic, Show)

instance FromJSON Span

data GhcOutput = GhcOutput
    { span :: Maybe Span,
      doc :: T.Text,
      reason :: Maybe T.Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON GhcOutput

data Tool = Ghc | Cabal deriving (Show)

data GhcMode = Binds | Imports | Indent | MissingImport | HiddenImport | PerhapsYouMeant | NotInScope deriving (Eq, Show)

data ProjectType = Executable | Library

instance Show ProjectType where
    show Executable = "executable"
    show Library = "library"

data Interesting = Interesting | Uninteresting
    deriving (Eq, Show)

pragma2Extension :: Pragma -> Maybe Extension
pragma2Extension (Language e) =
    case e of
        "AllowAmbiguousTypes" -> Just AllowAmbiguousTypes
        "ConstraintKinds" -> Just ConstraintKinds
        "RankNTypes" -> Just RankNTypes
        "TypeApplications " -> Just TypeApplications
        "TypeFamilies" -> Just TypeFamilies
        "TypeInType " -> Just TypeInType
        "TypeOperators" -> Just TypeOperators
        _ -> Nothing
pragma2Extension _ = Nothing

showExtension :: Pragma -> T.Text
showExtension (Language e) = e
showExtension (OptionsGhc _) = ""
showExtension (Include _) = ""

instance Show Pragma where
    show (Language e) = "{-# LANGUAGE " ++ T.unpack e ++ " #-}"
    show (OptionsGhc o) = "{-# OPTIONS_GHC " ++ T.unpack o ++ " #-}"
    show (Include i) = "{-# INCLUDE " ++ T.unpack i ++ " #-}"

type Parser = MP.Parsec Void T.Text

type WaysToChange a = a -> [a -> a]

data Pass
    = AST String (ParsedSource -> [ParsedSource -> ParsedSource])
    | Arst String (RState -> [RState -> RState])

-- ##### KATIP STUFF
instance MonadBase b m => MonadBase b (R m) where
    liftBase = liftBaseDefault

instance MonadTransControl R where
    type StT R a = StT (ReaderT RConf) a
    liftWith = defaultLiftWith R unR
    restoreT = defaultRestoreT R

instance MonadBaseControl b m => MonadBaseControl b (R m) where
    type StM (R m) a = ComposeSt R m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

-- These instances get even easier with lenses!
instance (MonadIO m) => Katip (R m) where
    getLogEnv = asks logEnv
    localLogEnv f (R m) = R (local (\s -> s {logEnv = f (logEnv s)}) m)

instance (MonadIO m) => KatipContext (R m) where
    getKatipContext = asks logContext
    localKatipContext f (R m) = R (local (\s -> s {logContext = f (logContext s)}) m)
    getKatipNamespace = asks logNamespace
    localKatipNamespace f (R m) = R (local (\s -> s {logNamespace = f (logNamespace s)}) m)

-- #####

mkPerformance :: Word -> Word -> UTCTime -> UTCTime -> Word -> R IO Performance
mkPerformance oldSize newSize t1 t2 n = do
    c <- fromIntegral <$> liftIO getNumCapabilities
    return $ Performance (utctDay t1) oldSize newSize ratio t1 t2 duration c n
    where
        ratio = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Word
        offset =
            if utctDayTime t2 < utctDayTime t1
                then 86401
                else 0
        duration = utctDayTime t2 + offset - utctDayTime t1

instance ParseRecord (CLIOptions Wrapped)

instance Show (CLIOptions Unwrapped)