module Util.Types where

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
import Data.Csv
import Data.IORef (IORef)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (Day, DiffTime, UTCTime (utctDay, utctDayTime))
import Data.Void (Void)
import Distribution.PackageDescription hiding (Executable, Library)
import Distribution.PackageDescription.PrettyPrint
import GHC hiding (Parsed, Pass, Renamed)
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
import Data.Word

data CLIOptions w
    = Reduce
          { test :: w ::: FilePath <?> "path to the interestingness test",
            sourceFile :: w ::: FilePath <?> "path to the source file",
            numberOfThreads :: w ::: Word64 <?> "how many threads you want to run concurrently",
            timeOut :: w ::: Word64 <?> "timout in seconds",
            recordStatistics :: w ::: Bool <?> "whether or not do record statistics",
            debug :: w ::: Bool <?> "whether to print additional, more verbose debug information"
          }
    | Merge {sourceFile :: w ::: FilePath <?> "path to the source file"}
    | PackageDesc
          { test :: w ::: FilePath <?> "path to the interestingness test",
            sourceFile :: w ::: FilePath <?> "path to the source file",
            numberOfThreads :: w ::: Word64 <?> "how many threads you want to run concurrently",
            timeOut :: w ::: Word64 <?> "timout in seconds"
          }
    deriving (Generic)

data Pragma = Language T.Text | OptionsGhc T.Text | Include T.Text
    deriving (Eq)

data Performance = Performance
    { _day :: Day,
      _origSize :: Word64,
      _endSize :: Word64,
      _ratio :: Word64,
      _startTime :: UTCTime,
      _endTime :: UTCTime,
      _duration :: DiffTime,
      _capabilities :: Word64,
      _threads :: Word64
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
      _successfulAttempts :: Word64,
      _totalAttempts :: Word64,
      _removedBytes :: Integer,
      _removedTokens :: Integer
    }
    deriving (Generic, Show)

instance Num PassStats where
  (PassStats n1 sa1 ta1 b1 t1) + (PassStats _ sa2 ta2 b2 t2) = PassStats n1 (sa1 + sa2) (ta1 + ta2) (b1 + b2) (t1 + t2)

instance FromField PassStats
instance ToRecord PassStats
instance ToNamedRecord PassStats
instance FromNamedRecord (String, PassStats)
instance FromNamedRecord PassStats
instance DefaultOrdered PassStats

makeLenses ''PassStats

stats2NamedTuple :: PassStats -> (String, PassStats)
stats2NamedTuple p@PassStats{..} = (_passName, p)

newtype Statistics = Statistics
    { _passStats :: M.Map String PassStats
    }
    deriving (Generic, Show)

makeLenses ''Statistics

emptyStats :: Statistics
emptyStats = Statistics M.empty

data RState
    = ParsedState
          { _pragmas :: [Pragma],
            _parsed :: ParsedSource,
            _isAlive :: Bool,
            _statistics :: Statistics,
            _numRenamedNames :: Word64,
            _dflags :: DynFlags
          }
    | TypecheckedState
          { _pragmas :: [Pragma],
            _parsed :: ParsedSource,
            _isAlive :: Bool,
            _statistics :: Statistics,
            _numRenamedNames :: Word64,
            _typechecked :: TypecheckedModule,
            _hscEnv :: HscEnv,
            _dflags :: DynFlags
          }
    | CabalState
          { _isAlive :: Bool,
            _statistics :: Statistics,
            _pkgDesc :: GenericPackageDescription
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
      _logRef :: IORef [String],
      _timeout :: Word64,
      _debug :: Bool
    }

runR :: RConf -> R m a -> m a
runR conf (R a) = runReaderT a conf

newtype R m a = R {unR :: ReaderT RConf m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf, MonadTrans)

data ShowMode = Parsed | Renamed

showState :: ShowMode -> RState -> T.Text
showState _ s@(CabalState {}) = T.pack . showGenericPackageDescription $ _pkgDesc s
showState _ (ParsedState prags ps _ _ _ dynFlags) =
    T.unlines $
        showLanguagePragmas prags
            : [T.pack . showSDoc dynFlags . ppr . unLoc $ ps]
showState Parsed (TypecheckedState prags p _ _ _ _ _ dynFlags) =
    T.unlines $
        showLanguagePragmas prags
            : [T.pack . showSDoc dynFlags . ppr . unLoc $ p]
showState Renamed (TypecheckedState prags p _ _ _ (TypecheckedModule {tm_renamed_source = Just (rs, _, _, _)}) _ currentDynFlags) =
    T.unlines $
        showLanguagePragmas prags
            : maybe "" (\n -> "module " <> (T.pack . showSDoc currentDynFlags . ppr $ unLoc n) <> " where") (hsmodName $ unLoc p)
            : T.unlines (map (T.pack . showSDoc currentDynFlags . ppr) . hsmodImports $ unLoc p)
            : [T.pack . showSDoc currentDynFlags . ppr $ rs]
showState _ _ = error "Util.Types.showState: unexpected arguments"

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

data GhcMode = UnusedBinds | UnusedImports | Indent | MissingImport | HiddenImport | PerhapsYouMeant | NotInScope deriving (Eq, Show)

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
    | STATE String (RState -> [RState -> RState])
    | CabalPass String (GenericPackageDescription -> [GenericPackageDescription -> GenericPackageDescription])

-- instance Semigroup Pass where
--     AST s1 f <> AST s2 g = AST (s1 <> "<>" <> s2) $ \ast -> (.) <$> f ast <*> g ast
--     STATE s1 f <> STATE s2 g = STATE (s1 <> "<>" <> s2) $ \state -> (.) <$> f state <*> g state
--     l@(STATE _ _) <> _ = l -- STATE (s1 <> "<>AST:" <> s2) $ \state -> (.) <$> f state <*> state { _parsed = g (_parsed state)}
--     _ <> r@(STATE _ _) = r

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

mkPerformance :: Word64 -> Word64 -> UTCTime -> UTCTime -> Word64 -> R IO Performance
mkPerformance oldSize newSize t1 t2 n = do
    c <- fromIntegral <$> liftIO getNumCapabilities
    return $ Performance (utctDay t1) oldSize newSize ratio t1 t2 duration c n
    where
        ratio = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Word64
        offset =
            if utctDayTime t2 < utctDayTime t1
                then 86401
                else 0
        duration = utctDayTime t2 + offset - utctDayTime t1

instance ParseRecord (CLIOptions Wrapped)

deriving instance Show (CLIOptions Unwrapped)