module Util.Types where

import Data.Word8
import Data.List
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Data.Csv
import Data.Time
import Data.Void
import GHC
import GHC.Generics (Generic)
import GHC.LanguageExtensions.Type
import Lens.Micro.Platform
import Outputable hiding ((<>))
import Path
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Megaparsec as MP


type WaysToChange a = a -> [a -> a]

data Pass = Pass String (ParsedSource -> ParsedSource)

data Pragma = Language T.Text | OptionsGhc T.Text | Include T.Text
    deriving Eq


data Performance = Performance
    { _day              :: Day
    , _origSize         :: Int
    , _endSize          :: Int
    , _ratio            :: Int
    , _startTime        :: UTCTime
    , _endTime          :: UTCTime
    , _duration         :: DiffTime
    , _capabilities     :: Int
    , _threads          :: Int
    }

instance Show Performance where
    show Performance{..} = 
        intercalate "," 
            [ show _day 
            , init $ show _duration
            , show _capabilities
            , show _threads
            , show _origSize
            , show _endSize 
            , show _ratio 
            ] <> "\n"

mkPerformance :: Int -> Int -> UTCTime -> UTCTime -> Int -> IO Performance
mkPerformance oldSize newSize t1 t2 n = do
    c <- getNumCapabilities
    return $ Performance (utctDay t1) oldSize newSize ratio t1 t2 duration c n
  where 
    ratio       = round ((fromIntegral (oldSize - newSize) / fromIntegral oldSize) * 100 :: Double) :: Int
    offset      = 
        if utctDayTime t2 < utctDayTime t1
        then 86401
        else 0
    duration    = utctDayTime t2 + offset - utctDayTime t1 


data PassStats = PassStats 
    { _passName             :: String
    , _successfulAttempts   :: Int
    , _totalAttempts        :: Int
    , _removedBytes         :: Int 
    }
    deriving (Generic, Show)
instance ToNamedRecord   PassStats
instance DefaultOrdered  PassStats
makeLenses ''PassStats

newtype Statistics = Statistics 
    { _passStats            :: M.Map String PassStats
    }
    deriving (Generic, Show)
makeLenses ''Statistics

emptyStats :: Statistics
emptyStats = Statistics M.empty

data RState = RState
    { _pragmas          :: [Pragma]
    , _parsed           :: ParsedSource
    , _renamed          :: Maybe RenamedSource
    , _typechecked      :: Maybe TypecheckedSource
    , _isAlive          :: Bool
    , _statistics       :: Statistics 
    , _numRenamedNames  :: Word8
    }
makeLenses ''RState

data RConf = RConf
    { _test            :: Path Rel File
    , _sourceFile      :: Path Rel File
    , _numberOfThreads :: Int
    , _tempDirs        :: TChan (Path Abs Dir)
    , _tState          :: TVar RState
    -- , _tAST            :: TVar ParsedSource
    -- , _tAlive          :: TVar Bool
    }

runR :: RConf -> R a -> IO a
runR c (R a) = runReaderT a c

newtype R a = R (ReaderT RConf IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RConf)


showState :: RState -> T.Text
showState (RState []    ps _ _ _ _ _)   = T.pack . showSDocUnsafe . ppr . unLoc $ ps
showState (RState prags ps _ _ _ _ _)   =
    T.unlines
    $ ("{-# LANGUAGE " <> (T.intercalate ", " $ map showExtension prags) <> " #-}")
    : [T.pack . showSDocUnsafe . ppr . unLoc $ ps]


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
