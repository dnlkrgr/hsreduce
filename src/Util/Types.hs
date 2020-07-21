module Util.Types where

import Lens.Micro.Platform
import qualified Data.Map as M
import Control.Concurrent.STM
import GHC.LanguageExtensions.Type
import Data.Void
import qualified Text.Megaparsec as MP
import Path
import qualified Data.Text as T
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics (Generic)
import GHC
import Outputable hiding ((<>))
import Data.Csv

type WaysToChange a = a -> [a -> a]

data Pass = Pass String (ParsedSource -> ParsedSource)

data Pragma = Language T.Text | OptionsGhc T.Text | Include T.Text
    deriving Eq

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

data Statistics = Statistics 
    { _passStats            :: M.Map String PassStats
    }
    deriving (Generic, Show)
makeLenses ''Statistics

data RState = RState
    { _pragmas      :: [Pragma]
    , _parsed       :: ParsedSource
    , _renamed      :: Maybe RenamedSource
    , _typechecked  :: Maybe TypecheckedSource
    , _isAlive      :: Bool
    , _statistics   :: Statistics 
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

-- instance Show Statistics where
--     show stats = unlines $
--         ("***Statistics***" :) $
--         flip map (reverse . sortOn (_removedBytes . snd) . M.toList $ _passStats stats) $ \(k, p@(PassStats n _ _)) ->
--             k <> ": " <> map (const ' ') [1 .. 30 - length k - length (show n)] <> show p
-- 
--         -- putStrLn $ "\n\nExecution took " ++ show (flip div (10^(12 :: Integer)) . diffTimeToPicoseconds $ t2 - t1) ++ " seconds."

emptyStats :: Statistics
emptyStats = Statistics M.empty


-- instance Show PassStats where
--     show (PassStats n d r) =
--         let 
--             snr = show n
--             sdr = show d
--             sr  = show r
--         in 
--             snr <> " /" <> map (const ' ') [1 .. 4 - length sdr] <> sdr 
--             <> map (const ' ') [1 .. 5 - length sr] <> sr

showState :: RState -> T.Text
showState (RState []    ps _ _ _ _)   = T.pack . showSDocUnsafe . ppr . unLoc $ ps
showState (RState prags ps _ _ _ _)   =
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
