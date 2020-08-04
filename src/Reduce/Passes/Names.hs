module Reduce.Passes.Names (shortenNames) where

import Debug.Trace
import Data.Word8 hiding (isUpper, isLower, toUpper)
import Lens.Micro.Platform
import Data.Char
import Control.Monad.Random
import Control.Concurrent.STM
import Data.Generics.Uniplate.Data
import Control.Monad.Reader
import OccName
import GHC

import Util.Types
import Util.Util


shortenNames :: R ()
shortenNames = do
    printInfo "shortenName"

    conf        <- ask
    oldState    <- liftIO . atomically . readTVar $ _tState conf

    let oldAST = _parsed oldState

    void $ foldM shortenNamesHelper oldAST [ n | n :: OccName <- universeBi oldAST ]


shortenNamesHelper :: ParsedSource -> OccName -> R ParsedSource
shortenNamesHelper oldAST n = do
    conf        <- ask
    oldState    <- liftIO . atomically . readTVar $ _tState conf

    let
        lenElems            = oldState ^. numRenamedNames
        newName             = shortenName lenElems n
        newAST              = transformBi (\otherN -> if oshow otherN == oshow n then newName else otherN) oldAST
        newState            = oldState 
            & parsed          .~ newAST
            & isAlive         .~ False -- (oldState ^. isAlive || oshow oldAST /= oshow newAST)
            & numRenamedNames .~ lenElems + 1

    if length (oshow newAST) < length (oshow oldAST) 
        then liftIO (tryNewValue conf newState) >>= \case
            False   -> return oldAST
            True    -> do
                liftIO . atomically $ writeTVar (_tState conf) newState
                return newAST
        else
            return oldAST

    -- let 
    --     oldAST      = oldState ^. parsed
    --     newAST      = (case mConstrName of
    --         Nothing             -> id
    --         Just constrName     -> transformBi (handlePatterns constrName)) $ transformBi (handleTypes nn argName) oldAST
    --     sizeDiff    = length (lshow oldAST) - length (lshow newAST)
    --     newState    = 
    --         oldState 
    --             & parsed    .~ newAST 
    --             & isAlive   .~ (oldState ^. isAlive || oshow oldAST /= oshow newAST)
 
    -- liftIO (tryNewValue conf newState) >>= \case
    --     True  -> liftIO . atomically $ do
    --         writeTVar (_tState conf) newState
 
    --         updateStatistics_ conf "rmvUnusedParams" True sizeDiff

    --     False -> liftIO $ updateStatistics conf "rmvUnusedParams" False 0

-- alle gleichen Namen in einem Zug aendern und schaune ob sinnvoll

shortenName :: Word8 -> OccName -> OccName
shortenName m n 
    | isVarOcc  n       = mkVarOcc newString
    | isTvOcc   n       = mkTyVarOcc newString
    | isTcOcc   n       = mkTcOcc newString
    | isDataOcc n       = mkDataOcc newString
    | isDataSymOcc n    = mkDataOcc newString
    | otherwise         = n
    -- | isSymOcc n        = newString
    -- | isValOcc n        = newString
  where os          = occNameString n
        newString   = renameName m os

renameName :: Word8 -> String -> String
renameName m (c:_) 
    | isUpper c     = randomNameString m Upper
    | isLower c     = randomNameString m Lower
renameName m s 
    | isOperator s  = randomNameString m Operator
renameName m (':':s)
    | isOperator s  = ':': randomNameString m Operator
renameName _ s      = s

data Case = Upper | Lower | Operator

-- | create a random operator string
randomNameString :: Word8 -> Case -> String
randomNameString lenElems c =
    let 
        symbols = case c of
            Operator    -> operatorSymbols
            _           -> aToZ
        lenNewName      = fromIntegral lenElems `div` length symbols + 1
        stringsOfLenN   = case c of
            Upper   -> (:) <$> map toUpper symbols <*> replicateM (lenNewName-1) symbols
            _       -> replicateM lenNewName symbols
        newName         = traceShow (show stringsOfLenN) stringsOfLenN !! (fromIntegral lenElems `mod` length symbols)

    in newName

  where
      aToZ = ['a'..'z']
      operatorSymbols = "!#$%&*+<>?@^~"