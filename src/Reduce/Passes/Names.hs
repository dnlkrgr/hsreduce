module Reduce.Passes.Names (shortenNames) where

import Data.Word8 hiding (isUpper, isLower, toUpper)
import Lens.Micro.Platform
import Data.Char
import Control.Monad.Random
import Control.Concurrent.STM
import Data.Generics.Uniplate.Data
import Control.Monad.Reader
import OccName

import Util.Types
import Util.Util


shortenNames :: R ()
shortenNames = do
    printInfo "shortenNames"

    conf        <- ask
    oldState    <- liftIO . atomically . readTVar $ _tState conf

    let oldAST = _parsed oldState

    forM_ [ n | n :: OccName <- universeBi oldAST ] shortenNamesHelper 

shortenNamesHelper :: OccName -> R ()
shortenNamesHelper n = do
    tryNewState "shortenNames" $ \oldState -> 
        let newState = 
                oldState 
                    & parsed %~ transformBi (\otherN -> if oshow otherN == oshow n then shortenName (oldState ^. numRenamedNames) n else otherN)
                    & numRenamedNames +~ 1 
        in if showState newState < showState oldState
            then newState
            else oldState

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
        newName         = stringsOfLenN !! (fromIntegral lenElems `mod` length symbols)

    in newName

  where
      aToZ = ['a'..'z']
      operatorSymbols = "!#$%&*+<>?@^~"