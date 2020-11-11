module Reduce.Passes.Names (shortenNames, unqualNames) where

import Control.Concurrent.STM.Lifted (atomically, readTVar)
import Control.Monad (forM_, replicateM)
import Control.Monad.Reader (MonadReader (ask))
import Data.Char (isLower, isUpper, toUpper)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Lens.Micro.Platform ((%~), (&), (+~), (^.))
import OccName
    ( OccName,
      isDataOcc,
      isDataSymOcc,
      isTcOcc,
      isTvOcc,
      isVarOcc,
      mkDataOcc,
      mkTcOcc,
      mkTyVarOcc,
      mkVarOcc,
      occNameString,
    )
import RdrName (RdrName (Qual, Unqual))
import Util.Types
import Util.Util

unqualNames :: Pass
unqualNames = mkPass "unqualNames" f
    where
        f :: WaysToChange RdrName
        f (Qual _ on) = [const (Unqual on)]
        f _ = []

shortenNames :: R IO ()
shortenNames = do
    printInfo "shortenNames"

    conf <- ask
    oldState <- atomically . readTVar $ _tState conf

    let oldAST = _parsed oldState

    forM_ [n | n :: OccName <- universeBi oldAST] shortenNamesHelper

shortenNamesHelper :: OccName -> R IO ()
shortenNamesHelper n = do
    tryNewState "shortenNames" $ \case 
        oldState@ParsedState{} ->
            let newState =
                    oldState
                        & parsed %~ transformBi (\otherN -> if oshow otherN == oshow n then shortenName (oldState ^. numRenamedNames) n else otherN)
                        & numRenamedNames +~ 1
            in if showState Parsed newState < showState Parsed oldState
                    then newState
                    else oldState
        oldState -> oldState

shortenName :: Word -> OccName -> OccName
shortenName m n
    | isVarOcc n = mkVarOcc newString
    | isTvOcc n = mkTyVarOcc newString
    | isTcOcc n = mkTcOcc newString
    | isDataOcc n = mkDataOcc newString
    | isDataSymOcc n = mkDataOcc newString
    | otherwise = n
    where
        -- \| isSymOcc n        = newString
        -- \| isValOcc n        = newString
        os = occNameString n
        newString = renameName m os

renameName :: Word -> String -> String
renameName m (c : _)
    | isUpper c = randomNameString m Upper
    | isLower c = randomNameString m Lower
renameName m s
    | isOperator s = randomNameString m Operator
renameName m (':' : s)
    | isOperator s = ':' : randomNameString m Operator
renameName _ s = s

data Case = Upper | Lower | Operator

-- | create a random operator string
randomNameString :: Word -> Case -> String
randomNameString lenElems c =
    let symbols = case c of
            Operator -> operatorSymbols
            _ -> aToZ
        lenNewName = fromIntegral lenElems `div` length symbols + 1
        stringsOfLenN = case c of
            Upper -> (:) <$> map toUpper symbols <*> replicateM (lenNewName -1) symbols
            _ -> replicateM lenNewName symbols
        newName = stringsOfLenN !! (fromIntegral lenElems `mod` length symbols)
     in newName
    where
        aToZ = ['a' .. 'z']
        operatorSymbols = "!#$%&*+<>?@^~"