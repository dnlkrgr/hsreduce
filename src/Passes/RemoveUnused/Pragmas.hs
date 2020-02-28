{-# language LambdaCase #-}
module Passes.RemoveUnused.Pragmas where

import Ormolu.Parser.Result as OPR (ParseResult, prParsedSource, prExtensions)
import Ormolu.Parser.Pragma as OPP (Pragma(..))
import Control.Monad.State.Strict
import Data.Foldable

import Types
import Util

reduce :: FilePath -> FilePath -> OPR.ParseResult -> IO OPR.ParseResult
reduce test sourceFile oldOrmolu = do
    putStrLn "\n***Performing RemovePragmas***"
    let pragmas = prExtensions oldOrmolu
    _ormolu <$> execStateT (traverse tryAllPragmas pragmas) (IterState test sourceFile oldOrmolu)

tryAllPragmas :: OPP.Pragma -> StateT IterState IO ()
tryAllPragmas pragmaToTry@(PragmaLanguage ss)
    | length ss == 1 = tryToRemovePragma pragmaToTry
    | otherwise = do
        oldState@(IterState test sourceFile oldOrmolu) <- get
        traverse_ tryLanguagePragma ss
        --put (oldState { _ormolu = newOrmolu })
tryAllPragmas pragmaToTry = tryToRemovePragma pragmaToTry

tryToRemovePragma :: OPP.Pragma -> StateT IterState IO ()
tryToRemovePragma pragmaToTry = do
  oldState@(IterState test sourceFile oldOrmolu) <- get
  let oldPragmas = prExtensions oldOrmolu
      newOrmolu = oldOrmolu { prExtensions = filter (/= pragmaToTry) oldPragmas }
  writeOrmolu2FileAndTest newOrmolu
    >>= \case
        Uninteresting -> return ()
        Interesting -> do
            debugPrint "Could remove 1 pragma"
            put (oldState { _ormolu = newOrmolu })

tryLanguagePragma :: String -> StateT IterState IO ()
tryLanguagePragma s = do
    oldState@(IterState _ _ oldOrmolu) <- get
    liftIO . print . show . prExtensions $ oldOrmolu
    let PragmaLanguage ss:restExtensions = prExtensions oldOrmolu
        newOrmolu = oldOrmolu { prExtensions = PragmaLanguage (filter (/= s) ss) : restExtensions}
    writeOrmolu2FileAndTest newOrmolu
      >>= \case
        Uninteresting -> return ()
        Interesting -> do
            debugPrint "Could remove 1 pragma"
            put (oldState { _ormolu = newOrmolu })