{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import Prologue

import qualified Control.Monad.Exception as Exception
import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Data.Graph.Fold.Partition as Partition
import qualified Luna.IR as IR
import qualified Luna.Pass as Pass
import qualified Luna.Pass.Scheduler as Scheduler

import Luna.Pass (Pass)
import Luna.Pass.Basic (Compilation)

type OnDemandPass stage pass m =
    ( MonadIO m
    , Typeable pass
    , Pass.Compile stage pass m
    , Exception.MonadException Scheduler.Error m
    )

runPass :: forall stage pass m . OnDemandPass stage pass m
        => Pass stage pass () -> m ()
runPass !pass = Scheduler.evalT $ do
    Scheduler.registerPassFromFunction__ pass
    Scheduler.runPassSameThreadByType @pass
{-# INLINE runPass #-}

runPass' :: Pass Compilation Pass.BasicPass () -> IO ()
runPass' p = Graph.encodeAndEval @Compilation (runPass p)
{-# INLINE runPass' #-}

partitionsUnify :: Int -> IO ()
partitionsUnify i = runPass' $ do
    !a <- IR.var "a"
    !b <- IR.var "b"
    !u <- IR.unify a b
    let go !0 = let !o = pure () in o
        go !j = do
            !_ <- Partition.partition u
            go $! j - 1
    go i

main :: IO ()
main = partitionsUnify (10^6)
