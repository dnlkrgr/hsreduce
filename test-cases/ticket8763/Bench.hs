{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Control.Monad
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM


main :: IO ()
main = do

  defaultMain
    [ bench "forM_" $ whnfIO runForM_
    , bench "loop"  $ whnfIO runLoop
    , bench "matmultForM_" $ whnfIO matmultForM_
    , bench "matmultLoop"  $ whnfIO matmultLoop
    ]



{-# INLINE loop #-}
loop :: (Monad m) => Int -> (Int -> m ()) -> m ()
loop bex f = go 0
  where
    go !n | n == bex  = return ()
          | otherwise = f n >> go (n+1)



-- Simple example: Writing a singleton vector _MAX times

_MAX :: Int
_MAX = 1000


runForM_ :: IO Int
runForM_ = do
  v <- UM.unsafeNew 1
  forM_ [1.._MAX] $ \i -> do
    UM.unsafeWrite v 0 i
  UM.unsafeRead v 0


runLoop :: IO Int
runLoop = do
  v <- UM.unsafeNew 1
  loop _MAX $ \i -> do
    UM.unsafeWrite v 0 i
  UM.unsafeRead v 0



-- Slightly more complicated example: Matrix multiplication of _SIZE*_SIZE sized matrices

_SIZE :: Int
_SIZE = 10


a :: Vector (U.Vector Int)
a = V.replicate _SIZE (U.replicate _SIZE 1)

matmultForM_ :: IO (VM.IOVector (U.Vector Int))
matmultForM_ = do -- V.create $ do
      -- Initialize result vector with 0
      v :: VM.IOVector (UM.IOVector Int) <- VM.new _SIZE
      forM_ [0.._SIZE-1] $ \i -> do
        w <- UM.replicate _SIZE 0
        VM.write v i w

      -- Fill result vector
      loop _SIZE $ \i -> do
        y <- VM.read v i
        loop _SIZE $ \k -> do
          forM_ [0.._SIZE-1] $ \j -> do -- slow
            x <- UM.unsafeRead y j
            UM.unsafeWrite y j $! x + (a!i!.j) * (a!k!.j)

      -- Freeze inner vectors
      r <- VM.new _SIZE
      forM_ [0.._SIZE-1] $ \i -> do
        y <- VM.read v i
        w <- U.unsafeFreeze y
        VM.write r i w

      return r

matmultLoop :: IO (VM.IOVector (U.Vector Int))
matmultLoop = do -- V.create $ do
      -- Initialize result vector with 0
      v :: VM.IOVector (UM.IOVector Int) <- VM.new _SIZE
      forM_ [0.._SIZE-1] $ \i -> do
        w <- UM.replicate _SIZE 0
        VM.write v i w

      -- Fill result vector
      loop _SIZE $ \i -> do
        y <- VM.read v i
        loop _SIZE $ \k -> do
          loop _SIZE $ \j -> do -- fast
            x <- UM.unsafeRead y j
            UM.unsafeWrite y j $! x + (a!i!.j) * (a!k!.j)

      -- Freeze inner vectors
      r <- VM.new _SIZE
      forM_ [0.._SIZE-1] $ \i -> do
        y <- VM.read v i
        w <- U.unsafeFreeze y
        VM.write r i w

      return r

{-# INLINE (!.) #-}
(!.) :: (U.Unbox a) => U.Vector a -> Int -> a
(!.) = U.unsafeIndex

