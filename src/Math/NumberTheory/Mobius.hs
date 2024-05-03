-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Math.NumberTheory.Mobius
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
--
-- Tabulate the mobius and Mertens functions.
module Math.NumberTheory.Mobius
  ( -- * Mobius function
    mobius',
    mobiusChimera,
    mobiusVec,
    mobiusVec',

    -- * Mertens function
    mertens',
    mertensChimera,
    mertensVec,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Chimera (UChimera)
import Data.Chimera qualified as Chimera
import Data.List.Infinite qualified as Infinite
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (fromVectors, primes, primesVec, word2Int)

-- | Compute the mobius function.
--  mobius 0 is arbitrarily defined as 0.
--  Uses sharing.
mobius' :: Word -> Int
mobius' = Chimera.index mobiusChimera
{-# INLINE mobius' #-}

-- | Chimera of the mobius function
mobiusChimera :: UChimera Int
mobiusChimera = fromVectors mobiusVec'

-- | Generate the mobius function between the two inputs, inclusive.
mobiusVec :: Word -> Word -> Vector Int
mobiusVec n m =
  let sqrtm = integerSquareRoot m
      ps = primesVec 0 sqrtm

      m' :: Int
      m' = word2Int m

      n' :: Int
      n' = word2Int n
      v' = runST $ do
        v <- MVector.replicate (word2Int (m - n + 1)) (1 :: Int)

        G.forM_ ps $ \(word2Int -> p) -> do
          let lower = smallestMultipleGE p (word2Int n)
          let upper = largestMultipleLE p (word2Int m)
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v ((-p) *) (i - n')

          let p2 = p * p
          let lower' = smallestMultipleGE p2 n'
              upper' = largestMultipleLE p2 m'
          forM_ [lower', lower' + p2 .. upper'] $ \i -> do
            MVector.unsafeWrite v (i - n') 0

        forM_ [n' .. m'] $ \i ->
          flip (MVector.modify v) (i - n') $ \x ->
            if
              | x == 0 -> 0
              | abs x == i -> signum x
              | otherwise -> -signum x

        when (n' == 0) (MVector.write v 0 0)

        Vector.unsafeFreeze v
   in v'

-- | Like 'mobiusVec', but uses sharing.
mobiusVec' :: Word -> Word -> Vector Int
mobiusVec' n m =
  let m' :: Int
      m' = word2Int m

      n' :: Int
      n' = word2Int n

      sqrtm :: Word
      sqrtm = integerSquareRoot m

      ps :: [Word]
      ps = takeWhile (<= sqrtm) primes

      v' = runST $ do
        v <- MVector.replicate (word2Int (m - n + 1)) (1 :: Int)

        forM_ ps $ \(word2Int -> p) -> do
          let lower = smallestMultipleGE p (word2Int n)
          let upper = largestMultipleLE p (word2Int m)
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v ((-p) *) (i - n')

          let p2 = p * p
          let lower' = smallestMultipleGE p2 n'
              upper' = largestMultipleLE p2 m'
          forM_ [lower', lower' + p2 .. upper'] $ \i -> do
            MVector.unsafeWrite v (i - n') 0

        forM_ [n' .. m'] $ \i ->
          flip (MVector.modify v) (i - n') $ \x ->
            if
              | x == 0 -> 0
              | abs x == i -> signum x
              | otherwise -> -signum x

        when (n' == 0) (MVector.write v 0 0)

        Vector.unsafeFreeze v
   in v'

-- | Compute the Mertens function. Uses sharing
mertens' :: Word -> Int
mertens' = Chimera.index mertensChimera
{-# INLINE mertens' #-}

-- | Chimera of the Mertens function

-- TODO: Use traverseSubvectors instead
mertensChimera :: UChimera Int
mertensChimera =
  Chimera.fromInfinite . Infinite.scanl1 (+) . Chimera.toInfinite $
    mobiusChimera

mertensVec :: Word -> Word -> Vector Int
mertensVec n m =
  let sqrtm = integerSquareRoot m
      ps = primesVec 0 sqrtm

      m' :: Int
      m' = word2Int m

      v' = runST $ do
        v <- MVector.replicate (word2Int (m + 1)) (1 :: Int)

        G.forM_ ps $ \(word2Int -> p) -> do
          let lower = smallestMultipleGE p 0
          let upper = largestMultipleLE p (word2Int m)
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v ((-p) *) i

          let p2 = p * p
          let lower' = smallestMultipleGE p2 0
              upper' = largestMultipleLE p2 m'
          forM_ [lower', lower' + p2 .. upper'] $ \i -> do
            MVector.unsafeWrite v i 0

        forM_ [0 .. m'] $ \i ->
          flip (MVector.modify v) i $ \x ->
            if
              | x == 0 -> 0
              | abs x == i -> signum x
              | otherwise -> -signum x

        MVector.write v 0 0

        forM_ [1 .. m'] $ \i -> do
          a <- MVector.read v (i - 1)
          MVector.modify v (+ a) i

        Vector.unsafeFreeze v
   in Vector.drop (fromIntegral n) v'

smallestMultipleGE :: (Integral a) => a -> a -> a
smallestMultipleGE p n = p * (((n - 1) `quot` p) + 1)

largestMultipleLE :: (Integral a) => a -> a -> a
largestMultipleLE p n = p * (n `quot` p)
