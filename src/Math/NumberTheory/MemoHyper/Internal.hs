-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.MemoHyper.Internal
  ( numDivisorsVec,
    sumDivisorsVec,
    numSquarefreeVec,
    sumSquarefreeVec,
    totientVec,
    sumTotientVec,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bifunctor (bimap, first)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MVector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Math.NumberTheory.Mobius (mobiusVec)
import SublinearSummation.Util (primesVec, word2Int)

numDivisorsVec :: Word -> Word -> U.Vector Int
numDivisorsVec n m =
  let ps = primesVec 0 m

      n' :: Int
      n' = word2Int n

      u' :: U.Vector (Int, Int)
      u' = runST $ do
        v <- UM.replicate (word2Int (m - n + 1)) (1, 1)

        G.forM_ ps $ \p -> do
          forM_ (takeWhile (<= m) (iterate (* p) p)) $ \pk -> do
            let lower = smallestMultipleGE pk n
            let upper = largestMultipleLE pk m
            forM_ [lower, lower + pk .. upper] $ \i ->
              MVector.unsafeModify v (first (+ 1)) (word2Int i - n')

          let lower = smallestMultipleGE p n
          let upper = largestMultipleLE p m
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v (\(b, tot) -> (1, tot * b)) (word2Int i - n')

        G.unsafeFreeze v
   in G.map snd u'

sumDivisorsVec :: Word -> Word -> U.Vector Int
sumDivisorsVec n m =
  let ps = primesVec 0 m

      n' :: Int
      n' = word2Int n

      u' :: U.Vector (Int, Int)
      u' = runST $ do
        v <- UM.replicate (word2Int (m - n + 1)) (1, 1)

        G.forM_ ps $ \p -> do
          forM_ (takeWhile (<= m) (iterate (* p) p)) $ \pk -> do
            let lower = smallestMultipleGE pk n
            let upper = largestMultipleLE pk m
            forM_ [lower, lower + pk .. upper] $ \i ->
              MVector.unsafeModify v (first (+ word2Int pk)) (word2Int i - n')

          let lower = smallestMultipleGE p n
          let upper = largestMultipleLE p m
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v (\(b, tot) -> (1, tot * b)) (word2Int i - n')

        G.unsafeFreeze v
   in G.map snd u'

numSquarefreeVec :: Word -> Word -> U.Vector Int
numSquarefreeVec n m =
  G.drop (word2Int n) . G.scanl1 (+) . G.map abs $ mobiusVec 0 m

sumSquarefreeVec :: Word -> Word -> U.Vector Int
sumSquarefreeVec n m =
  G.drop (word2Int n) . G.scanl1 (+) . G.imap (*) . G.map abs $ mobiusVec 0 m

totientVec :: Word -> Word -> U.Vector Int
totientVec n m =
  let ps = primesVec 0 m

      m' :: Int
      m' = word2Int m

      n' :: Int
      n' = word2Int n

      u' :: U.Vector Int
      u' = runST $ do
        v <- UM.replicate (word2Int (m - n + 1)) (1, 1)

        G.forM_ ps $ \(word2Int -> p) -> do
          let lower = smallestMultipleGE p (word2Int n)
          let upper = largestMultipleLE p (word2Int m)
          forM_ [lower, lower + p .. upper] $ \i ->
            MVector.unsafeModify v (bimap (* (p - 1)) (* p)) (i - n')

        res <- MVector.unsafeNew (m' - n' + 1)
        forM_ [n' .. m'] $ \i -> do
          (num, denom) <- UM.unsafeRead v (i - n')
          UM.unsafeWrite res (i - n') (i * num `quot` denom)

        G.unsafeFreeze res
   in u'

sumTotientVec :: Word -> Word -> U.Vector Int
sumTotientVec n m = G.drop (word2Int n) . G.scanl1 (+) $ totientVec 0 m

smallestMultipleGE :: (Integral a) => a -> a -> a
smallestMultipleGE p n = p * (((n - 1) `quot` p) + 1)

largestMultipleLE :: (Integral a) => a -> a -> a
largestMultipleLE p n = p * (n `quot` p)
