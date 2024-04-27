-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.MemoHyper.Internal
  ( numSquarefreeVec,
    sumSquarefreeVec,
    totientVec,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bifunctor (bimap)
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MVector
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Math.NumberTheory.Mobius (mobiusVec)
import SublinearSummation.Util (primesVec, word2Int)

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

smallestMultipleGE :: (Integral a) => a -> a -> a
smallestMultipleGE p n = p * (((n - 1) `quot` p) + 1)

largestMultipleLE :: (Integral a) => a -> a -> a
largestMultipleLE p n = p * (n `quot` p)
