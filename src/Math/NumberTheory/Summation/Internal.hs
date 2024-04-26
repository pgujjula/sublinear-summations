-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Math.NumberTheory.Summation
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Math.NumberTheory.Summation.Internal
  ( numSquarefree,
    sumSquarefree,
  )
where

import Data.List.ApplyMerge (applyMerge)
import Data.List.Ordered (minus)
import Math.NumberTheory.Mobius (mobius')
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (word2Int)

nonOneSquares :: [Word]
nonOneSquares = map (^ (2 :: Int)) [2 ..]

squarefrees :: [Word]
squarefrees = [1 ..] `minus` applyMerge (*) nonOneSquares [1 ..]

-- | The number of square-free integers ≤ @n@.
numSquarefree :: (Integral a) => a -> a
numSquarefree n =
  let n' :: Word
      n' = fromIntegral (max 0 n)

      sq :: Word
      sq = integerSquareRoot n'
   in fromIntegral $
        sum $
          flip map (takeWhile (<= sq) squarefrees) $ \i ->
            mobius' i * fromIntegral (n' `quot` (i * i))

-- | The sum of the square-free integers ≤ @n@.
sumSquarefree :: (Integral a) => a -> a
sumSquarefree n =
  let n' :: Word
      n' = fromIntegral (max 0 n)

      sq :: Word
      sq = integerSquareRoot n'

      f :: Word -> Int
      f k = let k' = word2Int k in (k' * (k' + 1)) `quot` 2
   in fromIntegral $
        sum $
          flip map (takeWhile (<= sq) squarefrees) $ \i ->
            mobius' i * word2Int (i * i) * f (n' `quot` (i * i))
