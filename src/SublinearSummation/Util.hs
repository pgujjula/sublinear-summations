-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module SublinearSummation.Util
  ( word2Int,
    int2Word,
    primes,
    primesFrom,
    primesVec,
  )
where

import Data.Function ((&))
import Data.List.Infinite (Infinite ((:<)))
import Data.List.Infinite qualified as Infinite
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as Vector
import Data.Word (Word64)
import Math.Prime.FastSieve (generatePrimes)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

word2Int :: Word -> Int
word2Int = fromIntegral
{-# INLINE word2Int #-}

int2Word :: Int -> Word
int2Word = fromIntegral
{-# INLINE int2Word #-}

primes :: [Word]
primes = primesFrom 0

primesFrom :: Word -> [Word]
primesFrom i =
  let starts :: Infinite Word
      starts = Infinite.map (+ i) (0 :< Infinite.iterate (* 2) 1)

      ends :: Infinite Word
      ends = Infinite.map (\x -> x - 1) (Infinite.tail starts)
   in Infinite.zipWith primesVec starts ends
        & Infinite.toList
        & concatMap Vector.toList

primesVec :: Word -> Word -> Vector Word
primesVec n m =
  let v :: Vector Word64
      v = unsafePerformIO $ generatePrimes (fromIntegral n) (fromIntegral m)
   in unsafeCoerce v
