-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module SublinearSummation.Util (word2Int, int2Word, primes, primesFrom) where

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
  let starts :: [Word]
      starts = map (+ i) (0 : iterate (* 2) 1)

      ends :: [Word]
      ends = map (\x -> x - 1) (tail starts)
   in concatMap Vector.toList (zipWith primesVec starts ends)

primesVec :: Word -> Word -> Vector Word
primesVec n m =
  let v :: Vector Word64
      v = unsafePerformIO $ generatePrimes (fromIntegral n) (fromIntegral m)
   in unsafeCoerce v
