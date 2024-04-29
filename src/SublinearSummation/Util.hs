-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module SublinearSummation.Util
  ( word2Int,
    int2Word,
    primes,
    primesFrom,
    primesVec,
    primePiVec,
    primeSumVec,
    fromVectors,
  )
where

import Data.Chimera (Chimera, UChimera)
import Data.Chimera qualified as Chimera
import Data.Function ((&))
import Data.List (genericLength, genericReplicate)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.Infinite qualified as Infinite
import Data.Vector.Generic qualified as G
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

diffs :: (Num a) => [a] -> [a]
diffs [] = []
diffs xs@(x : xs') =
  x : zipWith (-) xs' xs

primePiVec :: Word -> Vector Word
primePiVec n =
  primesVec 0 n
    & Vector.toList
    & diffs
    & ( \xs ->
          concat (zipWith genericReplicate xs [0 ..])
            ++ repeat (genericLength xs)
      )
    & Vector.fromListN (fromIntegral n + 1)

primeSumVec :: Word -> Vector Word
primeSumVec n =
  let psVec = primesVec 0 n
      ps = Vector.toList psVec

      go :: Word -> [Word] -> [Word]
      go _ [] = repeat 0
      go i (q : qs) =
        if i == q
          then q : go (i + 1) qs
          else 0 : go (i + 1) (q : qs)
   in Vector.fromListN (word2Int (n + 1)) (scanl1 (+) (go 0 ps))

unitChimera :: UChimera ()
unitChimera = Chimera.tabulate (const ())

fromVectors :: (G.Vector v a) => (Word -> Word -> v a) -> Chimera v a
fromVectors f = flip Chimera.imapSubvectors unitChimera $ \i v ->
  let l :: Word
      l = int2Word (G.length v)
   in f i (i + l - 1)
