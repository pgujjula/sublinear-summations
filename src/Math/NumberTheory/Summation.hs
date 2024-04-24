-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Math.NumberTheory.Summation
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Math.NumberTheory.Summation
  ( -- * Divisor functions
    sumNumDivisors,
    sumSumDivisors,
    sumTotient,

    -- * Primes
    primePi,
    primeSum,

    -- * Square-free integers
    mertens,
    numSquarefree,
    sumSquarefree,
  )
where

import Control.Placeholder (todo)
import Data.Bits (shiftR)
import Data.Chimera (UChimera)
import Data.Chimera qualified as Chimera
import Data.Function ((&))
import Data.List.ApplyMerge (applyMerge)
import Data.List.Ordered (minus)
import Math.NumberTheory.HyperbolicConvolution (diff, hyper, hyperConvolveFast)
import SublinearSummation.Util (word2Int)

--
-- Divisor functions
--

-- | Let \(τ(n)\) be the number of positive divisors of @n@. Then
-- @'sumNumDivisors' n@ is the sum of \(τ\) from @1@ to @n@.
sumNumDivisors :: (Integral a) => a -> a
sumNumDivisors n =
  let n' :: Word
      n' = fromIntegral n
   in fromIntegral
        . hyperConvolveFast (const 1) (n' `quot`) (const 1) (n' `quot`)
        . fromIntegral
        . max 0
        $ n

-- | Let \(σ(n)\) be the sum of the positive divisors of @n@. Then
-- @'sumSumDivisors' n@ is the sum of \(σ\) from @1@ to @n@.
sumSumDivisors :: (Integral a) => a -> a
sumSumDivisors n =
  let n' :: Word
      n' = fromIntegral n
      hyper_sigma_f :: Word -> Word
      hyper_sigma_f k =
        let i = n' `quot` k
         in (i * (i + 1)) `shiftR` 1
   in fromIntegral
        . hyperConvolveFast id hyper_sigma_f (const 1) (n' `quot`)
        . fromIntegral
        . max 0
        $ n

-- | Let \(φ(n)\) be
-- [Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function),
-- i.e., the number of positive integers ≤ @n@ that are relatively prime to @n@.
-- Then @'sumTotient' n@ is the sum of \(φ\) from @1@ to @n@.
sumTotient :: (Integral a) => a -> a
sumTotient n =
  let n' :: Word
      n' = fromIntegral (max 0 n)

      square :: Word -> Int
      square x = let x' = word2Int x in x' * x'
      {-# INLINE square #-}

      s =
        fromIntegral $
          hyperConvolveFast
            mobius'
            (hyper n' mertens')
            (diff square)
            (hyper n' square)
            n'
   in (s + 1) `quot` 2
{-# INLINE sumTotient #-}

mobius' :: Word -> Int
mobius' n = Chimera.index mobiusChimera (n - 1)
{-# INLINE mobius' #-}

mertens' :: Word -> Int
mertens' n = Chimera.index mertensChimera (n - 1)
{-# INLINE mertens' #-}

mobiusChimera :: UChimera Int
mobiusChimera = Chimera.fromListWithDef 0 mobiuses

mertensChimera :: UChimera Int
mertensChimera = Chimera.fromListWithDef 0 mertenses

mobiuses :: [Int]
mobiuses =
  let primes :: [Word]
      primes = 2 : ([3 ..] `minus` composites)

      composites :: [Word]
      composites = applyMerge (\p i -> p * (p + i)) primes [0 ..]

      squarefrees :: [Word]
      squarefrees = [1 ..] `minus` applyMerge (*) squares [1 ..]
        where
          squares :: [Word]
          squares = map (\x -> x * x) [2 ..]

      isSquarefrees :: [Int]
      isSquarefrees = go 1 squarefrees
        where
          go :: Word -> [Word] -> [Int]
          go x (y : ys) =
            if x == y
              then 1 : go (x + 1) ys
              else 0 : go (x + 1) (y : ys)
          go _ _ = error "impossible"

      countUp :: [Word] -> [(Word, Int)]
      countUp = go 1 0
        where
          go :: Word -> Int -> [Word] -> [(Word, Int)]
          go x !i (y : ys) =
            if x == y
              then go x (i + 1) ys
              else (x, i) : go (x + 1) 0 (y : ys)
          go _ _ _ = error "impossible"
   in applyMerge (*) primes [1 ..]
        & countUp
        & map (\(_, n) -> if even n then 1 else -1)
        & zipWith (*) isSquarefrees

mertenses :: [Int]
mertenses = scanl1 (+) mobiuses

--
-- Primes
--

-- | @'primePi' n@ is the number of primes ≤ @n@.
primePi :: (Integral a) => a -> a
primePi = todo

-- | @'primeSum' n@ is the sum of the primes ≤ @n@.
primeSum :: (Integral a) => a -> a
primeSum = todo

--
-- Square-free integers
--

-- | The Mertens function.
mertens :: (Integral a) => a -> a
mertens = todo

-- | The number of square-free integers ≤ @n@.
numSquarefree :: (Integral a) => a -> a
numSquarefree = todo

-- | The sum of the square-free integers ≤ @n@.
sumSquarefree :: (Integral a) => a -> a
sumSquarefree = todo
