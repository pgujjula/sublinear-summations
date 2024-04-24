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
import Math.NumberTheory.HyperbolicConvolution (hyperConvolveFast)

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
sumTotient = todo

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
