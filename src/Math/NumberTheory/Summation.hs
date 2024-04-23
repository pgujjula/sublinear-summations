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

--
-- Divisor functions
--

-- | Let \(τ(n)\) be the number of positive divisors of @n@. Then
-- @'sumNumDivisors' n@ is the sum of \(τ\) from @1@ to @n@.
sumNumDivisors :: (Integral a) => a -> a
sumNumDivisors = undefined

-- | Let \(σ(n)\) be the sum of the positive divisors of @n@. Then
-- @'sumSumDivisors' n@ is the sum of \(σ\) from @1@ to @n@.
sumSumDivisors :: (Integral a) => a -> a
sumSumDivisors = undefined

-- | Let \(φ(n)\) be
-- [Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function),
-- i.e., the number of positive integers ≤ @n@ that are relatively prime to @n@.
-- Then @'sumTotient' n@ is the sum of \(φ\) from @1@ to @n@.
sumTotient :: (Integral a) => a -> a
sumTotient = undefined

--
-- Primes
--

-- | @'primePi' n@ is the number of primes ≤ @n@.
primePi :: (Integral a) => a -> a
primePi = undefined

-- | @'primeSum' n@ is the sum of the primes ≤ @n@.
primeSum :: (Integral a) => a -> a
primeSum = undefined

--
-- Square-free integers
--

-- | The Mertens function.
mertens :: (Integral a) => a -> a
mertens = undefined

-- | The number of square-free integers ≤ @n@.
numSquarefree :: (Integral a) => a -> a
numSquarefree = undefined

-- | The sum of the square-free integers ≤ @n@.
sumSquarefree :: (Integral a) => a -> a
sumSquarefree = undefined
