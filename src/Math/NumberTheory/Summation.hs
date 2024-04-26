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
import Math.NumberTheory.HyperbolicConvolution
  ( diff,
    hyper,
    hyperConvolveFast,
    hyperConvolveMobiusFast,
  )
import Math.NumberTheory.MemoHyper
  ( UMemoHyper,
    memoHyperMertens,
    unMemoHyper,
  )
import Math.NumberTheory.Summation.Internal
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
          hyperConvolveMobiusFast
            (diff square)
            (hyper n' square)
            n'
   in (s + 1) `quot` 2
{-# INLINE sumTotient #-}

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
mertens n =
  if n <= 0
    then 0
    else
      fromIntegral
        . flip unMemoHyper 1
        $ (memoHyperMertens (fromIntegral n) :: UMemoHyper Int)
