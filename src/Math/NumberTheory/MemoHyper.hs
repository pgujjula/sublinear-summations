-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- \| This module provides tools for memoizing functions of the form
-- \(x \mapsto f \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\).

-- |
-- Module: Math.NumberTheory.MemoHyper
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
module Math.NumberTheory.MemoHyper
  ( MemoHyper,
    UMemoHyper,
    VMemoHyper,
    unMemoHyper,
    unMemoSmall,

    -- * Construct @'MemoHyper'@s
    memoHyper,
    memoHyperDirect,
    memoHyperSigmaHyper,
    memoHyperSigmaMobiusHyper,

    -- * @'MemoHyper'@ for arithmetic functions

    -- ** Divisor functions
    memoHyperSumNumDivisors,
    memoHyperSumSumDivisors,
    memoHyperSumTotient,

    -- ** Primes
    memoHyperPrimePi,
    memoHyperPrimeSum,

    -- ** Square-free integers
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperSumSquarefree,
  )
where

import Data.Vector qualified as V
import Data.Vector.Generic ((!))
import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (word2Int)

-- | @'MemoHyper' v n b@ memoizes a function of the form
--  \(x \mapsto f \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\)
--  where \(f : \{1\ldots n\} \to B\), backed by @'Data.Vector.Generic.Vector'@
--  type @v@.
data MemoHyper v b = MemoHyper
  { mhLimit :: Word,
    mhSqrtLimit :: Word,
    mhFuncVec :: v b,
    mhHyperVec :: v b
  }
  deriving (Eq, Show, Ord)

instance (Functor v) => Functor (MemoHyper v) where
  fmap f (MemoHyper {..}) =
    MemoHyper
      { mhLimit = mhLimit,
        mhSqrtLimit = mhSqrtLimit,
        mhFuncVec = fmap f mhFuncVec,
        mhHyperVec = fmap f mhHyperVec
      }

-- | A @'MemoHyper'@ backed by unboxed vectors.
type UMemoHyper b = MemoHyper U.Vector b

-- | A @'MemoHyper'@ backed by boxed vectors.
type VMemoHyper b = MemoHyper V.Vector b

-- | Index a @'MemoHyper'@.
unMemoHyper :: (G.Vector v b) => MemoHyper v b -> Word -> b
unMemoHyper mh i =
  if i <= mhSqrtLimit mh
    then mhHyperVec mh ! toIndex i
    else mhFuncVec mh ! toIndex (mhLimit mh `quot` i)
{-# INLINE unMemoHyper #-}

-- | Given a @'MemoHyper'@, for a function
-- \(x \mapsto g \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\),
-- we can use @'unMemoSmall'@ to retrieve the function \(x \mapsto g(x)\)
-- for small values of \(x\) (up to \(\lfloor \sqrt{n} \rfloor\)).
unMemoSmall :: (G.Vector v b) => MemoHyper v b -> Word -> b
unMemoSmall mh x = mhFuncVec mh ! toIndex x
{-# INLINE unMemoSmall #-}

toIndex :: Word -> Int
toIndex k = word2Int k - 1
{-# INLINE toIndex #-}

-- | Given \(f : \mathbb{N}^{+} \to \mathbf{B}\), memoizes
-- \(x \mapsto f \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\).
memoHyper :: (G.Vector v b) => (Word -> b) -> Word -> MemoHyper v b
memoHyper f n =
  MemoHyper
    { mhLimit = n,
      mhSqrtLimit = sq,
      mhFuncVec = G.fromListN sqInt (map f [1 .. sq]),
      mhHyperVec = G.fromListN sqInt (map fh [1 .. sq])
    }
  where
    sq = integerSquareRoot n
    sqInt = word2Int sq
    fh k = f (n `quot` k)

-- | If \(f : \mathbb{N}^{+} \to \mathbf{B}\), memoizes
-- \(x \mapsto f \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\)
-- given \(x \mapsto f \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\)
-- itself. Note that not every function can be expressed in this form. Undefined
-- behavior if called on a function that cannot be expressed in this form.
memoHyperDirect :: (G.Vector v b) => (Word -> b) -> Word -> MemoHyper v b
memoHyperDirect fh n =
  MemoHyper
    { mhLimit = n,
      mhSqrtLimit = sq,
      mhFuncVec = G.fromListN sqInt (map f [1 .. sq]),
      mhHyperVec = G.fromListN sqInt (map fh [1 .. sq])
    }
  where
    sq = integerSquareRoot n
    sqInt = word2Int sq
    f k = fh (n `quot` k)

-- | Given \(f : \mathbb{N}^{+} \to \mathbf{B}\), define
-- \[g(n) = \sum_{i=1}^{n}
--     f\left(\left\lfloor \frac{n}{i} \right\rfloor\right)\]
-- This function memoizes
-- \(x \mapsto g \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\), when
-- provided @'Math.NumberTheory.HyperbolicConvolution.hyper' n f@, a list of
-- small values of @g@.
memoHyperSigmaHyper ::
  (Integral b) => (Word -> b) -> [b] -> Word -> VMemoHyper b
memoHyperSigmaHyper = undefined

-- | Given \(f : \mathbb{N}^{+} \to \mathbf{B}\), define
-- \[g(n) = \sum_{i=1}^{n}
--     \mu(i) f\left(\left\lfloor \frac{n}{i} \right\rfloor\right)\]
-- This function memoizes
-- \(x \mapsto g \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\),
-- when provided @f@, and a list of small values of @g@.
memoHyperSigmaMobiusHyper ::
  (Integral b) => (Word -> b) -> [b] -> Word -> VMemoHyper b
memoHyperSigmaMobiusHyper = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.mertens'.
memoHyperMertens :: (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperMertens = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumNumDivisors'.
memoHyperSumNumDivisors :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumNumDivisors = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumSumDivisors'.
memoHyperSumSumDivisors :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumSumDivisors = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumTotient'.
memoHyperSumTotient :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumTotient = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.numSquarefree'.
memoHyperNumSquarefree :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperNumSquarefree = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumSquarefree'.
memoHyperSumSquarefree :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumSquarefree = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.primePi'.
memoHyperPrimePi :: (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperPrimePi = undefined

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.primeSum'.
memoHyperPrimeSum :: (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperPrimeSum = undefined
