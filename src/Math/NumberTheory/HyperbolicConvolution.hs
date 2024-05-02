-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Math.NumberTheory.HyperbolicConvolution
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
--
-- If \(f, g :\mathbb{N}^{+} \to \mathbf{B}\), where \(\mathbf{B}\) is a
-- numeric type, then define the hyperbolic convolution of \(\ast_{h}\) as
-- \[ (f \ast_{h} g)(n) = \sum_{i=1}^{n} f(i)g\left(\left\lfloor \frac{n}{i}
-- \right\rfloor\right)\]
-- This module provides tools for hyperbolic convolutions.
module Math.NumberTheory.HyperbolicConvolution
  ( -- * Basic combinators
    hyper,
    sigma,
    diff,
    mul,

    -- ** Monadic combinators
    hyperM,
    sigmaM,
    diffM,
    mulM,

    -- * Computing hyperbolic convolutions
    hyperConvolve,
    hyperConvolveFast,
    hyperConvolveMobiusFast,

    -- ** Hyperbolic convolutions over monads
    hyperConvolveM,
    hyperConvolveFastM,
  )
where

import Math.NumberTheory.Mobius (mertens', mobius')
import Math.NumberTheory.Roots (integerSquareRoot)

-- | @'hyper' n f k = f (n \`'quot'\` k)@
hyper :: Word -> (Word -> b) -> (Word -> b)
hyper n f k = f (n `quot` k)
{-# INLINE hyper #-}

-- | @'sigma' f n = 'sum' ('map' f [1..n])@
sigma :: (Num b) => (Word -> b) -> (Word -> b)
sigma f n = sum (map f [1 .. n])
{-# INLINE sigma #-}

-- | @'diff' f 1 = f 1@ and @'diff' f n = f n - f (n-1)@ for @n > 1@.
diff :: (Num b) => (Word -> b) -> (Word -> b)
diff f n =
  if n == 1
    then f 1
    else f n - f (n - 1)
{-# INLINE diff #-}

-- | @'mul' f g n = f n * g n@
mul :: (Num b) => (Word -> b) -> (Word -> b) -> (Word -> b)
mul = liftA2 (*)
{-# INLINE mul #-}

-- | Lifted 'hyper'.
hyperM :: (Monad m) => Word -> (Word -> m b) -> (Word -> m b)
hyperM n f k = f (n `quot` k)
{-# INLINE hyperM #-}

-- | Lifted 'sigma'
sigmaM :: (Monad m, Num b) => (Word -> m b) -> (Word -> m b)
sigmaM f n = sum <$> mapM f [1 .. n]
{-# INLINE sigmaM #-}

-- | Lifted 'diff'.
diffM :: (Monad m, Num b) => (Word -> m b) -> (Word -> m b)
diffM f n =
  if n == 1
    then f 1
    else liftA2 (-) (f n) (f (n - 1))
{-# INLINE diffM #-}

-- | Lifted 'mul'.
mulM :: (Monad m, Num b) => (Word -> m b) -> (Word -> m b) -> (Word -> m b)
mulM = liftA2 (liftA2 (*))
{-# INLINE mulM #-}

-- | Given two functions \(f, g : \mathbb{N}^{+} \to \mathbf{B}\), produce the
-- hyperbolic convolution \(f \ast_{h} g\).
hyperConvolve ::
  (Num b) =>
  (Word -> b) ->
  (Word -> b) ->
  Word ->
  b
hyperConvolve f g n = sigma (mul f (hyper n g)) n
{-# INLINE hyperConvolve #-}

-- | Given two functions \(f, g : \mathbb{N}^{+} \to \mathbf{B}\), produce the
-- the hyperbolic convolution \(f \ast_{h} g\).
--
-- This function takes implementations of @f@, @'hyper' n ('sigma' f)@,
-- @'diff' g@, and @'hyper' n g@ to speed up the computation of the convolution.
hyperConvolveFast ::
  (Num b) =>
  -- | An implementation of @f@.
  (Word -> b) ->
  -- | An implementation of @'hyper' n ('sigma' f)@
  (Word -> b) ->
  -- | An implementation of @'diff' g@.
  (Word -> b) ->
  -- | An implementation of @'hyper' n g@.
  (Word -> b) ->
  Word ->
  b
hyperConvolveFast f hyper_sigma_f diff_g hyper_g n =
  let sq = integerSquareRoot n

      part1 = sigma (mul f hyper_g) sq
      part2 = sigma (mul diff_g hyper_sigma_f) sq
      correction = mul (sigma f) (sigma diff_g) sq
   in part1 + part2 - correction

-- | Let \(f : \mathbb{N}^{+} \to \mathbf{B}\). Then given @'diff' f@ and
-- @'hyper' n f@, produce the hyperbolic convolution
-- \[(\mu \ast_{h} f)(n) =
--   \sum_{i = 1}^{n}
--     \mu(i) f\left(\left\lfloor \frac{n}{i} \right\rfloor \right)\].
hyperConvolveMobiusFast ::
  (Num b) =>
  (Word -> b) ->
  (Word -> b) ->
  Word ->
  b
hyperConvolveMobiusFast diff_f hyper_f n =
  hyperConvolveFast
    (fromIntegral . mobius')
    (fromIntegral . hyper n mertens')
    diff_f
    hyper_f
    n
{-# INLINE hyperConvolveMobiusFast #-}

-- | Lifted 'hyperConvolve'.
hyperConvolveM ::
  (Monad m, Num b) =>
  (Word -> m b) ->
  (Word -> m b) ->
  Word ->
  m b
hyperConvolveM f g n = sigmaM (mulM f (hyperM n g)) n
{-# INLINE hyperConvolveM #-}

-- | Lifted 'hyperConvolveFast'.
hyperConvolveFastM ::
  (Monad m, Num b) =>
  -- | An implementation of @f@.
  (Word -> m b) ->
  -- | An implementation of @'hyperM' n ('sigmaM' f)@
  (Word -> m b) ->
  -- | An implementation of @'diffM' g@.
  (Word -> m b) ->
  -- | An implementation of @'hyperM' n g@.
  (Word -> m b) ->
  Word ->
  m b
hyperConvolveFastM f hyper_sigma_f diff_g hyper_g n = do
  let sq = integerSquareRoot n
  part1 <- sigmaM (mulM f hyper_g) sq
  part2 <- sigmaM (mulM diff_g hyper_sigma_f) sq
  correction <- mulM (sigmaM f) (sigmaM diff_g) sq
  pure $ part1 + part2 - correction
