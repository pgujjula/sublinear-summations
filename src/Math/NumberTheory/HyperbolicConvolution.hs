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

    -- * Computing hyperbolic convolutions
    hyperConvolve,
    hyperConvolveFast,
    hyperConvolveMobiusFast,
  )
where

import Control.Placeholder (todo)
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

-- diffM :: (Num b, Monad m) => (Word -> m b) -> (Word -> m b)
-- diffM f n =
--  if n == 1
--    then f 1
--    else liftA2 (-) (f n) (f (n - 1))
-- {-# INLINE diffM #-}

-- | @'mul' f g n = f n * g n@
mul :: (Num b) => (Word -> b) -> (Word -> b) -> (Word -> b)
mul = liftA2 (*)
{-# INLINE mul #-}

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
  forall b.
  (Num b) =>
  (Word -> b) ->
  (Word -> b) ->
  Word ->
  b
hyperConvolveMobiusFast = todo
{-# INLINE hyperConvolveMobiusFast #-}

-- hyperConvolveFastM ::
--  (Num b) =>
--  -- | An implementation of @f@.
--  (Word -> ST s b) ->
--  -- | An implementation of @'hyper' n ('sigma' f)@
--  (Word -> ST s b) ->
--  -- | An implementation of @'diff' g@.
--  (Word -> ST s b) ->
--  -- | An implementation of @'hyper' n g@.
--  (Word -> ST s b) ->
--  Word ->
--  ST s b
-- hyperConvolveFastM f hyper_sigma_f diff_g hyper_g n = do
--  let sq = integerSquareRoot n
--  part1 <- fmap sum $ forM [1 .. sq] $ \i ->
--    liftA2 (*) (f i) (hyper_g i)
--  part2 <- fmap sum $ forM [1 .. sq] $ \i ->
--    liftA2 (*) (diff_g i) (hyper_sigma_f i)
--  correction <-
--    liftA2
--      (*)
--      (sum <$> forM [1 .. sq] f)
--      (sum <$> forM [1 .. sq] diff_g)
--  pure $ part1 + part2 - correction
