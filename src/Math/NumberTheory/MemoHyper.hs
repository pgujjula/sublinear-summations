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
  ( MemoHyper (..),
    UMemoHyper,
    VMemoHyper,
    unMemoHyper,
    unMemoSmall,

    -- * Construct @'MemoHyper'@s
    memoHyper,
    memoHyperDirect,
    memoHyperSigmaHyper,
    memoHyperSigmaMobiusHyper,

    -- * Conversion
    freeze,
    unsafeFreeze,

    -- * Memoization
    memoHyperFixST,

    -- * @'MemoHyper'@ for arithmetic functions

    -- ** Divisor functions
    memoHyperSumNumDivisors,
    memoHyperSumSumDivisors,
    memoHyperSumTotient,

    -- ** Primes
    memoHyperPrimePi,
    memoHyperPrimeSum,
    memoHyperPrimePhi,

    -- ** Square-free integers
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperSumSquarefree,

    -- ** Miscellaneous
    memoHyperIntegerSquareRoot,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Control.Placeholder (todo)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic (Mutable, unsafeIndex, (!), (!?))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable (PrimMonad, PrimState)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.HyperbolicConvolution
import Math.NumberTheory.MemoHyper.Internal (numSquarefreeVec, sumSquarefreeVec)
import Math.NumberTheory.MemoHyper.Mutable
  ( MMemoHyper (..),
    UMMemoHyper,
    unsafeModifyHyper,
    unsafeModifySmall,
    unsafeReadHyper,
    unsafeReadSmall,
    unsafeWriteHyper,
    unsafeWriteSmall,
  )
import Math.NumberTheory.MemoHyper.Mutable qualified as MMemoHyper
import Math.NumberTheory.Mobius (mertensVec, mobius')
import Math.NumberTheory.Roots (integerRoot, integerSquareRoot)
import Math.NumberTheory.Summation.Internal (numSquarefree, sumSquarefree)
import SublinearSummation.Util (int2Word, primePiVec, primesVec, word2Int)

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
  (G.Vector v b, Integral b) => (Word -> b) -> [b] -> Word -> MemoHyper v b
memoHyperSigmaHyper = todo

-- | Given \(f : \mathbb{N}^{+} \to \mathbf{B}\), define
-- \[g(n) = \sum_{i=1}^{n}
--     \mu(i) f\left(\left\lfloor \frac{n}{i} \right\rfloor\right)\]
-- This function memoizes
-- \(x \mapsto g \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\),
-- when provided @f@, and a list of small values of @g@.
memoHyperSigmaMobiusHyper ::
  (G.Vector v b, Integral b) => (Word -> b) -> [b] -> Word -> MemoHyper v b
memoHyperSigmaMobiusHyper = todo

-- | Convert a 'MMemoHyper' to 'MemoHyper' by copying.
freeze ::
  (PrimMonad m, G.Vector v a) =>
  MMemoHyper (Mutable v) (PrimState m) a ->
  m (MemoHyper v a)
freeze mmh = do
  funcVec <- G.freeze (mmhFuncVec mmh)
  hyperVec <- G.freeze (mmhHyperVec mmh)
  let mh =
        MemoHyper
          { mhLimit = mmhLimit mmh,
            mhSqrtLimit = mmhSqrtLimit mmh,
            mhFuncVec = funcVec,
            mhHyperVec = hyperVec
          }
  pure mh

-- | Convert a 'MMemoHyper' to 'MemoHyper' without copying. The 'MMemoHyper'
-- cannot be used after this operation.
unsafeFreeze ::
  (PrimMonad m, G.Vector v a) =>
  MMemoHyper (Mutable v) (PrimState m) a ->
  m (MemoHyper v a)
unsafeFreeze mmh = do
  funcVec <- G.unsafeFreeze (mmhFuncVec mmh)
  hyperVec <- G.unsafeFreeze (mmhHyperVec mmh)
  let mh =
        MemoHyper
          { mhLimit = mmhLimit mmh,
            mhSqrtLimit = mmhSqrtLimit mmh,
            mhFuncVec = funcVec,
            mhHyperVec = hyperVec
          }
  pure mh

-- | Take @n@ and a recursive function @rec@. @rec@ takes a function @fh@ and an
-- input @i@, where @fh j = f (n `quot` j)@ and returns @f (n `quot` i)@.
memoHyperFixST ::
  (G.Vector v a, Integral a) =>
  Word ->
  (forall s. (Word -> ST s a) -> Word -> ST s a) ->
  MemoHyper v a
memoHyperFixST n rec = runST $ do
  mmh <- MMemoHyper.new n
  let sq = integerSquareRoot n
      fh = MMemoHyper.unsafeReadHyper mmh

  forM_ [(1 :: Word) .. sq] $ \i -> do
    x <- rec fh (n `quot` i)
    MMemoHyper.unsafeWriteSmall mmh i x

  forM_ [sq, sq - 1 .. (1 :: Word)] $ \i -> do
    x <- rec fh i
    MMemoHyper.unsafeWriteHyper mmh i x

  freeze mmh

pow23 :: (Integral a) => a -> a
pow23 x =
  let x' :: Integer
      x' = toInteger x

      y' :: Integer
      y' = integerRoot (3 :: Int) (x' * x')
   in fromIntegral y'

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.mertens'.
memoHyperMertens :: (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperMertens n =
  let n23 :: Word
      n23 = pow23 n

      mvec :: U.Vector Int
      mvec = mertensVec 0 n23

      mert :: Word -> Int
      mert t = mvec ! word2Int t
   in memoHyperFixST n $ \fh i ->
        if n `quot` i <= n23
          then pure (fromIntegral (mert (n `quot` i)))
          else do
            let g 1 = pure 0
                g j = fh (i * j)
                nqi = n `quot` i
            s <-
              hyperConvolveFastM
                (const (pure 1))
                (pure . fromIntegral . (nqi `quot`))
                (pure . fromIntegral . mobius')
                g
                nqi
            pure (1 - s)

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumNumDivisors'.
memoHyperSumNumDivisors :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumNumDivisors = todo

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumSumDivisors'.
memoHyperSumSumDivisors :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumSumDivisors = todo

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumTotient'.
memoHyperSumTotient :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumTotient = todo

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.numSquarefree'.
memoHyperNumSquarefree :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperNumSquarefree n =
  let n23 :: Word
      n23 = pow23 n

      nvec :: U.Vector Int
      nvec = numSquarefreeVec 0 n23

      nsfree :: Word -> Int
      nsfree t = nvec ! word2Int t
   in memoHyperFixST n $ \_ i ->
        let nqi = n `quot` i
         in pure $
              if nqi <= n23
                then fromIntegral (nsfree nqi)
                else fromIntegral (numSquarefree nqi)

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumSquarefree'.
memoHyperSumSquarefree :: (Integral a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumSquarefree n =
  let n23 :: Word
      n23 = pow23 n

      svec :: U.Vector Int
      svec = sumSquarefreeVec 0 n23

      ssfree :: Word -> Int
      ssfree t = svec ! word2Int t
   in memoHyperFixST n $ \_ i ->
        let nqi = n `quot` i
         in pure $
              if nqi <= n23
                then fromIntegral (ssfree nqi)
                else fromIntegral (sumSquarefree nqi)

square :: (Integral a) => a -> a
square x = x * x

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.primePi'.
memoHyperPrimePi :: Word -> UMemoHyper Word
memoHyperPrimePi n = runST $ do
  (phi_mmh :: UMMemoHyper s Word) <- MMemoHyper.new n
  (pi_mmh :: UMMemoHyper s Word) <- MMemoHyper.new n
  let sq = integerSquareRoot n
  let ps = primesVec 0 sq
  forM_ [1 .. sq] $ \i -> do
    unsafeWriteSmall phi_mmh i i
    unsafeWriteHyper phi_mmh i (n `quot` i)
  let ppi = primePiVec sq
  let bMax = ppi ! word2Int sq
  let writeDone j = do
        let j' :: Int
            j' = word2Int j

            doneIndicesLo :: [Word]
            doneIndicesLo =
              let low :: Word
                  low = maybe minBound square (ps !? (j' - 1))

                  high :: Word
                  high = maybe maxBound square (ps !? j')
               in [max 1 low .. min sq (high - 1)]

            doneIndicesHi :: [Word]
            doneIndicesHi =
              let low :: Word
                  low = maybe minBound ((n `quot`) . square) (ps !? j')

                  high :: Word
                  high = maybe maxBound ((n `quot`) . square) (ps !? (j' - 1))
               in map (n `quot`) [max 1 (low + 1) .. min sq high]

        forM_ doneIndicesLo $ \i -> do
          phi <- unsafeReadSmall phi_mmh i
          let pi' = ppi `unsafeIndex` integerSquareRoot (word2Int i)
          unsafeWriteSmall pi_mmh i (phi + pi' - 1)

        forM_ doneIndicesHi $ \i -> do
          let nqi = n `quot` i
          phi <- unsafeReadHyper phi_mmh nqi
          let pi' = ppi `unsafeIndex` integerSquareRoot (word2Int i)
          unsafeWriteHyper pi_mmh nqi (phi + pi' - 1)

  writeDone 0
  forM_ [1 .. bMax] $ \b -> do
    let p = ps `unsafeIndex` (word2Int b - 1)
    let iMin = square (ps `unsafeIndex` word2Int (b - 1))

    let nqiMin = n `quot` iMin
    let indices1 = takeWhile (<= nqiMin) [1 .. sq]
    forM_ indices1 $ \nqi -> do
      let iqp = n `quot` (nqi * p)
      let tooBig = b > ppi `unsafeIndex` integerSquareRoot (word2Int iqp)
      right <-
        if tooBig
          then do
            pi_iqp <- unsafeReadHyper pi_mmh (nqi * p)
            pure $ pi_iqp - b + 2
          else unsafeReadHyper phi_mmh (nqi * p)
      unsafeModifyHyper phi_mmh (\x -> x - right) nqi

    let indices2 = takeWhile (>= iMin) [sq, sq - 1 .. 1]
    forM_ indices2 $ \i -> do
      let iqp = i `quot` p
      let tooBig = b > ppi `unsafeIndex` integerSquareRoot (word2Int iqp)
      right <-
        if tooBig
          then do
            pi_iqp <- unsafeReadSmall pi_mmh iqp
            pure $ pi_iqp - b + 2
          else unsafeReadSmall phi_mmh iqp
      unsafeModifySmall phi_mmh (\x -> x - right) i

    writeDone b

  freeze pi_mmh

memoHyperPrimePhi :: Word -> VMemoHyper (U.Vector Int)
memoHyperPrimePhi n = runST (memoHyperPrimePhiST n)

memoHyperPrimePhiST :: forall s. Word -> ST s (VMemoHyper (U.Vector Int))
memoHyperPrimePhiST n = do
  let sq = integerSquareRoot n
      ps = primesVec 0 sq
      ppi = primePiVec sq

  mfv :: MV.MVector s (U.Vector Int) <- MV.replicate (word2Int sq) U.empty
  mhv :: MV.MVector s (U.Vector Int) <- MV.replicate (word2Int sq) U.empty

  forM_ [1 .. sq] $ \i -> do
    let bMax = ppi ! word2Int (integerSquareRoot i)
    xs <- forM [0 .. bMax] $ \b ->
      if b == 0
        then pure (word2Int i)
        else do
          let i' = word2Int i
              b' = word2Int b
              p = word2Int (ps ! (b' - 1))
          rv <- MV.read mfv ((i' `quot` p) - 1)
          let t =
                let iqp = i' `quot` p
                    sq_iqp = integerSquareRoot iqp
                    b_sq_iqp =
                      word2Int $
                        ppi ! sq_iqp
                    pi_iqp =
                      rv ! b_sq_iqp
                        + b_sq_iqp
                        - 1
                    fallback =
                      pi_iqp - (b' - 1) + 1
                 in fromMaybe fallback (rv !? (b' - 1))
          pure (-t)
    let ys = scanl1 (+) xs

    let v :: U.Vector Int
        v = U.fromListN (word2Int bMax + 1) ys
    MV.write mfv (toIndex i) v

  forM_ [sq, sq - 1 .. 1] $ \i -> do
    let nqi = n `quot` i
    let bMax = ppi ! word2Int (integerSquareRoot nqi)
    let sq' = word2Int sq
    let n' = word2Int n
    xs <- forM [0 .. bMax] $ \b ->
      if b == 0
        then pure (word2Int nqi)
        else
          let nqi' = word2Int nqi
              b' = word2Int b
              p = word2Int (ps ! (b' - 1))
           in do
                let nqip' = nqi' `quot` p
                rv <-
                  if nqip' <= sq'
                    then MV.read mfv (nqip' - 1)
                    else MV.read mhv ((n' `quot` nqip') - 1)
                let t =
                      let sq_nqip = integerSquareRoot nqip'
                          b_sq_nqip = word2Int (ppi ! sq_nqip)
                          pi_nqip =
                            (rv ! b_sq_nqip)
                              + b_sq_nqip
                              - 1
                          fallback = pi_nqip - (b' - 1) + 1
                       in fromMaybe
                            fallback
                            (rv !? (b' - 1))
                pure (-t)
    let ys = scanl1 (+) xs
    let v :: U.Vector Int
        v = U.fromListN (word2Int bMax + 1) ys
    MV.write mhv (toIndex i) v

  let mh =
        MMemoHyper
          { mmhLimit = n,
            mmhSqrtLimit = sq,
            mmhFuncVec = mfv,
            mmhHyperVec = mhv
          }
  freeze mh

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.primeSum'.
memoHyperPrimeSum :: (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperPrimeSum = todo

-- | A 'MemoHyper' for 'Math.NumberTheory.Roots.integerSquareRoot'
memoHyperIntegerSquareRoot ::
  (G.Vector v a, Integral a) => Word -> MemoHyper v a
memoHyperIntegerSquareRoot n =
  let sq = integerSquareRoot n
      fv = G.generate (word2Int sq) $ \i ->
        fromIntegral (integerSquareRoot (i + 1))
      hv = G.generate (word2Int sq) $ \i ->
        fromIntegral (integerSquareRoot (n `quot` int2Word (i + 1)))
   in MemoHyper
        { mhLimit = n,
          mhSqrtLimit = sq,
          mhFuncVec = fv,
          mhHyperVec = hv
        }
