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
    memoHyperHyperConvolve,

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
    memoHyperRoughSum,

    -- ** Square-free integers
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperSumSquarefree,

    -- ** Miscellaneous
    memoHyperIntegerSquareRoot,
    pow23,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Chimera qualified as Chimera
import Data.Function ((&))
import Data.List (genericReplicate)
import Data.List.Infinite qualified as Infinite
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic (Mutable, unsafeIndex, (!), (!?))
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable (PrimMonad, PrimState)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.HyperbolicConvolution
import Math.NumberTheory.MemoHyper.Internal
  ( numDivisorsVec,
    numSquarefreeVec,
    sumDivisorsVec,
    sumSquarefreeVec,
    sumTotientVec,
  )
import Math.NumberTheory.MemoHyper.Mutable
  ( MMemoHyper (..),
    UMMemoHyper,
    readHyper,
    readSmall,
    unsafeModifyHyper,
    unsafeModifySmall,
    unsafeReadHyper,
    unsafeReadSmall,
    unsafeWriteHyper,
    unsafeWriteSmall,
    writeHyper,
    writeSmall,
  )
import Math.NumberTheory.MemoHyper.Mutable qualified as MMemoHyper
import Math.NumberTheory.Mobius (mertensVec, mobius', mobiusChimera)
import Math.NumberTheory.Roots (integerRoot, integerSquareRoot)
import Math.NumberTheory.Summation.Internal
  ( numSquarefree,
    sumSquarefree,
    sumTotient,
  )
import SublinearSummation.Util
  ( int2Word,
    primePiVec,
    primeSumVec,
    primesVec,
    word2Int,
  )

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
  (G.Vector v b, Num b) => (Word -> b) -> [b] -> Word -> MemoHyper v b
memoHyperSigmaHyper f vals n = runST (memoHyperSigmaHyperST f vals n)

memoHyperSigmaHyperST ::
  forall v s b.
  (G.Vector v b, Num b) =>
  (Word -> b) ->
  [b] ->
  Word ->
  ST s (MemoHyper v b)
memoHyperSigmaHyperST f vals n = do
  let n23 :: Word
      n23 = pow23 n

      sq :: Word
      sq = integerSquareRoot n

      mertH :: MemoHyper v b
      mertH = memoHyperMertens n

  mmh <- MMemoHyper.new n
  let fillLower :: [b] -> ST s ()
      fillLower vs = do
        let lowerVals = take (word2Int sq) vs
        forM_ (zip [1 ..] lowerVals) (uncurry (writeSmall mmh))

      fillUpper :: [b] -> ST s ()
      fillUpper vs = do
        let indices =
              map (\x -> x - 1) $
                takeWhile (<= n23) $
                  map (n `quot`) [sq, sq - 1 .. 1]
        forM_
          (zip [sq, sq - 1 .. 1] (getIndices (map word2Int indices) vs))
          (uncurry (writeHyper mmh))

      calc :: Word -> ST s b
      calc i = do
        let k :: Word -> ST s b
            k j =
              if j == 1
                then pure 0
                else readHyper mmh (i * j)

            gM :: Word -> ST s b
            gM = readSmall mmh

            diff_gM :: Word -> ST s b
            diff_gM = diffM gM

        (gsum :: b) <-
          hyperConvolveFastM
            (pure . fromIntegral . mobius')
            (pure . unMemoHyper mertH . (* i))
            diff_gM
            k
            (n `quot` i)
        pure (f (n `quot` i) - gsum)

  fillLower vals
  fillUpper vals
  let is = dropWhile (\i -> n `quot` i <= n23) [sq, sq - 1 .. 1]
  forM_ is $ \i -> do
    x <- calc i
    writeHyper mmh i x

  freeze mmh

-- | Given \(f : \mathbb{N}^{+} \to \mathbf{B}\), define
-- \[g(n) = \sum_{i=1}^{n}
--     \mu(i) f\left(\left\lfloor \frac{n}{i} \right\rfloor\right)\]
-- This function memoizes
-- \(x \mapsto g \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\),
-- when provided @f@, and a list of small values of @g@.
memoHyperSigmaMobiusHyper ::
  (G.Vector v b, Num b) => (Word -> b) -> [b] -> Word -> MemoHyper v b
memoHyperSigmaMobiusHyper f vals n =
  runST (memoHyperSigmaMobiusHyperST f vals n)

memoHyperSigmaMobiusHyperST ::
  forall v s b.
  (G.Vector v b, Num b) =>
  (Word -> b) ->
  [b] ->
  Word ->
  ST s (MemoHyper v b)
memoHyperSigmaMobiusHyperST f vals n = do
  let n23 :: Word
      n23 = pow23 n

      sq :: Word
      sq = integerSquareRoot n

  mmh <- MMemoHyper.new n
  let fillLower :: [b] -> ST s ()
      fillLower vs = do
        let lowerVals = take (word2Int sq) vs
        forM_ (zip [1 ..] lowerVals) (uncurry (writeSmall mmh))

      fillUpper :: [b] -> ST s ()
      fillUpper vs = do
        let indices =
              map (\x -> x - 1) $
                takeWhile (<= n23) $
                  map (n `quot`) [sq, sq - 1 .. 1]
        forM_
          (zip [sq, sq - 1 .. 1] (getIndices (map word2Int indices) vs))
          (uncurry (writeHyper mmh))

      calc :: Word -> ST s b
      calc i = do
        let k :: Word -> ST s b
            k j =
              if j == 1
                then pure 0
                else readHyper mmh (i * j)

            gM :: Word -> ST s b
            gM = readSmall mmh

            diff_gM :: Word -> ST s b
            diff_gM = diffM gM
        let nqi = n `quot` i

        (gsum :: b) <-
          hyperConvolveFastM
            (const (pure 1))
            (pure . fromIntegral . (nqi `quot`))
            diff_gM
            k
            (n `quot` i)
        pure (f (n `quot` i) - gsum)

  fillLower vals
  fillUpper vals
  let is = dropWhile (\i -> n `quot` i <= n23) [sq, sq - 1 .. 1]
  forM_ is $ \i -> do
    x <- calc i
    writeHyper mmh i x

  freeze mmh

getIndices :: [Int] -> [a] -> [a]
getIndices xs = go xs 0
  where
    go :: [Int] -> Int -> [a] -> [a]
    go [] _ _ = []
    go (i : is) j (y : ys) =
      if i == j
        then y : go is (j + 1) ys
        else go (i : is) (j + 1) ys
    go _ _ _ = error "memoHyperSigmaMobiusHyper: oops"

-- | Given \(f, g : \mathbb{N}^{+} \to \mathbf{B}\), define
-- \[h(n) = \sum_{i=1}^{n}
--     f(i) g\left(\left\lfloor \frac{n}{i} \right\rfloor\right)\]
-- This function memoizes
-- \(x \mapsto h \left(\left\lfloor \frac{n}{x} \right\rfloor\right)\),
-- when provided a 'MemoHyper' for the 'sigma' of the Dirichlet inverse of @f@,
-- an implementation of @g@, and a list of small values of @h@. Also, @f 1@
-- should be @1@.
memoHyperHyperConvolve ::
  (G.Vector u b, G.Vector v b, Num b) =>
  MemoHyper u b ->
  (Word -> b) ->
  [b] ->
  Word ->
  MemoHyper v b
memoHyperHyperConvolve mhSigmaFInv g hVals n =
  runST (memoHyperHyperConvolveST mhSigmaFInv g hVals n)

memoHyperHyperConvolveST ::
  forall u v s b.
  (G.Vector u b, G.Vector v b, Num b) =>
  MemoHyper u b ->
  (Word -> b) ->
  [b] ->
  Word ->
  ST s (MemoHyper v b)
memoHyperHyperConvolveST mhSigmaFInv g hVals n = do
  let n23 :: Word
      n23 = pow23 n

      sq :: Word
      sq = integerSquareRoot n

  mmh <- MMemoHyper.new n
  let fillLower :: [b] -> ST s ()
      fillLower vs = do
        let lowerVals = take (word2Int sq) vs
        forM_ (zip [1 ..] lowerVals) (uncurry (writeSmall mmh))

      fillUpper :: [b] -> ST s ()
      fillUpper vs = do
        let indices =
              map (\x -> x - 1) $
                takeWhile (<= n23) $
                  map (n `quot`) [sq, sq - 1 .. 1]
        forM_
          (zip [sq, sq - 1 .. 1] (getIndices (map word2Int indices) vs))
          (uncurry (writeHyper mmh))

      calc :: Word -> ST s b
      calc i = do
        let hyperHPatched :: Word -> ST s b
            hyperHPatched j =
              if j == 1
                then pure 0
                else readHyper mmh (i * j)

            hM :: Word -> ST s b
            hM = readSmall mmh

            diff_hM :: Word -> ST s b
            diff_hM = diffM hM

            sigmaFInv :: Word -> b
            sigmaFInv = unMemoSmall mhSigmaFInv

            fInv :: Word -> b
            fInv = diff sigmaFInv

            fInvM :: Word -> ST s b
            fInvM = pure . fInv

            hyperSigmaFInv :: Word -> b
            hyperSigmaFInv j = unMemoHyper mhSigmaFInv (i * j)

            hyperSigmaFInvM :: Word -> ST s b
            hyperSigmaFInvM = pure . hyperSigmaFInv

        let nqi = n `quot` i

        (conv :: b) <-
          hyperConvolveFastM
            fInvM
            hyperSigmaFInvM
            diff_hM
            hyperHPatched
            nqi
        pure (g nqi - conv)

  fillLower hVals
  fillUpper hVals
  let is = dropWhile (\i -> n `quot` i <= n23) [sq, sq - 1 .. 1]
  forM_ is $ \i -> do
    x <- calc i
    writeHyper mmh i x

  freeze mmh

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
  (G.Vector v a, Num a) =>
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
memoHyperMertens :: (G.Vector v a, Num a) => Word -> MemoHyper v a
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
memoHyperSumNumDivisors :: (Num a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumNumDivisors n =
  let sumNumDivisorsList =
        map fromIntegral (G.toList (G.scanl1 (+) (numDivisorsVec 1 n)))
   in memoHyperSigmaHyper fromIntegral sumNumDivisorsList n

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumSumDivisors'.
memoHyperSumSumDivisors ::
  forall v a. (Num a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumSumDivisors n =
  let xs :: [a]
      xs =
        Chimera.toInfinite mobiusChimera
          & Infinite.tail
          & Infinite.toList
          & zipWith (*) [1 ..]
          & map fromIntegral
          & scanl1 (+)

      mhSigmaFInv :: VMemoHyper a
      mhSigmaFInv =
        memoHyperHyperConvolve
          (memoHyper (sigma fromIntegral) n :: VMemoHyper a)
          (const 1)
          xs
          n

      sumSumDivisorsList :: [a]
      sumSumDivisorsList =
        map fromIntegral (G.toList (G.scanl1 (+) (sumDivisorsVec 1 n)))
   in memoHyperHyperConvolve mhSigmaFInv fromIntegral sumSumDivisorsList n

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.sumTotient'.
memoHyperSumTotient :: (Num a, G.Vector v a) => Word -> MemoHyper v a
memoHyperSumTotient n =
  let n23 :: Word
      n23 = pow23 n

      stvec :: U.Vector Int
      stvec = sumTotientVec 0 n23

      stotient :: Word -> Int
      stotient t = stvec ! word2Int t
   in memoHyperFixST n $ \_ i ->
        let nqi = n `quot` i
         in pure $
              if nqi <= n23
                then fromIntegral (stotient nqi)
                else fromIntegral (sumTotient nqi)

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.numSquarefree'.
memoHyperNumSquarefree :: (Num a, G.Vector v a) => Word -> MemoHyper v a
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
memoHyperSumSquarefree :: (Num a, G.Vector v a) => Word -> MemoHyper v a
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
    let indices1 = [1 .. min sq nqiMin]
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

    let indices2 =
          [sq, sq - 1 .. 1]
            & zip (quotAll p sq)
            & takeWhile ((>= iMin) . snd)

    forM_ indices2 $ \(iqp, i) -> do
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

-- quotAll p x returns [x `quot` p, (x - 1) `quot` p, .. 1 `quot` p]
quotAll :: Word -> Word -> [Word]
quotAll p x =
  let (q, r) = x `quotRem` p
   in if q == 0
        then genericReplicate r 0
        else
          genericReplicate (r + 1) q
            ++ concatMap
              (genericReplicate p)
              ( takeWhile
                  (> 0)
                  (iterate (\t -> t - 1) (q - 1))
              )
            ++ genericReplicate (p - 1) 0

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

memoHyperRoughSum :: Word -> VMemoHyper (U.Vector Int)
memoHyperRoughSum n = runST (memoHyperRoughSumST n)

memoHyperRoughSumST :: forall s. Word -> ST s (VMemoHyper (U.Vector Int))
memoHyperRoughSumST n = do
  let sq = integerSquareRoot n
      ps = primesVec 0 sq
      sumOfPrimes = G.scanl (+) 0 ps
      ppi = primePiVec sq
      pps = primeSumVec sq

  mfv :: MV.MVector s (U.Vector Int) <- MV.replicate (word2Int sq) U.empty
  mhv :: MV.MVector s (U.Vector Int) <- MV.replicate (word2Int sq) U.empty

  forM_ [1 .. sq] $ \i -> do
    let bMax = ppi ! word2Int (integerSquareRoot i)

    (xs :: U.Vector Int) <-
      U.generateM (word2Int bMax + 1) $ \b ->
        let p = word2Int (ps ! (b - 1))
         in if b == 0
              then pure $ word2Int (i * (i + 1) `quot` 2)
              else do
                let i' = word2Int i
                rv <- MV.read mfv ((i' `quot` p) - 1)
                let iqp = i' `quot` p
                    sq_iqp = integerSquareRoot iqp
                    b_sq_iqp = word2Int $ ppi ! sq_iqp
                    p_sum_iqp =
                      (rv ! b_sq_iqp)
                        + word2Int (pps ! sq_iqp)
                        - 1
                    x = word2Int $ sumOfPrimes ! (b - 1)

                let fallback = p_sum_iqp - x + 1

                let ans = fromMaybe fallback (rv !? (b - 1))
                pure (-(p * ans))
    let ys :: U.Vector Int
        ys = G.scanl1 (+) xs
    MV.write mfv (toIndex i) ys

  forM_ [sq, sq - 1 .. 1] $ \i -> do
    let nqi = n `quot` i
    let bMax = ppi ! word2Int (integerSquareRoot nqi)
    let sq' = word2Int sq
    let n' = word2Int n
    let nqi' = word2Int nqi

    (xs :: U.Vector Int) <-
      U.generateM (word2Int bMax + 1) $ \b ->
        let p = word2Int (ps ! (b - 1))
         in if b == 0
              then pure $ word2Int (nqi * (nqi + 1) `quot` 2)
              else do
                let nqip' = nqi' `quot` p
                rv <-
                  if nqip' <= sq'
                    then MV.read mfv (nqip' - 1)
                    else MV.read mhv ((n' `quot` nqip') - 1)
                let sq_nqip = integerSquareRoot nqip'
                    b_sq_nqip = word2Int $ ppi ! sq_nqip
                    p_sum_nqip =
                      (rv ! b_sq_nqip)
                        + word2Int (pps ! sq_nqip)
                        - 1
                    x = word2Int $ sumOfPrimes ! (b - 1)

                let fallback = p_sum_nqip - x + 1

                let ans = fromMaybe fallback (rv !? (b - 1))
                pure (-(p * ans))
    let ys :: U.Vector Int
        ys = G.scanl1 (+) xs
    MV.write mhv (toIndex i) ys

  let mh =
        MMemoHyper
          { mmhLimit = n,
            mmhSqrtLimit = sq,
            mmhFuncVec = mfv,
            mmhHyperVec = mhv
          }
  freeze mh

-- | A 'MemoHyper' for 'Math.NumberTheory.Summations.primeSum'.
memoHyperPrimeSum :: Word -> UMemoHyper Word
memoHyperPrimeSum n = runST $ do
  (roughSum_mmh :: UMMemoHyper s Word) <- MMemoHyper.new n
  (primeSum_mmh :: UMMemoHyper s Word) <- MMemoHyper.new n
  let sq = integerSquareRoot n
  let ps = primesVec 0 sq
  let sumOfPrimes = G.scanl (+) 0 ps
  forM_ [1 .. sq] $ \i -> do
    unsafeWriteSmall roughSum_mmh i (i * (i + 1) `quot` 2)
    let nqi = n `quot` i
    unsafeWriteHyper roughSum_mmh i (nqi * (nqi + 1) `quot` 2)
  let ppi = primePiVec sq
  let psum = primeSumVec sq
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
          roughSum <- unsafeReadSmall roughSum_mmh i
          let pi' = psum `unsafeIndex` integerSquareRoot (word2Int i)
          unsafeWriteSmall primeSum_mmh i (roughSum + pi' - 1)

        forM_ doneIndicesHi $ \i -> do
          let nqi = n `quot` i
          phi <- unsafeReadHyper roughSum_mmh nqi
          let pi' = psum `unsafeIndex` integerSquareRoot (word2Int i)
          unsafeWriteHyper primeSum_mmh nqi (phi + pi' - 1)

  writeDone 0
  forM_ [1 .. bMax] $ \b -> do
    let p = ps `unsafeIndex` (word2Int b - 1)
    let iMin = square (ps `unsafeIndex` word2Int (b - 1))

    let nqiMin = n `quot` iMin
    let indices1 = [1 .. min sq nqiMin]
    forM_ indices1 $ \nqi -> do
      let iqp = n `quot` (nqi * p)
      let tooBig = b > ppi `unsafeIndex` integerSquareRoot (word2Int iqp)
      right <-
        if tooBig
          then do
            pi_iqp <- unsafeReadHyper primeSum_mmh (nqi * p)
            let corr = sumOfPrimes ! (word2Int b - 1)
            pure $ pi_iqp - corr + 1
          else unsafeReadHyper roughSum_mmh (nqi * p)
      unsafeModifyHyper roughSum_mmh (\x -> x - p * right) nqi

    let indices2 =
          [sq, sq - 1 .. 1]
            & zip (quotAll p sq)
            & takeWhile ((>= iMin) . snd)

    forM_ indices2 $ \(iqp, i) -> do
      let tooBig = b > ppi `unsafeIndex` integerSquareRoot (word2Int iqp)
      right <-
        if tooBig
          then do
            psum_iqp <- unsafeReadSmall primeSum_mmh iqp
            let corr = sumOfPrimes ! (word2Int b - 1)
            pure $ psum_iqp - corr + 1
          else unsafeReadSmall roughSum_mmh iqp
      unsafeModifySmall roughSum_mmh (\x -> x - p * right) i

    writeDone b

  freeze primeSum_mmh

-- | A 'MemoHyper' for 'Math.NumberTheory.Roots.integerSquareRoot'
memoHyperIntegerSquareRoot ::
  (G.Vector v a, Num a) => Word -> MemoHyper v a
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
