-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module: Math.NumberTheory.MemoHyper.Mutable
-- License: BSD-3-Clause
-- Maintainer: Preetham Gujjula <libraries@mail.preetham.io>
-- Stability: experimental
--
-- Mutable MemoHypers.
module Math.NumberTheory.MemoHyper.Mutable
  ( MMemoHyper (..),
    UMMemoHyper,
    VMMemoHyper,

    -- * Accessors

    -- ** Read
    readHyper,
    readHyperMaybe,
    readSmall,
    unsafeReadHyper,
    unsafeReadSmall,

    -- ** Write
    writeHyper,
    writeSmall,
    unsafeWriteHyper,
    unsafeWriteSmall,

    -- ** Modify
    modifyHyper,
    modifySmall,
    unsafeModifyHyper,
    unsafeModifySmall,

    -- * Construction
    new,
  )
where

import Data.CallStack (HasCallStack)
import Data.Vector.Generic.Mutable (MVector, PrimMonad, PrimState)
import Data.Vector.Generic.Mutable qualified as MVector
import Data.Vector.Mutable qualified as V
import Data.Vector.Unboxed.Mutable qualified as U
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (word2Int)

data MMemoHyper v s b = MMemoHyper
  { mmhLimit :: Word,
    mmhSqrtLimit :: Word,
    mmhFuncVec :: v s b,
    mmhHyperVec :: v s b
  }

-- | A @'MMemoHyper'@ backed by unboxed vectors.
type UMMemoHyper b = MMemoHyper U.MVector b

-- | A @'MMemoHyper'@ backed by boxed vectors.
type VMMemoHyper b = MMemoHyper V.MVector b

--
-- Construct MMemoHypers
--

new :: (PrimMonad m, MVector v a) => Word -> m (MMemoHyper v (PrimState m) a)
new n = do
  let sq = integerSquareRoot n
      sqInt = word2Int sq
  funcVec <- MVector.new sqInt
  hyperVec <- MVector.new sqInt
  pure $
    MMemoHyper
      { mmhLimit = n,
        mmhSqrtLimit = sq,
        mmhFuncVec = funcVec,
        mmhHyperVec = hyperVec
      }

-- | Read a 'MMemoHyper'.
readHyper ::
  (HasCallStack, PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  m b
readHyper mmh i =
  if i <= mmhSqrtLimit mmh
    then MVector.read (mmhHyperVec mmh) (toIndex i)
    else MVector.read (mmhFuncVec mmh) (toIndex (mmhLimit mmh `quot` i))
{-# INLINE readHyper #-}

readHyperMaybe ::
  (PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  m (Maybe b)
readHyperMaybe mmh i =
  if i <= mmhSqrtLimit mmh
    then MVector.readMaybe (mmhHyperVec mmh) (toIndex i)
    else MVector.readMaybe (mmhFuncVec mmh) (toIndex (mmhLimit mmh `quot` i))

readSmall ::
  (HasCallStack, PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  m b
readSmall mmh x = MVector.read (mmhFuncVec mmh) (toIndex x)
{-# INLINE readSmall #-}

-- | Read a 'MMemoHyper'.
unsafeReadHyper ::
  (PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  m b
unsafeReadHyper mmh i =
  if i <= mmhSqrtLimit mmh
    then MVector.unsafeRead (mmhHyperVec mmh) (toIndex i)
    else MVector.unsafeRead (mmhFuncVec mmh) (toIndex (mmhLimit mmh `quot` i))
{-# INLINE unsafeReadHyper #-}

unsafeReadSmall ::
  (PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  m b
unsafeReadSmall mmh x = MVector.unsafeRead (mmhFuncVec mmh) (toIndex x)
{-# INLINE unsafeReadSmall #-}

-- | Write a 'MMemoHyper'.
writeHyper ::
  (HasCallStack, PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  b ->
  m ()
writeHyper mmh i x =
  if i <= mmhSqrtLimit mmh
    then MVector.write (mmhHyperVec mmh) (toIndex i) x
    else MVector.write (mmhFuncVec mmh) (toIndex (mmhLimit mmh `quot` i)) x
{-# INLINE writeHyper #-}

writeSmall ::
  (HasCallStack, PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  b ->
  m ()
writeSmall mmh i = MVector.write (mmhFuncVec mmh) (toIndex i)
{-# INLINE writeSmall #-}

-- | Write a 'MMemoHyper'.
unsafeWriteHyper ::
  (PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  b ->
  m ()
unsafeWriteHyper mmh i x =
  if i <= mmhSqrtLimit mmh
    then MVector.unsafeWrite (mmhHyperVec mmh) (toIndex i) x
    else
      MVector.unsafeWrite
        (mmhFuncVec mmh)
        (toIndex (mmhLimit mmh `quot` i))
        x
{-# INLINE unsafeWriteHyper #-}

unsafeWriteSmall ::
  (PrimMonad m, MVector v b) =>
  MMemoHyper v (PrimState m) b ->
  Word ->
  b ->
  m ()
unsafeWriteSmall mmh i = MVector.unsafeWrite (mmhFuncVec mmh) (toIndex i)
{-# INLINE unsafeWriteSmall #-}

modifyHyper ::
  (HasCallStack, PrimMonad m, MVector v a) =>
  MMemoHyper v (PrimState m) a ->
  (a -> a) ->
  Word ->
  m ()
modifyHyper mmh f i =
  if i <= mmhSqrtLimit mmh
    then MVector.modify (mmhHyperVec mmh) f (toIndex i)
    else MVector.modify (mmhFuncVec mmh) f (toIndex (mmhLimit mmh `quot` i))
{-# INLINE modifyHyper #-}

modifySmall ::
  (HasCallStack, PrimMonad m, MVector v a) =>
  MMemoHyper v (PrimState m) a ->
  (a -> a) ->
  Word ->
  m ()
modifySmall mmh f i = MVector.modify (mmhFuncVec mmh) f (toIndex i)
{-# INLINE modifySmall #-}

unsafeModifyHyper ::
  (PrimMonad m, MVector v a) =>
  MMemoHyper v (PrimState m) a ->
  (a -> a) ->
  Word ->
  m ()
unsafeModifyHyper mmh f i =
  if i <= mmhSqrtLimit mmh
    then MVector.unsafeModify (mmhHyperVec mmh) f (toIndex i)
    else
      MVector.unsafeModify
        (mmhFuncVec mmh)
        f
        (toIndex (mmhLimit mmh `quot` i))
{-# INLINE unsafeModifyHyper #-}

unsafeModifySmall ::
  (PrimMonad m, MVector v a) =>
  MMemoHyper v (PrimState m) a ->
  (a -> a) ->
  Word ->
  m ()
unsafeModifySmall mmh f i = MVector.unsafeModify (mmhFuncVec mmh) f (toIndex i)
{-# INLINE unsafeModifySmall #-}

toIndex :: Word -> Int
toIndex k = word2Int k - 1
{-# INLINE toIndex #-}
