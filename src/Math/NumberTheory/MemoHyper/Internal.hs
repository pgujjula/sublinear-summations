-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.MemoHyper.Internal
  ( numSquarefreeVec,
    sumSquarefreeVec,
  )
where

import Data.Vector.Generic qualified as G
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.Mobius (mobiusVec)
import SublinearSummation.Util (word2Int)

numSquarefreeVec :: Word -> Word -> U.Vector Int
numSquarefreeVec n m =
  G.drop (word2Int n) . G.scanl1 (+) . G.map abs $ mobiusVec 0 m

sumSquarefreeVec :: Word -> Word -> U.Vector Int
sumSquarefreeVec n m =
  G.drop (word2Int n) . G.scanl1 (+) . G.imap (*) . G.map abs $ mobiusVec 0 m
