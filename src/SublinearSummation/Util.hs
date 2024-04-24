-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module SublinearSummation.Util (word2Int, int2Word) where

import Control.Placeholder (todo)

word2Int :: Word -> Int
word2Int = fromIntegral
{-# INLINE word2Int #-}

int2Word :: Int -> Word
int2Word = fromIntegral
{-# INLINE int2Word #-}

primes :: [Int]
primes = todo
