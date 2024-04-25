-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.Mobius (benchmarks) where

import Data.Vector.Generic qualified as Vector
import Math.NumberTheory.Mobius
  ( mobiusVec,
    mobiusVec',
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.Mobius"
    [ mobiusVecBenchmarks,
      mobiusVec'Benchmarks
    ]

mobiusVecBenchmarks :: Benchmark
mobiusVecBenchmarks =
  bgroup "mobiusVec" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf (\j -> Vector.last (mobiusVec j (2 * j))) (10 ^ i)

mobiusVec'Benchmarks :: Benchmark
mobiusVec'Benchmarks =
  bgroup "mobiusVec'" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf (\j -> Vector.last (mobiusVec' j (2 * j))) (10 ^ i)
