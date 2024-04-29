-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.MemoHyper.Internal (benchmarks) where

import Bench.Util (todoBenchmark)
import Data.Vector.Generic qualified as Vector
import Math.NumberTheory.MemoHyper.Internal (sumTotientVec, totientVec)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.MemoHyper.Internal"
    [ numSquarefreeVecBenchmarks,
      sumSquarefreeVecBenchmarks,
      totientVecBenchmarks,
      sumTotientVecBenchmarks
    ]

numSquarefreeVecBenchmarks :: Benchmark
numSquarefreeVecBenchmarks = todoBenchmark "numSquarefreeVec"

sumSquarefreeVecBenchmarks :: Benchmark
sumSquarefreeVecBenchmarks = todoBenchmark "sumSquarefreeVec"

totientVecBenchmarks :: Benchmark
totientVecBenchmarks =
  bgroup "totientVec" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf (Vector.last . totientVec 0) (10 ^ i)

sumTotientVecBenchmarks :: Benchmark
sumTotientVecBenchmarks =
  bgroup "sumTotientVec" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf (Vector.last . sumTotientVec 0) (10 ^ i)
