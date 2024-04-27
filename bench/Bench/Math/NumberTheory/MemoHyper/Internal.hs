-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.MemoHyper.Internal (benchmarks) where

import Bench.Util (todoBenchmark)
import Test.Tasty.Bench (Benchmark, bgroup)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.MemoHyper.Internal"
    [ numSquarefreeVecBenchmarks,
      sumSquarefreeVecBenchmarks,
      totientVecBenchmarks
    ]

numSquarefreeVecBenchmarks :: Benchmark
numSquarefreeVecBenchmarks = todoBenchmark "numSquarefreeVec"

sumSquarefreeVecBenchmarks :: Benchmark
sumSquarefreeVecBenchmarks = todoBenchmark "sumSquarefreeVec"

totientVecBenchmarks :: Benchmark
totientVecBenchmarks = todoBenchmark "totientVec"
