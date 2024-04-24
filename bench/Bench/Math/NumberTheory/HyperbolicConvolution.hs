-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.HyperbolicConvolution (benchmarks) where

import Bench.Util (todoBenchmark)
import Test.Tasty.Bench (Benchmark, bgroup)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.HyperbolicConvolution"
    [ bgroup
        "Basic combinators"
        [ hyperBenchmarks,
          sigmaBenchmarks,
          diffBenchmarks,
          mulBenchmarks
        ],
      bgroup
        "Computing hyperbolic convolutions"
        [ hyperConvolveBenchmarks,
          hyperConvolveFastBenchmarks,
          hyperConvolveMobiusFastBenchmarks
        ]
    ]

--
-- Basic combinators
--

hyperBenchmarks :: Benchmark
hyperBenchmarks = todoBenchmark "hyper"

sigmaBenchmarks :: Benchmark
sigmaBenchmarks = todoBenchmark "sigma"

diffBenchmarks :: Benchmark
diffBenchmarks = todoBenchmark "diff"

mulBenchmarks :: Benchmark
mulBenchmarks = todoBenchmark "mul"

--
-- Computing hyperbolic convolutions
--

hyperConvolveBenchmarks :: Benchmark
hyperConvolveBenchmarks = todoBenchmark "hyperConvolve"

hyperConvolveFastBenchmarks :: Benchmark
hyperConvolveFastBenchmarks = todoBenchmark "hyperConvolveFast"

hyperConvolveMobiusFastBenchmarks :: Benchmark
hyperConvolveMobiusFastBenchmarks = todoBenchmark "hyperConvolveMobius"
