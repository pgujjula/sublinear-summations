-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.Summation (benchmarks) where

import Bench.Util (todoBenchmark)
import Test.Tasty.Bench (Benchmark, bgroup)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.Summation"
    [ bgroup
        "Divisor functions"
        [ sumNumDivisorsBenchmarks,
          sumSumDivisorsBenchmarks,
          sumTotientBenchmarks
        ],
      bgroup
        "Primes"
        [ primePiBenchmarks,
          primeSumBenchmarks
        ],
      bgroup
        "Square-free integers"
        [ mertensBenchmarks,
          numSquarefreeBenchmarks,
          sumSquarefreeBenchmarks
        ]
    ]

--
-- Divisor functions
--

sumNumDivisorsBenchmarks :: Benchmark
sumNumDivisorsBenchmarks = todoBenchmark "sumNumDivisors"

sumSumDivisorsBenchmarks :: Benchmark
sumSumDivisorsBenchmarks = todoBenchmark "sumSumDivisors"

sumTotientBenchmarks :: Benchmark
sumTotientBenchmarks = todoBenchmark "sumTotient"

--
-- Primes
--

primePiBenchmarks :: Benchmark
primePiBenchmarks = todoBenchmark "primePi"

primeSumBenchmarks :: Benchmark
primeSumBenchmarks = todoBenchmark "primeSum"

--
-- Square-free integers
--

mertensBenchmarks :: Benchmark
mertensBenchmarks = todoBenchmark "mertens"

numSquarefreeBenchmarks :: Benchmark
numSquarefreeBenchmarks = todoBenchmark "numSquarefree"

sumSquarefreeBenchmarks :: Benchmark
sumSquarefreeBenchmarks = todoBenchmark "sumSquarefree"
