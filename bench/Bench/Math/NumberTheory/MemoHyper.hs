-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.MemoHyper (benchmarks) where

import Bench.Util (todoBenchmark)
import Test.Tasty.Bench (Benchmark, bgroup)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Math.NumberTheory.MemoHyper"
    [ bgroup
        "Index MemoHyper"
        [ unMemoHyperBenchmarks,
          unMemoSmallBenchmarks
        ],
      bgroup
        "Construct MemoHypers"
        [ memoHyperBenchmarks,
          memoHyperDirectBenchmarks,
          memoHyperSigmaHyperBenchmarks,
          memoHyperSigmaMobiusHyperBenchmarks
        ],
      bgroup
        "MemoHyper for arithmetic functions"
        [ bgroup
            "Divisor functions"
            [ memoHyperSumNumDivisorsBenchmarks,
              memoHyperSumSumDivisorsBenchmarks,
              memoHyperSumTotientBenchmarks
            ],
          bgroup
            "Primes"
            [ memoHyperPrimePiBenchmarks,
              memoHyperPrimeSumBenchmarks
            ],
          bgroup
            "Square-free integers"
            [ memoHyperMertensBenchmarks,
              memoHyperNumSquarefreeBenchmarks,
              memoHyperSumSquarefreeBenchmarks
            ]
        ]
    ]

--
-- Index MemoHyper
--

unMemoHyperBenchmarks :: Benchmark
unMemoHyperBenchmarks = todoBenchmark "unMemoHyper"

unMemoSmallBenchmarks :: Benchmark
unMemoSmallBenchmarks = todoBenchmark "unMemoSmall"

--
-- Construct MemoHyper
--

memoHyperBenchmarks :: Benchmark
memoHyperBenchmarks = todoBenchmark "memoHyper"

memoHyperDirectBenchmarks :: Benchmark
memoHyperDirectBenchmarks = todoBenchmark "memoHyperDirect"

memoHyperSigmaHyperBenchmarks :: Benchmark
memoHyperSigmaHyperBenchmarks = todoBenchmark "memoHyperSigmaHyper"

memoHyperSigmaMobiusHyperBenchmarks :: Benchmark
memoHyperSigmaMobiusHyperBenchmarks = todoBenchmark "memoHyperSigmaMobiusHyper"

--
-- MemoHyper for arithmetic functions
--

-- Divisor functions

memoHyperSumNumDivisorsBenchmarks :: Benchmark
memoHyperSumNumDivisorsBenchmarks = todoBenchmark "memoHyperSumNumDivisors"

memoHyperSumSumDivisorsBenchmarks :: Benchmark
memoHyperSumSumDivisorsBenchmarks = todoBenchmark "memoHyperSumSumDivisors"

memoHyperSumTotientBenchmarks :: Benchmark
memoHyperSumTotientBenchmarks = todoBenchmark "memoHyperSumTotient"

-- Primes

memoHyperPrimePiBenchmarks :: Benchmark
memoHyperPrimePiBenchmarks = todoBenchmark "memoHyperPrimePi"

memoHyperPrimeSumBenchmarks :: Benchmark
memoHyperPrimeSumBenchmarks = todoBenchmark "memoHyperPrimeSum"

-- Square-free integers

memoHyperMertensBenchmarks :: Benchmark
memoHyperMertensBenchmarks = todoBenchmark "memoHyperMertens"

memoHyperNumSquarefreeBenchmarks :: Benchmark
memoHyperNumSquarefreeBenchmarks = todoBenchmark "memoHyperNumSquarefree"

memoHyperSumSquarefreeBenchmarks :: Benchmark
memoHyperSumSquarefreeBenchmarks = todoBenchmark "memoHyperSumSquarefree"
