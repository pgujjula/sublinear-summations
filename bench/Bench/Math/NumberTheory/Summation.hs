-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.Summation (benchmarks) where

import Bench.Util (todoBenchmark)
import Math.NumberTheory.Summation
  ( mertens,
    numSquarefree,
    sumNumDivisors,
    sumSquarefree,
    sumSumDivisors,
    sumTotient,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

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
        [ primeSumBenchmarks
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
sumNumDivisorsBenchmarks =
  bgroup "sumNumDivisors" $ flip map [(1 :: Int) .. 12] $ \i ->
    bench ("10^" ++ show i) $
      nf sumNumDivisors (10 ^ i :: Int)

sumSumDivisorsBenchmarks :: Benchmark
sumSumDivisorsBenchmarks =
  bgroup "sumSumDivisors" $ flip map [(1 :: Int) .. 12] $ \i ->
    bench ("10^" ++ show i) $
      nf sumSumDivisors (10 ^ i :: Int)

sumTotientBenchmarks :: Benchmark
sumTotientBenchmarks =
  bgroup "sumTotient" $ flip map [(1 :: Int) .. 7] $ \i ->
    bench ("10^" ++ show i) $
      nf sumTotient (10 ^ i :: Int)

--
-- Primes
--

primeSumBenchmarks :: Benchmark
primeSumBenchmarks = todoBenchmark "primeSum"

--
-- Square-free integers
--

mertensBenchmarks :: Benchmark
mertensBenchmarks =
  bgroup "mertens" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf mertens (10 ^ i :: Int)

numSquarefreeBenchmarks :: Benchmark
numSquarefreeBenchmarks =
  bgroup "numSquarefree" $ flip map [(1 :: Int) .. 12] $ \i ->
    bench ("10^" ++ show i) $
      nf numSquarefree (10 ^ i :: Int)

sumSquarefreeBenchmarks :: Benchmark
sumSquarefreeBenchmarks =
  bgroup "sumSquarefree" $ flip map [(1 :: Int) .. 12] $ \i ->
    bench ("10^" ++ show i) $
      nf sumSquarefree (10 ^ i :: Int)
