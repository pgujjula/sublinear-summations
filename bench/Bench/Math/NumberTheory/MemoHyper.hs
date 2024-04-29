-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Math.NumberTheory.MemoHyper (benchmarks) where

import Bench.Util (todoBenchmark)
import Data.Vector.Generic qualified as G
import Math.NumberTheory.MemoHyper
  ( MemoHyper (..),
    UMemoHyper,
    memoHyperIntegerSquareRoot,
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperPrimePi,
    memoHyperSumSquarefree,
    memoHyperSumTotient,
    unMemoHyper,
  )
import Math.NumberTheory.Prime.Count (primePi)
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (word2Int)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

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
              memoHyperPrimePiNaiveBenchmarks,
              memoHyperPrimeSumBenchmarks
            ],
          bgroup
            "Square-free integers"
            [ memoHyperMertensBenchmarks,
              memoHyperNumSquarefreeBenchmarks,
              memoHyperSumSquarefreeBenchmarks
            ],
          bgroup
            "Miscellaneous"
            [ memoHyperIntegerSquareRootBenchmarks
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

-- TODO: This benchmark runs out of memory for 10^7, 10^8, which shouldn't
-- happen.
memoHyperSumTotientBenchmarks :: Benchmark
memoHyperSumTotientBenchmarks =
  bgroup "memoHyperSumTotient" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf
        ( collectMemoHyper
            . (memoHyperSumTotient :: Word -> UMemoHyper Int)
        )
        (10 ^ i)

-- Primes

collectMemoHyper :: (G.Vector v a, Num a) => MemoHyper v a -> a
collectMemoHyper mh = G.sum (mhFuncVec mh) + G.sum (mhHyperVec mh)

memoHyperPrimePiBenchmarks :: Benchmark
memoHyperPrimePiBenchmarks =
  bgroup "memoHyperPrimePi" $ flip map [(1 :: Int) .. 10] $ \i ->
    bench ("10^" ++ show i) $
      nf
        (collectMemoHyper . memoHyperPrimePi)
        (10 ^ i)

memoHyperPrimePiNaiveBenchmarks :: Benchmark
memoHyperPrimePiNaiveBenchmarks =
  bgroup "memoHyperPrimePiNaive" $ flip map [(1 :: Int) .. 10] $ \i ->
    bench ("10^" ++ show i) $
      nf
        (collectMemoHyper . memoHyperPrimePiNaive)
        (10 ^ i)

memoHyperPrimePiNaive :: Word -> UMemoHyper Word
memoHyperPrimePiNaive n =
  let sq = integerSquareRoot n
   in MemoHyper
        { mhLimit = n,
          mhSqrtLimit = sq,
          mhFuncVec =
            G.fromListN (word2Int sq) . flip map [1 .. sq] $ \i ->
              primePi i,
          mhHyperVec =
            G.fromListN (word2Int sq) . flip map [1 .. sq] $ \i ->
              primePi (n `quot` i)
        }

memoHyperPrimeSumBenchmarks :: Benchmark
memoHyperPrimeSumBenchmarks = todoBenchmark "memoHyperPrimeSum"

-- Square-free integers

memoHyperMertensBenchmarks :: Benchmark
memoHyperMertensBenchmarks =
  bgroup "memoHyperMertens" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf
        (flip unMemoHyper 1 . (memoHyperMertens :: Word -> UMemoHyper Int))
        (10 ^ i)

memoHyperNumSquarefreeBenchmarks :: Benchmark
memoHyperNumSquarefreeBenchmarks =
  bgroup "memoHyperNumSquarefree" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf
        ( flip unMemoHyper 1
            . (memoHyperNumSquarefree :: Word -> UMemoHyper Int)
        )
        (10 ^ i)

memoHyperSumSquarefreeBenchmarks :: Benchmark
memoHyperSumSquarefreeBenchmarks =
  bgroup "memoHyperSumSquarefree" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf
        ( flip unMemoHyper 1
            . (memoHyperSumSquarefree :: Word -> UMemoHyper Int)
        )
        (10 ^ i)

memoHyperIntegerSquareRootBenchmarks :: Benchmark
memoHyperIntegerSquareRootBenchmarks =
  bgroup "memoHyperIntegerSquareRoot" $ flip map [(1 :: Int) .. 12] $ \i ->
    bench ("10^" ++ show i) $
      nf
        ( collectMemoHyper
            . (memoHyperIntegerSquareRoot :: Word -> UMemoHyper Int)
        )
        (10 ^ i)
