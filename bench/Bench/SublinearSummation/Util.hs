-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.SublinearSummation.Util (benchmarks) where

import Data.Vector.Generic qualified as Vector
import SublinearSummation.Util (primeSumVec, primesFrom)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "SublinearSummation.Util"
    [primesToBenchmarks, primeSumVecBenchmarks]

primesToBenchmarks :: Benchmark
primesToBenchmarks =
  bgroup "primesTo" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf ((!! (10 ^ i)) . primesFrom) (10 ^ i)

primeSumVecBenchmarks :: Benchmark
primeSumVecBenchmarks =
  bgroup "primeSumVec" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf (Vector.last . primeSumVec) (10 ^ i)
