-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.SublinearSummation.Util (benchmarks) where

import SublinearSummation.Util (primesFrom)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "SublinearSummation.Util"
    [primesToBenchmarks]

primesToBenchmarks :: Benchmark
primesToBenchmarks =
  bgroup "primesTo" $ flip map [(1 :: Int) .. 8] $ \i ->
    bench ("10^" ++ show i) $
      nf ((!! (10 ^ i)) . primesFrom) (10 ^ i)
