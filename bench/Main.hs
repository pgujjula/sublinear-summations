-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Bench.Math.NumberTheory.HyperbolicConvolution (benchmarks)
import Bench.Math.NumberTheory.MemoHyper (benchmarks)
import Bench.Math.NumberTheory.Summation (benchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Math.NumberTheory.HyperbolicConvolution.benchmarks,
      Bench.Math.NumberTheory.MemoHyper.benchmarks,
      Bench.Math.NumberTheory.Summation.benchmarks
    ]
