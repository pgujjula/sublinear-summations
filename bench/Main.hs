-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Bench.Math.NumberTheory.HyperbolicConvolution (benchmarks)
import Bench.Math.NumberTheory.MemoHyper (benchmarks)
import Bench.Math.NumberTheory.MemoHyper.Internal (benchmarks)
import Bench.Math.NumberTheory.Mobius (benchmarks)
import Bench.Math.NumberTheory.Summation (benchmarks)
import Bench.SublinearSummation.Util (benchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Math.NumberTheory.HyperbolicConvolution.benchmarks,
      Bench.Math.NumberTheory.MemoHyper.benchmarks,
      Bench.Math.NumberTheory.MemoHyper.Internal.benchmarks,
      Bench.Math.NumberTheory.Mobius.benchmarks,
      Bench.Math.NumberTheory.Summation.benchmarks,
      Bench.SublinearSummation.Util.benchmarks
    ]
