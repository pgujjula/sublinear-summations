-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Bench.Math.NumberTheory.Summation (benchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ Bench.Math.NumberTheory.Summation.benchmarks
    ]
