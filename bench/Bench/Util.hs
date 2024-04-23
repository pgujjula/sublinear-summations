-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Bench.Util (todoBenchmark) where

import Test.Tasty.Bench (Benchmark)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase)

todoBenchmark :: (HasCallStack) => String -> Benchmark
todoBenchmark message =
  expectFailBecause "unimplemented benchmark" $
    testCase message (assertFailure "")
