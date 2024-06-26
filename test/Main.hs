-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Test.Math.NumberTheory.HyperbolicConvolution qualified (tests)
import Test.Math.NumberTheory.MemoHyper qualified (tests)
import Test.Math.NumberTheory.MemoHyper.Internal qualified (tests)
import Test.Math.NumberTheory.Mobius qualified (tests)
import Test.Math.NumberTheory.Summation qualified (tests)
import Test.SublinearSummation.Util qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "sublinear-summations"
    [ Test.Math.NumberTheory.HyperbolicConvolution.tests,
      Test.Math.NumberTheory.MemoHyper.tests,
      Test.Math.NumberTheory.MemoHyper.Internal.tests,
      Test.Math.NumberTheory.Mobius.tests,
      Test.Math.NumberTheory.Summation.tests,
      Test.SublinearSummation.Util.tests
    ]
