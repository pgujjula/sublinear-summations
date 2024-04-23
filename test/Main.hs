-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Test.Math.NumberTheory.Summation qualified (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "sublinear-summations"
    [ Test.Math.NumberTheory.Summation.tests
    ]
