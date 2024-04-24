-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.HyperbolicConvolution (tests) where

import Math.NumberTheory.HyperbolicConvolution (hyper)
import Math.NumberTheory.Roots (integerSquareRoot)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util (todoTest)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.HyperbolicConvolution"
    [ testGroup
        "Basic combinators"
        [ hyperTests,
          sigmaTests,
          diffTests,
          mulTests
        ],
      testGroup
        "Computing hyperbolic convolutions"
        [ hyperConvolveTests,
          hyperConvolveFastTests,
          hyperConvolveMobiusFastTests
        ]
    ]

--
-- Basic combinators
--

hyperTests :: TestTree
hyperTests =
  testGroup
    "hyper"
    [ testCase "id" $
        map (hyper 10 id) [1 .. 10]
          @?= [10, 5, 3, 2, 2, 1, 1, 1, 1, 1],
      testCase "integerSquareRoot" $
        map (hyper 10 integerSquareRoot) [1 .. 10]
          @?= [3, 2, 1, 1, 1, 1, 1, 1, 1, 1]
    ]

sigmaTests :: TestTree
sigmaTests = todoTest "sigma"

diffTests :: TestTree
diffTests = todoTest "diff"

mulTests :: TestTree
mulTests = todoTest "mul"

--
-- Computing hyperbolic convolutions
--

hyperConvolveTests :: TestTree
hyperConvolveTests = todoTest "hyperConvolve"

hyperConvolveFastTests :: TestTree
hyperConvolveFastTests = todoTest "hyperConvolveFast"

hyperConvolveMobiusFastTests :: TestTree
hyperConvolveMobiusFastTests = todoTest "hyperConvolveMobiusFast"
