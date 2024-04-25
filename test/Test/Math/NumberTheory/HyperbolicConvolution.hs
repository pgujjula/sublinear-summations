-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.HyperbolicConvolution (tests) where

import Math.NumberTheory.HyperbolicConvolution (diff, hyper, mul, sigma)
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (word2Int)
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
sigmaTests =
  testGroup
    "sigma"
    [ testCase "sigma id" $
        map (sigma id) [1 .. 10]
          @?= [1, 3, 6, 10, 15, 21, 28, 36, 45, 55],
      testCase "sigma (hyper 10 id)" $ do
        sigma (hyper 10 id) 10 @?= 27
        sigma (hyper 10 id) 5 @?= 22
        sigma (hyper 5 id) 5 @?= 10
    ]

diffTests :: TestTree
diffTests =
  testGroup
    "diff"
    [ testCase "diff id" $
        map (diff id) [1 .. 10]
          @?= replicate 10 1,
      testCase "diff (hyper n word2Int)" $
        map (diff (hyper 10 word2Int)) [1 .. 10]
          @?= [10, -5, -2, -1, 0, -1, 0, 0, 0, 0]
    ]

square :: (Integral a) => a -> a
square x = x * x

mulTests :: TestTree
mulTests =
  testGroup
    "mul"
    [ testCase "mul id id" $
        map (mul id id) [1 .. 10]
          @?= map square [1 .. 10]
    ]

--
-- Computing hyperbolic convolutions
--

hyperConvolveTests :: TestTree
hyperConvolveTests = todoTest "hyperConvolve"

hyperConvolveFastTests :: TestTree
hyperConvolveFastTests = todoTest "hyperConvolveFast"

hyperConvolveMobiusFastTests :: TestTree
hyperConvolveMobiusFastTests = todoTest "hyperConvolveMobiusFast"
