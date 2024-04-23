-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.HyperbolicConvolution (tests) where

import Test.Tasty (TestTree, testGroup)
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
hyperTests = todoTest "hyper"

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
