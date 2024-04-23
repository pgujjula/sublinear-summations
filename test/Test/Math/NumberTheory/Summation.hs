-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Summation (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Summation"
    [ testGroup
        "Divisor functions"
        [ sumNumDivisorsTests,
          sumSumDivisorsTests,
          sumTotientTests
        ],
      testGroup
        "Primes"
        [ primePiTests,
          primeSumTests
        ],
      testGroup
        "Square-free integers"
        [ mertensTests,
          numSquarefreeTests,
          sumSquarefreeTests
        ]
    ]

--
-- Divisor functions
--

sumNumDivisorsTests :: TestTree
sumNumDivisorsTests = undefined

sumSumDivisorsTests :: TestTree
sumSumDivisorsTests = undefined

sumTotientTests :: TestTree
sumTotientTests = undefined

--
-- Primes
--

primePiTests :: TestTree
primePiTests = undefined

primeSumTests :: TestTree
primeSumTests = undefined

--
-- Square-free integers
--

mertensTests :: TestTree
mertensTests = undefined

numSquarefreeTests :: TestTree
numSquarefreeTests = undefined

sumSquarefreeTests :: TestTree
sumSquarefreeTests = undefined
