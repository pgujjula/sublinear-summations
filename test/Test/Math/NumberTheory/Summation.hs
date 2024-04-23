-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Summation (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Util (todoTest)

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
sumNumDivisorsTests = todoTest "sumNumDivisors"

sumSumDivisorsTests :: TestTree
sumSumDivisorsTests = todoTest "sumSumDivisors"

sumTotientTests :: TestTree
sumTotientTests = todoTest "sumTotient"

--
-- Primes
--

primePiTests :: TestTree
primePiTests = todoTest "primePi"

primeSumTests :: TestTree
primeSumTests = todoTest "primeSum"

--
-- Square-free integers
--

mertensTests :: TestTree
mertensTests = todoTest "mertens"

numSquarefreeTests :: TestTree
numSquarefreeTests = todoTest "numSquarefree"

sumSquarefreeTests :: TestTree
sumSquarefreeTests = todoTest "sumSquarefree"
