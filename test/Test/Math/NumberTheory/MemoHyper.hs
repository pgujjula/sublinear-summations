-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Util (todoTest)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.MemoHyper"
    [ testGroup
        "Index MemoHyper"
        [ unMemoHyperTests,
          unMemoSmallTests
        ],
      testGroup
        "Construct MemoHypers"
        [ memoHyperTests,
          memoHyperDirectTests,
          memoHyperSigmaHyperTests,
          memoHyperSigmaMobiusHyperTests
        ],
      testGroup
        "MemoHyper for arithmetic functions"
        [ testGroup
            "Divisor functions"
            [ memoHyperSumNumDivisorsTests,
              memoHyperSumSumDivisorsTests,
              memoHyperSumTotientTests
            ],
          testGroup
            "Primes"
            [ memoHyperPrimePiTests,
              memoHyperPrimeSumTests
            ],
          testGroup
            "Square-free integers"
            [ memoHyperMertensTests,
              memoHyperNumSquarefreeTests,
              memoHyperSumSquarefreeTests
            ]
        ]
    ]

--
-- Index MemoHyper
--

unMemoHyperTests :: TestTree
unMemoHyperTests = todoTest "unMemoHyper"

unMemoSmallTests :: TestTree
unMemoSmallTests = todoTest "unMemoSmall"

--
-- Construct MemoHyper
--

memoHyperTests :: TestTree
memoHyperTests = todoTest "memoHyper"

memoHyperDirectTests :: TestTree
memoHyperDirectTests = todoTest "memoHyperDirect"

memoHyperSigmaHyperTests :: TestTree
memoHyperSigmaHyperTests = todoTest "memoHyperSigmaHyper"

memoHyperSigmaMobiusHyperTests :: TestTree
memoHyperSigmaMobiusHyperTests = todoTest "memoHyperSigmaMobiusHyper"

--
-- MemoHyper for arithmetic functions
--

-- Divisor functions

memoHyperSumNumDivisorsTests :: TestTree
memoHyperSumNumDivisorsTests = todoTest "memoHyperSumNumDivisors"

memoHyperSumSumDivisorsTests :: TestTree
memoHyperSumSumDivisorsTests = todoTest "memoHyperSumSumDivisors"

memoHyperSumTotientTests :: TestTree
memoHyperSumTotientTests = todoTest "memoHyperSumTotient"

-- Primes

memoHyperPrimePiTests :: TestTree
memoHyperPrimePiTests = todoTest "memoHyperPrimePi"

memoHyperPrimeSumTests :: TestTree
memoHyperPrimeSumTests = todoTest "memoHyperPrimeSum"

-- Square-free integers

memoHyperMertensTests :: TestTree
memoHyperMertensTests = todoTest "memoHyperMertens"

memoHyperNumSquarefreeTests :: TestTree
memoHyperNumSquarefreeTests = todoTest "memoHyperNumSquarefree"

memoHyperSumSquarefreeTests :: TestTree
memoHyperSumSquarefreeTests = todoTest "memoHyperSumSquarefree"
