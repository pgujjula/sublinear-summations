-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper (tests) where

import Control.Monad (forM_)
import Math.NumberTheory.MemoHyper (UMemoHyper, memoHyperMertens, unMemoHyper)
import SublinearSummation.Util (primes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util (todoCode, todoTest)

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
memoHyperMertensTests =
  todoCode $
    testCase "memoHyperMertens" $
      forM_ [1 .. 100] $ \n ->
        let mh :: UMemoHyper Int
            mh = memoHyperMertens n
         in forM_ [1 .. n] $ \i ->
              unMemoHyper mh i @?= mertensNaive (fromIntegral (n `quot` i))

memoHyperNumSquarefreeTests :: TestTree
memoHyperNumSquarefreeTests = todoTest "memoHyperNumSquarefree"

memoHyperSumSquarefreeTests :: TestTree
memoHyperSumSquarefreeTests = todoTest "memoHyperSumSquarefree"

--
-- Utilities
--
mertensNaive :: (Integral a) => a -> a
mertensNaive n = sum (map mobiusNaive [1 .. n])

isSquarefulNaive :: (Integral a) => a -> Bool
isSquarefulNaive n = any (`divides` n) (takeWhile (<= n) squares)

squares :: (Integral a) => [a]
squares = map (^ (2 :: Int)) [2 ..]

divides :: (Integral a) => a -> a -> Bool
divides a b = (b `rem` a) == 0

mobiusNaive :: (Integral a) => a -> a
mobiusNaive 0 = 0
mobiusNaive n =
  if isSquarefulNaive n
    then 0
    else
      let ps = takeWhile (<= n) (map fromIntegral primes)
          primeDivisors = filter (`divides` n) ps
       in if even (length primeDivisors)
            then 1
            else -1
