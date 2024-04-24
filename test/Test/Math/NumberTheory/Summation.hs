-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Summation (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Math.NumberTheory.Summation (sumNumDivisors)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
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
divides :: (Integral a) => a -> a -> Bool
divides a b = b `rem` a == 0

numDivisorsNaive :: (Integral a) => a -> a
numDivisorsNaive n = genericLength (filter (`divides` n) [1 .. n])

sumNumDivisorsNaive :: (Integral a) => a -> a
sumNumDivisorsNaive n = sum (map numDivisorsNaive [1 .. n])

sumNumDivisorsTests :: TestTree
sumNumDivisorsTests =
  testCase "sumNumDivisors" $ do
    forM_ [(-10 :: Int) .. 100] $ \n ->
      assertEqual (show n) (sumNumDivisors n) (sumNumDivisorsNaive n)

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
