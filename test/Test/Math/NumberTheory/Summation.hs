-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Summation (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Math.NumberTheory.Summation
  ( numSquarefree,
    sumNumDivisors,
    sumSquarefree,
    sumSumDivisors,
    sumTotient,
  )
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

sumDivisorsNaive :: (Integral a) => a -> a
sumDivisorsNaive n = sum (filter (`divides` n) [1 .. n])

sumSumDivisorsNaive :: (Integral a) => a -> a
sumSumDivisorsNaive n = sum (map sumDivisorsNaive [1 .. n])

sumSumDivisorsTests :: TestTree
sumSumDivisorsTests =
  testCase "sumSumDivisors" $ do
    forM_ [(-10 :: Int) .. 100] $ \n ->
      assertEqual (show n) (sumSumDivisors n) (sumSumDivisorsNaive n)

relativelyPrime :: (Integral a) => a -> a -> Bool
relativelyPrime a b = gcd a b == 1

totientNaive :: (Integral a) => a -> a
totientNaive n = genericLength (filter (relativelyPrime n) [1 .. n])

sumTotientNaive :: (Integral a) => a -> a
sumTotientNaive n = sum (map totientNaive [1 .. n])

sumTotientTests :: TestTree
sumTotientTests =
  testCase "sumTotient" $ do
    forM_ [(-10 :: Int) .. 100] $ \n ->
      assertEqual (show n) (sumTotient n) (sumTotientNaive n)

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

isSquarefreeNaive :: (Integral a) => a -> Bool
isSquarefreeNaive n =
  let squares = map (^ (2 :: Int)) [1 ..]
   in not (any (`divides` n) (takeWhile (<= n) (tail squares)))

numSquarefreeNaive :: (Integral a) => a -> a
numSquarefreeNaive n = genericLength (filter isSquarefreeNaive [1 .. n])

sumSquarefreeNaive :: (Integral a) => a -> a
sumSquarefreeNaive n = sum (filter isSquarefreeNaive [1 .. n])

numSquarefreeTests :: TestTree
numSquarefreeTests =
  testCase "numSquarefree" $ do
    forM_ [(-10 :: Int) .. 1000] $ \n ->
      assertEqual (show n) (numSquarefree n) (numSquarefreeNaive n)

sumSquarefreeTests :: TestTree
sumSquarefreeTests =
  testCase "sumSquarefree" $ do
    forM_ [(-10 :: Int) .. 1000] $ \n ->
      assertEqual (show n) (sumSquarefree n) (sumSquarefreeNaive n)
