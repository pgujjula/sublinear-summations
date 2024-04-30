-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Summation (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Math.NumberTheory.Summation
  ( mertens,
    numSquarefree,
    primeSum,
    sumNumDivisors,
    sumSquarefree,
    sumSumDivisors,
    sumTotient,
  )
import SublinearSummation.Util (primes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

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
        [ primeSumTests
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

primeSumTests :: TestTree
primeSumTests =
  testCase "primeSum" $ do
    forM_ [(1 :: Word) .. 100] $ \n ->
      assertEqual (show n) (primeSumNaive n) (primeSum n)

primeSumNaive :: (Integral a) => a -> a
primeSumNaive n = sum (filter isPrimeNaive [1 .. n])

isPrimeNaive :: (Integral a) => a -> Bool
isPrimeNaive n = length (filter (`divides` n) [1 .. n]) == 2

--
-- Square-free integers
--

mertensTests :: TestTree
mertensTests =
  testCase "mertens" $ do
    forM_ [(-10 :: Int) .. 100] $ \n ->
      assertEqual (show n) (mertens n) (mertensNaive n)

isSquarefulNaive :: (Integral a) => a -> Bool
isSquarefulNaive n = any (`divides` n) (takeWhile (<= n) squares)

squares :: (Integral a) => [a]
squares = map (^ (2 :: Int)) [2 ..]

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

mertensNaive :: (Integral a) => a -> a
mertensNaive n = sum (map mobiusNaive [1 .. n])

isSquarefreeNaive :: (Integral a) => a -> Bool
isSquarefreeNaive n =
  let nonOneSquares = map (^ (2 :: Int)) [2 ..]
   in not (any (`divides` n) (takeWhile (<= n) nonOneSquares))

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
