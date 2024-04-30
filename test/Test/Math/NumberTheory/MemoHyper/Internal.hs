-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper.Internal (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Data.Vector.Generic ((!))
import Math.NumberTheory.MemoHyper.Internal
  ( numDivisorsVec,
    numSquarefreeVec,
    sumDivisorsVec,
    sumSquarefreeVec,
    sumTotientVec,
    totientVec,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.MemoHyper.Internal"
    [ numDivisorsVecTests,
      sumDivisorsVecTests,
      numSquarefreeVecTests,
      sumSquarefreeVecTests,
      totientVecTests,
      sumTotientVecTests
    ]

numDivisorsVecTests :: TestTree
numDivisorsVecTests =
  testCase "numDivisorsVec" $ do
    forM_ [1 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = numDivisorsVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show (n, m, i)) (v ! (i - n)) (numDivisorsNaive i)

sumDivisorsVecTests :: TestTree
sumDivisorsVecTests =
  testCase "sumDivisorsVec" $ do
    forM_ [1 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = sumDivisorsVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (sumDivisorsNaive i)

numSquarefreeVecTests :: TestTree
numSquarefreeVecTests =
  testCase "numSquarefreeVec" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = numSquarefreeVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (numSquarefreeNaive i)

sumSquarefreeVecTests :: TestTree
sumSquarefreeVecTests =
  testCase "sumSquarefreeVec" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = sumSquarefreeVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (sumSquarefreeNaive i)

totientVecTests :: TestTree
totientVecTests =
  testCase "totientVec" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = totientVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show (n, m, i)) (v ! (i - n)) (totientNaive i)

sumTotientVecTests :: TestTree
sumTotientVecTests =
  testCase "sumTotientVec" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = sumTotientVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show (n, m, i)) (v ! (i - n)) (sumTotientNaive i)

--
-- Utilities
--
numDivisorsNaive :: (Integral a) => a -> a
numDivisorsNaive n = genericLength (filter (`divides` n) [1 .. n])

sumDivisorsNaive :: (Integral a) => a -> a
sumDivisorsNaive n = sum (filter (`divides` n) [1 .. n])

numSquarefreeNaive :: (Integral a) => a -> a
numSquarefreeNaive n = genericLength (filter isSquarefreeNaive [1 .. n])

sumSquarefreeNaive :: (Integral a) => a -> a
sumSquarefreeNaive n = sum (filter isSquarefreeNaive [1 .. n])

sumTotientNaive :: (Integral a) => a -> a
sumTotientNaive n = sum (map totientNaive [1 .. n])

totientNaive :: (Integral a) => a -> a
totientNaive n = genericLength (filter (\x -> gcd n x == 1) [1 .. n])

isSquarefreeNaive :: (Integral a) => a -> Bool
isSquarefreeNaive = not . isSquarefulNaive

isSquarefulNaive :: (Integral a) => a -> Bool
isSquarefulNaive n = any (`divides` n) (takeWhile (<= n) squares)

squares :: (Integral a) => [a]
squares = map (^ (2 :: Int)) [2 ..]

divides :: (Integral a) => a -> a -> Bool
divides a b = (b `rem` a) == 0
