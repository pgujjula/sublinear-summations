-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper.Internal (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Data.Vector.Generic ((!))
import Math.NumberTheory.MemoHyper.Internal
  ( numSquarefreeVec,
    sumSquarefreeVec,
    totientVec,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.MemoHyper.Internal"
    [ numSquarefreeVecTests,
      sumSquarefreeVecTests,
      totientVecTests
    ]

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

--
-- Utilities
--
numSquarefreeNaive :: (Integral a) => a -> a
numSquarefreeNaive n = genericLength (filter isSquarefreeNaive [1 .. n])

sumSquarefreeNaive :: (Integral a) => a -> a
sumSquarefreeNaive n = sum (filter isSquarefreeNaive [1 .. n])

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
