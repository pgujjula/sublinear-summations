-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper.Internal (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Data.Vector.Generic ((!))
import Math.NumberTheory.MemoHyper.Internal (numSquarefreeVec, sumSquarefreeVec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Util (todoCode)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.MemoHyper.Internal"
    [ numSquarefreeVecTests,
      sumSquarefreeVecTests
    ]

numSquarefreeVecTests :: TestTree
numSquarefreeVecTests =
  todoCode $
    testCase "numSquarefreeVec" $ do
      forM_ [0 .. 30] $ \n ->
        forM_ [n .. 30] $ \m ->
          let v = numSquarefreeVec (fromIntegral n) (fromIntegral m)
           in forM_ [n .. m] $ \i ->
                assertEqual (show i) (v ! (i - n)) (numSquarefreeNaive i)

sumSquarefreeVecTests :: TestTree
sumSquarefreeVecTests =
  todoCode $
    testCase "sumSquarefreeVec" $ do
      forM_ [0 .. 30] $ \n ->
        forM_ [n .. 30] $ \m ->
          let v = sumSquarefreeVec (fromIntegral n) (fromIntegral m)
           in forM_ [n .. m] $ \i ->
                assertEqual (show i) (v ! (i - n)) (sumSquarefreeNaive i)

--
-- Utilities
--
numSquarefreeNaive :: (Integral a) => a -> a
numSquarefreeNaive n = genericLength (filter isSquarefreeNaive [1 .. n])

sumSquarefreeNaive :: (Integral a) => a -> a
sumSquarefreeNaive n = sum (filter isSquarefreeNaive [1 .. n])

isSquarefreeNaive :: (Integral a) => a -> Bool
isSquarefreeNaive = not . isSquarefulNaive

isSquarefulNaive :: (Integral a) => a -> Bool
isSquarefulNaive n = any (`divides` n) (takeWhile (<= n) squares)

squares :: (Integral a) => [a]
squares = map (^ (2 :: Int)) [2 ..]

divides :: (Integral a) => a -> a -> Bool
divides a b = (b `rem` a) == 0
