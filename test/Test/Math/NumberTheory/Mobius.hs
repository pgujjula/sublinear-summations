-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Mobius (tests) where

import Control.Monad (forM_)
import Data.Vector.Generic ((!))
import Math.NumberTheory.Mobius (mobiusVec, mobiusVec', mobiusChimera)
import Data.Chimera qualified as Chimera
import SublinearSummation.Util (primes, word2Int)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Test.Util (todoTest)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Mobius"
    [ mobiusTests,
      mobiusChimeraTests,
      mobiusVecTests,
      mobiusVec'Tests,
      mertensTests
    ]

isSquarefulNaive :: (Integral a) => a -> Bool
isSquarefulNaive n = any (`divides` n) (takeWhile (<= n) squares)

squares :: (Integral a) => [a]
squares = map (^ (2 :: Int)) [2 ..]

divides :: (Integral a) => a -> a -> Bool
divides a b = (b `rem` a) == 0

mobiusNaive :: (Integral a) => a -> a
mobiusNaive n =
  if isSquarefulNaive n
    then 0
    else
      let ps = takeWhile (<= n) (map fromIntegral primes)
          primeDivisors = filter (`divides` n) ps
       in if even (length primeDivisors)
            then 1
            else -1

mobiusChimeraTests :: TestTree
mobiusChimeraTests =
  testCase "mobiusChimera" $ do
    forM_ [1 .. 100] $ \i ->
      Chimera.index mobiusChimera i @?= mobiusNaive (word2Int i)

mobiusTests :: TestTree
mobiusTests = todoTest "mobius"

mobiusVecTests :: TestTree
mobiusVecTests =
  testCase "mobiusVec" $ do
    forM_ [1 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = mobiusVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (mobiusNaive i)

mobiusVec'Tests :: TestTree
mobiusVec'Tests =
  testCase "mobiusVec'" $ do
    forM_ [1 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = mobiusVec' (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (mobiusNaive i)

mertensTests :: TestTree
mertensTests = todoTest "mertens"
