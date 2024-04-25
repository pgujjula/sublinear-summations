-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Mobius (tests) where

import Control.Monad (forM_)
import Data.Chimera qualified as Chimera
import Data.Vector.Generic ((!))
import Data.Vector.Unboxed qualified as Vector
import Math.NumberTheory.Mobius
  ( mertensChimera,
    mertensVec,
    mobiusChimera,
    mobiusVec,
    mobiusVec',
  )
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
      mertensChimeraTests,
      mertensVecTests
    ]

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
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = mobiusVec (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (mobiusNaive i)

mobiusVec'Tests :: TestTree
mobiusVec'Tests =
  testCase "mobiusVec'" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = mobiusVec' (fromIntegral n) (fromIntegral m)
         in forM_ [n .. m] $ \i ->
              assertEqual (show i) (v ! (i - n)) (mobiusNaive i)

mertensNaive :: (Integral a) => a -> a
mertensNaive n = sum (map mobiusNaive [1 .. n])

mertensChimeraTests :: TestTree
mertensChimeraTests =
  testCase "mertensChimera" $ do
    forM_ [0 .. 100] $ \i ->
      assertEqual
        ("i == " ++ show i)
        (mertensNaive (word2Int i))
        (Chimera.index mertensChimera i)

mertensVecTests :: TestTree
mertensVecTests =
  testCase "mertensVec" $ do
    forM_ [0 .. 30] $ \n ->
      forM_ [n .. 30] $ \m ->
        let v = mertensVec (fromIntegral n) (fromIntegral m)
            vNaive = Vector.fromListN (m - n + 1) $ map mertensNaive [n .. m]
         in assertEqual ("n == " ++ show n ++ ", m == " ++ show m) vNaive v
