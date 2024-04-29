-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.SublinearSummation.Util (tests) where

import Control.Monad (forM_)
import Data.Vector.Generic ((!))
import SublinearSummation.Util
  ( int2Word,
    primeSumVec,
    primes,
    primesFrom,
    word2Int,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "SublinearSummation.Util"
    [primesTests, primesFromTests, primeSumVecTests]

divides :: (Integral a) => a -> a -> Bool
divides a b = b `rem` a == 0

isPrimeNaive :: Word -> Bool
isPrimeNaive n = length (filter (`divides` n) [1 .. n]) == 2

primesTests :: TestTree
primesTests =
  testCase "primes" $
    take 100 primes @?= take 100 (filter isPrimeNaive [1 ..])

primesFromTests :: TestTree
primesFromTests =
  testCase "primesFrom" $ do
    forM_ [1 .. 30] $ \start ->
      take 100 (primesFrom start) @?= take 100 (filter isPrimeNaive [start ..])

primeSumVecTests :: TestTree
primeSumVecTests =
  testCase "primeSumVec" $ do
    forM_ [0 .. 30] $ \n ->
      let v = primeSumVec n
          n' :: Int
          n' = word2Int n
       in forM_ [0 .. n'] $ \i' ->
            assertEqual
              (show i')
              (v ! i')
              (primeSumNaive (int2Word i'))

primeSumNaive :: Word -> Word
primeSumNaive n = sum (filter isPrimeNaive [1 .. n])
