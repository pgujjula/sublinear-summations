-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.SublinearSummation.Util (tests) where

import Control.Monad (forM_)
import SublinearSummation.Util (primes, primesFrom)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "SublinearSummation.Util"
    [primesTests, primesFromTests]

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
