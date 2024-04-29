-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper (tests) where

import Control.Monad (forM_)
import Data.List (genericLength, genericTake)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.MemoHyper
  ( MemoHyper (..),
    UMemoHyper,
    VMemoHyper,
    memoHyperIntegerSquareRoot,
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperPrimePhi,
    memoHyperPrimePi,
    memoHyperRoughSum,
    memoHyperSumSquarefree,
    memoHyperSumTotient,
  )
import Math.NumberTheory.Prime.Count (primePhi, primePi)
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (primes, word2Int)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util (todoTest)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.MemoHyper"
    [ testGroup
        "Index MemoHyper"
        [ unMemoHyperTests,
          unMemoSmallTests
        ],
      testGroup
        "Construct MemoHypers"
        [ memoHyperTests,
          memoHyperDirectTests,
          memoHyperSigmaHyperTests,
          memoHyperSigmaMobiusHyperTests
        ],
      testGroup
        "MemoHyper for arithmetic functions"
        [ testGroup
            "Divisor functions"
            [ memoHyperSumNumDivisorsTests,
              memoHyperSumSumDivisorsTests,
              memoHyperSumTotientTests
            ],
          testGroup
            "Primes"
            [ memoHyperPrimePiTests,
              memoHyperPrimePhiTests,
              memoHyperPrimeSumTests,
              memoHyperRoughSumTests
            ],
          testGroup
            "Square-free integers"
            [ memoHyperMertensTests,
              memoHyperNumSquarefreeTests,
              memoHyperSumSquarefreeTests
            ],
          testGroup
            "Miscellaneous"
            [ memoHyperIntegerSquareRootTests
            ]
        ]
    ]

--
-- Index MemoHyper
--

unMemoHyperTests :: TestTree
unMemoHyperTests = todoTest "unMemoHyper"

unMemoSmallTests :: TestTree
unMemoSmallTests = todoTest "unMemoSmall"

--
-- Construct MemoHyper
--

memoHyperTests :: TestTree
memoHyperTests = todoTest "memoHyper"

memoHyperDirectTests :: TestTree
memoHyperDirectTests = todoTest "memoHyperDirect"

memoHyperSigmaHyperTests :: TestTree
memoHyperSigmaHyperTests = todoTest "memoHyperSigmaHyper"

memoHyperSigmaMobiusHyperTests :: TestTree
memoHyperSigmaMobiusHyperTests = todoTest "memoHyperSigmaMobiusHyper"

--
-- MemoHyper for arithmetic functions
--

-- Divisor functions

memoHyperSumNumDivisorsTests :: TestTree
memoHyperSumNumDivisorsTests = todoTest "memoHyperSumNumDivisors"

memoHyperSumSumDivisorsTests :: TestTree
memoHyperSumSumDivisorsTests = todoTest "memoHyperSumSumDivisors"

memoHyperSumTotientTests :: TestTree
memoHyperSumTotientTests =
  testCase "memoHyperSumTotient" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperSumTotient n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (sumTotientNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map
                          (sumTotientNaive . fromIntegral . (n `quot`))
                          [1 .. sq]
                  }
       in mh @?= mhNaive

-- Primes

memoHyperPrimePiTests :: TestTree
memoHyperPrimePiTests =
  testCase "memoHyperPrimePi" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Word
          mh = memoHyperPrimePi n
          mhNaive :: UMemoHyper Word
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (primePi . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (primePi . fromIntegral . (n `quot`)) [1 .. sq]
                  }
       in mh @?= mhNaive

memoHyperPrimePhiTests :: TestTree
memoHyperPrimePhiTests =
  testCase "memoHyperPrimePhi" $
    forM_ [1 .. 100] $ \n ->
      let mh :: VMemoHyper (U.Vector Int)
          mh = memoHyperPrimePhi n

          mhNaive :: VMemoHyper (U.Vector Int)
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        flip map [1 .. sq] $ \i ->
                          let b = primePi (integerSquareRoot i)
                           in Vector.fromListN (word2Int b + 1) $
                                flip map [0 .. b] $ \j ->
                                  word2Int (primePhi i j),
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        flip map [1 .. sq] $ \i ->
                          let nqi = n `quot` i
                              b = primePi (integerSquareRoot nqi)
                           in Vector.fromListN (word2Int b + 1) $
                                flip map [0 .. b] $ \j ->
                                  word2Int (primePhi nqi j)
                  }
       in mh @?= mhNaive

memoHyperPrimeSumTests :: TestTree
memoHyperPrimeSumTests = todoTest "memoHyperPrimeSum"

memoHyperRoughSumTests :: TestTree
memoHyperRoughSumTests =
  testCase "memoHyperRoughSum" $
    forM_ [1 .. 1000] $ \n ->
      let mh :: VMemoHyper (U.Vector Int)
          mh = memoHyperRoughSum n

          mhNaive :: VMemoHyper (U.Vector Int)
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        flip map [1 .. sq] $ \i ->
                          let b = primePi (integerSquareRoot i)
                           in Vector.fromListN (word2Int b + 1) $
                                flip map [0 .. b] $ \j ->
                                  word2Int (roughSumNaive i j),
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        flip map [1 .. sq] $ \i ->
                          let nqi = n `quot` i
                              b = primePi (integerSquareRoot nqi)
                           in Vector.fromListN (word2Int b + 1) $
                                flip map [0 .. b] $ \j ->
                                  word2Int (roughSumNaive nqi j)
                  }
       in mh @?= mhNaive

-- Square-free integers

memoHyperMertensTests :: TestTree
memoHyperMertensTests =
  testCase "memoHyperMertens" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperMertens n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (mertensNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (mertensNaive . fromIntegral . (n `quot`)) [1 .. sq]
                  }
       in mh @?= mhNaive

memoHyperNumSquarefreeTests :: TestTree
memoHyperNumSquarefreeTests =
  testCase "memoHyperNumSquarefree" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperNumSquarefree n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (numSquarefreeNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (numSquarefreeNaive . fromIntegral . (n `quot`)) [1 .. sq]
                  }
       in mh @?= mhNaive

memoHyperSumSquarefreeTests :: TestTree
memoHyperSumSquarefreeTests =
  testCase "memoHyperSumSquarefree" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperSumSquarefree n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (sumSquarefreeNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (sumSquarefreeNaive . fromIntegral . (n `quot`)) [1 .. sq]
                  }
       in mh @?= mhNaive

memoHyperIntegerSquareRootTests :: TestTree
memoHyperIntegerSquareRootTests =
  testCase "memoHyperIntegerSquareRoot" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperIntegerSquareRoot n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map
                          (integerSquareRoot . fromIntegral)
                          [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq)
                        . flip map [1 .. sq]
                        $ integerSquareRoot
                          . fromIntegral
                          . (n `quot`)
                  }
       in mh @?= mhNaive

--
-- Utilities
--

-- Sum of the numbers <= n that aren't divisible by the first k primes
roughSumNaive :: forall a. (Integral a) => a -> a -> a
roughSumNaive n k =
  let valid :: a -> Bool
      valid m =
        not (any (`divides` m) (genericTake k (map fromIntegral primes)))
   in sum (filter valid [1 .. n])

mertensNaive :: (Integral a) => a -> a
mertensNaive n = sum (map mobiusNaive [1 .. n])

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

relativelyPrime :: (Integral a) => a -> a -> Bool
relativelyPrime a b = gcd a b == 1

totientNaive :: (Integral a) => a -> a
totientNaive n = genericLength (filter (relativelyPrime n) [1 .. n])

sumTotientNaive :: (Integral a) => a -> a
sumTotientNaive n = sum (map totientNaive [1 .. n])
