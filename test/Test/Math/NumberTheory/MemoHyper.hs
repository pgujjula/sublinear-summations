-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper (tests) where

import Control.Monad (forM_)
import Data.List (genericLength)
import Data.Vector.Generic qualified as Vector
import Math.NumberTheory.MemoHyper
  ( MemoHyper (..),
    UMemoHyper,
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperPrimePi,
    memoHyperSumSquarefree,
  )
import Math.NumberTheory.Prime.Count (primePi)
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (primes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util (todoCode, todoTest)

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
              memoHyperPrimeSumTests
            ],
          testGroup
            "Square-free integers"
            [ memoHyperMertensTests,
              memoHyperNumSquarefreeTests,
              memoHyperSumSquarefreeTests
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
memoHyperSumTotientTests = todoTest "memoHyperSumTotient"

-- Primes

memoHyperPrimePiTests :: TestTree
memoHyperPrimePiTests =
  todoCode . testCase "memoHyperPrimePi" $
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

memoHyperPrimeSumTests :: TestTree
memoHyperPrimeSumTests = todoTest "memoHyperPrimeSum"

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

--
-- Utilities
--
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
