-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.MemoHyper (tests) where

import Control.Monad (forM_)
import Data.List (genericLength, genericTake)
import Data.Vector.Generic qualified as Vector
import Data.Vector.Unboxed qualified as U
import Math.NumberTheory.HyperbolicConvolution (hyperConvolve, sigma)
import Math.NumberTheory.MemoHyper
  ( MemoHyper (..),
    UMemoHyper,
    VMemoHyper,
    memoHyper,
    memoHyperHyperConvolve,
    memoHyperIntegerSquareRoot,
    memoHyperMertens,
    memoHyperNumSquarefree,
    memoHyperPrimePhi,
    memoHyperPrimePi,
    memoHyperPrimeSum,
    memoHyperRoughSum,
    memoHyperSigmaHyper,
    memoHyperSigmaMobiusHyper,
    memoHyperSumNumDivisors,
    memoHyperSumSquarefree,
    memoHyperSumSumDivisors,
    memoHyperSumTotient,
  )
import Math.NumberTheory.Mobius (mobius')
import Math.NumberTheory.Prime.Count (primePhi, primePi)
import Math.NumberTheory.Roots (integerSquareRoot)
import SublinearSummation.Util (primes, word2Int)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
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
          memoHyperSigmaMobiusHyperTests,
          memoHyperHyperConvolveTests
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
memoHyperSigmaHyperTests =
  testCase "memoHyperSigmaHyper" $
    forM_ [1000 .. 1000] $ \n -> do
      forM_ [("const 1", const 1), ("id", word2Int), ("(2*", (2 *) . word2Int)] $ \(fName, f) -> do
        let g :: Word -> Int
            g m = sum (flip map [1 .. m] $ \i -> f (m `quot` i))

            mh :: UMemoHyper Int
            mh = memoHyperSigmaHyper f (map g [1 ..]) n

            mhNaive :: UMemoHyper Int
            mhNaive = memoHyper g n
         in assertEqual (show (n, fName)) mhNaive mh

memoHyperSigmaMobiusHyperTests :: TestTree
memoHyperSigmaMobiusHyperTests =
  testCase "memoHyperSigmaMobiusHyper" $
    forM_ [1 .. 1000] $ \n -> do
      forM_ [("const 1", const 1), ("id", word2Int), ("(2*", (2 *) . word2Int)] $ \(fName, f) -> do
        let g :: Word -> Int
            g m = sum (flip map [1 .. m] $ \i -> mobius' i * f (m `quot` i))

            mh :: UMemoHyper Int
            mh = memoHyperSigmaMobiusHyper f (map g [1 ..]) n

            mhNaive :: UMemoHyper Int
            mhNaive = memoHyper g n
         in assertEqual (show (n, fName)) mhNaive mh

memoHyperHyperConvolveTests :: TestTree
memoHyperHyperConvolveTests =
  testCase "memoHyperHyperConvolve" $
    forM_ [1 .. 1000] $ \n -> do
      forM_
        [ ("mobius", mobius', const 1),
          ("mobius n * n", \x -> mobius' x * fromIntegral x, fromIntegral)
        ]
        $ \(fName, f, fInv :: Word -> Int) ->
          forM_
            [ ("const 1", const 1),
              ("2*", fromIntegral . (2 *))
            ]
            $ \(gName, g :: Word -> Int) -> do
              let h = hyperConvolve f g
              let mhNaive :: UMemoHyper Int
                  mhNaive = memoHyper h n
                  mh :: UMemoHyper Int
                  mh =
                    memoHyperHyperConvolve
                      (memoHyper (sigma fInv) n :: UMemoHyper Int)
                      g
                      (map h [1 ..])
                      n
              assertEqual (show (fName, gName, n)) mhNaive mh

--
-- MemoHyper for arithmetic functions
--

-- Divisor functions

memoHyperSumNumDivisorsTests :: TestTree
memoHyperSumNumDivisorsTests =
  testCase "memoHyperSumNumDivisors" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperSumNumDivisors n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (sumNumDivisorsNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map
                          (sumNumDivisorsNaive . fromIntegral . (n `quot`))
                          [1 .. sq]
                  }
       in mh @?= mhNaive

memoHyperSumSumDivisorsTests :: TestTree
memoHyperSumSumDivisorsTests =
  testCase "memoHyperSumSumDivisors" $
    forM_ [1 .. 100] $ \n ->
      let mh :: UMemoHyper Int
          mh = memoHyperSumSumDivisors n

          mhNaive :: UMemoHyper Int
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (sumSumDivisorsNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map
                          (sumSumDivisorsNaive . fromIntegral . (n `quot`))
                          [1 .. sq]
                  }
       in mh @?= mhNaive

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
memoHyperPrimeSumTests =
  testCase "memoHyperPrimeSum" $
    forM_ [1 .. 1000] $ \n ->
      let mh :: UMemoHyper Word
          mh = memoHyperPrimeSum n
          mhNaive :: UMemoHyper Word
          mhNaive =
            let sq = integerSquareRoot n
             in MemoHyper
                  { mhLimit = n,
                    mhSqrtLimit = integerSquareRoot n,
                    mhFuncVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (primeSumNaive . fromIntegral) [1 .. sq],
                    mhHyperVec =
                      Vector.fromListN (fromIntegral sq) $
                        map (primeSumNaive . fromIntegral . (n `quot`)) [1 .. sq]
                  }
       in mh @?= mhNaive

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

sumNumDivisorsNaive :: (Integral a) => a -> a
sumNumDivisorsNaive n = sum (map numDivisorsNaive [1 .. n])

sumSumDivisorsNaive :: (Integral a) => a -> a
sumSumDivisorsNaive n = sum (map sumDivisorsNaive [1 .. n])

numDivisorsNaive :: (Integral a) => a -> a
numDivisorsNaive n = genericLength (filter (`divides` n) [1 .. n])

sumDivisorsNaive :: (Integral a) => a -> a
sumDivisorsNaive n = sum (filter (`divides` n) [1 .. n])

primeSumNaive :: (Integral a) => a -> a
primeSumNaive n = sum (filter isPrimeNaive [1 .. n])

isPrimeNaive :: (Integral a) => a -> Bool
isPrimeNaive n = length (filter (`divides` n) [1 .. n]) == 2

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
