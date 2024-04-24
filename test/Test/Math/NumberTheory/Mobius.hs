-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Math.NumberTheory.Mobius (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Util (todoTest)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Mobius"
    [ mobiusTests,
      mertensTests
    ]

mobiusTests :: TestTree
mobiusTests = todoTest "mobius"

mertensTests :: TestTree
mertensTests = todoTest "mertens"
