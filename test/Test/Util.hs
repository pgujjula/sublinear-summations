-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

module Test.Util (todoTest, todoCode) where

import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase)

todoTest :: (HasCallStack) => String -> TestTree
todoTest message =
  expectFailBecause "unimplemented test" $
    testCase message (assertFailure "")

todoCode :: (HasCallStack) => TestTree -> TestTree
todoCode = expectFailBecause "unimplemented code"
