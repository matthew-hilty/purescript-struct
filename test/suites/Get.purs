module Test.Suites.Data.Struct.Get.Get
  ( suites
  ) where

import Prelude (discard)

import Data.Struct.Get (get)
import Data.Symbol (SProxy(SProxy))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "get" do
    suite "Record" do
      test "#0" do
        let value = { a: 0 }
        let fn = get (SProxy :: SProxy "a")
        fn value `shouldEqual` value.a
      test "#1" do
        let value = { a0: 0, a1: 1 }
        let fn = get (SProxy :: SProxy "a1")
        fn value `shouldEqual` value.a1
