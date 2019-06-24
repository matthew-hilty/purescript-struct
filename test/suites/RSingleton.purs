module Test.Suites.Data.Struct.RSingleton
  ( suites
  ) where

import Data.Struct.Singleton (rsingleton)
import Data.Symbol (SProxy(SProxy))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

import Prelude (discard)

suites :: TestSuite
suites =
  suite "rsingleton" do
    test "#0" do
      rsingleton (SProxy :: SProxy "a0") 0 `shouldEqual` { a0: 0 }
    test "#1" do
      let sproxy = SProxy :: SProxy " a . 1 "
      let value = [0, 1, 2, 3]
      rsingleton sproxy value `shouldEqual` { " a . 1 ": value }
