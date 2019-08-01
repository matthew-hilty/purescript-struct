module Test.Suites.Data.Struct.RGet
  ( suites
  ) where

import Prelude (discard)

import Data.Struct.Get (rget)
import Data.Symbol (SProxy(SProxy))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rget" do
    suite "Record" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value = { a: 0 }
        let fn = rget l (SProxy :: SProxy "a")
        fn value `shouldEqual` value.a
      test "#1" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value = { a0: 0, a1: 1 }
        let fn = rget l (SProxy :: SProxy "a1")
        fn value `shouldEqual` value.a1
