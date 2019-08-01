module Test.Suites.Data.Struct.RCompare
  ( suites
  ) where

import Prelude (discard, negate, Ordering(EQ, GT, LT))

import Data.Struct.Compare (rcompare)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rcompare" do
    suite "Unmixed" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        rcompare l { a: 0 } { a: 0 } `shouldEqual` EQ
      test "#1" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        rcompare l { a: 1 } { a: 0 } `shouldEqual` GT
      test "#2" do
        let l = RLProxy :: RLProxy (Cons "a" Int Nil)
        rcompare l { a: 0 } { a: 1 } `shouldEqual` LT
      test "#3" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 0, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` EQ
      test "#4" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 1, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#5" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 0, a1: 2 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#6" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 0, a1: 2 } { a0: 0, a1: 1 } `shouldEqual` GT
      test "#7" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: -1, a1: 1 } { a0: 0, a1: 1 } `shouldEqual` LT
      test "#8" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 0, a1: 0 } { a0: 0, a1: 1 } `shouldEqual` LT
      test "#9" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: -1, a1: 0 } { a0: 0, a1: 1 } `shouldEqual` LT
    suite "Mixed -- Ordering depends on first field of RowList representation" do
      test "#0" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 0, a1: 2 } { a0: 1, a1: 1 } `shouldEqual` LT
      test "#1" do
        let l = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        rcompare l { a0: 2, a1: 0 } { a0: 1, a1: 1 } `shouldEqual` GT
