module Test.Suites.Data.Struct.RDisjointUnion
  ( suites
  ) where

import Prelude (discard)

import Data.Struct.DisjointUnion (rdisjointUnion)
import Record.Builder (build)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.RowList (Cons, Nil, RLProxy(RLProxy))

suites :: TestSuite
suites =
  suite "rdisjointUnion" do
    suite "Builder" do
      test "#0" do
        let l = RLProxy :: RLProxy Nil
        let value = {}
        let builder = rdisjointUnion l l l value
        build builder value `shouldEqual` value
      test "#1" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value0 = {}
        let value1 = { a: 0 }
        let builder = rdisjointUnion l0 l1 l1 value0
        build builder value1 `shouldEqual` value1
      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value0 = { a0: 0 }
        let value1 = { a1: 1 }
        let builder = rdisjointUnion l0 l1 l2 value0
        build builder value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
    suite "Function" do
      test "#0" do
        let l = RLProxy :: RLProxy Nil
        let value = {}
        let fn = rdisjointUnion l l l value
        fn value `shouldEqual` value
      test "#1" do
        let l0 = RLProxy :: RLProxy Nil
        let l1 = RLProxy :: RLProxy (Cons "a" Int Nil)
        let value0 = {}
        let value1 = { a: 0 }
        let fn = rdisjointUnion l0 l1 l1 value0
        fn value1 `shouldEqual` value1
      test "#2" do
        let l0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let l1 = RLProxy :: RLProxy (Cons "a1" Int Nil)
        let l2 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value0 = { a0: 0 }
        let value1 = { a1: 1 }
        let fn = rdisjointUnion l0 l1 l2 value0
        fn value1 `shouldEqual` { a0: value0.a0, a1: value1.a1 }
